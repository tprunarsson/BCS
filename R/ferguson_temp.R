library(dplyr)
library(ggplot2)

infected_distr <- infections_predicted_per_date %>%
    group_by(date) %>%
    mutate(prob=count/sum(count))

length_of_stay_dat <- patient_transitions_state_blocks %>%
    group_by(patient_id,state_block_nr,state,state_next) %>%
    summarise(state_duration=sum(state_duration)) %>%
    group_by(patient_id) %>%
    mutate(home_before_hospital= state=='home' & state_block_nr==1 & any(state!='home')) %>%
    filter(state!='home' | home_before_hospital) %>%
    summarise(time_hospital=sum(state_duration[state!='home']),
              time_icu=sum(state_duration[state=='intensive_care_unit']),
              time_before_icu=sum(state_duration[state=='inpatient_ward' & state_next=='intensive_care_unit']),
              time_home_before_hospital=sum(state_duration[state=='home'])) %>%
    mutate(time_icu=if_else(time_icu==0,NA_real_,time_icu)) %>%
    mutate(time_before_icu=if_else(time_before_icu==0,NA_real_,time_before_icu)) %>%
    summarise(median_time_hospital=median(time_hospital),
              median_time_icu=median(time_icu,na.rm=T),
              median_time_before_icu=median(time_before_icu,na.rm=T),
              median_time_before_hospital=median(time_home_before_hospital)) %>%
    pivot_longer(cols=names(.),names_to='type','values_to'='time') %>%
    mutate(time=round(time))

historical_hospital_data <- historical_data %>% 
    filter(state!='home') %>%
    group_by(date) %>% 
    summarise(hospital=sum(count),intensive_care_unit=count[state=='intensive_care_unit']) %>%
    pivot_longer(cols=c('hospital','intensive_care_unit'),names_to = 'state',values_to = 'count')

run_ferguson_simulation <- function(splitting_variable_name,infected_distr,transitions_location='wuhan',los_location='wuhan'){
    if(transitions_location=='wuhan'){
        transition_prob <- filter(prior_transitions,location=='wuhan') %>%
                            inner_join(select(age_groups,age_official,matches(paste0('^',splitting_variable_name,'$'))),by='age_official') %>%
                            rename(splitting_variable=!!splitting_variable_name) %>%
                            group_by(splitting_variable) %>%
                            summarise(p_hospital=sum(infected*inpatient_ward)/sum(infected),
                                      p_icu=sum(infected*intensive_care_unit)/sum(infected),
                                      p_death=sum(infected*deceased)/sum(infected))
    }else{
        transition_prob <- patient_transitions_state_blocks %>%
            inner_join(select(individs_splitting_variables,patient_id,matches(paste0('^',splitting_variable_name,'$'))),by='patient_id') %>%
            rename(splitting_variable=!!splitting_variable_name) %>%
            group_by(patient_id,splitting_variable) %>%
            summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
            group_by(splitting_variable) %>%
            summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
            ungroup() %>%
            mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu)))
    }

    splitting_distribution <- get_patient_transitions_at_date('base',date_observed = as.Date('2020-05-08')) %>%
                        distinct(patient_id) %>%
                        inner_join(individs_splitting_variables,by='patient_id') %>%
                        rename(splitting_variable=!!splitting_variable_name) %>%
                        group_by(splitting_variable) %>%
                        summarise(prop=n()) %>%
                        ungroup() %>%
                        mutate(prop=prop/sum(prop))
    
    dates <- seq(min(infected_distr$date), max(infected_distr$date),by=1)
    active_cases <- matrix(0,ncol=length(dates)+21,nrow=1000) 
    hospital_cases <- matrix(0,ncol=length(dates)+21,nrow=1000)
    icu_cases <- matrix(0,ncol=length(dates)+21,nrow=1000)
    for(i in 1:length(dates)){
        new_cases <- sample(infected_distr$new_cases[infected_distr$date==dates[i]],
                            size = 1000,
                            prob=infected_distr$prob[infected_distr$date==dates[i]],
                            replace = T)
        active_cases[,i:(i+21-1)] <-active_cases[,i:(i+21-1)] + matrix(rep(new_cases,21),ncol=21)
        splitting_new_cases <- sapply(new_cases,function(x){
            splitting_samples <- sample(1:nrow(splitting_distribution),size=x,replace=T,prob=splitting_distribution$prop)
            splitting_samples_summary <- rep(0,nrow(splitting_distribution))
            for(s in splitting_samples){
                splitting_samples_summary[s] <- splitting_samples_summary[s]+1 
            }
            return(splitting_samples_summary)
        }) %>% t()
        hospital_cases_per_splitting <- matrix(0,nrow=nrow(splitting_new_cases),ncol=ncol(splitting_new_cases))
        for(j in 1:ncol(splitting_new_cases)){
            hospital_cases_per_splitting[,j] <- rbinom(size=splitting_new_cases[,j],n=nrow(splitting_new_cases),prob=transition_prob$p_hospital[j])
        }
        hospital_cases[,(i+7):(i+21-1)] <- hospital_cases[,(i+7):(i+21-1)] + matrix(rep(rowSums(hospital_cases_per_splitting),14),ncol=14) 
        icu_cases_per_splitting <- matrix(0,nrow=nrow(hospital_cases_per_splitting),ncol=ncol(hospital_cases_per_splitting))
        for(j in 1:ncol(hospital_cases_per_splitting)){
            icu_cases_per_splitting[,j] <- rbinom(size=hospital_cases_per_splitting[,j],n=nrow(hospital_cases_per_splitting),prob=transition_prob$p_icu[j])
        }
        icu_cases[,(i+10):(i+10+10-1)] <- icu_cases[,(i+10):(i+10+10-1)] + matrix(rep(rowSums(icu_cases_per_splitting),10),ncol=10)
    }
    hospital_dat <- tibble(date=dates,
                           state='hospital',
                           median=apply(hospital_cases,2,quantile,probs=0.5)[1:length(dates)],
                           lower=apply(hospital_cases,2,quantile,probs=0.025)[1:length(dates)],
                           upper=apply(hospital_cases,2,quantile,probs=0.975)[1:length(dates)])
    icu_dat <- tibble(date=dates,
                      state='intensive_care_unit',
                      median=apply(icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                      lower=apply(icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                      upper=apply(icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    return(bind_rows(hospital_dat,icu_dat))
}


wuhan_fixed_LOS <- run_ferguson_simulation('age_official',infected_distr,transitions_location ='wuhan',los_location='wuhan')
#wuhan_data_LOS <- run_ferguson_simulation('age_official',infected_distr,data='Wuhan',length_of_stay='from_data')
iceland_age_official_fixed_LOS <- run_ferguson_simulation('age_official',infected_distr,transitions_location ='iceland',los_location='wuhan')



#iceland_age_simple_fixed_LOS <- run_ferguson_simulation('age_simple',infection_distr,data='Iceland',length_of_stay='fixed')
#iceland_age_official_data_LOS <- run_ferguson_simulation('age_official',infection_distr,data='Iceland',length_of_stay='from_data')
#iceland_age_simple_dataLOS <- run_ferguson_simulation('age_simple',infection_distr,data='Iceland',length_of_stay='from_data')

tot_dat <- bind_rows(wuhan_fixed_LOS,iceland_age_official_fixed_LOS,.id='model') %>% mutate(state=ifelse(state=="hospital", "inpatient_ward", state))

plot_ferguson_prediction <- function(prediction_dat,historical_dat){
    ggplot(prediction_dat) + 
        geom_line(aes(date,median,col=model)) + 
        geom_line(aes(date,lower,col=model),linetype='dashed') +
        geom_line(aes(date,upper,col=model),linetype='dashed') +
        geom_point(data=historical_dat,aes(date,count))+
        facet_wrap(~state,scales='free') 
}

# Bestunaraðferðir - Signý # ## # Ekki sourcað neinsstaðar, bara kóði til að besta LOS #

run_ferguson_simulation2 <- function(infected_distr, los){
    dates <- seq(min(infected_distr$date), max(infected_distr$date),by=1)
    active_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000) 
    hospital_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000)
    icu_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000)
    for(i in 1:length(dates)){
        new_cases <- sample(infected_distr$new_cases[infected_distr$date==dates[i]],
                            size = 1000,
                            prob=infected_distr$prob[infected_distr$date==dates[i]],
                            replace = T)
        active_cases[,i:(i+los$vd-1)] <-active_cases[,i:(i+los$vd-1)] + matrix(rep(new_cases,los$vd),ncol=los$vd)
        splitting_new_cases <- sapply(new_cases,function(x){
            splitting_samples <- sample(1:nrow(splitting_distribution),size=x,replace=T,prob=splitting_distribution$prop)
            splitting_samples_summary <- rep(0,nrow(splitting_distribution))
            for(s in splitting_samples){
                splitting_samples_summary[s] <- splitting_samples_summary[s]+1 
            }
            return(splitting_samples_summary)
        }) %>% t()
        hospital_cases_per_splitting <- matrix(0,nrow=nrow(splitting_new_cases),ncol=ncol(splitting_new_cases))
        for(j in 1:ncol(splitting_new_cases)){
            hospital_cases_per_splitting[,j] <- rbinom(size=splitting_new_cases[,j],n=nrow(splitting_new_cases),prob=transition_prob$p_hospital[j])
        }
        hospital_cases[,(i+los$pre_lega):(i+los$vd-1)] <- hospital_cases[,(i+los$pre_lega):(i+los$vd-1)] + matrix(rep(rowSums(hospital_cases_per_splitting),los$ld),ncol=los$ld) 
        icu_cases_per_splitting <- matrix(0,nrow=nrow(hospital_cases_per_splitting),ncol=ncol(hospital_cases_per_splitting))
        for(j in 1:ncol(hospital_cases_per_splitting)){
            icu_cases_per_splitting[,j] <- rbinom(size=hospital_cases_per_splitting[,j],n=nrow(hospital_cases_per_splitting),prob=transition_prob$p_icu[j])
        }
        icu_cases[,(i+los$pre_lega+los$pre_icu):(i+los$pre_lega+los$pre_icu+los$gd-1)] <- icu_cases[,(i+los$pre_lega+los$pre_icu):(i+los$pre_lega+los$pre_icu+los$gd-1)] + matrix(rep(rowSums(icu_cases_per_splitting),los$gd),ncol=los$gd)
    }
    home_dat <- tibble(date=dates,
                      state='home',
                      median=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                      lower=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                      upper=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    hospital_dat <- tibble(date=dates,
                           state='hospital',
                           median=apply(hospital_cases,2,quantile,probs=0.5)[1:length(dates)],
                           lower=apply(hospital_cases,2,quantile,probs=0.025)[1:length(dates)],
                           upper=apply(hospital_cases,2,quantile,probs=0.975)[1:length(dates)])
    icu_dat <- tibble(date=dates,
                      state='intensive_care_unit',
                      median=apply(icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                      lower=apply(icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                      upper=apply(icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    return(bind_rows(home_dat,hospital_dat,icu_dat))
}

## LOS bestað

### Var í fallinu
transition_prob <- patient_transitions_state_blocks %>%
    inner_join(select(individs_splitting_variables,patient_id,matches(paste0('^','age_official','$'))),by='patient_id') %>%
    rename(splitting_variable=!!'age_official') %>%
    group_by(patient_id,splitting_variable) %>%
    summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
    group_by(splitting_variable) %>%
    summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
    ungroup() %>%
    mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu)))

splitting_distribution <- get_patient_transitions_at_date('base',date_observed = as.Date('2020-05-08')) %>%
    distinct(patient_id) %>%
    inner_join(individs_splitting_variables,by='patient_id') %>%
    rename(splitting_variable=!!'age_official') %>%
    group_by(splitting_variable) %>%
    summarise(prop=n()) %>%
    ungroup() %>%
    mutate(prop=prop/sum(prop))

dates <- seq(min(infected_distr$date), max(infected_distr$date),by=1)
###

veikindadagar <- seq(12, 22)
outcome <- data.frame("veikindagar"=0, "legudagar"=0, "icu_dagar"=0, "pre_lega"=0, "pre_icu"=0, "MSE_home"=0,"MSE_inpatient"=0, "MSE_icu"=0, "MSE"=0)

for(v in veikindadagar){
    for(l in 4:v-1){
        for(i in 3:l-1){
            legudagar <- l
            icu_dagar <- i
            dagar <- tibble(vd=v, ld=round(legudagar), gd=round(icu_dagar)-1)
            dagar <- mutate(dagar, pre_lega=vd-ld, pre_icu=ld-gd-1)
            tmp <- run_ferguson_simulation2(infected_distr, dagar) %>%
                mutate(., state=if_else(state=="hospital", "inpatient_ward", state))
            tmp2 <- inner_join(tmp,historical_data,by=c('date','state'),suffix=c('_sim','_historical'))
            tmp2_home <- filter(tmp2, state=="home")
            tmp2_inpatient <- filter(tmp2, state=="inpatient_ward")
            tmp2_icu <- filter(tmp2, state=="intensive_care_unit")
            MSE_home <- mean((tmp2_home$median-tmp2_home$count)^2)
            MSE_inpatient <- mean((tmp2_inpatient$median-tmp2_inpatient$count)^2)
            MSE_icu <- mean((tmp2_icu$median-tmp2_icu$count)^2)
            MSE <- mean((tmp2$median-tmp2$count)^2)
            outcome[nrow(outcome) + 1,] = c(dagar, MSE_home, MSE_inpatient, MSE_icu, MSE)
        }
    }
}

#Outcome breytt eftir að það er búið til
outcome <- outcome[-1,] #fyrsta línan 0
#

#empirical distribution confidence interval (googla)

# Nokkur bestu LOS:
best_los_methods <- data.frame("Aðferð"=NA, "veikindagar"=0, "legudagar"=0, "icu_dagar"=0, "pre_lega"=0, "pre_icu"=0, "MSE_home"=0,"MSE_inpatient"=0, "MSE_icu"=0, "MSE"=0, "Lýsing"=NA, "mynd"=0)

## Skalað MSE fyrir hvert state
outcome <- mutate(outcome, MSE_home_p = sqrt(MSE_home)/max(tmp2_home$median), 
                           MSE_inpateint_p = sqrt(MSE_inpatient)/max(tmp2_inpatient$median), 
                           MSE_icu_p = sqrt(MSE_icu)/max(tmp2_icu$median), 
                           MSE_p=MSE_home_p+MSE_inpateint_p+MSE_icu_p) #MSE skalað með því að deila með hápunktu (sögulegra gagna)

best_los_scaled <- outcome[which.min(outcome$MSE_p),] %>%
    rename(vd=veikindagar, ld=legudagar, gd=icu_dagar) 

best_los_scaled_dat <- run_ferguson_simulation2(infected_distr, best_los_scaled) %>%
    mutate(., state=if_else(state=="hospital", "inpatient_ward", state))

p1 <- ggplot(best_los_scaled_dat) + 
    geom_point(data=historical_data,aes(date,count))+
    geom_line(aes(date,median), color="steelblue3") + 
    geom_line(aes(date,lower),linetype='dashed', color="steelblue2") +
    geom_line(aes(date,upper),linetype='dashed', color="steelblue2") +
    facet_wrap(~state,scales='free') + 
    ggtitle("Aðferð 1", subtitle="minnsta samanlegt MSE (skalað)")

best_los_methods[1,] <- c("Aðferð 1", best_los_scaled[1:9], "minnsta samanlegt MSE (skalað)", 1)
ggsave('~/downloads/p1.png', plot=p1, device='png', width=16, height=10)

## Meðaltalið af meðaltali af 10 bestu MSE fyrir hvert state
best_los_home <- outcome %>% slice_min(MSE_home, n=10)%>%
    rename(vd=veikindagar, ld=legudagar, gd=icu_dagar)
best_los_inpatient <- outcome %>% slice_min(MSE_inpatient, n=10) %>%
    rename(vd=veikindagar, ld=legudagar, gd=icu_dagar)
best_los_icu <- outcome %>% slice_min(MSE_icu, n=10)%>%
    rename(vd=veikindagar, ld=legudagar, gd=icu_dagar)

best_los_mean <- data.frame(vd=round((mean(best_los_home$vd) + mean(best_los_inpatient$vd) + mean(best_los_icu$vd))/3),
                            ld=round((mean(best_los_home$ld) + mean(best_los_inpatient$ld) + mean(best_los_icu$ld))/3),
                            gd=round((mean(best_los_home$gd) + mean(best_los_inpatient$gd) + mean(best_los_icu$gd))/3))
best_los_mean <- mutate(best_los_mean, pre_lega=vd-ld, pre_icu=ld-gd-1)

best_los_mean_dat <- run_ferguson_simulation2(infected_distr, best_los_mean) %>%
    mutate(., state=if_else(state=="hospital", "inpatient_ward", state))

best_los_mean_all <- filter(outcome, veikindadagar==best_los_mean$vd & legudagar==best_los_mean$ld & icu_dagar==best_los_mean$gd & pre_lega==best_los_mean$pre_lega & pre_icu==best_los_mean$pre_icu )

p2 <- ggplot(best_los_mean_dat) + 
    geom_point(data=historical_data,aes(date,count))+
    geom_line(aes(date,median), color="steelblue3") + 
    geom_line(aes(date,lower),linetype='dashed', color="steelblue2") +
    geom_line(aes(date,upper),linetype='dashed', color="steelblue2") +
    facet_wrap(~state,scales='free') + 
    ggtitle("Aðferð 2", subtitle="Meðaltal af 10 bestu fyrir hvert state og svo meðaltal af því")

best_los_methods[2,] <- c("Aðferð 2", best_los_mean_all[1:9], "Meðaltal af 10 bestu fyrir hvert state og svo meðaltal af því", 2)
ggsave('~/downloads/p2.png', plot=p2, device='png', width=16, height=10)

## Minnsta MSE fyrir home og inpatient_ward (sama)
best_los_home_inpatient <- best_los_home[1,]
best_los_home_inpatient_dat <- run_ferguson_simulation2(infected_distr, best_los_home_inpatient) %>%
    mutate(., state=if_else(state=="hospital", "inpatient_ward", state))
p3 <- ggplot(best_los_home_inpatient_dat) + 
    geom_point(data=historical_data,aes(date,count))+
    geom_line(aes(date,median), color="steelblue3") + 
    geom_line(aes(date,lower),linetype='dashed', color="steelblue2") +
    geom_line(aes(date,upper),linetype='dashed', color="steelblue2") +
    facet_wrap(~state,scales='free') + 
    ggtitle("Aðferð 3", subtitle="Minnsta MSE fyrir home og inpatient_ward (sama)")

best_los_methods[3,] <- c("Aðferð 3", best_los_home_inpatient[1:9], "Minnsta MSE fyrir home og inpatient_ward (sama)", 3)
ggsave('~/downloads/p3.png', plot=p3, device='png', width=16, height=10)

## Minnsta MSE fyrir ICU
best_los_icu_single <- best_los_icu[1,]
best_los_icu_single_dat <- run_ferguson_simulation2(infected_distr, best_los_icu_single) %>%
    mutate(., state=if_else(state=="hospital", "inpatient_ward", state))
p4 <- ggplot(best_los_icu_single_dat) + 
    geom_point(data=historical_data,aes(date,count))+
    geom_line(aes(date,median), color="steelblue3") + 
    geom_line(aes(date,lower),linetype='dashed', color="steelblue2") +
    geom_line(aes(date,upper),linetype='dashed', color="steelblue2") +
    facet_wrap(~state,scales='free') + 
    ggtitle("Aðferð 4", subtitle="Minnsta MSE fyrir ICU")

best_los_methods[4,] <- c("Aðferð 4", best_los_icu_single[1:9], "Minnsta MSE fyrir ICU", 4)
ggsave('~/downloads/p4.png', plot=p4, device='png', width=16, height=10)

# Skrifa út töflur
write.csv(outcome,'~/downloads/outcome_LOS.csv')
write.csv(best_los_methods,'~/downloads/best_LOS_methods.csv')

p1_p2 <- grid.arrange(p1, p2, ncol=2)
ggsave('~/downloads/p1_p2.png', plot=p1_p2, device='png', width=16, height=10)

outcome_plot <- gather(outcome, MSE_type, MSE_value, c(MSE_home, MSE_inpatient, MSE_icu, MSE), factor_key=TRUE) %>%
                mutate(., number=rep(seq(1:nrow(outcome)),4))
points <- filter(outcome_plot, number %in% c(371, 369, 356, 309))
p5 <- ggplot(outcome_plot, aes(x=number)) +
    geom_line(aes(y=MSE_value)) +
    geom_point(data=points, aes(x=number, y=MSE_value), size=1, color="indianred3")+
    facet_wrap(~MSE_type,scales='free')
ggsave('~/downloads/p5.png', plot=p5, device='png', width=16, height=10)

## Transition bestað?
### Nah
transition_prob$splitting_variable <- seq(1:nrow(transition_prob))

f1 <- ggplot(transition_prob, aes(x=splitting_variable)) + geom_col(aes(y=p_hospital))
ggsave('~/downloads/f1.png', plot=f1, device='png', width=16, height=10)
f2 <- ggplot(transition_prob, aes(x=splitting_variable)) + geom_col(aes(y=p_icu))
ggsave('~/downloads/f2.png', plot=f2, device='png', width=16, height=10)
f3 <- ggplot(transition_prob, aes(x=splitting_variable)) + geom_col(aes(y=p_death))
ggsave('~/downloads/f3.png', plot=f3, device='png', width=16, height=10)

### Mynd til að bera saman ###

# iceland_los <- data.frame("vd"=21, "ld"=14, "gd"=10, "pre_lega"=7, "pre_icu"=3)
# iceland_los2 <- data.frame("vd"=14, "ld"=11, "gd"=9, "pre_lega"=3, "pre_icu"=1)
# best_los <- data.frame("vd"=17, "ld"=8, "gd"=6, "pre_lega"=9, "pre_icu"=1)
# 
# ice_los_dat <- run_ferguson_simulation2(infected_distr, iceland_los) %>%
#     mutate(., state=if_else(state=="hospital", "Legudeild", state)) %>%
#     mutate(., state=if_else(state=="home", "Heimaeinangrun", state)) %>%
#     mutate(., state=if_else(state=="intensive_care_unit", "Gjörgæsla", state)) %>%
#     transform(., state=factor(state, levels = c("Heimaeinangrun", "Legudeild", "Gjörgæsla")))
# 
# ice_los_dat2 <- run_ferguson_simulation2(infected_distr, iceland_los2) %>%
#     mutate(., state=if_else(state=="hospital", "Legudeild", state)) %>%
#     mutate(., state=if_else(state=="home", "Heimaeinangrun", state)) %>%
#     mutate(., state=if_else(state=="intensive_care_unit", "Gjörgæsla", state)) %>%
#     transform(., state=factor(state, levels = c("Heimaeinangrun", "Legudeild", "Gjörgæsla")))
# 
# best_los_dat <- run_ferguson_simulation2(infected_distr, best_los) %>%
#     mutate(., state=if_else(state=="hospital", "Legudeild", state)) %>%
#     mutate(., state=if_else(state=="home", "Heimaeinangrun", state)) %>%
#     mutate(., state=if_else(state=="intensive_care_unit", "Gjörgæsla", state)) %>%
#     transform(., state=factor(state, levels = c("Heimaeinangrun", "Legudeild", "Gjörgæsla")))
# 
# historical_data_filtered <- historical_data %>% filter(date<ymd("2020-05-05")) %>%
#     mutate(., state=if_else(state=="inpatient_ward", "Legudeild", state)) %>%
#     mutate(., state=if_else(state=="home", "Heimaeinangrun", state)) %>%
#     mutate(., state=if_else(state=="intensive_care_unit", "Gjörgæsla", state)) %>%
#     transform(., state=factor(state, levels = c("Heimaeinangrun", "Legudeild", "Gjörgæsla")))
# 
# combined_dat <- rbind(data.frame(Legutímar = "Meðaltal úr LSH gögnum", ice_los_dat2), 
#                   data.frame(Legutímar = "Niðurstaða bestunar", best_los_dat))
# 
# p_lsh <- ggplot(ice_los_dat) +
#     geom_point(data=historical_data_filtered, aes(date,count)) +
#     geom_line(aes(date, median), color="dodgerblue") +
#     geom_line(aes(date, lower), linetype='dashed', color="dodgerblue", alpha=0.5) +
#     geom_line(aes(date, upper), linetype='dashed', color="dodgerblue", alpha=0.5) +
#     facet_wrap(~state, scales='free') +
#     ggtitle("IC-líkan með legutímum sem covid.hi.is notar") +
#     labs(x="Dagsetning", y="Fjöldi") +
#     theme(legend.position="bottom")
#     
# 
# ggsave('~/downloads/p_lsh.png', plot=p_lsh, device='png', width=16, height=10)

