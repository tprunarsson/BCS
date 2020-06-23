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
    
    dates <- seq(min(infected_distr$date),max(infected_distr$date),by=1)
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
wuhan_data_LOS <- run_ferguson_simulation('age_official',infected_distr,data='Wuhan',length_of_stay='from_data')
iceland_age_official_fixed_LOS <- run_ferguson_simulation('age_official',infected_distr,transitions_location ='iceland',los_location='wuhan')



iceland_age_simple_fixed_LOS <- run_ferguson_simulation('age_simple',infection_distr,data='Iceland',length_of_stay='fixed')
iceland_age_official_data_LOS <- run_ferguson_simulation('age_official',infection_distr,data='Iceland',length_of_stay='from_data')
iceland_age_simple_dataLOS <- run_ferguson_simulation('age_simple',infection_distr,data='Iceland',length_of_stay='from_data')

tot_dat <- bind_rows(wuhan_fixed_LOS,iceland_age_official_fixed_LOS,.id='model')

plot_ferguson_prediction <- function(prediction_dat,historical_dat){
    ggplot(prediction_dat) + 
        geom_line(aes(date,median,col=model)) + 
        geom_line(aes(date,lower,col=model),linetype='dashed') +
        geom_line(aes(date,upper,col=model),linetype='dashed') +
        geom_point(data=historical_dat,aes(date,count))+
        facet_wrap(~state,scales='free') 
}






