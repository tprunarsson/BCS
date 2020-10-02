# Þetta fall reiknar út bestu stærðina alpha þar sem 
# alpha*(ferguson spá) + (1 - alpha)*(spá með bættu líkani)
# er sú spá sem MSE er lágmarkað fyrir.
# Lágmörkunin er miðuð við síðustu 7 daga (minna ef faraldurinn byrjaði fyrir minna en 7 dögum).
# Spáin fer svo tvær vikur fram í tímann.
# Það er hægt að stilla þetta
# Spáin sem kemur út úr þessu með besta alpha er svo send áfram og búin til skýrsla í bcs_gagnabord2

#Setup
oldw <- getOption("warn")
options(warn = -1)
library(tidyverse)
library(lubridate)
library(gdata)
options(warn = oldw)

# Hafa inni ef ekki er kallað úr BCS_gagnabord2
setwd("../lsh_data_processing")
source("covid19_lsh_data_processing.R") #Þurfum nokkra hluti úr þessari skrá. 
gdata::keep(patient_transitions_state_blocks, individs_splitting_variables, get_patient_transitions_at_date, patient_transitions, infections_predicted_per_date, historical_data, sure = TRUE)

#best_weighted_function <- function(today){
  # Stillingar
  run_id <- 17
  date_data <- '2020-05-08'
  today <- '2020-05-08'   # Hafa inni ef ekki er kallað úr bcs_gagnabord2
  # today <- Sys.Date()
  back_in_time <- 7 #Number of days to calculate MSE from
  forward_in_time <- 14 # Number of days to predict (show on graph)
  week_ago <- (ymd(today)-back_in_time) #%>% as.character()
  #week_ago <- today-back_in_time
  if(ymd(today)<ymd("2020-03-08")){ # Ef minna en vika er liðin af faraldri, útfæra fyrir almennt
    week_ago <- ymd("2020-03-01")
  }
  
  # Keyra bash
  # Ath. í run_evaluation.R, hafa write_table neðst
  run_execute_run <- paste("./execute_run.sh -d", date_data, "-s", week_ago, "-r", run_id, "-f")
  setwd("../experiments")
  system(run_execute_run)
  
  # Lesum inn simulation_summary og fáum bætt módel
  path_simulation_summary_output <- paste0("../output/", date_data, "_", week_ago, "_", run_id, "_simulation_summary.csv")
  improved_model <- read.csv(path_simulation_summary_output)
  
  state_translation <- data.frame("state_english"=c("home", "inpatient_ward", "intensive_care_unit"), "state_icelandic"=c("Heimaeinangrun", "Legudeild", "Gjörgæsla"))
  
  improved_model <- improved_model %>%
    left_join(., state_translation, by = c("state"="state_icelandic")) %>%
    arrange(state_english) %>%
    select(-state) %>%
    rename("state"="state_english") %>%
    mutate(date=ymd(date))
  
  # Ferguson
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
                           state='inpatient_ward',
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
  
  infected_distr <- infections_predicted_per_date %>%
    group_by(date) %>%
    mutate(prob=count/sum(count))
  
  los_wuhan <- data.frame("vd"=21, "ld"=14, "gd"=10, "pre_lega"=7, "pre_icu"=3)
  los_best <- data.frame("vd"=17, "ld"=8, "gd"=6, "pre_lega"=9, "pre_icu"=1)
  
  ferguson_model <- run_ferguson_simulation2(infected_distr, los_wuhan) 
  
  ferguson_model <- ferguson_model %>%
    left_join(., state_translation, by = c("state"="state_english")) %>%
    select(-state) %>%
    rename("state"="state_icelandic") %>%
    filter(date %in% seq(min(improved_model$date), max(improved_model$date), 1)) #Passa að hafa sömu dagsetningar
  
  improved_model <- improved_model %>%
    filter(date %in% seq(min(ferguson_model$date), max(ferguson_model$date), 1))
  
  # Making a grid for alpha
  alphas <- seq(0, 1, 0.01)
  outcome_table <- data.frame("alpha"=0, "MSE_home"=0,"MSE_inpatient"=0, "MSE_icu"=0, "MSE"=0)
  combined_model <- data.frame("date"=rep(seq(min(improved_model$date), max(improved_model$date), 1), 3), 
                               "state"=c(rep("home", nrow(ferguson_model)/3), rep("inpatient_ward", nrow(ferguson_model)/3), rep("intensive_care_unit", nrow(ferguson_model)/3)), 
                               "median"=rep(NA, nrow(ferguson_model)), 
                               "lower"=rep(NA, nrow(ferguson_model)), 
                               "upper"=rep(NA, nrow(ferguson_model)))
  
  # Af því today er í raun ekki dagurinn í dag
  historical_data_filtered <- historical_data %>% filter(date<ymd(today))
  
  combined_model <- left_join(combined_model, historical_data_filtered, by=c('date','state')) %>%
    rename("count_historical"=count)
  
  # Sameining á spám með mismunandi vægi
  for(alpha in alphas){
    combined_model <- combined_model %>% mutate(median=alpha*ferguson_model$median + (1-alpha)*improved_model$median)
    combined_model <- combined_model %>% mutate(lower=alpha*ferguson_model$lower + (1-alpha)*improved_model$lower)
    combined_model <- combined_model %>% mutate(upper=alpha*ferguson_model$upper + (1-alpha)*improved_model$upper)
    combined_model_filtered <- combined_model %>% filter(!is.na(count_historical))
    combined_model_home <- filter(combined_model_filtered, state=="home")
    combined_model_inpatient <- filter(combined_model_filtered, state=="inpatient_ward")
    combined_model_icu <- filter(combined_model_filtered, state=="intensive_care_unit")
    MSE_home <- mean((combined_model_home$median-combined_model_home$count_historical)^2)
    MSE_inpatient <- mean((combined_model_inpatient$median-combined_model_inpatient$count_historical)^2)
    MSE_icu <- mean((combined_model_icu$median-combined_model_icu$count_historical)^2)
    MSE <- mean((combined_model_filtered$median-combined_model_filtered$count_historical)^2)
    outcome_table[nrow(outcome_table) + 1,] = c(alpha, MSE_home, MSE_inpatient, MSE_icu, MSE)
  }
  
  outcome_table <- outcome_table[-1,]
  outcome_table <- outcome_table %>% mutate(MSE_home_scaled=MSE_home/max(combined_model_home$count_historical),
                                            MSE_inpatient_scaled=MSE_inpatient/max(combined_model_inpatient$count_historical),
                                            MSE_icu_scaled=MSE_icu/max(combined_model_icu$count_historical)) %>%
    mutate(MSE_scaled_sum=MSE_home_scaled + !is.na(MSE_inpatient_scaled) + !is.na(MSE_icu_scaled))
  
  #Finnum besta alpha fyrir "today-7" til "today". 
  #Búum svo til nýja spá, 2 vikur fram í tímann frá deginum í dag með þessu alpha
  best_alpha <- outcome_table[which.min(outcome_table$MSE_scaled_sum),]$alpha
  
  run_execute_run_today <- paste("./execute_run.sh -d", date_data, "-s", today, "-r", run_id)
  setwd("../experiments")
  system(run_execute_run_today)
  
  # Lesum inn simulation_summary og fáum bætt módel
  path_simulation_summary_output_today <- paste0("../output/", date_data, "_", today, "_", run_id, "_simulation_summary.csv")
  improved_model_today <- read.csv(path_simulation_summary_output_today)
  
  improved_model_today <- improved_model_today %>%
    left_join(., state_translation, by = c("state"="state_icelandic")) %>%
    arrange(state_english) %>%
    select(-state) %>%
    rename("state"="state_english") %>%
    mutate(date=ymd(date))
  
  ferguson_model_today <- ferguson_model %>% 
    filter(date %in% seq(min(improved_model_today$date), max(improved_model_today$date), 1)) #Passa að hafa sömu dagsetningar
  
  combined_model_today <- data.frame("date"=rep(seq(min(improved_model_today$date), max(improved_model_today$date), 1), 3), 
                                     "state"=c(rep("home", nrow(ferguson_model_today)/3), rep("inpatient_ward", nrow(ferguson_model_today)/3), rep("intensive_care_unit", nrow(ferguson_model_today)/3)), 
                                     "median"=rep(NA, nrow(ferguson_model_today)), 
                                     "lower"=rep(NA, nrow(ferguson_model_today)), 
                                     "upper"=rep(NA, nrow(ferguson_model_today)))
  
  combined_model_today <- left_join(combined_model_today, historical_data_filtered, by=c('date','state')) %>%
    rename("count_historical"=count)
  
  combined_model_today <- combined_model_today %>% mutate(median=best_alpha*ferguson_model_today$median + (1-best_alpha)*improved_model_today$median)
  combined_model_today <- combined_model_today %>% mutate(lower=best_alpha*ferguson_model_today$lower + (1-best_alpha)*improved_model_today$lower)
  combined_model_today <- combined_model_today %>% mutate(upper=best_alpha*ferguson_model_today$upper + (1-best_alpha)*improved_model_today$upper)
  
  combined_model_today <- combined_model_today %>% filter(date<ymd(today)+forward_in_time)
  
  combined_model_today <- combined_model_today %>% mutate("best_alpha"=best_alpha)
  
  #Plot historical data up until today
  #Plot the predicted numbers from today until two weeks from today
  
  plot <- ggplot(data=combined_model_today, aes(x=date)) +
    geom_line(aes(y=median), color="dodgerblue3") + 
    geom_line(aes(y=lower), linetype="dashed", color="dodgerblue3") + 
    geom_line(aes(y=upper), linetype="dashed", color="dodgerblue3") + 
    #geom_point(aes(y=count_historical)) +
    geom_vline(xintercept = ymd(today), linetype="dotted", color="indianred3") +
    geom_point(data=historical_data_filtered, aes(y=count)) +
    geom_point(data=historical_data, aes(y=count), alpha=0.4) +
    facet_wrap(~state,scales='free') +
    labs(x="Dagsetning", y="Fjöldi", title = today, subtitle = paste("alpha =", best_alpha)) + 
    theme_bw() +
    theme(legend.title=element_blank())
  
  # Hafa inni ef ekki er kallað úr BCS_gagnabord2
  
  #setwd("../graphs")
  #ggsave(paste0(today, "_graph_weighted.png"), plot, device='png', width=16, height=10)
  
  #Athuga hvort hægt sé að skila lista af hlutum (í staðinn fyrir að setja best_alpha inn í data_frame)
  #return(combined_model_today)
#}
  
  
  
  