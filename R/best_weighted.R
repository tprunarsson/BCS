#Þetta fall reiknar út bestu stærðina alpha þar sem 
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

best_weighted_function <- function(date_data, today, los_setting, prediction_type, date_prediction){
  # Stillingar
  run_id <- 19
  #date_data <- '2020-10-02'     #covid19_lsh_data_processing #BREYTA með nýjum lsh gögnum
  #date_data <- today            #dagsetning skýrslu er sú sama og dagsetning gagna
  #date_data <- Sys.Date()-1    #Notar alltaf nýjustu gögnin
  back_in_time <- 7 #Number of days to calculate MSE from
  week_ago <- today-back_in_time
  if((historical_data %>% distinct(date) %>% nrow())<7){ # Ef minna en vika er liðin af faraldri
    week_ago <- min(historical_data$date)
  }
  
  # Keyra bash
  # Ath. í run_evaluation.R, hafa write_table neðst
  run_data_processing <- paste("Rscript lsh_data_processing/covid19_lsh_data_processing.R -d", date_data, "-r", run_id, "-c", prediction_type, "-p", date_prediction)
 # setwd("../lsh_data_processing")
  system(run_data_processing)
  
  #Það sem bash skráin að ofan skrifaði út (í lsh_data_processing)
  infected_distr <- read.csv(paste0(path_tables, date_data,'_infections_predicted.csv')) %>% 
    mutate(date=ymd(date)) %>%
    group_by(date) %>%
    mutate(prob=count/sum(count))
  
  sim_days <- as.numeric(max(infected_distr$date)-today)
  
  run_execute_run <- paste("./execute_run.sh -d", date_data, "-n 7", "-s", week_ago, "-b", date_prediction, "-r", run_id, "-c", prediction_type, "-e", today, "-f")
#  setwd("../experiments")
  system(run_execute_run)
#  setwd("../dashboard")
  
  # Lesum inn simulation_summary og fáum bætt módel
  path_simulation_summary_output <- paste0("output/", date_data, "_", week_ago, "_", run_id, "_simulation_summary.csv")
  improved_model <- read.csv(path_simulation_summary_output)
  
  state_translation <- data.frame("state_english"=c("home", "inpatient_ward", "intensive_care_unit"), "state_icelandic"=c("Heimaeinangrun", "Legudeild", "Gjörgæsla"))
  
  improved_model <- improved_model %>%
    left_join(., state_translation, by = c("state"="state_icelandic")) %>%
    arrange(state_english) %>%
    select(-state) %>%
    rename("state"="state_english") %>%
    mutate(date=ymd(date))
  
  # Ferguson
  run_ferguson_simulation_set_age <- function(infected_distr, los){
    
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
    inner_join(select(individs_splitting_variables,patient_id,vacc_at_diagnosis, matches(paste0('^','age_official','$'))),by='patient_id') %>%
    rename(splitting_variable=!!'age_official') %>%
    group_by(patient_id,splitting_variable, vacc_at_diagnosis) %>%
    summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
    group_by(splitting_variable, vacc_at_diagnosis) %>%
    summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
    ungroup() %>%
    mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu))) %>%
    unite(splitting_variable, splitting_variable, vacc_at_diagnosis, sep="_", remove = TRUE) %>%
    arrange(splitting_variable)
  
  splitting_distribution <- get_patient_transitions_at_date('base', date_observed = date_data) %>%
    distinct(patient_id) %>%
    inner_join(individs_splitting_variables,by='patient_id') %>%
    rename(splitting_variable=!!'age_official') %>%
    group_by(splitting_variable, vacc_at_diagnosis) %>%
    summarise(prop=n()) %>%
    ungroup() %>%
    mutate(prop=prop/sum(prop)) %>%
    unite(splitting_variable, splitting_variable, vacc_at_diagnosis, sep="_", remove = TRUE) %>%
    arrange(splitting_variable)
  
  los_wuhan <- data.frame("vd"=21, "ld"=14, "gd"=10, "pre_lega"=7, "pre_icu"=3)
  los_best <- data.frame("vd"=17, "ld"=8, "gd"=6, "pre_lega"=9, "pre_icu"=1)
  
  ferguson_model <- run_ferguson_simulation_set_age(infected_distr, los_setting) 
  
  ferguson_model_old <- ferguson_model %>%
    filter(date %in% seq(min(improved_model$date), max(improved_model$date), 1)) #Passa að hafa sömu dagsetningar
  
  improved_model_old <- improved_model %>%
    filter(date %in% seq(min(ferguson_model_old$date), max(ferguson_model_old$date), 1))
  
  # Making a grid for alpha
  alphas <- seq(0, 1, 0.01)
  outcome_table <- data.frame("alpha"=0, "MSE_home"=0,"MSE_inpatient"=0, "MSE_icu"=0, "MSE"=0)
  combined_model <- data.frame("date"=rep(seq(min(improved_model_old$date), max(improved_model_old$date), 1), 3), 
                               "state"=c(rep("home", nrow(ferguson_model_old)/3), rep("inpatient_ward", nrow(ferguson_model_old)/3), rep("intensive_care_unit", nrow(ferguson_model_old)/3)), 
                               "median"=rep(NA, nrow(ferguson_model_old)), 
                               "lower"=rep(NA, nrow(ferguson_model_old)), 
                               "upper"=rep(NA, nrow(ferguson_model_old)))
  
  # Af því today er í raun ekki dagurinn í dag (alltaf)
  historical_data_filtered <- historical_data %>% filter(date<ymd(today))
  
  combined_model <- left_join(combined_model, historical_data_filtered, by=c('date','state')) %>%
    rename("count_historical"=count)
  
  # Sameining á spám með mismunandi vægi
  for(alpha in alphas){
    combined_model <- combined_model %>% mutate(median=alpha*ferguson_model_old$median + (1-alpha)*improved_model_old$median)
    combined_model <- combined_model %>% mutate(lower=alpha*ferguson_model_old$lower + (1-alpha)*improved_model_old$lower)
    combined_model <- combined_model %>% mutate(upper=alpha*ferguson_model_old$upper + (1-alpha)*improved_model_old$upper)
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
  
  run_execute_run_today <- paste("./execute_run.sh -d", date_data, "-n", sim_days, "-s", today, "-b", date_prediction, "-r", run_id, "-c", prediction_type, "-f")
 # setwd("../experiments")
  system(run_execute_run_today)
 # setwd("../dashboard")
  
  # Lesum inn simulation_summary og fáum bætt módel
  path_simulation_summary_output_today <- paste0("output/", date_data, "_", today, "_", run_id, "_simulation_summary.csv")
  improved_model_today <- read.csv(path_simulation_summary_output_today)
  
  improved_model_today <- improved_model_today %>%
    left_join(., state_translation, by = c("state"="state_icelandic")) %>%
    arrange(state_english) %>%
    select(-state) %>%
    rename("state"="state_english") %>%
    mutate(date=ymd(date))
  
  #Passa að hafa sömu dagsetningar
  ferguson_model_today <- ferguson_model %>% 
    filter(date %in% seq(min(improved_model_today$date), max(improved_model_today$date), 1))
  
  improved_model_today <- improved_model_today %>%
    filter(date %in% seq(min(ferguson_model_today$date), max(ferguson_model_today$date), 1))
  
  combined_model_today <- data.frame("date"=rep(seq(min(improved_model_today$date), max(improved_model_today$date), 1), 3), 
                                     "state"=c(rep("home", nrow(ferguson_model_today)/3), rep("inpatient_ward", nrow(ferguson_model_today)/3), rep("intensive_care_unit", nrow(ferguson_model_today)/3)), 
                                     "median"=rep(NA, nrow(ferguson_model_today)), 
                                     "lower"=rep(NA, nrow(ferguson_model_today)), 
                                     "upper"=rep(NA, nrow(ferguson_model_today)))
  
  combined_model_today <- combined_model_today %>% mutate(median=best_alpha*ferguson_model_today$median + (1-best_alpha)*improved_model_today$median)
  combined_model_today <- combined_model_today %>% mutate(lower=best_alpha*ferguson_model_today$lower + (1-best_alpha)*improved_model_today$lower)
  combined_model_today <- combined_model_today %>% mutate(upper=best_alpha*ferguson_model_today$upper + (1-best_alpha)*improved_model_today$upper)
  
  combined_model_today <- combined_model_today %>% mutate("best_alpha"=best_alpha)
  improved_model_today <- improved_model_today %>% mutate("best_alpha"=0)
  ferguson_model_today <- ferguson_model_today %>% mutate("best_alpha"=1)
  
  models <- list("ferguson_model" = ferguson_model_today,
                 "improved_model" = improved_model_today,
                 "combined_model" = combined_model_today)
  
  return(models)
}
