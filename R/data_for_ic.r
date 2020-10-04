setwd('../lsh_data_processing')
source('covid19_lsh_data_processing.R')
library(tidyverse)

## Hér breytum við spá frá covid.hi.is með því að setja inn raunverulegan fjölda nýrra smita á þeim dögum sem eru liðnir

splitting_distribution <- get_patient_transitions_at_date('base', date_observed = date_data) %>%
  distinct(patient_id) %>%
  inner_join(individs_splitting_variables,by='patient_id') %>%
  rename(splitting_variable=!!'age_official') %>%
  group_by(splitting_variable) %>%
  summarise(prop=n()) %>%
  ungroup() %>%
  mutate(prop=prop/sum(prop))

transition_prob <- patient_transitions_state_blocks %>%
  inner_join(select(individs_splitting_variables,patient_id,matches(paste0('^','age_official','$'))),by='patient_id') %>%
  rename(splitting_variable=!!'age_official') %>%
  group_by(patient_id,splitting_variable) %>%
  summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
  group_by(splitting_variable) %>%
  summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
  ungroup() %>%
  mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu)))

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


date_data_end<-date_data

day1 <- infections_predicted_per_date %>% filter(date==min(date))
n <- nrow(day1)
firstday <- day1$date[1]
day_n <- date_data_end-firstday
new_cases_manual <- data.frame("date"=rep(seq.Date(from=firstday, length.out = day_n, by=1), each=n),
                               "new_cases"=rep(0:(n-1),times=day_n))
infections_per_date<-covid_diagnosis %>% group_by(date_diagnosis_pcr) %>% summarize(n=n())
new_cases_manual<-left_join(new_cases_manual,infections_per_date,by=c(date="date_diagnosis_pcr"))

new_cases_manual<-new_cases_manual%>%mutate(count=if_else(n==new_cases, 8000, 0)) %>% select(date,new_cases,count)
new_cases_predicted <- infections_predicted_per_date %>% filter(date>(date_data_end-1))

infections <- rbind(new_cases_manual,new_cases_predicted) %>% 
  group_by(date) %>%
  mutate(prob=count/sum(count))

los_best <- data.frame("vd"=17, "ld"=8, "gd"=6, "pre_lega"=9, "pre_icu"=1)

ferguson <- run_ferguson_simulation_set_age(infections,los_best)

#### PLOT ####
color_palette <- c("green"=rgb(171, 202, 106, max=255), 
                   "yellow"=rgb(229, 174, 62, max=255), 
                   "red"=rgb(191, 92, 89, max=255))

data_home <- ferguson %>% filter(state=="home", date>=date_data-1) %>% 
  gather(key="key", value="value", median, lower, upper) %>% 
  mutate(value=round(value, 0)) %>%
  mutate(key = key %>% recode(median = "Líkleg spá", upper = "Svartsýn spá", lower = "Bjartsýn spá"))
data_home$key <- factor(data_home$key, levels = c("Svartsýn spá", "Líkleg spá", "Bjartsýn spá"))

data_iw <- ferguson %>% filter(state=="inpatient_ward", date>=date_data-1) %>% 
  gather(key="key", value="value", median, lower, upper) %>% 
  mutate(value=round(value, 0)) %>%
  mutate(key = key %>% recode(median = "Líkleg spá", upper = "Svartsýn spá", lower = "Bjartsýn spá"))
data_iw$key <- factor(data_iw$key, levels = c("Svartsýn spá", "Líkleg spá", "Bjartsýn spá"))

data_icu <- ferguson %>% filter(state=="intensive_care_unit", date>=date_data-1) %>% 
  gather(key="key", value="value", median, lower, upper) %>% 
  mutate(value=round(value, 0)) %>%
  mutate(key = key %>% recode(median = "Líkleg spá", upper = "Svartsýn spá", lower = "Bjartsýn spá"))
data_icu$key <- factor(data_icu$key, levels = c("Svartsýn spá", "Líkleg spá", "Bjartsýn spá"))

historical_data_filtered <- historical_data %>% filter(date<date_data, date>ymd("2020-09-13"))

plot_home <- data_home %>% ggplot(aes(x=date, y=value)) + 
  geom_line(aes(lty=key, alpha=key), col = color_palette[1], size=1) + 
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
  labs(x = '', y = 'Fjöldi', linetype = '', title="Heimaeinangrun", alpha='') +
  scale_alpha_manual(values = c(0.8, 1, 0.8)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0), panel.grid.major=element_line(colour = "grey90")) +
  geom_text(aes(label = round(value, 1)),
            vjust=-1.8,
            show.legend = FALSE, size=2.3) + 
  scale_x_date(date_breaks = "1 day", date_labels =  "%d.%m") +
  geom_point(data=filter(historical_data_filtered, state=="home"), aes(x=date, y=count))

plot_iw <- data_iw %>% ggplot(aes(x=date, y=value)) + 
  geom_line(aes(lty=key, alpha=key), col = color_palette[2], size=1) + 
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
  labs(x = '', y = 'Fjöldi', linetype = '', title="Legudeild", alpha='') +
  scale_alpha_manual(values = c(0.8, 1, 0.8)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0), panel.grid.major=element_line(colour = "grey90")) +
  geom_text(aes(label = round(value, 1)),
            vjust=-1.8,
            show.legend = FALSE, size=2.3) + 
  scale_x_date(date_breaks = "1 day", date_labels =  "%d.%m") +
  geom_point(data=filter(historical_data_filtered, state=="inpatient_ward"), aes(x=date, y=count))

plot_icu <- data_icu %>% ggplot(aes(x=date, y=value)) + 
  geom_line(aes(lty=key, alpha=key), col = color_palette[3], size=1) + 
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
  labs(x = '', y = 'Fjöldi', linetype = '', title="Gjörgæsla", alpha='') +
  scale_alpha_manual(values = c(0.8, 1, 0.8)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0), panel.grid.major=element_line(colour = "grey90")) +
  geom_text(aes(label = round(value, 1)),
            vjust=-1.8,
            show.legend = FALSE, size=2.3) + 
  scale_x_date(date_breaks = "1 day", date_labels =  "%d.%m") +
  geom_point(data=filter(historical_data_filtered, state=="intensive_care_unit"), aes(x=date, y=count))

### Vista myndir ###

setwd("~/Downloads")
ggsave("plot_home.png", plot_home, device='png', width=16, height=10)
ggsave("plot_iw.png", plot_iw, device='png', width=16, height=10)
ggsave("plot_icu.png", plot_icu, device='png', width=16, height=10)







