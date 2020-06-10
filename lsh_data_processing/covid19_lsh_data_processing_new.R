#!/usr/bin/env Rscript
invisible(Sys.setlocale("LC_ALL","IS_is"))
suppressPackageStartupMessages({
  library(optparse)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
})

source('create_input_for_simulation.R')
source('help_functions.R')
#source('functions_kt.R')

##### ----- Read data ----- #####

path_to_data <- '~/projects/covid/BCS/lsh_data_new/'
#path_to_data <- ''


date_data <- as.Date('2020-05-03','%Y-%m-%d')
date_last_known_state <- date_data-1
date_observed_start <- date_data-3

clinical_assessment_category_type <- 'simple_red'

file_path_bb_data <- paste0(path_to_data,'df_bb_2020-05-05.csv')
file_path_lsh_data <- paste0(path_to_data,'df_lsh_2020-05-05.csv')
file_path_pcr_data <- paste0(path_to_data,'df_pcr_2020-05-09.csv')
file_path_phone_data <- paste0(path_to_data,'df_phone_2020-05-13.csv')
file_path_covid_id <- paste0(path_to_data,'2020-06-04_covid_id_conversion.xlsx')
file_path_coding <- 'lsh_coding.xlsx'
file_path_experiment_template <- 'experiment_template.xlsx'

data_bb_raw  <- suppressMessages(read_csv(file_path_bb_data))
data_lsh_raw  <- suppressMessages(read_csv(file_path_lsh_data))
data_pcr_raw  <- suppressMessages(read_csv(file_path_pcr_data))
data_phone_raw  <- suppressMessages(read_csv(file_path_phone_data, col_types = cols(freetext_immunosuppression='c')))

data_covid_id  <- read_excel(file_path_covid_id, skip = 3)
covid_id <- rename(data_covid_id , kt = `Kennitala/gervikennitala`, patient_id = `Person Key`)

experiment_description <- read_excel(file_path_experiment_template,sheet='experiment_description')
experiment_specification <- read_excel(file_path_experiment_template,sheet='experiment_specification')
run_description <- read_excel(file_path_experiment_template,sheet='run_description')
run_specification <- read_excel(file_path_experiment_template,sheet='run_specification')
heuristics_description <- read_excel(file_path_experiment_template,sheet='heuristics_description')
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories') %>%
  mutate(clinical_assessment_category=!!as.name(paste0('clinical_assessment_category_',clinical_assessment_category_type)),
         clinical_assessment_category_order=!!as.name(paste0('clinical_assessment_category_order_',clinical_assessment_category_type)))

# Categories töflur
priority_categories <- read_excel(file_path_coding,sheet = 'priority_categories')
age_groups <- read_excel(file_path_coding,sheet = 'age_groups')
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories')
unit_category_type <- 'simple'
unit_categories <- read_excel(file_path_coding,sheet = 'lsh_unit_categories') %>%
  mutate(unit_category=!!as.name(paste0('unit_category_',unit_category_type)),
         unit_category_order=!!as.name(paste0('unit_category_order_',unit_category_type)))


##### -----Cleaning----- #####

#Tengjum kennitölur og patient_id saman
data_bb_raw  <- left_join(data_bb_raw,covid_id, by ='kt') 
data_lsh_raw  <- left_join(data_lsh_raw,covid_id, by ='kt')
data_pcr_raw  <- left_join(data_pcr_raw,covid_id, by ='kt') 
data_phone_raw  <- left_join(data_phone_raw,covid_id, by ='kt') 

#laga þrjár vitlausar dates
for(i in data_lsh_raw$patient_id) if(i %in% 428857) data_lsh_raw$date_discharge[data_lsh_raw$date_discharge == as.Date('2020-03-16',"%Y-%m-%d")] <- as.Date('2020-04-16',"%Y-%m-%d")
for(i in data_lsh_raw$patient_id) if(i %in% 405117) data_lsh_raw$date_admission[data_lsh_raw$date_admission == as.Date('2020-04-18',"%Y-%m-%d")] <- as.Date('2020-03-18',"%Y-%m-%d")
for(i in data_lsh_raw$patient_id) if(i %in% 410748) data_lsh_raw$date_admission[data_lsh_raw$date_admission == as.Date('2020-04-27',"%Y-%m-%d")] <- as.Date('2020-03-27',"%Y-%m-%d")

data_bb_raw <- select(data_bb_raw,-kt) 
data_bb <- data_bb_raw[c(ncol(data_bb_raw),1:(ncol(data_bb_raw)-1))]
data_lsh_raw <- select(data_lsh_raw,-kt)
data_lsh <- data_lsh_raw[c(ncol(data_lsh_raw),1:(ncol(data_lsh_raw)-1))]
data_pcr_raw <- select(data_pcr_raw,-kt)
data_pcr <- data_pcr_raw[c(ncol(data_pcr_raw),1:(ncol(data_pcr_raw)-1))]
data_phone_raw <- select(data_phone_raw,-kt)
data_phone <- data_phone_raw[c(ncol(data_phone_raw),1:(ncol(data_phone_raw)-1))]

data_phone <- mutate(data_phone,state=ifelse(green==1,'green',
                                             ifelse(yellow==1,'yellow',
                                                    ifelse(red==1,'red',
                                                           ifelse(blue==1,'blue',NA)))))

##### -----Processing----- #####

individs_extended <-select(data_pcr , patient_id, sex, age, test_nr, date, result) %>%
  filter(result=='positive') %>%
  mutate(.,date_diagnosis=as.Date(date,"%Y-%m-%d")) %>%
  group_by(patient_id) %>%
  filter(date_diagnosis==min(date_diagnosis))  %>% 
  filter(test_nr==min(test_nr)) %>% 
  filter(age==min(age)) %>%
  select(., patient_id, age, sex, date) %>%
  rename(date_diagnosis=date) %>%
  ungroup()


# Bæta við þeim sem eru clinically diagnosed
clinically_diagnosed_yes <- select(data_phone, patient_id, call_nr, age, sex, date_diagnosis, `diagnosis_clinical:Yes`) %>%
  group_by(patient_id) %>%
  filter(call_nr==min(call_nr)) %>%
  filter(`diagnosis_clinical:Yes`==1) %>%
  mutate(.,date_diagnosis=as.Date(date_diagnosis,"%Y-%m-%d")) %>%
  filter(!is.na(date_diagnosis)) %>%
  select(.,patient_id,age,sex,date_diagnosis) %>%
  ungroup() %>%
  filter(!is.na(patient_id))

individs_extended <- rbind(individs_extended,clinically_diagnosed_yes) 


# Bæta við línum fyrir þá sem vantar línur vegna icu
tmp <- bind_rows(select(data_lsh, patient_id, mortality, date_mort, date, hospital_day, icu_given, date_start_icu, date_end_icu, date_admission, date_discharge, admitted_from),
                 data_lsh %>% 
                   group_by(patient_id) %>% 
                   mutate(., n_rows=n()) %>% 
                   select(., patient_id, mortality, date_mort, date, hospital_day, n_rows, icu_given, date_start_icu, date_end_icu, date_admission, date_discharge, admitted_from) %>% 
                   filter(., n_rows<3, date_discharge-date_admission>1 | is.na(date_discharge), icu_given=="Já") %>%
                   mutate(., date=date_start_icu, hospital_day=as.numeric(date_start_icu-date_admission+1)))

tmp2 <- bind_rows(tmp, 
                  data_lsh %>% 
                    group_by(patient_id) %>% 
                    mutate(., n_rows=n()) %>% 
                    select(., patient_id, mortality, date_mort, date, hospital_day, n_rows, icu_given, date_start_icu, date_end_icu, date_admission, date_discharge, admitted_from) %>% 
                    filter(., n_rows<3, date_discharge-date_admission>1 | is.na(date_discharge), icu_given=="Já") %>%
                    mutate(., date=date_end_icu, hospital_day=as.numeric(date_end_icu-date_admission+1))) %>%
  group_by(patient_id) %>%
  mutate(n_rows=n())

hospital_visits_filtered <- select(tmp2 , patient_id, mortality, date_mort, date, hospital_day, icu_given, date_start_icu, date_end_icu, date_admission, date_discharge, admitted_from) %>%
  filter_at(vars(-patient_id, -hospital_day), any_vars(!is.na(.))) %>%
  filter(., date_admission<date_discharge | is.na(date_discharge)) %>% #Einhverjir með rangar dagsetningar
  mutate(., date=if_else(is.na(date), date_admission+hospital_day-1, date)) %>%
  mutate(., outcome=if_else(mortality=="Já", "death", "recovered")) %>%
  mutate(., in_hospital_before=admitted_from %in% c("Annarri deild"), text_out=ifelse(is.finite(date_discharge), "home", "at_hospital"),
         unit_in=if_else(icu_given == 'Nei', "inpatient_ward", if_else((date>=date_start_icu & date < date_end_icu) | (date>=date_start_icu & !is.finite(date_end_icu)), "intensive_care_unit", "inpatient_ward")), 
         unit_in=if_else(is.na(unit_in), "inpatient_ward", unit_in)) %>%
  group_by(patient_id, unit_in, text_out, in_hospital_before, date_start_icu, date_end_icu, date_admission, date_discharge, outcome, date_mort) %>%
  mutate(., date_in=case_when(unit_in=="intensive_care_unit" ~ date_start_icu,
                              unit_in=="inpatient_ward" & date<date_start_icu ~ date_admission,
                              unit_in=="inpatient_ward" & date>=date_end_icu ~ date_end_icu,
                              TRUE ~ date_admission),
         date_out=case_when(unit_in=="intensive_care_unit" ~ date_end_icu,
                            unit_in=="inpatient_ward" & date<=date_start_icu ~ date_start_icu,
                            unit_in=="inpatient_ward" & date>date_end_icu ~ date_discharge,
                            mortality=="Já" ~ date_mort,
                            TRUE ~ date_discharge)) %>%
  mutate(.,date_in=as.Date(date_in,"%Y-%m-%d")) %>%
  mutate(.,date_out=as.Date(date_out,"%Y-%m-%d")) %>%
  select(patient_id, unit_in, date_in, date_out, text_out, in_hospital_before, outcome, date_mort) %>%
  ungroup() %>% distinct()


### ath patient_id 417355 kemur 2x..
# skoða reglu með state_first=home
individs_extended <- left_join(individs_extended, select(hospital_visits_filtered, patient_id, outcome, unit_in, date_in, in_hospital_before, date_mort), by="patient_id") %>%
  left_join(., select(unit_categories, unit_category, unit_category_order), by=c("unit_in" = "unit_category")) %>%
  mutate(state_first=if_else(!in_hospital_before | is.na(in_hospital_before), "home", "inpatient_ward")) %>%
  group_by(patient_id) %>%
  mutate(state_worst_order=max(unit_category_order)) %>%
  left_join(., select(unit_categories, unit_category, unit_category_order), by=c("state_worst_order"="unit_category_order")) %>%
  select(., -state_worst_order, -unit_in, -date_in, -in_hospital_before, -unit_category_order) %>%
  distinct() %>%
  group_by(patient_id) %>%
  mutate(outcome=if_else(patient_id==417355, "recovered", outcome)) %>% #ein manneskja með tvær færslur
  distinct() %>%
  rename(.,state_worst=unit_category) %>%
  # left_join(., select(data_lsh , patient_id, risk), by="patient_id") %>%
  # rename(priority=`risk`) %>%
  # mutate(priority=gsub('\\s.*','', priority)) %>%
  # left_join(.,select(priority_categories,priority_raw,priority_all,priority_all_order),by=c('priority'='priority_raw')) %>%
  # mutate(.,priority=priority_all, priority_order=priority_all_order) %>%
  # select(-priority_all,-priority_all_order, -priority_order)
  ungroup()


#hafa recovered í outcome úr data_phone 
recovered_phone <- select(data_phone,patient_id,call_nr,date_discharge) %>%
  group_by(.,patient_id) %>%
  filter(call_nr == min(call_nr)) %>%
  mutate(.,outcome_phone=if_else(!is.na(patient_id),"recovered","NA")) %>%
  filter(!is.na(patient_id)) %>% 
  ungroup()

individs_extended <- left_join(individs_extended,select(recovered_phone,patient_id,outcome_phone),by='patient_id') %>%
  mutate(.,outcome_lagad=if_else(is.na(outcome),outcome_phone,outcome)) %>%
  select(.,patient_id,age,sex,date_diagnosis,state_first,outcome_lagad,date_mort,state_worst) %>%
  rename(outcome = outcome_lagad)


date_discharge_phone <- data_phone  %>%
  group_by(.,patient_id) %>%
  filter(call_nr==min(call_nr)) %>%
  mutate(.,date_discharge=as.Date(date_discharge,"%Y-%m-%d")) %>%
  filter(!is.na(patient_id)) %>%
  ungroup() 

individs_extended <- left_join(individs_extended,select(date_discharge_phone,patient_id,date_discharge), by= 'patient_id') #%>% 


date_symptoms_phone <- data_phone  %>%
  group_by(.,patient_id) %>%
  filter(call_nr==min(call_nr)) %>%
  mutate(.,date_symptoms=as.Date(date_symptoms,"%Y-%m-%d")) %>%
  filter(!is.na(patient_id)) %>%
  ungroup()

individs_extended <- left_join(individs_extended,select(date_symptoms_phone,patient_id,date_symptoms), by= 'patient_id') #%>%


#Undirliggjandi sjukdomar ur data_phone 
comorbidities_table <- select(data_phone ,patient_id,call_nr,date,dm_i,dm_ii,cardiovascular_disease,hypertension,pulmonary_disease,chronic_kidney_disease,current_cancer) 
comorbidities_phone <- comorbidities_table %>%
  mutate(.,num_comorbidity=rowSums(comorbidities_table[,-c(1,2,3)],na.rm=FALSE)) %>%
  group_by(.,patient_id,num_comorbidity) %>%
  filter(call_nr==min(call_nr)) %>%
  filter(!is.na(patient_id)) %>%
  ungroup()

individs_extended <- left_join(individs_extended,select(comorbidities_phone,patient_id,num_comorbidity), by= 'patient_id') %>%
  unique()


dates_hospital <- lapply(1:nrow(hospital_visits_filtered),function(i){
  date_out_imputed <- if_else(!is.finite(hospital_visits_filtered$date_out[i]),date_last_known_state,hospital_visits_filtered$date_out[i])
  tmp <- tibble(patient_id=hospital_visits_filtered$patient_id[i],
                state=hospital_visits_filtered$unit_in[i],
                date=seq(as.Date(hospital_visits_filtered$date_in[i]),as.Date(date_out_imputed), by=1))
  if(hospital_visits_filtered$text_out[i] %in% c('death','home')){
    tmp <- bind_rows(tmp,tibble(patient_id=hospital_visits_filtered$patient_id[i],
                                state=hospital_visits_filtered$text_out[i],
                                date=hospital_visits_filtered$date_out[i]))
  }
  return(tmp)}) %>%
  bind_rows() %>%
  group_by(.,patient_id,date) %>%
  summarize(state=tail(state,1)) %>%
  ungroup()

dates_home <- lapply(1:nrow(individs_extended),function(i){
  return(tibble(patient_id=individs_extended$patient_id[i],state='home',date=seq(individs_extended$date_diagnosis[i],date_last_known_state,by=1)))
}) %>%
  bind_rows() %>%
  left_join(.,select(individs_extended,patient_id,date_discharge),by='patient_id') %>%
  group_by(patient_id) %>%
  filter(!is.finite(date_discharge) | date<=date_discharge) %>%
  select(.,patient_id,state,date) %>%
  ungroup()


individs_extended <- mutate(individs_extended, date_outcome=if_else(is.na(date_mort), date_discharge, date_mort)) %>%
  select(., -date_mort)


patient_transitions <- right_join(dates_hospital,dates_home,by=c('patient_id','date'),suffix=c('_hospital','_home')) %>%
  mutate(.,state=if_else(!is.na(state_hospital) & state_hospital!=state_home,state_hospital,state_home)) %>%
  arrange(.,patient_id,date) %>%
  group_by(.,patient_id) %>%
  mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
  #group_by(.,patient_id,state_block_nr)%>%
  #mutate(severity=impute_severity(min(state),severity)) %>%
  ungroup() %>%
  #mutate(state=paste0(state,'_',severity)) %>%
  select(patient_id,date,state) %>%
  mutate(yesterday=date-1) %>%
  left_join(.,.,by=c('patient_id'='patient_id','date'='yesterday'),suffix=c('','_tomorrow')) %>%
  left_join(select(individs_extended,patient_id,outcome,date_outcome),by='patient_id') %>%
  filter(!(is.na(state_tomorrow) & outcome!='in_hospital_system')) %>%
  mutate(state_tomorrow=if_else(outcome=='recovered' & date_tomorrow==date_outcome & date_outcome<=date_last_known_state,'recovered',state_tomorrow)) %>%
  select(-yesterday,-date_tomorrow,-outcome,-date_outcome) %>%
  ungroup() %>%
  select(patient_id,date,state,state_tomorrow)