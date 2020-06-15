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

##### ----- Read data ----- #####

path_to_data <- '~/projects/covid/BCS/lsh_data_new/'
#path_to_data <- ''


date_data <- as.Date('2020-05-03','%Y-%m-%d')
date_last_known_state <- as.Date('2020-05-15','%Y-%m-%d')

file_path_bb_data <- paste0(path_to_data,'df_bb_2020-05-05.csv')
file_path_lsh_data <- paste0(path_to_data,'df_lsh_2020-05-05.csv')
file_path_pcr_data <- paste0(path_to_data,'df_pcr_2020-05-09.csv')
file_path_phone_data <- paste0(path_to_data,'df_phone_2020-05-13.csv')
file_path_covid_id <- paste0(path_to_data,'2020-06-04_covid_id_conversion.xlsx')
file_path_coding <- 'lsh_coding.xlsx'

data_bb  <- suppressMessages(read_csv(file_path_bb_data))
data_lsh  <- suppressMessages(read_csv(file_path_lsh_data))
data_pcr  <- suppressMessages(read_csv(file_path_pcr_data))
data_phone  <- suppressMessages(read_csv(file_path_phone_data, col_types = cols(freetext_immunosuppression='c')))

data_covid_id  <- read_excel(file_path_covid_id, skip = 3)
covid_id <- rename(data_covid_id , kt = `Kennitala/gervikennitala`, person_id = `Person Key`)

#Tengjum kennitölur og person_id saman
data_bb  <- left_join(data_bb ,covid_id, by ='kt')
data_lsh  <- left_join(data_lsh ,covid_id, by ='kt')
data_pcr  <- left_join(data_pcr ,covid_id, by ='kt')
data_phone  <- left_join(data_phone ,covid_id, by ='kt')

# Categories töflur
priority_categories <- read_excel(file_path_coding,sheet = 'priority_categories')
age_groups <- read_excel(file_path_coding,sheet = 'age_groups')
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories')
unit_category_type <- 'simple'
unit_categories <- read_excel(file_path_coding,sheet = 'lsh_unit_categories') %>%
  mutate(unit_category=!!as.name(paste0('unit_category_',unit_category_type)),
         unit_category_order=!!as.name(paste0('unit_category_order_',unit_category_type)))

##### -----Processing----- #####

individs_extended <-select(data_pcr , kt, sex, age, test_nr, date, result) %>%
                    filter(result=='positive') %>%
                    mutate(.,date_diagnosis=as.Date(date,"%Y-%m-%d")) %>%
                    group_by(kt) %>%
                    filter(date_diagnosis==min(date_diagnosis))  %>% 
                    filter(test_nr==min(test_nr)) %>% 
                    filter(age==min(age)) %>%
                    select(., kt, age, sex, date) %>%
                    rename(date_diagnosis=date) %>%
                    ungroup()

hospital_visits_filtered <- select(data_lsh , kt, mortality, date_mort, date, hospital_day, icu_given, date_start_icu, date_end_icu, date_admission, date_discharge, admitted_from) %>%
                            filter_at(vars(-kt, -hospital_day), any_vars(!is.na(.))) %>%
                            filter(., date_admission<date_discharge | is.na(date_discharge)) %>% #Einhverjir með rangar dagsetningar
                            mutate(., date=if_else(is.na(date), date_admission+hospital_day-1, date)) %>%
                            mutate(., outcome=if_else(mortality=="Já", "death", "recovered")) %>%
                            mutate(., in_hospital_before=admitted_from %in% c("Annarri deild"), text_out=ifelse(is.finite(date_discharge), "home", "at_hospital"),
                                    unit_in=if_else(icu_given == 'Nei', "inpatient_ward", if_else((date>=date_start_icu & date <= date_end_icu) | (date>=date_start_icu & !is.finite(date_end_icu)), "intensive_care_unit", "inpatient_ward")), 
                                    unit_in=if_else(is.na(unit_in), "inpatient_ward", unit_in)) %>%
                            group_by(kt, unit_in, text_out, in_hospital_before, date_start_icu, date_end_icu, date_admission, date_discharge, outcome, date_mort) %>%
                            summarize(date_in=min(date), date_out=max(date)) %>%
                            ungroup() %>%
                            mutate(date_in=if_else(unit_in=="intensive_care_unit", date_start_icu, date_admission), 
                                    date_out=if_else(unit_in=="intensive_care_unit", date_end_icu, date_discharge)) %>%
                            mutate(.,date_in=as.Date(date_in,"%Y-%m-%d")) %>%
                            mutate(.,date_out=as.Date(date_out,"%Y-%m-%d")) %>%
                            select(kt, unit_in, date_in, date_out, text_out, in_hospital_before, outcome, date_mort) %>%
                            ungroup()

# skoða reglu með state_first=home
individs_extended <- left_join(individs_extended, select(hospital_visits_filtered, kt, outcome, unit_in, date_in, in_hospital_before, date_mort), by="kt") %>%
                     left_join(., select(unit_categories, unit_category, unit_category_order), by=c("unit_in" = "unit_category")) %>%
                     mutate(state_first=if_else(!in_hospital_before | is.na(in_hospital_before), "home", "inpatient_ward")) %>%
                     group_by(kt, age, sex, date_diagnosis, state_first, outcome, date_mort) %>%
                     summarise(state_worst_order=max(unit_category_order)) %>%
                     left_join(., select(unit_categories, unit_category, unit_category_order), by=c("state_worst_order"="unit_category_order")) %>%
                     select(., -state_worst_order) %>%
                     distinct() %>%
                     rename(.,state_worst=unit_category) %>%
                     # left_join(., select(data_lsh , kt, risk), by="kt") %>%
                     # rename(priority=`risk`) %>%
                     # mutate(priority=gsub('\\s.*','', priority)) %>%
                     # left_join(.,select(priority_categories,priority_raw,priority_all,priority_all_order),by=c('priority'='priority_raw')) %>%
                     # mutate(.,priority=priority_all, priority_order=priority_all_order) %>%
                     # select(-priority_all,-priority_all_order, -priority_order)
                     ungroup()

interview_last_date <- data_phone  %>%
                       group_by(.,kt) %>%
                       filter(call_nr==min(call_nr)) %>%
                       filter(`diagnosis_clinical:Yes`==0) %>%
                       ungroup() 

#Kannski otharfi ad setja date_lastphonecall i individs_extended                             
individs_extended <- left_join(individs_extended,select(interview_last_date,kt,date_lastphonecall), by= 'kt') #%>%    

date_discharge_phone <- data_phone  %>%
                        group_by(.,kt) %>%
                        filter(call_nr==min(call_nr)) %>%
                        filter(`diagnosis_clinical:Yes`==0) %>%
                        mutate(.,date_discharge=as.Date(date_discharge,"%Y-%m-%d")) %>%
                        ungroup() 

individs_extended <- left_join(individs_extended,select(date_discharge_phone,kt,date_discharge), by= 'kt') #%>% 

date_symptoms_phone <- data_phone  %>%
                       group_by(.,kt) %>%
                       filter(call_nr==min(call_nr)) %>%
                       filter(`diagnosis_clinical:Yes`==0) %>%
                       mutate(.,date_symptoms=as.Date(date_symptoms,"%Y-%m-%d")) %>%
                       ungroup()

individs_extended <- left_join(individs_extended,select(date_symptoms_phone,kt,date_symptoms), by= 'kt') #%>%

#Undirliggjandi sjukdomar ur data_phone 
comorbidities_tafla <- select(data_phone ,kt,call_nr,date,dm_i,dm_ii,cardiovascular_disease,hypertension,pulmonary_disease,chronic_kidney_disease,current_cancer) 
comorbidities_phone <- comorbidities_tafla %>%
                       mutate(.,num_comorbidity=rowSums(comorbidities_tafla[,-c(1,2,3)],na.rm=FALSE)) %>%
                       group_by(.,kt,num_comorbidity) %>%
                       filter(call_nr==min(call_nr)) %>%
                       ungroup()

individs_extended <- left_join(individs_extended,select(comorbidities_phone,kt,num_comorbidity), by= 'kt') %>%
                     unique()


dates_hospital <- lapply(1:nrow(hospital_visits_filtered),function(i){
                  date_out_imputed <- if_else(!is.finite(hospital_visits_filtered$date_out[i]),date_last_known_state,hospital_visits_filtered$date_out[i])
                  tmp <- tibble(kt=hospital_visits_filtered$kt[i],
                          state=hospital_visits_filtered$unit_in[i],
                          date=seq(as.Date(hospital_visits_filtered$date_in[i]),as.Date(date_out_imputed), by=1))
                  if(hospital_visits_filtered$text_out[i] %in% c('death','home')){
                        tmp <- bind_rows(tmp,tibble(kt=hospital_visits_filtered$kt[i],
                                state=hospital_visits_filtered$text_out[i],
                                date=hospital_visits_filtered$date_out[i]))
                  }
                  return(tmp)}) %>% 
                  bind_rows() %>%
                  group_by(.,kt,date) %>%
                  summarize(state=tail(state,1)) %>%
                  ungroup()

dates_home <- lapply(1:nrow(individs_extended),function(i){  
              return(tibble(kt=individs_extended$kt[i],state='home',date=seq(individs_extended$date_diagnosis[i],date_last_known_state,by=1))) 
              }) %>% 
              bind_rows() %>%
              left_join(.,select(individs_extended,kt,date_discharge),by='kt') %>%
              group_by(kt) %>%
              filter(!is.finite(date_discharge) | date<=date_discharge) %>%
              select(.,kt,state,date) %>%
              ungroup()

individs_extended <- mutate(individs_extended, date_outcome=if_else(is.na(date_mort), date_discharge, date_mort)) %>%
                     select(., -date_mort)
patient_transitions <- right_join(dates_hospital,dates_home,by=c('kt','date'),suffix=c('_hospital','_home')) %>%
                       mutate(.,state=if_else(!is.na(state_hospital) & state_hospital!=state_home,state_hospital,state_home)) %>%
                       arrange(.,kt,date) %>%
                       group_by(.,kt) %>%
                       mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
                       group_by(.,kt,state_block_nr)%>%
                       #mutate(severity=impute_severity(min(state),severity)) %>%
                       ungroup() %>%
                       #mutate(state=paste0(state,'_',severity)) %>%
                       select(kt,date,state) %>%
                       mutate(yesterday=date-1) %>% 
                       left_join(.,.,by=c('kt'='kt','date'='yesterday'),suffix=c('','_tomorrow')) %>%
                       left_join(select(individs_extended,kt,outcome,date_outcome),by='kt') %>%
                       filter(!(is.na(state_tomorrow) & outcome!='in_hospital_system')) %>%
                       mutate(state_tomorrow=if_else(outcome=='recovered' & date_tomorrow==date_outcome & date_outcome<=date_last_known_state,'recovered',state_tomorrow)) %>%
                       select(-yesterday,-date_tomorrow,-outcome,-date_outcome) %>%
                       ungroup() %>%
                       select(kt,date,state,state_tomorrow)


