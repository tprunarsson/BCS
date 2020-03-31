Sys.setlocale("LC_ALL","IS_is")

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

today <- Sys.Date()

#date on input data and output files
current_date=as.Date('2020-03-30','%Y-%m-%d')
#we assume we only know the state of patient at midnight before current_date (except for patients diagnosed on current date)
date_last_known_state <- current_date-1

path_to_root <- '~/projects/covid/BCS/'
path_data <- paste0(path_to_root,'Data/')
path_coding <- paste0(path_to_root,'lsh_data_processing/')
path_hi_predictions <- paste0(path_to_root,'lsh_data_processing/')
path_to_output <- paste0(path_to_root,'Data/')

file_name_lsh_data <- '03282020 Covid-19__test_fyrir_spálíkan_dags_28.XLSX'
#file_name_lsh_data <- 'Covid-19__test_fyrir_spálíkan_dags_30_03_2020.XLSX'
file_path_coding <- 'lsh_coding.xlsx'
file_path_predictions <- 'Iceland_Predictions_2020-03-27.csv'

file_path_data <- paste0(path_data,file_name_lsh_data)

individs_raw <- read_excel(file_path_data,sheet = 'Einstaklingar', skip=3)
hospital_visits_raw <- read_excel(file_path_data,sheet ='Komur og innlagnir', skip=3)
interview_extra_raw <- read_excel(file_path_data,sheet = 'Áhættuflokkur ofl úr hóp', skip=3) 
interview_first_raw <- read_excel(file_path_data,sheet = 'Fyrsta viðtal úr forms', skip=3)
interview_follow_up_raw <- read_excel(file_path_data,sheet = 'Spurningar úr forms Pivot', skip=1)
NEWS_score_raw <- read_excel(file_path_data,sheet = 'NEWS score', skip=3)

unit_categories <- read_excel(file_path_coding,sheet = 3) %>% mutate(unit_category=unit_category_simple,unit_category_order=unit_category_order_simple)
text_out_categories <- read_excel(file_path_coding,sheet = 4) %>% mutate(text_out_category=text_out_category_simple)
hi_predictions_raw <- read_csv(file_path_predictions)

#Cleaning

#individs NOTE: duplicate entries 2020-03-28
individs <- rename(individs_raw,patient_id=`Person Key`,age=`Aldur heil ár`, sex=`Yfirfl. kyns`,zip_code=`Póstnúmer`,
                   covid_class=`Heiti sjúklingahóps`) %>% 
                    group_by(.,patient_id,zip_code,age,sex) %>%
                    summarize(.,covid_class=min(covid_class,na.rm=TRUE)) %>%
                    ungroup()

#hospital_visits
hospital_visits <- rename(hospital_visits_raw, patient_id=`Person Key`,unit_in=`Deild Heiti`,date_time_in=`Dagurtími innskriftar`, date_time_out=`Dagurtími útskriftar`, 
                          text_out=`Heiti afdrifa`,ventilator=`Öndunarvél`) %>% 
                  select(patient_id,unit_in,date_time_in,date_time_out,text_out,ventilator) %>%
                  mutate(date_time_out=gsub('9999-12-31 00:00:00',NA,date_time_out)) %>%
                  separate(col='date_time_in',into=c('date_in','time_in'),sep=' ',remove=FALSE) %>% 
                  separate(col='date_time_out',into=c('date_out','time_out'),sep=' ',remove=FALSE) %>%
                  mutate(.,date_in=as.Date(date_in,"%Y-%m-%d"),date_out=as.Date(date_out,"%Y-%m-%d")) %>% 
                  select(-time_in,-time_out) %>% 
                  mutate(ventilator=!is.na(ventilator)) 

#forms data
interview_first <- rename(interview_first_raw,patient_id=`Person Key`,date_first_symptoms=`Upphafsdagur einkenna`, date_diagnosis=`Dagsetning greiningar`,
                          priority=`Forgangur`, clinical_assessment=`Klínískt mat - flokkun`,date_clinical_assessment=`Síma eftirfylgd hefst`) %>% 
                    select(.,patient_id,date_first_symptoms,date_diagnosis,priority,date_clinical_assessment,clinical_assessment) %>%
                    mutate_at(.,vars(matches('date')),~as.Date(gsub('\\s.*','',.),"%Y-%m-%d")) %>%
                    mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                    group_by(.,patient_id) %>%
                    summarize_all(.,~min(.,na.rm=TRUE)) %>%
                    ungroup()

interview_follow_up <- rename(interview_follow_up_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`,clinical_assessment=`Klínískt mat`) %>%
                        select(patient_id,date_clinical_assessment,clinical_assessment) %>%
                        mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                        mutate(clinical_assessment=gsub('\\s.*','', clinical_assessment))

interview_extra <- rename(interview_extra_raw,patient_id=`Person Key`,date_time_clinical_assessment=`Dags breytingar`,col_name=`Heiti dálks`,col_value=`Skráningar - breytingar.Skráð gildi`) %>% 
                    select(.,patient_id,date_time_clinical_assessment,col_name,col_value) %>%
                    mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_time_clinical_assessment),"%Y-%m-%d")) %>%
                    group_by(.,patient_id,date_clinical_assessment,col_name) %>%
                    summarize(col_value=col_value[which.max(date_time_clinical_assessment)]) %>%
                    ungroup(.) %>%
                    spread(col_name,col_value) %>% 
                    rename(clinical_assessment=`Klínískt daglegt mat`, priority=`Áhættuflokkur`) %>%
                    mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                    select(patient_id,priority, date_clinical_assessment,clinical_assessment)

#create some help tables

hospital_visit_first_date <- group_by(hospital_visits,patient_id) %>% 
                              summarise(min_date_in=min(date_in,na.rm=TRUE)) %>%
                              ungroup()
# fix to remove redundancies due to simple classification
#filter out units that are not predicted and relabel the units in the terms to be predicted.Also relabel text_out
hospital_visits_filtered <- inner_join(hospital_visits,select(unit_categories,unit_category_raw,unit_category_all),by=c('unit_in'='unit_category_raw')) %>%
                            filter(!(unit_category_all) %in% c('emergency_room','outpatient_clinic')) %>% 
                            select(-unit_category_all) %>%
                            inner_join(.,select(unit_categories,unit_category_raw,unit_category),by=c('unit_in'='unit_category_raw')) %>%
                            mutate(unit_in=unit_category) %>%
                            select(-unit_category) %>%
                            inner_join(.,select(text_out_categories,text_out_category_raw,text_out_category),by=c('text_out'='text_out_category_raw')) %>%
                            mutate(.,text_out=text_out_category) %>%
                            select(.,-text_out_category) %>%
                            arrange(patient_id,date_time_in) #arrange precisely in time

hospital_outcomes <- group_by(hospital_visits_filtered,patient_id) %>%
                      summarise(.,outcome_tmp=if_else(any(grepl('death',text_out)),'death',NULL),
                                date_outcome_tmp=min(date_out[grepl('death',text_out)],na.rm=TRUE)) %>%
                      ungroup()

interview_exta_first_date <- group_by(interview_extra,patient_id) %>%
                              summarise(priority_tmp=min(priority,na.rm=TRUE),min_date_clincial_assessment=min(date_clinical_assessment,na.rm=TRUE)) %>%
                              ungroup()

#find last date for each patient
interview_last_date <- bind_rows(select(interview_first,patient_id,date_clinical_assessment),select(interview_follow_up,patient_id,date_clinical_assessment),
                                 select(interview_extra,patient_id,date_clinical_assessment)) %>%
                        filter(date_clinical_assessment<=current_date) %>% # We have some future dates - check.
                        group_by(.,patient_id) %>%
                        summarise(.,date_last_known=max(date_clinical_assessment,na.rm=TRUE)) %>%
                        ungroup()

#Add information about first diagnosis, first symptoms, and priority to individs
#First from hospital visits, then from interview extra and finally from forms (forms has highest priority).

individs_extended <- left_join(individs,hospital_visit_first_date,by='patient_id') %>%
                      left_join(.,interview_exta_first_date,by='patient_id') %>%
                      mutate(.,date_diagnosis_tmp=pmin(min_date_in,min_date_clincial_assessment,na.rm=TRUE)) %>%
                      select(.,-min_date_clincial_assessment,-min_date_in) %>%
                      left_join(.,select(interview_first,-date_clinical_assessment,-clinical_assessment),by='patient_id') %>%
                      mutate(.,date_diagnosis=pmin(date_diagnosis_tmp,date_diagnosis,na.rm=TRUE)) %>%
                      mutate(.,priority=ifelse(is.na(priority),priority_tmp,priority)) %>%
                      select(.,-date_diagnosis_tmp) %>%
                      filter(.,is.finite(date_diagnosis)) %>%
                      mutate(outcome=ifelse(covid_class=='COVID-19 útskrifaðir úr eftirliti','recovered','in_hospital_system')) %>%
                      left_join(.,hospital_outcomes,by='patient_id') %>%
                      mutate(outcome=if_else(!is.na(outcome_tmp),outcome_tmp,outcome)) %>%
                      select(-outcome_tmp) %>%
                      left_join(.,interview_last_date,by='patient_id') %>%
                      mutate(.,date_outcome=pmin(date_outcome_tmp,if_else(covid_class=='COVID-19 útskrifaðir úr eftirliti',date_last_known,NULL),na.rm=TRUE)) %>%
                      mutate(.,age_group_std=as.character(cut(age,breaks=c(-Inf,seq(10,80,by=10),Inf),labels=c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80+'),right=FALSE)),
                             age_group_simple=as.character(cut(age,breaks=c(-Inf,50,Inf),labels=c('0-50','51+'),right=TRUE))) %>%
                      select(.,patient_id,zip_code,age,age_group_std,age_group_simple,sex,priority,date_first_symptoms,date_diagnosis,outcome,date_outcome)

#patient transitions.
#Start by assuming everybody is at home from the time diagnosed to today
#Note: patients diagnosed on current_date have a known state on that date, but others do not. Applies both for dates_home and dates_hospital

dates_home <- lapply(1:nrow(individs_extended),function(i){  
    date_home_latest_imputed <- if_else(individs_extended$date_diagnosis[i]==current_date,current_date, date_last_known_state)
                return(tibble(patient_id=individs_extended$patient_id[i],state='home',date=seq(individs_extended$date_diagnosis[i],date_home_latest_imputed,by=1))) 
              }) %>% 
              bind_rows() %>%
              left_join(.,select(individs_extended,patient_id,date_outcome),by='patient_id') %>%
              group_by(patient_id) %>%
              filter(!is.finite(date_outcome) | date<=date_outcome) %>%
              select(-date_outcome) %>%
              ungroup()


dates_hospital <- lapply(1:nrow(hospital_visits_filtered),function(i){
  date_out_imputed <- if_else(is.na(hospital_visits_filtered$date_out[i]),if_else(hospital_visits_filtered$date_in[i]==current_date,current_date,date_last_known_state),hospital_visits_filtered$date_out[i])
  tmp <- tibble(patient_id=hospital_visits_filtered$patient_id[i],
                state=hospital_visits_filtered$unit_in[i],
                date=seq(hospital_visits_filtered$date_in[i],date_out_imputed,by=1))
  if(hospital_visits_filtered$text_out[i] %in% c('death','home')){
    tmp <- bind_rows(tmp,tibble(patient_id=hospital_visits_filtered$patient_id[i],
                                state=hospital_visits_filtered$text_out[i],
                                date=hospital_visits_filtered$date_out[i]))
  }
  return(tmp)
}) %>% bind_rows()

 
patient_transitions <- right_join(dates_hospital,dates_home,by=c('patient_id','date'),suffix=c('_hospital','_home')) %>%
                        mutate(state=if_else(state_hospital!=state_home & !is.na(state_hospital),state_hospital,state_home)) %>%
                        select(-state_hospital,-state_home) %>%
                        group_by(.,patient_id,date) %>%
                        summarize(state=tail(state,1)) %>%
                        ungroup() %>%
                        mutate(yesterday=date-1) %>% 
                        left_join(.,.,by=c('patient_id'='patient_id','date'='yesterday'),suffix=c('','_tomorrow')) %>%
                        filter(!is.na(state_tomorrow)) %>%
                        select(-yesterday,-date_tomorrow)

recovered_transitions <- mutate(patient_transitions,tomorrow=date+1) %>%
                          right_join(., select(individs_extended, patient_id, outcome, date_outcome), by=c('patient_id','tomorrow'='date_outcome')) %>%
                          filter(outcome=='recovered',!is.na(state)) %>%
                          mutate(state_tomorrow=outcome) %>%
                          select(-tomorrow,-outcome)

patient_transitions <- bind_rows(patient_transitions,recovered_transitions)

#Add worst case state to each patient
#Find those who have at least one transition
state_worst_case <- inner_join(distinct(patient_transitions,patient_id,state),unit_categories,by=c('state'='unit_category')) %>%
                    group_by(.,patient_id) %>%
                    summarize(.,state_worst=state[which.max(unit_category_order)]) %>%
                    ungroup()
#Find those who have no transition e.g. are new today.
state_worst_case_special <- anti_join(select(individs_extended,patient_id),state_worst_case,by='patient_id') %>%
                            mutate(.,state_worst='home') %>%
                            left_join(.,select(hospital_visits_filtered,patient_id,unit_in),by='patient_id') %>%
                            mutate(state_worst=if_else(is.na(unit_in),state_worst,unit_in)) %>%
                            left_join(.,unit_categories,by=c('state_worst'='unit_category')) %>%
                            group_by(.,patient_id) %>%
                            summarize(.,state_worst=state_worst[which.max(unit_category_order)]) %>%
                            ungroup()

state_worst_case <- bind_rows(state_worst_case,state_worst_case_special)

individs_extended <- left_join(individs_extended,state_worst_case,by='patient_id')


#identify and sequencially number blocks of states for each patient_id 
get_state_block_numbers <- function(state_vec){
  state_nr=1
  state_block_numbers <- c()
  
  if(length(state_vec)==0){
    return(state_block_numbers)
  }
  
  state_block_numbers[1]=state_nr=1
  
  if(length(state_vec)==1){
    return(state_block_numbers)
  }
  for(i in 2:length(state_vec)){
    if(state_vec[i]!=state_vec[i-1]){
      state_nr <- state_nr+1
    }
    state_block_numbers[i] <- state_nr
  }
  return(state_block_numbers)
}

patient_transitions_state_blocks <- group_by(patient_transitions,patient_id) %>%
  mutate(state_block_nr=get_state_block_numbers(state))
#summarise state blocks, extracting min and max date to calculate length of each state. Note:states entered yesterday are not used to estimate length of stay
patient_transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr) %>% arrange(.,date) %>% 
  summarize(state=min(state,na.rm=TRUE),state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=tail(state_tomorrow,1)) %>%
  mutate(censored=(state==state_next)) %>%
  mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+if_else(state!=state_next,1,2)) %>%
  ungroup()

