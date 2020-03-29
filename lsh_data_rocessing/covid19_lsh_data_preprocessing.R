Sys.setlocale("LC_ALL","IS_is")

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

today <- Sys.Date()

path_data <-'~/COVID19/Data/'
path_coding <- '~/COVID19/Processing/'
path_hi_predictions <- '~/COVID19/Processing/'

file_name_lsh_data <- '03282020 Covid-19__test_fyrir_spálíkan_dags_28.XLSX'
file_name_lsh_coding <- 'lsh_coding.xlsx'
file_name_hi_predictions <- 'Iceland_Predictions_2020-03-27.csv'

file_path_data <- paste0(path_data,file_name_lsh_data)
file_path_coding <- paste0(path_coding,file_name_lsh_coding)
file_path_predictions <- paste0(path_hi_predictions,file_name_hi_predictions)

individs_raw <- read_excel(file_path_data,sheet = 1, skip=3)
hospital_visits_raw <- read_excel(file_path_data,sheet =2, skip=3)
interview_extra_raw <- read_excel(file_path_data,sheet = 3, skip=3) 
interview_first_raw <- read_excel(file_path_data,sheet = 4, skip=3)
interview_follow_up_raw <- read_excel(file_path_data,sheet = 5, skip=1)
NEWS_score_raw <- read_excel(file_path_data,sheet = 6, skip=3)

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

#filter out units that are not predicted and relabel the units in the terms to be predicted.Also relabel text_out
hospital_visits_filtered <- inner_join(hospital_visits,select(unit_categories,unit_category_raw,unit_category),by=c('unit_in'='unit_category_raw')) %>%
                            mutate(unit_in=unit_category) %>%
                            select(-unit_category) %>%
                            inner_join(.,select(text_out_categories,text_out_category_raw,text_out_category),by=c('text_out'='text_out_category_raw')) %>%
                            mutate(.,text_out=text_out_category) %>%
                            select(.,-text_out_category)

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
                        filter(date_clinical_assessment<=today) %>% # We have some future dates - check.
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
                      filter(.,!is.na(date_diagnosis)) %>%
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

dates_home <- lapply(1:nrow(individs_extended),function(i){     
                return(tibble(patient_id=individs_extended$patient_id[i],state='home',date=seq(individs_extended$date_diagnosis[i],today,by=1))) 
              }) %>% 
              bind_rows() %>%
              left_join(.,select(individs_extended,patient_id,date_outcome),by='patient_id') %>%
              group_by(patient_id) %>%
              filter(!is.finite(date_outcome) | date<=date_outcome) %>%
              select(-date_outcome) %>%
              ungroup()


dates_hospital <- lapply(1:nrow(hospital_visits_filtered),function(i){
  date_out_imputed <- if_else(is.na(hospital_visits_filtered$date_out[i]),today,hospital_visits_filtered$date_out[i])
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

#Here we start to generate output

current_date=as.Date('2020-03-28','%Y-%m-%d')

#transition matrix


states_active <- distinct(unit_categories,unit_category,unit_category_order) %>%
                  arrange(unit_category_order) %>%
                  select(unit_category) %>%
                  unlist() %>%
                  unname()
states_end <- c('death','recovered')
states <- c(states_active,states_end)

#all age groups
state_transitions_all <- expand.grid(states,states,stringsAsFactors = FALSE) %>%
                          rename(state=Var1,state_tomorrow=Var2) %>%
                          as_tibble()
patient_transition_counts_all <- group_by(patient_transitions,state,state_tomorrow) %>% summarise(count=as.numeric(n())) %>% 
                              right_join(.,state_transitions_all,by=c('state','state_tomorrow')) %>%
                              mutate(count=if_else(is.na(count),0,count))
patient_transition_counts_matrix_all <- matrix(patient_transition_counts_all$count,ncol=length(states),nrow=length(states))
write.table(patient_transition_counts_matrix_all,file=paste0('~/Downloads/transition_matrix_',current_date,'.csv'),sep=',',row.names=FALSE,col.names=states,quote=FALSE)

#simple age groups
age_group_simple=c('0-50','51+')
state_transitions_age_simple <- expand.grid(age_group_simple,states,states,stringsAsFactors = FALSE) %>%
                                    rename(age_group_simple=Var1,state=Var2,state_tomorrow=Var3) %>%
                                    as_tibble()
patient_transition_counts_age_simple <- inner_join(select(individs_extended,patient_id,age_group_simple),patient_transitions,by='patient_id') %>%
                                        group_by(.,age_group_simple,state,state_tomorrow) %>%
                                        summarize(count=as.numeric(n())) %>%
                                        group_by(age_group_simple) %>%
                                        right_join(.,state_transitions_age_simple,by=c('age_group_simple','state','state_tomorrow')) %>%
                                        mutate(count=if_else(is.na(count),0,count)) %>%
                                        ungroup()
  
patient_transition_counts_matrix_age_simple_under_50 <- filter(patient_transition_counts_age_simple,age_group_simple=='0-50') %>% 
select(count) %>% unlist() %>% 
matrix(.,ncol=length(states),nrow=length(states))
patient_transition_counts_matrix_age_simple_over_50 <- filter(patient_transition_counts_age_simple,age_group_simple=='51+') %>% 
select(count) %>% 
unlist() %>% 
matrix(.,ncol=length(states),nrow=length(states))
write.table(patient_transition_counts_matrix_age_simple_under_50,file=paste0('~/Downloads/transition_matrix_under_50_',current_date,'.csv'),sep=',',row.names=F,col.names=states,quote=F)
write.table(patient_transition_counts_matrix_age_simple_over_50,file=paste0('~/Downloads/transition_matrix_over_50_',current_date,'.csv'),sep=',',row.names=F,col.names=states,quote=F)

# get length of stay for each state

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
                            
patient_transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr) %>%
                                    summarize(state=min(state,na.rm=TRUE),state_block_nr_start=min(date,na.rm=TRUE),state_block_nr_end=max(date,na.rm=TRUE)) %>%
                                    mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)) %>%
                                    ungroup()
                          
#current state

current_state <-  filter(patient_transitions_state_blocks,date==current_date) %>%
                  inner_join(.,select(patient_transitions_state_blocks_summary,patient_id,state_block_nr,state_block_nr_start),by=c('patient_id','state_block_nr')) %>%
                  mutate(days_in_state=as.numeric(current_date-state_block_nr_start)) %>%
                  inner_join(individs_extended,.,by='patient_id') %>%
                  mutate(days_from_diagnosis=as.numeric(current_date-date_diagnosis)) %>%
                  select(patient_id,age,sex,state,days_in_state,days_from_diagnosis)

write.table(current_state,file=paste0('~/Downloads/current_state_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#length of stay by age 

length_of_stay_by_age_simple <- inner_join(select(patient_transitions_state_blocks_summary,patient_id,state,state_block_nr,state_duration),
                                           select(individs_extended,patient_id,age_group_simple), by='patient_id') %>%
                                            group_by(state,age_group_simple) %>%
                                            group_by(state,age_group_simple,state_duration) %>%
                                            summarise(count=n()) %>%
                                            arrange(state,age_group_simple)

write.table(length_of_stay_by_age_simple,file=paste0('~/Downloads/length_of_stay_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#first state of all patients that have been diagnosed

first_state <- group_by(hospital_visits_filtered,patient_id) %>%
                summarize(initial_state_hospital=unit_in[which.min(date_time_in)],min_date_in=min(date_in,na.rm=TRUE)) %>%
                right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
                mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
                select(age,sex,initial_state)

write.table(first_state,file=paste0('~/Downloads/first_state_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#get predictions from covid hi model

hi_predictions <- filter(hi_predictions_raw,name=='cases',type=='new',age=='total') %>%
                  select(date,median,upper)

write.table(hi_predictions,file=paste0('~/Downloads/hi_predictions_','2020-03-27','.csv'),sep=',',row.names=F,quote=F)

