#!/usr/bin/env Rscript
Sys.setlocale("LC_ALL","IS_is")
library(optparse)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
source('test_covid19_lsh_data_processing.R')
source('create_input_for_simulation.R')
source('impute_length_of_stay.R')

current_date_tmp <- as.Date('2020-04-10','%Y-%m-%d')
prediction_date_tmp <- as.Date('2020-04-08','%Y-%m-%d')
path_to_lsh_data_tmp <- '~/projects/covid/BCS/lsh_data/'
write_tables_for_simulation_tmp <- TRUE
save_additional_data_tmp <- FALSE
max_num_days_inpatient_ward <- 21
max_num_days_intensive_care_unit <- 28

option_list <-  list(
  make_option(c("-c", "--current_date"), type="character", default=NULL, 
              help="current date of data being used", metavar="character"),
  make_option(c("-p", "--prediction_date"), type="character", default=NULL, 
              help="date of prediction from covid.hi.is", metavar="character"),
  make_option(c("-d", "--path_to_lsh_data"), type="character", default=path_to_lsh_data_tmp, 
              help="path to data from LSH", metavar="character")
) 

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['current_date']])){
  current_date <- current_date_tmp 
  warning(paste0('You did not provide a current date. ',current_date,' will be used'))
}else{
  current_date <- as.Date(as.character(opt['current_date']),'%Y-%m-%d')
}

if(is.null(opt[['prediction_date']])){
  prediction_date <- prediction_date_tmp 
  warning(paste0('You did not provide a prediction date. ',prediction_date,' will be used'))
}else{
  prediction_date <- as.Date(as.character(opt['prediction_date']),'%Y-%m-%d')
}

if(opt[['path_to_lsh_data']]==path_to_lsh_data_tmp){
  path_to_lsh_data <- path_to_lsh_data_tmp
}else{
  path_to_lsh_data <- opt[['path_to_lsh_data']]
}

if(length(opt)>2){
  write_tables_for_simulation <- TRUE
  save_additional_data <- TRUE
}else{
  write_tables_for_simulation <- write_tables_for_simulation_tmp
  save_additional_data <- save_additional_data_tmp
}

#date of prediction by covid.hi.is


#we assume we only know the state of patient at midnight before current_date (except for patients diagnosed on current date)
date_last_known_state <- current_date-1

#Assuming working directory is lsh_data_processing in github repo
path_tables <- '../input/'
path_sensitive_tables <- '../lsh_data/'
path_dashboard_tables <- '../dashboard/input/'

file_name_lsh_data <- paste0(current_date,'_lsh_covid_data.xlsx')
file_path_coding <- 'lsh_coding.xlsx'
file_path_data <- paste0(path_to_lsh_data,file_name_lsh_data)

################## ----- Read LSH data and coding data ----- ########################################

individs_raw <- read_excel(file_path_data,sheet = 'Einstaklingar', skip=3)
hospital_visits_raw <- read_excel(file_path_data,sheet ='Komur og innlagnir', skip=3)
interview_first_raw <- read_excel(file_path_data,sheet = 'Fyrsta viðtal úr forms', skip=3)
interview_follow_up_raw <- read_excel(file_path_data,sheet = 'Spurningar úr forms Pivot', skip=1)
interview_last_raw <- read_excel(file_path_data,sheet = 'Lokaviðtal-Spurning úr forms', skip=1)
interview_extra_raw <- read_excel(file_path_data,sheet = 'Áhættuflokkur ofl úr hóp', skip=3) 
NEWS_score_raw <- read_excel(file_path_data,sheet = 'NEWS score ', skip=3)
ventilator_info_raw <- read_excel(file_path_data,sheet = 'NEWS score ', skip=3)


covid_groups <- read_excel(file_path_coding,sheet = 1)
unit_categories <- read_excel(file_path_coding,sheet = 3) %>% mutate(unit_category=unit_category_simple,
                                                                     unit_category_order=unit_category_order_simple)
text_out_categories <- read_excel(file_path_coding,sheet = 4) %>% mutate(text_out_category=text_out_category_simple)

clinical_assessment_categories <- read_excel(file_path_coding,sheet = 5) %>% mutate(clinical_assessment_category=clinical_assessment_category_simple,
                                                                                    clinical_assessment_category_order=clinical_assessment_category_order_simple)
priority_categories <- read_excel(file_path_coding,sheet = 7) %>% mutate(priority_category=priority_category_simple,
                                                                          priority_category_order=priority_category_order_simple)
sheet_names <- read_excel(file_path_coding,sheet = 6,trim_ws = FALSE)

comorbidities_categories <- read_excel(file_path_coding,sheet = 8) %>%
  arrange(comorbidities_raw)


#test_lsh_data_file()

################## ----- Cleaning ----- ##############################################################

individs <- rename(individs_raw,patient_id=`Person Key`,age=`Aldur heil ár`, sex=`Yfirfl. kyns`,zip_code=`Póstnúmer`,
                   covid_group_raw=`Heiti sjúklingahóps`) %>% 
                    left_join(.,select(covid_groups,covid_group_raw,covid_group),by='covid_group_raw') %>%
                    group_by(.,patient_id) %>%
                    summarize(.,zip_code=min(zip_code),age=min(age),sex=min(sex),covid_group=if_else(any(grepl('recovered',covid_group)),'recovered','infected')) %>%
                    ungroup()

#forms data
interview_first <- rename(interview_first_raw,patient_id=`Person Key`,date_first_symptoms=`Upphafsdagur einkenna`, date_diagnosis=`Dagsetning greiningar`,
                          priority=`Forgangur`, clinical_assessment=`Klínískt mat - flokkun`,date_clinical_assessment=`Síma eftirfylgd hefst`,comorbidities_raw=`Sjúkdómar`) %>% 
                    select(.,patient_id,date_first_symptoms,date_diagnosis,priority,date_clinical_assessment,clinical_assessment,comorbidities_raw) %>%
                    mutate_at(.,vars(matches('date')),~as.Date(gsub('\\s.*','',.),"%Y-%m-%d")) %>%
                    mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                    left_join(.,priority_categories,by=c('priority'='priority_category_raw')) %>%
                    mutate(.,priority=priority_category) %>%
                    left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                    mutate(.,clinical_assessment=clinical_assessment_category) %>%
                    group_by(.,patient_id) %>%
                    summarise(.,date_first_symptoms=min(date_first_symptoms,na.rm=TRUE),
                                    date_diagnosis=min(date_diagnosis,na.rm=TRUE),
                                    date_clinical_assessment=min(date_clinical_assessment,na.rm=TRUE),
                                    #date_clinical_assessment=if(!any(is.finite(clinical_assessment))) NA else date_clinical_assessment[which.max(clinical_assessment_category_order)],
                                    priority=if(all(is.na(priority))) NA_character_ else priority[which.max(priority_category_order)],
                                    clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)],
                                    comorbidities_raw=if(any(!is.na(comorbidities_raw))) paste(comorbidities_raw[!is.na(comorbidities_raw)],collapse="; ") else NA_character_ #So as to not lose data we paste together the raw comorbidities of duplictes. Deal with duplicated comorbidities below.
                              ) %>%
                    filter(.,if_else(is.finite(date_diagnosis),date_diagnosis<=date_last_known_state,TRUE)) %>%
                    ungroup()%>%
                    separate(comorbidities_raw,into = paste("comorbidity",c(1:10)),sep="; ") %>% #Adding comorbidities
                    gather(key="comorb_number",value="comorbidity",matches("comorbidity")) %>%
                    distinct(patient_id,comorbidity,.keep_all=T) %>%
                    arrange(comorb_number) %>%
                    filter(!is.na(comorbidity) | !duplicated(patient_id)) %>%
                    left_join(comorbidities_categories,by=c("comorbidity"="comorbidities_raw"))%>%
                    mutate(comorbidity=paste("comorb_",comorbidities_names,sep=""))%>%
                    group_by(patient_id)%>%
                    mutate(n_comorbidity=sum(comorbidities_included,na.rm=T))%>%
                    ungroup() %>%
                    select(-comorbidities_included,-comorbidities_names)%>%
                    pivot_wider(.,id_cols=c("patient_id","date_first_symptoms","date_diagnosis","date_clinical_assessment","priority","clinical_assessment","n_comorbidity"),
                                names_from = "comorbidity",values_from="comorbidity") %>%
                    select(.,-comorb_NA,-comorb_healthy) %>%
                    mutate_at(vars(matches('comorb_')),~!is.na(.))

#note, min clinical assessment chosen for each date. TODO: get time stamps of interviews
interview_follow_up <- rename(interview_follow_up_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`,clinical_assessment=`Klínískt mat`) %>%
                        select(patient_id,date_clinical_assessment,clinical_assessment) %>%
                        mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                        filter(.,is.finite(date_clinical_assessment)) %>%
                        filter(.,date_clinical_assessment<=date_last_known_state) %>%
                        mutate(.,clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                        left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                        mutate(.,clinical_assessment=clinical_assessment_category) %>%
                        select(.,patient_id,date_clinical_assessment,clinical_assessment,clinical_assessment_category_order) %>%
                        group_by(.,patient_id,date_clinical_assessment) %>%
                        summarise(.,clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)])
                        

#date_clinical_assessment is the last interview by a physician. Remove scheduled future phone calls
interview_last <- rename(interview_last_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`) %>%
                  mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                  filter(is.finite(date_clinical_assessment)) %>%
                  filter(date_clinical_assessment<=date_last_known_state) %>%
                  group_by(.,patient_id) %>%
                  summarise(.,date_clinical_assessment=max(date_clinical_assessment,na.rm=T))
    

interview_extra <- rename(interview_extra_raw,patient_id=`Person Key`,date_time_clinical_assessment=`Dags breytingar`,col_name=`Heiti dálks`,col_value=`Skráningar - breytingar.Skráð gildi`) %>% 
                    select(.,patient_id,date_time_clinical_assessment,col_name,col_value) %>%
                    mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_time_clinical_assessment),"%Y-%m-%d")) %>%
                    group_by(.,patient_id,date_clinical_assessment,col_name) %>%
                    summarize(date_time_clinical_assessment=max(date_time_clinical_assessment),col_value=col_value[which.max(date_time_clinical_assessment)]) %>%
                    ungroup(.) %>%
                    spread(col_name,col_value) %>% 
                    rename(clinical_assessment=`Klínískt daglegt mat`, priority=`Áhættuflokkur`) %>%
                    filter(!is.na(clinical_assessment) | !is.na(priority)) %>%
                    filter(date_clinical_assessment<=date_last_known_state) %>%
                    mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                    left_join(.,priority_categories,by=c('priority'='priority_category_raw')) %>%
                    mutate(.,priority=priority_category) %>%
                    left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                    mutate(.,clinical_assessment=clinical_assessment_category) %>%
                    group_by(.,patient_id,date_clinical_assessment) %>%
                    summarise(.,priority=if(all(is.na(priority))) NA_character_ else min(priority,na.rm=T),
                              clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)]) %>%
                    select(patient_id,priority, date_clinical_assessment,clinical_assessment) 
                    

#hospital_visits
hospital_visits <- rename(hospital_visits_raw, patient_id=`Person Key`,unit_in=`Deild Heiti`,date_time_in=`Dagurtími innskriftar`, date_time_out=`Dagurtími útskriftar`, 
                          text_out=`Heiti afdrifa`,ventilator=`Öndunarvél - inniliggjandi`,date_diagnosis_hospital=`Dagsetning skráningar - NR 1`) %>% 
                    select(patient_id,unit_in,date_time_in,date_time_out,text_out,date_diagnosis_hospital,ventilator) %>%
                    mutate(date_time_out=gsub('9999-12-31 00:00:00',NA,date_time_out)) %>%
                    inner_join(.,select(unit_categories,unit_category_raw,unit_category_all),by=c('unit_in'='unit_category_raw')) %>%
                    filter(!(unit_category_all=='inpatient_ward_geriatric' & is.na(date_diagnosis_hospital))) %>%
                    mutate(date_time_in=if_else(unit_category_all=='inpatient_ward_geriatric',date_diagnosis_hospital-1,date_time_in)) %>%
                    filter(!(unit_category_all %in% c('maternity_clinic','endoscopy_clinic','inpatient_ward_maternity'))) %>%
                    separate(col='date_time_in',into=c('date_in','time_in'),sep=' ',remove=FALSE) %>% 
                    separate(col='date_time_out',into=c('date_out','time_out'),sep=' ',remove=FALSE) %>%
                    mutate(.,date_in=as.Date(date_in,"%Y-%m-%d"),date_out=as.Date(date_out,"%Y-%m-%d")) %>%
                    filter(date_in<=date_last_known_state) %>%
                    select(-time_in,-time_out) %>% 
                    mutate(ventilator=!is.na(ventilator))

NEWS_score <- rename(NEWS_score_raw, patient_id=`Person Key`,date_time=`Dagurtími skráningar`,NEWS_score=`News score`) %>% 
              select(.,patient_id,date_time,NEWS_score) %>%
              mutate(date=as.Date(gsub('\\s.*','',date_time),"%Y-%m-%d")) %>%
              filter(!is.na(NEWS_score)) %>%
              filter(date<=date_last_known_state) %>%
              group_by(.,patient_id,date) %>%
              summarise(NEWS_score=NEWS_score[which.max(date_time)]) %>%
              mutate(NEWS_score=if_else(NEWS_score>5,'red','green'))

ventilator_info <- rename(NEWS_score_raw, patient_id=`Person Key`,date_time=`Dagurtími skráningar`,NEWS_score=`News score`) %>% 
  select(.,patient_id,date_time,NEWS_score)
              
#When running the bash script
if (save_additional_data){
  save(hospital_visits, file = paste0(path_sensitive_tables, "hospital_visits.RData"))
}

test_cleaning()

################## ----- Data processing ----- ##############################################################

#create some help tables

hospital_visit_first_date <- group_by(hospital_visits,patient_id) %>% 
                              summarise(min_date_in=min(date_in,na.rm=TRUE)) %>%
                              ungroup()
# fix to remove redundancies due to simple classification
#filter out units that are not predicted and relabel the units in the terms to be predicted.Also relabel text_out
hospital_visits_filtered <- inner_join(hospital_visits,select(unit_categories,unit_category_raw,unit_category),by=c('unit_in'='unit_category_raw')) %>%
                            filter(unit_category!='home') %>%
                            mutate(unit_in=unit_category) %>%
                            select(-unit_category)  %>%
                            inner_join(.,select(text_out_categories,text_out_category_raw,text_out_category),by=c('text_out'='text_out_category_raw')) %>%
                            mutate(.,text_out=text_out_category) %>%
                            select(.,-text_out_category) %>%
                            arrange(patient_id,date_time_in) #arrange precisely in time

hospital_outcomes <- group_by(hospital_visits_filtered,patient_id) %>%
                      summarise(.,outcome_tmp=if_else(any(grepl('death',text_out)),'death',NULL),
                                date_outcome_tmp=min(date_out[grepl('death',text_out)],na.rm=TRUE)) %>%
                      ungroup()

interview_extra_first_date <- group_by(interview_extra,patient_id) %>%
                              summarise(priority_tmp=min(priority,na.rm=TRUE),min_date_clincial_assessment=min(date_clinical_assessment,na.rm=TRUE)) %>%
                              ungroup()

#Find latest interview for each patient
interview_last_date <- bind_rows(select(interview_first,patient_id,date_clinical_assessment),select(interview_follow_up,patient_id,date_clinical_assessment),
                                 select(interview_extra,patient_id,date_clinical_assessment)) %>%
                        filter(date_clinical_assessment<=current_date) %>% # We have some future dates - check.
                        group_by(.,patient_id) %>%
                        summarise(.,date_last_known=max(date_clinical_assessment,na.rm=TRUE)) %>%
                        ungroup() %>% 
                        left_join(.,interview_last,by='patient_id') %>% 
                        mutate(date_last_known=if_else(is.finite(date_clinical_assessment),date_clinical_assessment,date_last_known))
                      

#Add information about first diagnosis, first symptoms, and priority to individs
#First diagnosis is found by extracting the minimum date from hospital visits, interview extra and interview_first 
individs_extended <- left_join(individs,hospital_visit_first_date,by='patient_id') %>%
                      left_join(.,interview_extra_first_date,by='patient_id') %>%
                      mutate(.,date_diagnosis_tmp=pmin(min_date_in,min_date_clincial_assessment,na.rm=TRUE)) %>%
                      select(.,-min_date_clincial_assessment,-min_date_in) %>%
                      left_join(.,select(interview_first,-date_clinical_assessment,-clinical_assessment),by='patient_id') %>%
                      mutate(.,date_diagnosis=pmin(date_diagnosis_tmp,date_diagnosis,na.rm=TRUE)) %>%
                      mutate(.,priority=ifelse(is.na(priority),priority_tmp,priority)) %>%
                      select(.,-date_diagnosis_tmp) %>%
                      filter(.,is.finite(date_diagnosis)) %>%
                      mutate(outcome=ifelse(covid_group=='recovered','recovered','in_hospital_system')) %>%
                      left_join(.,hospital_outcomes,by='patient_id') %>%
                      mutate(outcome=if_else(!is.na(outcome_tmp),outcome_tmp,outcome)) %>%
                      select(-outcome_tmp) %>%
                      left_join(.,interview_last_date,by='patient_id') %>%
                      mutate(.,date_outcome=pmin(date_outcome_tmp,if_else(covid_group=='recovered',date_last_known,NULL),na.rm=TRUE)) %>%
                      filter(if_else(is.finite(date_outcome) & outcome=='recovered',(date_outcome-date_diagnosis)>0,TRUE)) %>%
                      mutate(.,age_group_std=as.character(cut(age,breaks=c(-Inf,seq(10,80,by=10),Inf),labels=c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80+'),right=FALSE)),
                             age_group_simple=as.character(cut(age,breaks=c(-Inf,50,Inf),labels=c('0-50','51+'),right=TRUE)))%>%
                      select(.,patient_id,zip_code,age,age_group_std,age_group_simple,sex,priority,n_comorbidity,date_first_symptoms,date_diagnosis,outcome,date_outcome)

#patient transitions.
#Start by assuming everybody is at home from the time diagnosed to today
#Note: patients diagnosed on current_date have a known state on that date, but others do not. Applies both for dates_home and dates_hospital

dates_home <- lapply(1:nrow(individs_extended),function(i){  
    #date_home_latest_imputed <- if_else(individs_extended$date_diagnosis[i]==current_date,current_date, date_last_known_state)
                return(tibble(patient_id=individs_extended$patient_id[i],state='home',date=seq(individs_extended$date_diagnosis[i],date_last_known_state,by=1))) 
              }) %>% 
              bind_rows() %>%
              left_join(.,select(individs_extended,patient_id,date_outcome),by='patient_id') %>%
              group_by(patient_id) %>%
              filter(!is.finite(date_outcome) | date<=date_outcome) %>%
              select(-date_outcome) %>%
              ungroup()

dates_clinical_assessment <- bind_rows(select(interview_extra,patient_id,date_clinical_assessment,clinical_assessment),
                                       interview_follow_up,
                                       select(interview_first,patient_id,date_clinical_assessment,clinical_assessment),
                                       .id='source') %>%
                              rename(confidence=source) %>%
                              arrange(patient_id,date_clinical_assessment) %>%
                              filter(!is.na(clinical_assessment)) %>%
                              filter(clinical_assessment!='unknown') %>%
                              group_by(patient_id,date_clinical_assessment) %>%  
                              summarise(clinical_assessment=clinical_assessment[which.max(confidence)]) %>%
                              rename(date=date_clinical_assessment)

dates_hospital <- lapply(1:nrow(hospital_visits_filtered),function(i){
  date_out_imputed <- if_else(!is.finite(hospital_visits_filtered$date_out[i]),date_last_known_state,hospital_visits_filtered$date_out[i])
  tmp <- tibble(patient_id=hospital_visits_filtered$patient_id[i],
                state=hospital_visits_filtered$unit_in[i],
                date=seq(hospital_visits_filtered$date_in[i],date_out_imputed,by=1))
  if(hospital_visits_filtered$text_out[i] %in% c('death','home')){
    tmp <- bind_rows(tmp,tibble(patient_id=hospital_visits_filtered$patient_id[i],
                                state=hospital_visits_filtered$text_out[i],
                                date=hospital_visits_filtered$date_out[i]))
  }
  return(tmp)
}) %>% bind_rows() %>% group_by(.,patient_id,date) %>% summarize(state=tail(state,1)) %>% ungroup()

impute_severity <- function(state,severity_vec){
  output_vec <- vector('character',length = length(severity_vec))
  if(length(severity_vec)>0){
    if(is.na(severity_vec[1])){
      if(state=='home'){
        output_vec[1] <- 'green'
      }else{
        output_vec[1] <- 'red'
      }
      
    }else{
      output_vec[1] <- severity_vec[1]
    }
    if(length(severity_vec)>1){
      current_severity <- output_vec[1]
      for(i in 2:length(severity_vec)){
        if(is.na(severity_vec[i])){
          output_vec[i] <- current_severity
        }else{
          output_vec[i] <- severity_vec[i]
          current_severity <- output_vec[i]
        }
      }
    }
  }
  return(output_vec)
}

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
#no score for ICU in data - use NEWS score instead
patient_transitions <- right_join(dates_hospital,dates_home,by=c('patient_id','date'),suffix=c('_hospital','_home')) %>%
                                mutate(.,state=if_else(!is.na(state_hospital) & state_hospital!=state_home,state_hospital,state_home)) %>%
                                left_join(.,dates_clinical_assessment,by=c('patient_id','date')) %>%
                                left_join(.,NEWS_score,by=c('patient_id','date')) %>%
                                mutate(.,severity=case_when(state=='home' ~ clinical_assessment,
                                                          state=='inpatient_ward' ~ NEWS_score,
                                                          state=='intensive_care_unit' ~ NEWS_score)) %>%
                                arrange(.,patient_id,date) %>%
                                group_by(.,patient_id) %>%
                                mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
                                group_by(.,patient_id,state_block_nr)%>%
                                mutate(severity=impute_severity(min(state),severity)) %>%
                                ungroup() %>%
                                #mutate(state=paste0(state,'_',severity)) %>%
                                select(patient_id,date,state,severity) %>%
                                mutate(yesterday=date-1) %>% 
                                left_join(.,.,by=c('patient_id'='patient_id','date'='yesterday'),suffix=c('','_tomorrow')) %>%
                                left_join(select(individs_extended,patient_id,outcome,date_outcome),by='patient_id') %>%
                                filter(!(is.na(state_tomorrow) & outcome!='in_hospital_system')) %>%
                                mutate(state_tomorrow=if_else(outcome=='recovered' & date_tomorrow==date_outcome,'recovered',state_tomorrow),
                                      severity_tomorrow=if_else(outcome %in% c('death','recovered') & date_tomorrow==date_outcome,NA_character_,severity_tomorrow)) %>%
                                select(-yesterday,-date_tomorrow,-outcome,-date_outcome) %>%
                                ungroup() %>%
                                select(patient_id,date,state,severity,state_tomorrow,severity_tomorrow)



#Add worst case state to each patient
#Find those who have at least one transition

get_state_worst <- function(state_vec,order_vec){
  state_worst_vec <- vector('character',length=length(state_vec))
  if(length(state_vec)==0){
    return(state_worst_vec)
  }
  state_worst=state_vec[1]
  state_worst_order=order_vec[1]
  for(i in 1:length(state_vec)){
    if(order_vec[i]>state_worst_order){
      state_worst <- state_vec[i]
      state_worst_order <- order_vec[i]
      state_worst_vec[i] <- state_vec[i]
    }else{
      state_worst_vec[i] <- state_worst
    }
  }
  return(state_worst_vec)
}

state_worst_case_per_date <- inner_join(patient_transitions,distinct(unit_categories,unit_category,.keep_all = T),by=c('state'='unit_category')) %>%
                    inner_join(.,distinct(clinical_assessment_categories,clinical_assessment_category,.keep_all = T),by=c('severity'='clinical_assessment_category')) %>%
                    group_by(.,patient_id) %>%
                    arrange(.,date) %>%
                    mutate(state_worst=get_state_worst(paste0(state,'-',severity),paste0(unit_category_order,clinical_assessment_category_order))) %>%
                    separate(.,state_worst,into=c('state_worst','state_worst_severity'),sep='-') %>%
                    ungroup() %>%
                    select(patient_id,date,state_worst,state_worst_severity)
state_worst_case <- group_by(state_worst_case_per_date,patient_id) %>% arrange(date) %>% slice(n()) %>% select(-date)
individs_extended <- left_join(individs_extended,state_worst_case,by='patient_id')
rm(state_worst_case)



#summarise state blocks, extracting min and max date to calculate length of each state. Note:states entered yesterday are not used to estimate length of stay
patient_transitions_state_blocks <- group_by(patient_transitions,patient_id) %>%
                                    mutate(state_block_nr=get_state_block_numbers(state),
                                           state_with_severity_block_nr=get_state_block_numbers(paste0(state,severity))) %>%
                                    group_by(.,patient_id,state_block_nr,state_with_severity_block_nr,state,severity) %>% arrange(.,date) %>% 
                                    summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)],severity_next=severity_tomorrow[which.max(date)]) %>%
                                    mutate(censored=(is.na(state_next))) %>%
                                    mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
                                    ungroup()

#test_data_processing()

############################## ------ Create input without clinical assessment for simulation ----- ##############################

current_state_per_date <- get_current_state_per_date()
current_state_per_date_summary <- group_by(current_state_per_date,date,state) %>% summarise(count=n())
current_state <- filter(current_state_per_date,date==date_last_known_state) %>% select(-date)
current_state_write <- filter(current_state,!(days_from_diagnosis > 14 & state == 'home'))
recovered_imputed_by_age <- anti_join(current_state,current_state_write) %>%
  inner_join(.,select(individs_extended,patient_id,age_group_simple),by='patient_id') %>% 
  mutate(date=current_date,state_tomorrow='recovered') %>%
  select(patient_id,age_group_simple,date,state,state_tomorrow)
patient_transition_counts_matrix_all <- get_transition_matrix_all('',select(recovered_imputed_by_age,-age_group_simple))
patient_transition_counts_matrix_age_simple_under_50 <- get_transition_matrix_by_age('',recovered_imputed_by_age)$under_50 
patient_transition_counts_matrix_age_simple_over_50 <- get_transition_matrix_by_age('',recovered_imputed_by_age)$over_50
length_of_stay_empirical_by_age_simple <- get_length_of_stay_empirical_by_age_simple('') 
length_of_stay_predicted_by_age_simple <- get_length_of_stay_predicted_by_age_simple('') 

first_state <- get_first_state()
first_state_write <- select(first_state,age,sex,initial_state)
#add to create input for simulation
first_state_per_date <- select(first_state,date_diagnosis,age,sex,initial_state) %>% arrange(date_diagnosis)
first_state_per_date_summary_age <- inner_join(first_state,select(individs_extended,patient_id,age_group_simple),by='patient_id') %>%
  select(date_diagnosis,age_group_simple,initial_state) %>%
  group_by(date_diagnosis,initial_state,age_group_simple) %>% summarise(count=n())
first_state_per_date_summary <- group_by(first_state,date_diagnosis,initial_state) %>% summarise(count=n())


#When running the bash script
if (save_additional_data){
  save(current_state_per_date, file = paste0(path_sensitive_tables, "current_state_per_date.RData"))
}
############### ----- Write simple tables to disk ----- ############################
if(write_tables_for_simulation){
  write.table(current_state_per_date,file=paste0(path_sensitive_tables,current_date,'_current_state_per_date','.csv'),sep=',',row.names=FALSE,quote=FALSE)
  write.table(current_state_per_date_summary,file=paste0(path_tables,current_date,'_current_state_per_date_summary','.csv'),sep=',',row.names=FALSE,quote=FALSE)
  write.table(current_state_write,file=paste0(path_sensitive_tables,current_date,'_current_state','.csv'),sep=',',row.names=FALSE,quote=FALSE)
  write.table(patient_transition_counts_matrix_all,file=paste0(path_tables,current_date,'_transition_matrix','.csv'),sep=',',row.names=FALSE,col.names=T,quote=FALSE)
  write.table(patient_transition_counts_matrix_age_simple_under_50,file=paste0(path_tables,current_date,'_transition_matrix_under_50','.csv'),sep=',',row.names=F,col.names=T,quote=F)
  write.table(patient_transition_counts_matrix_age_simple_over_50,file=paste0(path_tables,current_date,'_transition_matrix_over_50','.csv'),sep=',',row.names=F,col.names=T,quote=F)
  write.table(length_of_stay_empirical_by_age_simple,file=paste0(path_dashboard_tables,current_date,'_length_of_stay_empirical','.csv'),sep=',',row.names=F,quote=F)
  write.table(length_of_stay_predicted_by_age_simple,file=paste0(path_tables,current_date,'_length_of_stay','.csv'),sep=',',row.names=F,quote=F)
  write.table(first_state_write,file=paste0(path_sensitive_tables,current_date,'_first_state','.csv'),sep=',',row.names=F,quote=F)
  write.table(first_state_per_date,file=paste0(path_sensitive_tables,current_date,'_first_state_per_date','.csv'),sep=',',row.names=F,quote=F)
  write.table(first_state_per_date_summary,file=paste0(path_sensitive_tables,current_date,'_first_state_per_date_summary','.csv'),sep=',',row.names=F,quote=F)
  write.table(first_state_per_date_summary_age,file=paste0(path_sensitive_tables,current_date,'_first_state_per_date_summary_age','.csv'),sep=',',row.names=F,quote=F)
}

# current_state_per_date_extended <- get_current_state_per_date(type='extended')
# patient_transition_counts_matrix_all_extended <- get_transition_matrix_all(type='extended')
# patient_transition_counts_matrix_age_simple_under_50_extended <- get_transition_matrix_by_age(type='extended')$under_50 
# patient_transition_counts_matrix_age_simple_over_50_extended <- get_transition_matrix_by_age(type='extended')$over_50
# current_state_extended <- get_current_state(type='extended')
# current_state_extended_write <- filter(current_state,!(days_from_diagnosis > 14 & state == 'home_green'))
# #length_of_stay_by_age_simple_extended <- get_length_of_stay_by_age_simple(type='extended') 
# first_state_extended <- get_first_state(type='extended')
# #TODO: add to create_output_for_simulation
# # first_state_per_date <- select(first_state,date_diagnosis,age,sex,initial_state) %>% arrange(date_diagnosis)
# # first_state_per_date_summary_age <- inner_join(first_state,select(individs_extended,patient_id,age_group_simple),by='patient_id') %>%
# #   select(date_diagnosis,age_group_simple,initial_state) %>%
# #   group_by(date_diagnosis,initial_state,age_group_simple) %>% summarise(count=n())
# # first_state_per_date_summary <- group_by(first_state,date_diagnosis,initial_state) %>% summarise(count=n())
# 
# ############### ----- Write extended tables to disk ----- ############################
# if(write_tables_for_simulation){
#   write.table(current_state_per_date_extended,file=paste0(path_tables,current_date,'_current_state_per_date_extended','.csv'),sep=',',row.names=FALSE,quote=FALSE)
#   write.table(patient_transition_counts_matrix_all_extended,file=paste0(path_tables,current_date,'_transition_matrix_extended','.csv'),sep=',',row.names=FALSE,col.names=states,quote=FALSE)
#   write.table(patient_transition_counts_matrix_age_simple_under_50_extended,file=paste0(path_tables,current_date,'_transition_matrix_under_50_extended','.csv'),sep=',',row.names=F,col.names=T,quote=F)
#   write.table(patient_transition_counts_matrix_age_simple_over_50_extended,file=paste0(path_tables,current_date,'_transition_matrix_over_50_extended','.csv'),sep=',',row.names=F,col.names=T,quote=F)
#   write.table(current_state_write_extended,file=paste0(path_sensitive_tables,current_date,'_current_state_extended','.csv'),sep=',',row.names=F,quote=F)
#   write.table(length_of_stay_by_age_simple_extended,file=paste0(path_tables,current_date,'_length_of_stay_extended','.csv'),sep=',',row.names=F,quote=F)
#   write.table(first_state_extended,file=paste0(path_sensitive_tables,current_date,'_first_state_extended','.csv'),sep=',',row.names=F,quote=F)
# }


################# ----- Extract CDF from posterior predictive distribution from the stats group model of number infected
hi_posterior_predictive_distr <- read_csv(paste0('https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Iceland_Posterior/Iceland_Posterior_',prediction_date,'.csv'))
dates <- unique(hi_posterior_predictive_distr$date)
max_new_cases = max(hi_posterior_predictive_distr$new_cases)
#note plus 1 to include the possibility of 0 new cases 
hi_mat_CDF <- matrix(rep(0,length(dates)*(max_new_cases+1)), nrow = length(dates), ncol=(max_new_cases+1))
rownames(hi_mat_CDF) <- as.character(dates)
colnames(hi_mat_CDF) <- c(0:max_new_cases)
for (i in 1:length(dates)) {
  new_cases <- table(filter(hi_posterior_predictive_distr,date==dates[i])$new_cases)
  hi_mat_CDF[i,names(new_cases)] = cumsum(new_cases)/sum(new_cases)
}
if(write_tables_for_simulation){
  write.csv(hi_mat_CDF, file = paste0(path_tables,current_date,'_iceland_posterior.csv'), quote = F)
}


############################## ------ Create tables for stats group ----- ##############################

path_stats_tables='../output_stats_group/'
#Create table of new hospital cases,new icu,out of hospital and out of icu per day
finished_states <- inner_join(select(individs_extended,patient_id,age_group_std),
                              select(patient_transitions_state_blocks,-censored,-state_duration),
                              by='patient_id')%>%
  filter(state!=state_next) %>%
  mutate(date=state_block_nr_end+1) %>%
  select(patient_id,date,age_group_std,state,state_next)

first_states_hospitals <- inner_join(select(individs_extended,patient_id,age_group_std),
                                     select(patient_transitions_state_blocks,-censored,-state_duration),
                                     by='patient_id') %>%
  filter(state_block_nr==1 & state!='home') %>%
  mutate(date=state_block_nr_start,state_next=state) %>%
  mutate(state=NA) %>% select(patient_id,date,age_group_std,state,state_next)

states_by_date_and_age_std <- bind_rows(first_states_hospitals,finished_states) %>%
  group_by(.,date,age_group_std,state,state_next) %>%
  summarise(.,value=n()) %>% ungroup() %>%
  mutate(.,variable=case_when(
    (state=='home' & state_next %in% c('inpatient_ward','intensive_care_unit')) | is.na(state)  ~ 'fj_innlagna_a_spitala',
    state_next=='intensive_care_unit' ~ 'fj_innlagna_a_icu',
    state_next=='home' ~ 'fj_utskrifta_af_spitala',
    state=='intensive_care_unit' ~ 'fj_utskrifta_af_icu',
    state_next=='recovered' ~ 'fj_batnad',
    state_next=='death' ~ 'fj_andlata'
  )) %>% rename(agegroup='age_group_std') %>% select(date,agegroup,variable,value)

write.table(states_by_date_and_age_std,paste0(path_stats_tables,current_date,'_events_per_date_and_age.csv'),sep=',')

#hospital and icu distributions for Brynjólfur
group_by(hospital_visits_filtered,patient_id) %>% summarise(icu=any(grepl('intensive_care_unit',unit_in))) %>%
  ungroup() %>% left_join(select(individs_extended,patient_id,age_group_std),.,by='patient_id') %>%
  group_by(age_group_std) %>% summarise(fj_smitadra=n(),fj_spitala=sum(!is.na(icu)),fj_icu=sum(icu,na.rm=T)) %>%
  ungroup() %>% arrange(age_group_std)
write.table(paste0(path_stats_tables,current_date,'_hospital_and_icu_distr.csv'),row.names=F,quote=F,sep=',')

#extract length of hopspital stays including icu and icu times censored and uncensored
previous_hospital_stays <- filter(patient_transitions_state_blocks,state!='home')
 
