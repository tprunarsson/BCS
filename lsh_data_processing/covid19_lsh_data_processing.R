#!/usr/bin/env Rscript
Sys.setlocale("LC_ALL","IS_is")
library(optparse)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
source('test_covid19_lsh_data_processing.R')
source('create_input_for_simulation.R')
source('help_functions.R')

start_date_tmp <- as.Date('2020-03-02','%Y-%m-%d')
current_date_tmp <- as.Date('2020-04-20','%Y-%m-%d')
prediction_date_tmp <- as.Date('2020-04-20','%Y-%m-%d')
path_to_lsh_data_tmp <- '~/projects/covid/BCS/lsh_data/'
#path_to_lsh_data_tmp <- '../../'
write_tables_tmp <- TRUE
run_id_tmp <- 1

#Supported unit category types: all,simple
unit_category_type <- 'simple'
#Supported text out category types: simple
text_out_category_type <- 'simple'
#Supported clnical assessment category types: all,simple
clinical_assessment_category_type <- 'simple_red'

option_list <-  list(
  make_option(c("-c", "--current_date"), type="character", default=NULL, 
              help="current date of data being used", metavar="character"),
  make_option(c("-p", "--prediction_date"), type="character", default=NULL, 
              help="date of prediction from covid.hi.is", metavar="character"),
  make_option(c("-s", "--start_date"), type="character", default=NULL, 
              help="start date of simulation", metavar="character"),
  make_option(c("-r", "--run_id"), type="integer", default=NULL, 
              help="run_id to identify run of a set of experiments", metavar="integer"),
  make_option(c("-d", "--path_to_lsh_data"), type="character", default=NULL, 
              help="path to data from LSH", metavar="character")
  
)

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['current_date']])){
  current_date <- current_date_tmp 
  warning(paste0('You did not provide a current date. ',current_date,' will be used'))
}else{
  current_date <- as.Date(as.character(opt[['current_date']]),'%Y-%m-%d')
}

if(is.null(opt[['prediction_date']])){
  prediction_date <- prediction_date_tmp 
  warning(paste0('You did not provide a prediction date. ',prediction_date,' will be used'))
}else{
  prediction_date <- as.Date(as.character(opt[['prediction_date']]),'%Y-%m-%d')
}

if(is.null(opt[['start_date']])){
  start_date <- start_date_tmp 
  warning(paste0('You did not provide a prediction date. ',start_date,' will be used'))
}else{
  start_date <- as.Date(as.character(opt[['start_date']]),'%Y-%m-%d')
}

if(is.null(opt[['path_to_lsh_data']])){
  path_to_lsh_data <- path_to_lsh_data_tmp
}else{
  path_to_lsh_data <- opt[['path_to_lsh_data']]
}

if(is.null(opt[['run_id']])){
  run_id=run_id_tmp
  warning(paste0('You did not provide a run_id. ',run_id_tmp,' will be used'))
}else{
  run_id <- opt[['run_id']]
}

if (length(opt)>1){
  write_tables <- TRUE
}else{
  write_tables <- write_tables_tmp
}


#date of prediction by covid.hi.is


#we assume we only know the state of patient at midnight before current_date (except for patients diagnosed on current date)
date_last_known_state <- current_date-1

#Assuming working directory is lsh_data_processing in github repo
path_tables <- '../input/'
path_sensitive_tables <- '../lsh_data/'
path_dashboard_tables <- '../dashboard/input/'
path_outpatient_clinic <- '../outpatient_clinic_history/'

file_name_lsh_data <- paste0(current_date,'_lsh_covid_data.xlsx')
file_path_coding <- 'lsh_coding.xlsx'
file_path_data <- paste0(path_to_lsh_data,file_name_lsh_data)
file_path_experiment_template <- 'experiment_template.xlsx'

################## ----- Read LSH data and coding data ----- ########################################

individs_raw <- read_excel(file_path_data,sheet = 'Einstaklingar', skip=3)
hospital_visits_raw <- read_excel(file_path_data,sheet ='Komur og innlagnir', skip=3)
interview_first_raw <- read_excel(file_path_data,sheet = 'Fyrsta viðtal úr forms', skip=3)
interview_follow_up_raw <- read_excel(file_path_data,sheet = 'Spurningar úr forms Pivot', skip=1)
interview_last_raw <- read_excel(file_path_data,sheet = 'Lokaviðtal-Spurning úr forms', skip=1)
interview_extra_raw <- read_excel(file_path_data,sheet = 'Áhættuflokkur ofl úr hóp', skip=3) 
NEWS_score_raw <- read_excel(file_path_data,sheet = 'NEWS score ', skip=3)
ventilator_times_raw <- read_excel(file_path_data,sheet = 'Öndunarvél - tímar', skip=3)

covid_groups <- read_excel(file_path_coding,sheet = 'lsh_covid_groups')
unit_categories <- read_excel(file_path_coding,sheet = 'lsh_unit_categories') %>%
                    mutate(unit_category=!!as.name(paste0('unit_category_',unit_category_type)),
                           unit_category_order=!!as.name(paste0('unit_category_order_',unit_category_type)))
text_out_categories <- read_excel(file_path_coding,sheet = 'lsh_text_out_categories') %>%
                        mutate(text_out_category=!!as.name(paste0('text_out_category_',text_out_category_type)))
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories') %>%
                                  mutate(clinical_assessment_category=!!as.name(paste0('clinical_assessment_category_',clinical_assessment_category_type)),
                                         clinical_assessment_category_order=!!as.name(paste0('clinical_assessment_category_order_',clinical_assessment_category_type)))
priority_categories <- read_excel(file_path_coding,sheet = 'priority_categories')
age_groups <- read_excel(file_path_coding,sheet = 'age_groups')
length_of_stay_categories <- read_excel(file_path_coding,sheet = 'length_of_stay_categories') 

comorbidities_categories <- read_excel(file_path_coding,sheet = 'comorbidities') %>%
                              arrange(comorbidities_raw)
sheet_names <- read_excel(file_path_coding,sheet = 'lsh_sheet_names',trim_ws = FALSE)

experiment_description <- read_excel(file_path_experiment_template,sheet='experiment_description')
experiment_specification <- read_excel(file_path_experiment_template,sheet='experiment_specification')
run_description <- read_excel(file_path_experiment_template,sheet='run_description')
run_specification <- read_excel(file_path_experiment_template,sheet='run_specification')
heuristics_description <- read_excel(file_path_experiment_template,sheet='heuristics_description')


test_lsh_data_file()

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
                    left_join(.,priority_categories,by=c('priority'='priority_raw')) %>%
                    mutate(.,priority=priority_all) %>%
                    left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                    mutate(.,clinical_assessment=clinical_assessment_category) %>%
                    group_by(.,patient_id) %>%
                    summarize(.,date_first_symptoms=min(date_first_symptoms,na.rm=TRUE),
                                    date_diagnosis=min(date_diagnosis,na.rm=TRUE),
                                    date_clinical_assessment=min(date_clinical_assessment,na.rm=TRUE),
                                    #date_clinical_assessment=if(!any(is.finite(clinical_assessment))) NA else date_clinical_assessment[which.max(clinical_assessment_category_order)],
                                    priority=if(all(is.na(priority))) NA_character_ else priority[which.max(priority_all_order)],
                                    clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)],
                                    comorbidities_raw=if(any(!is.na(comorbidities_raw))) paste(comorbidities_raw[!is.na(comorbidities_raw)],collapse="; ") else NA_character_ #So as to not lose data we paste together the raw comorbidities of duplictes. Deal with duplicated comorbidities below.
                              ) %>%
                    filter(.,if_else(is.finite(date_diagnosis),date_diagnosis<=date_last_known_state,TRUE)) %>%
                    ungroup()%>%
                    separate(comorbidities_raw,into = paste("comorbidity",c(1:10)),sep="; ") %>% #Adding comorbidities
                    pivot_longer(matches("comorbidity"),names_to="comorb_number",values_to="comorbidity") %>%
                    group_by(patient_id) %>%
                    filter(!duplicated(comorbidity)) %>% 
                    arrange(comorb_number) %>%
                    ungroup() %>%
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
                        summarize(.,clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)])
                        

#date_clinical_assessment is the last interview by a physician. Remove scheduled future phone calls
interview_last <- rename(interview_last_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`) %>%
                  mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                  filter(is.finite(date_clinical_assessment)) %>%
                  filter(date_clinical_assessment<=date_last_known_state) %>%
                  group_by(.,patient_id) %>%
                  summarize(.,date_clinical_assessment=max(date_clinical_assessment,na.rm=T))
    

interview_extra <- rename(interview_extra_raw,patient_id=`Person Key`,date_time_clinical_assessment=`Dags breytingar`,col_name=`Heiti dálks`,col_value=`Skráningar - breytingar.Skráð gildi`) %>% 
                    select(.,patient_id,date_time_clinical_assessment,col_name,col_value) %>%
                    mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_time_clinical_assessment),"%Y-%m-%d")) %>%
                    group_by(.,patient_id,date_clinical_assessment,col_name) %>%
                    summarize(date_time_clinical_assessment=max(date_time_clinical_assessment),col_value=col_value[which.max(date_time_clinical_assessment)]) %>%
                    ungroup(.) %>%
                    pivot_wider(.,id_cols=c('patient_id','date_clinical_assessment','date_time_clinical_assessment'),
                                  names_from=col_name,
                                  values_from=col_value) %>% 
                    rename(clinical_assessment=`Klínískt daglegt mat`, priority=`Áhættuflokkur`) %>%
                    filter(!is.na(clinical_assessment) | !is.na(priority)) %>%
                    filter(date_clinical_assessment<=date_last_known_state) %>%
                    mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                    left_join(.,priority_categories,by=c('priority'='priority_raw')) %>%
                    mutate(.,priority=priority_all) %>%
                    left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                    mutate(.,clinical_assessment=clinical_assessment_category) %>%
                    group_by(.,patient_id,date_clinical_assessment) %>%
                    summarize(.,priority=if(all(is.na(priority))) NA_character_ else priority[which.max(priority_all_order)],
                              clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)]) %>%
                    select(patient_id,priority, date_clinical_assessment,clinical_assessment) 
                    

#hospital_visits
#hardcode date_diagnosis_hospital for geriatic patients while confusion in how they are registered.
hospital_visits_raw <- mutate_at(hospital_visits_raw, vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==13173,as.Date('2020-03-29 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==56481,as.Date('2020-03-29 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==78970,as.Date('2020-03-29 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==94463,as.Date('2020-03-29 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==205678,as.Date('2020-03-29 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==217960,as.Date('2020-03-31 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==358268,as.Date('2020-03-26 00:00:00'))) %>%
                        mutate_at( vars(`Dagsetning skráningar - NR 1`),~replace(.,`Person Key`==381951,as.Date('2020-04-10 00:00:00')))
  
          
hospital_visits <- rename(hospital_visits_raw, patient_id=`Person Key`,unit_in=`Deild Heiti`,date_time_in=`Dagurtími innskriftar`, date_time_out=`Dagurtími útskriftar`, 
                          text_out=`Heiti afdrifa`,ventilator=`Öndunarvél - inniliggjandi`,date_diagnosis_hospital=`Dagsetning skráningar - NR 1`) %>% 
                    select(patient_id,unit_in,date_time_in,date_time_out,text_out,date_diagnosis_hospital,ventilator) %>%
                    mutate(date_time_out=gsub('9999-12-31 00:00:00',NA,date_time_out)) %>%
                    inner_join(.,select(unit_categories,unit_category_raw,unit_category_all),by=c('unit_in'='unit_category_raw')) %>%
                    filter(!(unit_category_all=='inpatient_ward_geriatric' & is.na(date_diagnosis_hospital))) %>%
                    filter(!(unit_category_all=='inpatient_ward_geriatric' & date_time_out<date_diagnosis_hospital)) %>%
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
              #summarize(NEWS_score=NEWS_score[which.max(date_time)]) %>%
              summarize(NEWS_score=mean(as.numeric(NEWS_score),na.rm=T)) %>%
              mutate(NEWS_score=if_else(NEWS_score>5,'red','green'))

ventilator_times <- rename(ventilator_times_raw, patient_id=`Person Key`,date_time_ventilator_start=`Dags settur í öndunarvél`,date_time_ventilator_end=`Dags tekin úr öndunarvél`) %>% 
                      select(.,patient_id,date_time_ventilator_start,date_time_ventilator_end) %>%
                      mutate(unit_in='intensive_care_unit') %>%
                      mutate(date_ventilator_start=as.Date(gsub('\\s.*','',date_time_ventilator_start),'%Y-%m-%d'),
                             date_ventilator_end=as.Date(gsub('\\s.*','',date_time_ventilator_end),'%Y-%m-%d')) %>%
                      select(-date_time_ventilator_start,-date_time_ventilator_end) %>%
                      filter(!is.na(date_ventilator_start)) %>%
                      inner_join(.,select(hospital_visits,patient_id,unit_category_all,date_in),by=c('patient_id','unit_in'='unit_category_all')) %>%
                      group_by(.,patient_id,unit_in) %>%
                      summarize(date_in=min(date_in,na.rm=T),date_ventilator_start=min(date_ventilator_start),date_ventilator_end=suppressWarnings(max(date_ventilator_end,na.rm=T))) %>%
                      pivot_longer(cols=c('date_in','date_ventilator_start','date_ventilator_end'),names_to='ventilator',values_to='date') %>%
                      filter(is.finite(date)) %>%
                      mutate(ventilator=if_else(ventilator=='date_ventilator_start','red','green')) %>%
                      group_by(patient_id,unit_in,date) %>%
                      summarize(ventilator=ventilator[which.max(ventilator=='red')])
                
              

test_cleaning()

################## ----- Data processing ----- ##############################################################

#create some help tables

hospital_visit_first_date <- group_by(hospital_visits,patient_id) %>% 
                              summarize(min_date_in=min(date_in,na.rm=TRUE)) %>%
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
                      summarize(.,outcome_tmp=if_else(any(grepl('death',text_out)),'death',NULL),
                                date_outcome_tmp=min(date_out[grepl('death',text_out)],na.rm=TRUE)) %>%
                      ungroup()

interview_extra_first_date <- group_by(interview_extra,patient_id) %>%
                              summarize(priority_tmp=min(priority,na.rm=TRUE),min_date_clincial_assessment=min(date_clinical_assessment,na.rm=TRUE)) %>%
                              ungroup()

#Find latest interview for each patient
interview_last_date <- bind_rows(select(interview_first,patient_id,date_clinical_assessment),select(interview_follow_up,patient_id,date_clinical_assessment),
                                 select(interview_extra,patient_id,date_clinical_assessment)) %>%
                        filter(date_clinical_assessment<=current_date) %>% # We have some future dates - check.
                        group_by(.,patient_id) %>%
                        summarize(.,date_last_known=max(date_clinical_assessment,na.rm=TRUE)) %>%
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
                      mutate(.,priority=impute_priority(priority,age,n_comorbidity)) %>%
                      select(.,patient_id,zip_code,age,sex,priority,n_comorbidity,date_first_symptoms,date_diagnosis,outcome,date_outcome)
                            
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
                              summarize(clinical_assessment=clinical_assessment[which.max(confidence)]) %>%
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

#no score for ICU in data - use NEWS score instead
patient_transitions <- right_join(dates_hospital,dates_home,by=c('patient_id','date'),suffix=c('_hospital','_home')) %>%
                                mutate(.,state=if_else(!is.na(state_hospital) & state_hospital!=state_home,state_hospital,state_home)) %>%
                                left_join(.,dates_clinical_assessment,by=c('patient_id','date')) %>%
                                left_join(.,NEWS_score,by=c('patient_id','date')) %>%
                                left_join(.,ventilator_times,by=c('patient_id','date')) %>%
                                mutate(.,severity=case_when(state=='home' ~ clinical_assessment,
                                                          state=='inpatient_ward' ~ NEWS_score,
                                                          state=='intensive_care_unit' ~ ventilator)) %>%
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
state_worst_case_per_date <- filter(unit_categories,!duplicated(unit_category)) %>% inner_join(patient_transitions,.,by=c('state'='unit_category')) %>%
                    inner_join(.,filter(clinical_assessment_categories,!duplicated(clinical_assessment_category)),by=c('severity'='clinical_assessment_category')) %>%
                    group_by(.,patient_id) %>%
                    arrange(.,date) %>%
                    mutate(state_worst=get_state_worst(paste0(state,'-',severity),paste0(unit_category_order,clinical_assessment_category_order))) %>%
                    separate(.,state_worst,into=c('state_worst','state_worst_severity'),sep='-') %>%
                    ungroup() %>%
                    select(patient_id,date,state_worst,state_worst_severity)
state_worst_case <- group_by(state_worst_case_per_date,patient_id) %>% arrange(date) %>% slice(n()) %>% select(-date)
individs_extended <- left_join(individs_extended,state_worst_case,by='patient_id')
rm(state_worst_case)

#add point of diagnosis
individs_extended <- left_join(individs_extended,select(patient_transitions,patient_id,date,state,severity),by=c('patient_id','date_diagnosis'='date')) %>%
                        rename(state_first=state,severity_first=severity)
  
  
individs_splitting_variables <- select(individs_extended,patient_id,age,sex,priority,state_first) %>%
                                left_join(age_groups,by='age') %>%
                                mutate_at(vars(matches('age_'),-matches('order')),list( ~paste0('age_',.))) %>%
                                left_join(priority_categories,by=c('priority'='priority_all')) %>%
                                rename(priority_all=priority) %>%
                                mutate_at(vars(matches('priority'),-matches('order')),list( ~paste0('priority_',.))) %>%
                                select(-matches('raw')) %>%
                                mutate(point_of_diagnosis=if_else(state_first=='home','outpatient','inpatient')) %>%
                                mutate(point_of_diagnosis_order=if_else(point_of_diagnosis=='outpatient',1,2)) %>%
                                mutate(age_simple_point_of_diagnosis=if_else(age_simple=='age_0-50',age_simple,paste(age_simple,point_of_diagnosis,sep='_'))) %>%
                                mutate(age_simple_point_of_diagnosis_order=if_else(age_simple=='age_0-50',1,if_else(point_of_diagnosis=='outpatient',2,3)))


#summarize state blocks, extracting min and max date to calculate length of each state. Note:states entered yesterday are not used to estimate length of stay
patient_transitions_state_blocks <- group_by(patient_transitions,patient_id) %>%
                                    mutate(state_block_nr=get_state_block_numbers(state),
                                           state_with_severity_block_nr=get_state_block_numbers(paste0(state,severity))) %>%
                                    group_by(.,patient_id,state_block_nr,state_with_severity_block_nr,state,severity) %>% arrange(.,date) %>% 
                                    summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)],severity_next=severity_tomorrow[which.max(date)]) %>%
                                    mutate(censored=(is.na(state_next))) %>%
                                    mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
                                    ungroup()

test_data_processing()

################# ----- Historical data ------ ###############
historical_data <- group_by(patient_transitions,date,state) %>% summarise(count=n())
historical_turnover <- get_historical_turnover()
historical_state_sequences_base <- get_state_sequences(model='base',seq_type = 'finished')
historical_state_sequences_extended <- get_state_sequences(model='extended',seq_type = 'finished')


################# ----- Predicted number of infections ------ ##############################
infections_predicted_per_date <- get_infections_predicted_per_date(source='hi',prediction_date)

################# ----- proportion of patients going to outpatient clinic ------ #################
prop_outpatient_clinic <- get_prop_outpatient_clinic(historical_data)


if(write_tables){
  write.table(historical_data, file = paste0(path_tables,current_date,'_historical_data.csv'), quote = F,row.names=F,sep=',')
  write.table(historical_turnover, file = paste0(path_tables,current_date,'_historical_turnover.csv'), quote = F,row.names=F,sep=',')
  write.table(prop_outpatient_clinic, file=paste0(path_outpatient_clinic,current_date,'_prop_outpatient_clinic.csv'), quote = F,row.names=F,sep=',')
  write.table(infections_predicted_per_date, file = paste0(path_tables,current_date,'_infections_predicted.csv'), quote = F,row.names=F,sep=',')
}

################# ----- Transition summary and length of stay distribution for all experiments ------ ##############################
run_info <- get_run_info(run_id) 

if(write_tables){
  write.table(run_info, file = paste0(path_tables,current_date,'_',run_id,'_run_info.csv'), quote = F,row.names=F,sep='\t',col.names=F)
}

 for(id in run_info$experiment_id){
  experiment_table_list <- get_tables_for_experiment(id)
  tables_to_convert_to_cdf <- c('transition_summary','length_of_stay','first_state')
  if(write_tables){
    write.table(experiment_table_list$transition_summary,file=paste0(path_tables,current_date,'_',id,'_transition_summary.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$length_of_stay,file=paste0(path_tables,current_date,'_',id,'_length_of_stay.csv'),sep=',',row.names=F,quote=F)
    write.table(experiment_table_list$current_state_per_date,file=paste0(path_sensitive_tables,current_date,'_',id,'_current_state_per_date.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$current_state_filtered,file=paste0(path_sensitive_tables,current_date,'_',id,'_current_state.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$current_state_from_start,file=paste0(path_sensitive_tables,start_date,'_',id,'_current_state.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$first_state,file=paste0(path_tables,current_date,'_',id,'_first_state.csv'),sep=',',row.names=F,quote=F)
  }
}
