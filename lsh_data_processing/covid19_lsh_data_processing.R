#!/usr/bin/env Rscript
invisible(Sys.setlocale("LC_ALL","IS_is"))
suppressPackageStartupMessages({
  library(optparse)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(httr)
  library(tibble)
})

options(dplyr.summarise.inform = FALSE) #Óþolandi skilaboð

cat("Processing data...\n")
source('lsh_data_processing/test_covid19_lsh_data_processing.R')
source('lsh_data_processing/create_input_for_simulation.R')
source('lsh_data_processing/help_functions.R')

date_data_tmp <- as.Date('2021-08-26','%Y-%m-%d') #BREYTA með nýjum lsh gögnum
date_prediction_tmp <- as.Date('2021-07-06','%Y-%m-%d') #BREYTA með nýrri spá (þarf að vísu ekki endilega því þessu er breytt í gagnaborðinu líka)
date_observed_start_tmp <- date_data_tmp-1
#path_to_lsh_data_tmp <- '~/projects/covid/BCS/lsh_data/'
path_to_lsh_data_tmp <- 'lsh_data/'
write_tables_tmp <- TRUE
run_id_tmp <- 19
forecast_tmp <- 'manual'

#Supported unit category types: all,simple
unit_category_type <- 'simple'
#Supported text out category types: simple
text_out_category_type <- 'simple'
#Supported isolation category type: simple
isolation_category_type <- 'simple'
#Supported clnical assessment category types: all,simple
clinical_assessment_category_type <- 'simple_red'

option_list <-  list(
  make_option(c("-d", "--date_data"), type="character", default=NULL, 
              help="current date of data being used", metavar="character"),
  make_option(c("-p", "--date_prediction"), type="character", default=NULL, 
              help="date of prediction from covid.hi.is", metavar="character"),
  make_option(c("-o", "--date_observed_start"), type="character", default=NULL, 
              help="oberved date start", metavar="character"),
  make_option(c("-r", "--run_id"), type="integer", default=NULL, 
              help="run_id to identify run of a set of experiments", metavar="integer"),
  make_option(c("-l", "--path_to_lsh_data"), type="character", default=NULL, 
              help="path to data from LSH", metavar="character"),
  make_option(c("-c", "--forecast"), type="character", default=NULL, 
              help="Which forecast to use, hi, manual or from_file", metavar="character")
)

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['date_data']])){
  date_data <- date_data_tmp 
  warning(paste0('You did not provide a current date. ',date_data,' will be used'))
}else{
  date_data <- as.Date(as.character(opt[['date_data']]),'%Y-%m-%d')
}

if(is.null(opt[['date_prediction']])){
  date_prediction <- date_prediction_tmp 
  warning(paste0('You did not provide a prediction date. ',date_prediction,' will be used'))
}else{
  date_prediction <- as.Date(as.character(opt[['date_prediction']]),'%Y-%m-%d')
}

if(is.null(opt[['date_observed_start']])){
  date_observed_start <- date_observed_start_tmp 
  warning(paste0('You did not provide date observed start. ',date_observed_start,' will be used'))
}else{
  date_observed_start <- as.Date(as.character(opt[['date_observed_start']]),'%Y-%m-%d')
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

if(is.null(opt[['forecast']])){
  forecast=forecast_tmp
  warning(paste0('You did not provide a forecast. ', forecast_tmp,' will be used'))
}else{
  forecast <- opt[['forecast']]
}
# if(!(forecast=='from_file')){
#   if(date_prediction>ymd("2020-04-20")){
#     forecast <- 'manual'
#     warning(paste0('No forecast available for this date. ', forecast,' will be used'))
#   }
# }

if (length(opt)>1){
  write_tables <- TRUE
}else{
  write_tables <- write_tables_tmp
}

#we assume we only know the state of patient at midnight before date_data
date_last_known_state <- date_data-1

#Assuming working directory is lsh_data_processing in github repo
path_tables <- 'input/'
path_sensitive_tables <- 'lsh_data/'
path_dashboard_tables <- 'dashboard/input/'
path_outpatient_clinic <- 'outpatient_clinic_history/'

#file_name_lsh_data <- paste0(date_data,'_lsh_covid_data.xlsx')
file_name_lsh_data <- 'lsh_covid_data.xlsx'
file_name_lsh_vax <- 'voruhus_covid_bolusetn_v1.xlsx'
file_path_coding <- 'lsh_data_processing/lsh_coding_new.xlsx'
#file_path_coding <- 'lsh_coding.xlsx'
file_path_priors <- 'lsh_data_processing/priors.xlsx'
file_path_data <- paste0(path_to_lsh_data,file_name_lsh_data)
file_path_experiment_template <- 'lsh_data_processing/experiment_template.xlsx'

################## ----- Read LSH data and coding data ----- ########################################

individs_raw <- read_excel(file_path_data,sheet = 'Einstaklingar', skip=3)
hospital_visits_raw <- read_excel(file_path_data,sheet ='Komur og innlagnir', skip=3)
hospital_isolations_raw <- read_excel(file_path_data,sheet ='Einangrun-soguleg gogn', skip=6)
hospital_isolations_heilsugatt_raw <- read_excel(file_path_data,sheet='Einangrun - af skjáborði Heilsu',skip=3)
covid_diagnosis_raw <- read_excel(file_path_data,sheet ='Dag. greiningar frá veirurannsó', skip=6)
outpatient_BB_raw <- read_excel(file_path_data,sheet ='GD Birkiborg-form', skip=3)
interview_first_raw <- read_excel(file_path_data,sheet = 'Fyrsta viðtal úr forms', skip=3)
interview_follow_up_raw <- read_excel(file_path_data,sheet = 'Spurningar úr forms Pivot', skip=1)
interview_last_raw <- read_excel(file_path_data,sheet = 'Lokaviðtal-Spurning úr forms', skip=1)
interview_extra_raw <- read_excel(file_path_data,sheet = 'Áhættuflokkur ofl úr hóp', skip=3) 
NEWS_score_raw <- read_excel(file_path_data,sheet = 'NEWS score ', skip=3)
ventilator_times_raw <- read_excel(file_path_data,sheet = 'Öndunarvél - tímar', skip=3)
treatment_contraints_raw <- read_excel(file_path_data,sheet = 'Takmörkun meðferðar', skip=3)
date_data_raw <- read_excel(file_path_data,sheet = 'tímasetning', skip=1)

covid_groups <- read_excel(file_path_coding, sheet = 'lsh_covid_groups')
unit_categories <- read_excel(file_path_coding,sheet = 'lsh_unit_categories') %>%
                    mutate(unit_category=!!as.name(paste0('unit_category_',unit_category_type)),
                           unit_category_order=!!as.name(paste0('unit_category_order_',unit_category_type)))
text_out_categories <- read_excel(file_path_coding,sheet = 'lsh_text_out_categories') %>%
                        mutate(text_out_category=!!as.name(paste0('text_out_category_',text_out_category_type)))
isolation_categories <- read_excel(file_path_coding,sheet = 'lsh_isolation_categories') %>%
                        mutate(isolation_category=!!as.name(paste0('isolation_category_',isolation_category_type)))
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories') %>%
                                  mutate(clinical_assessment_category=!!as.name(paste0('clinical_assessment_category_',clinical_assessment_category_type)),
                                         clinical_assessment_category_order=!!as.name(paste0('clinical_assessment_category_order_',clinical_assessment_category_type)))
priority_categories <- read_excel(file_path_coding,sheet = 'priority_categories')
age_groups <- read_excel(file_path_coding,sheet = 'age_groups')
length_of_stay_categories <- read_excel(file_path_coding,sheet = 'length_of_stay_categories') 

comorbidities_categories <- read_excel(file_path_coding,sheet = 'comorbidities') %>%
                              arrange(comorbidities_raw)
sheet_names <- read_excel(file_path_coding,sheet = 'lsh_sheet_names',trim_ws = FALSE)
prior_transitions <- read_excel(file_path_priors,sheet = 'transitions')
prior_length_of_stay <- read_excel(file_path_priors,sheet = 'length_of_stay')


experiment_description <- read_excel(file_path_experiment_template,sheet='experiment_description')
experiment_specification <- read_excel(file_path_experiment_template,sheet='experiment_specification')
run_description <- read_excel(file_path_experiment_template,sheet='run_description')
run_specification <- read_excel(file_path_experiment_template,sheet='run_specification')
heuristics_description <- read_excel(file_path_experiment_template,sheet='heuristics_description')



#vaccine_gamla <- read_excel(paste0(path_sensitive_tables,"/bolusetningar_an_person_key.xlsx"),sheet='Report 1',skip=3)
#vaccine_raw_gamla <- read_excel(paste0(path_sensitive_tables,"bolusetningar.xlsx"),sheet='Report 1',skip=9)

#check <- read_excel(paste0(path_sensitive_tables,"/afstemm.xlsx")) 
vaccine_raw <- read_excel(paste0(path_sensitive_tables, file_name_lsh_vax),sheet='línur',skip=1)
#vaccine_test <- read_excel(paste0(path_sensitive_tables,"bola.xlsx"),sheet='Report 1',skip=3)
#vaccine_raw <- read.csv(paste0(path_sensitive_tables,'bola.csv'))
#test_lsh_data_file()
#vaccine_raw <- vaccine_raw %>% filter(!is.na(`Fjöldi bólusetninga`)|`Kóði lögleg kennitala`==0)
#write.table(vaccine_raw,file=paste0(path_sensitive_tables,'bola.csv'),sep=',')


################## ----- Cleaning ----- ##############################################################
date_data <- date_data_raw$`Dagurtími þegar gögnin sótt` %>% as.Date()
date_last_known_state <- date_data-1
date_observed_start <- date_data-1

covid_diagnosis <- rename(covid_diagnosis_raw,patient_id=`Person Key`,date_time_diagnosis_pcr=`Dagsetning greiningar`) %>%
                    separate(.,col='date_time_diagnosis_pcr',into=c('date_diagnosis_pcr','time_diagnosis_pcr'),sep=' ',remove=FALSE) %>%
                    mutate(.,date_diagnosis_pcr=as.Date(date_diagnosis_pcr,"%Y-%m-%d")) %>%
                    select(.,patient_id, date_diagnosis_pcr) #%>%
                    #filter(!(patient_id %in% bad_ids))

# hospital_isolations_covid_now <- hospital_isolations_heilsugatt_raw %>%
#   rename(patient_id=`Person Key`,date_isolation=`Dags einangrun skráð`,isolation_type=`Sjúklings einangrun`) %>%
#   mutate(.,patient_id=suppressWarnings(as.numeric(patient_id))) %>%
#   filter(!is.na(isolation_type)) %>%
#   filter(isolation_type=='COVID-19'|isolation_type=='Grunur um COVID-19') %>%
#   select(patient_id,date_isolation)

individs <- rename(individs_raw,patient_id=`Person Key`,age=`Aldur heil ár`, sex=`Yfirfl. kyns`,zip_code=`Póstnúmer`,
                   covid_group_raw=`Heiti sjúklingahóps`) %>%
              filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
              left_join(.,select(covid_groups,covid_group_raw,covid_group),by='covid_group_raw') %>%
              mutate(clinically_diagnosed=covid_group=='clinically_diagnosed') %>%
              mutate(recovered=covid_group=='recovered') %>%
              #mutate(recovered=if_else(patient_id %in% hospital_isolations_covid_now$patient_id,FALSE,recovered)) %>%
              group_by(.,patient_id) %>%
              summarise(zip_code=min(zip_code,na.rm=T),
                        age=min(age,na.rm=T),
                        sex=min(sex,na.rm=T),
                        clinically_diagnosed=any(clinically_diagnosed),
                        recovered=any(recovered)) %>%
              ungroup() %>%
              filter(age>=0&age<=120)

#hospital visits
hospital_visits <- rename(hospital_visits_raw, patient_id=`Person Key`,unit_in=`Deild Heiti`,date_time_in=`Dagurtími innskriftar`, date_time_out=`Dagurtími útskriftar`,
                          text_out=`Heiti afdrifa`,date_diagnosis_hospital=`Dagsetning skráningar - NR 1`) %>%
                    select(patient_id,unit_in,date_time_in,date_time_out,text_out,date_diagnosis_hospital) %>%
                    filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                    mutate(date_time_out=as.POSIXct(gsub('9999-12-31 00:00:00',NA,date_time_out),format='%Y-%m-%d %H:%M:%S',tz='UTC')) %>%
                    inner_join(.,select(unit_categories,unit_category_raw,unit_category_all),by=c('unit_in'='unit_category_raw')) %>%
                    inner_join(.,select(text_out_categories,text_out_category_raw,text_out_category),by=c('text_out'='text_out_category_raw')) %>%
                    mutate(.,text_out=text_out_category) %>%
                    filter(!(unit_category_all %in% c('maternity_clinic','endoscopy_clinic','inpatient_ward_maternity'))) %>%
                    separate(col='date_time_in',into=c('date_in','time_in'),sep=' ',remove=FALSE) %>%
                    separate(col='date_time_out',into=c('date_out','time_out'),sep=' ',remove=FALSE) %>%
                    mutate(.,date_in=as.Date(date_in,"%Y-%m-%d"),date_out=as.Date(date_out,"%Y-%m-%d")) %>%
                    filter(date_in<=date_last_known_state) %>%
                    select(patient_id,unit_in,unit_category_all,date_time_in,date_in,date_time_out,date_out,text_out,date_diagnosis_hospital) #%>%
                    #filter(!(patient_id %in% bad_ids))

## Fixing data for one patient ##
hospital_visits <- hospital_visits %>% 
                  mutate(date_out = if_else(patient_id==114134, if_else(unit_in=="Öldrunarlækningadeild B (Lk-K2)", if_else(is.na(date_out), ymd("2020-10-23"), date_out), date_out), date_out)) %>%
                  mutate(date_time_out = if_else(patient_id==114134, if_else(unit_in=="Öldrunarlækningadeild B (Lk-K2)", if_else(is.na(date_time_out), ymd_hms("2020-10-23 20:21:00"), date_time_out), date_time_out), date_time_out))
                  
hospital_visits <- subset(hospital_visits, !(patient_id==114134 & is.na(date_diagnosis_hospital) & date_in==ymd("2020-10-23")))
#################################
outpatient_BB <- rename(outpatient_BB_raw, patient_id=`Person Key`, date_in=`Dagsetning komu - fix`, arrival_serial=`Raðnúmer komu`, 
                     arrival_number=`Koma á Birkiborg`, date_form_created=`Dags form búið til`, date_form_changed=`Dags form breytt`, text_out=`Afdrif`,
                     date_symptoms=`Dagsetning einkenna`, date_worsening=`Dagsetning versnunar`) %>% 
              select(patient_id, date_in, arrival_serial, arrival_number, date_form_created, date_form_changed, text_out, date_symptoms, date_worsening) %>%
              filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
              mutate(text_out=if_else(text_out=="Heim án bókaðrar endurkomu","home", if_else(text_out=="Heim með bókaðri endurkomu", "home_back", if_else(text_out=="Innlögn á spítala", "inpatient_ward", text_out)))) %>%
              mutate(., date_in=as.Date(date_in,"%Y-%m-%d", tz='UTC'), date_form_created=as.Date(date_form_created,"%Y-%m-%d", tz='UTC'), 
                     date_form_changed=as.Date(date_form_changed,"%Y-%m-%d", tz='UTC'), date_symptoms=as.Date(date_symptoms,"%Y-%m-%d", tz='UTC'), 
                     date_worsening=as.Date(date_worsening,"%Y-%m-%d", tz='UTC')) %>%
              filter(!is.na(date_in)) %>%
              mutate("outpatient"=TRUE)

hospital_isolations <- rename(hospital_isolations_raw,patient_id=`Person Key`,isolation=`Einangrun`,unit_in=`Deild heiti`,
                              date_time_in=`Dags einangrun byrjar`,date_time_out=`Dags einangrun endar`) %>%
                        select(patient_id,isolation,unit_in,date_time_in,date_time_out) %>%
                        mutate(.,patient_id=suppressWarnings(as.numeric(patient_id))) %>%
                        filter(!is.na(patient_id)) %>%
                        filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                        mutate(date_time_out=as.POSIXct(gsub('9999-12-31 00:00:00',NA,date_time_out),format='%Y-%m-%d %H:%M:%S',tz='UTC')) %>%
                        inner_join(.,select(unit_categories,unit_category_raw,unit_category_all),by=c('unit_in'='unit_category_raw')) %>%
                        filter(!(unit_category_all %in% c('maternity_clinic','endoscopy_clinic','inpatient_ward_maternity'))) %>%
                        inner_join(.,select(isolation_categories,isolation_category_raw,isolation_category),by=c('isolation'='isolation_category_raw')) %>%
                        separate(col='date_time_in',into=c('date_in','time_in'),sep=' ',remove=FALSE) %>%
                        separate(col='date_time_out',into=c('date_out','time_out'),sep=' ',remove=FALSE) %>%
                        mutate(.,date_in=as.Date(date_in,"%Y-%m-%d"),date_out=as.Date(date_out,"%Y-%m-%d")) %>%
                        filter(date_in<=date_last_known_state) %>%
                        select(patient_id,isolation_category,unit_in,date_time_in,date_in,date_time_out,date_out) #%>%
                        #filter(!(patient_id %in% bad_ids))
                    
#forms data
interview_first <- rename(interview_first_raw,patient_id=`Person Key`,date_first_symptoms=`Upphafsdagur einkenna`, date_diagnosis_interview=`Dagsetning greiningar`,
                              priority=`Forgangur`, clinical_assessment=`Klínískt mat - flokkun`,date_clinical_assessment=`Síma eftirfylgd hefst`,comorbidities_raw=`Sjúkdómar`) %>%
                        select(.,patient_id,date_first_symptoms,date_diagnosis_interview,priority,date_clinical_assessment,clinical_assessment,comorbidities_raw) %>%
                        filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                        mutate_at(.,vars(matches('date')),~as.Date(gsub('\\s.*','',.),"%Y-%m-%d")) %>%
                        mutate(priority=gsub('\\s.*','', priority),clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                        left_join(.,select(priority_categories,priority_raw,priority_all,priority_all_order),by=c('priority'='priority_raw')) %>%
                        mutate(.,priority=priority_all,
                               priority_order=priority_all_order) %>%
                        select(-priority_all,-priority_all_order) %>%
                        mutate(clinical_assessment=if_else((clinical_assessment=="Blár" & (date_clinical_assessment>ymd("2020-10-01") | date_first_symptoms>ymd("2020-10-01") | date_diagnosis_interview>ymd("2020-10-01"))), "Grænn", clinical_assessment)) %>%
                        left_join(.,select(clinical_assessment_categories,clinical_assessment_category_raw,clinical_assessment_category,clinical_assessment_category_order),
                                  by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                        mutate(.,clinical_assessment=clinical_assessment_category,
                                  clinical_assessment_order=clinical_assessment_category_order) %>%
                        select(-clinical_assessment_category,-clinical_assessment_category_order) %>%
                        mutate(.,comorbidities_raw=gsub('; ',';',comorbidities_raw)) %>%
                        separate_rows(.,comorbidities_raw,sep = ';') %>%
                        left_join(select(comorbidities_categories,comorbidities_raw,comorbidities_names),by='comorbidities_raw') %>%
                        select(-comorbidities_raw) %>%
                        group_by_at(vars(-comorbidities_names)) %>%
                        filter(!duplicated(comorbidities_names)) %>%
                        filter(if_else(n()>1 & comorbidities_names=='healthy',FALSE,TRUE)) %>%
                        summarise(comorbidities=paste(comorbidities_names,collapse=';'),
                                  num_comorbidity=sum(comorbidities_names!='healthy')) %>%
                        ungroup() %>%
                        inner_join(select(individs,patient_id,age,clinically_diagnosed),by='patient_id') %>%
                        left_join(covid_diagnosis,by='patient_id') %>%
                        mutate(is_valid=interview_first_is_valid(age,num_comorbidity,priority,date_first_symptoms,date_diagnosis_interview,date_diagnosis_pcr)) %>%
                        group_by(patient_id) %>%
                        filter(.,if(any(!is.na(date_diagnosis_interview))) !is.na(date_diagnosis_interview) else TRUE) %>%
                        filter(.,if(any(is_valid)) is_valid else TRUE) %>%
                        mutate(.,num_na=is.na(num_comorbidity) + is.na(priority) + is.na(date_first_symptoms) + is.na(date_diagnosis_interview) + is.na(clinical_assessment)) %>%
                        filter(num_na==min(num_na)) %>%
                        filter(if_else(!is.na(date_diagnosis_interview) & !is.na(date_diagnosis_pcr),abs(date_diagnosis_interview-date_diagnosis_pcr)==min(abs(date_diagnosis_interview-date_diagnosis_pcr)),TRUE)) %>%
                        slice(1) %>%
                        filter(.,if_else(is.finite(date_diagnosis_interview),date_diagnosis_interview<=date_last_known_state,TRUE)) %>% #remove entries where date_diagnosis_interview after date_last_known_state
                        select(patient_id,priority,comorbidities,num_comorbidity,date_first_symptoms,date_diagnosis_interview,clinical_assessment) #%>%
                        #filter(!(patient_id %in% bad_ids))

#note, min clinical assessment chosen for each date. TODO: get time stamps of interviews
interview_follow_up <- rename(interview_follow_up_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`,clinical_assessment=`Klínískt mat`) %>%
                        select(patient_id,date_clinical_assessment,clinical_assessment) %>%
                        filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                        mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                        filter(.,is.finite(date_clinical_assessment)) %>%
                        filter(.,date_clinical_assessment<=date_last_known_state) %>%
                        mutate(.,clinical_assessment=gsub('\\s.*','', clinical_assessment)) %>%
                        mutate(clinical_assessment=if_else((clinical_assessment=="Blár" & date_clinical_assessment>ymd("2020-10-01")), "Grænn", clinical_assessment)) %>%
                        left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                        mutate(.,clinical_assessment=clinical_assessment_category) %>%
                        select(.,patient_id,date_clinical_assessment,clinical_assessment,clinical_assessment_category_order) %>%
                        group_by(.,patient_id,date_clinical_assessment) %>%
                        summarize(.,clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)]) #%>%
                        #filter(!(patient_id %in% bad_ids))
                        

#date_clinical_assessment is the last interview by a physician. Remove scheduled future phone calls
interview_last <- rename(interview_last_raw,patient_id=`Person Key`,date_clinical_assessment=`Dagsetning símtals`) %>%
                  filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                  mutate(.,date_clinical_assessment=as.Date(gsub('\\s.*','',date_clinical_assessment),"%Y-%m-%d")) %>%
                  filter(is.finite(date_clinical_assessment)) %>%
                  filter(date_clinical_assessment<=date_last_known_state) %>%
                  group_by(.,patient_id) %>%
                  summarize(.,date_clinical_assessment=max(date_clinical_assessment,na.rm=T)) #%>%
                  #filter(!(patient_id %in% bad_ids))
    

interview_extra <- rename(interview_extra_raw,patient_id=`Person Key`,date_time_clinical_assessment=`Dags breytingar`,col_name=`Heiti dálks`,col_value=`Skráningar - breytingar.Skráð gildi`) %>% 
                    select(.,patient_id,date_time_clinical_assessment,col_name,col_value) %>%
                    filter_at(vars(-patient_id),any_vars(!is.na(.))) %>% 
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
                    mutate(clinical_assessment=if_else((clinical_assessment=="Blár" & date_clinical_assessment>ymd("2020-10-01")), "Grænn", clinical_assessment)) %>%
                    left_join(.,clinical_assessment_categories,by=c('clinical_assessment'='clinical_assessment_category_raw')) %>%
                    mutate(.,clinical_assessment=clinical_assessment_category) %>%
                    group_by(.,patient_id,date_clinical_assessment) %>%
                    summarize(.,priority=if(all(is.na(priority))) NA_character_ else priority[which.max(priority_all_order)],
                              clinical_assessment=if(all(is.na(clinical_assessment))) NA_character_ else clinical_assessment[which.max(clinical_assessment_category_order)]) %>%
                    select(patient_id,priority, date_clinical_assessment,clinical_assessment) #%>%
                    #filter(!(patient_id %in% bad_ids))

NEWS_score <- rename(NEWS_score_raw, patient_id=`Person Key`,date_time=`Dagurtími skráningar`,NEWS_score=`News score`) %>% 
              select(.,patient_id,date_time,NEWS_score) %>%
              filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
              mutate(date=as.Date(gsub('\\s.*','',date_time),"%Y-%m-%d")) %>%
              filter(!is.na(NEWS_score)) %>%
              filter(date<=date_last_known_state) %>%
              group_by(.,patient_id,date) %>%
              #summarize(NEWS_score=NEWS_score[which.max(date_time)]) %>%
              summarize(NEWS_score=mean(as.numeric(NEWS_score),na.rm=T)) %>%
              mutate(NEWS_score=if_else(NEWS_score>5,'red','green')) #%>%
              #filter(!(patient_id %in% bad_ids))

ventilator_times <- rename(ventilator_times_raw, patient_id=`Person Key`,date_time_ventilator_start=`Dags settur í öndunarvél`,date_time_ventilator_end=`Dags tekin úr öndunarvél`) %>% 
                      select(.,patient_id,date_time_ventilator_start,date_time_ventilator_end) %>%
                      filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
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
                      summarize(ventilator=ventilator[which.max(ventilator=='red')]) #%>%
                      #filter(!(patient_id %in% bad_ids))

treatment_contraints <- rename(treatment_contraints_raw,patient_id=`Person Key`,intensive_care_unit_restriction=`Gjörgæsluvistun`,ventilator_restriction=`Öndunarvél`) %>%
                        select(patient_id,intensive_care_unit_restriction,ventilator_restriction) %>%
                        filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
                        mutate(intensive_care_unit_restriction=if_else(!is.na(intensive_care_unit_restriction),'icu_restricted','not_icu_restricted')) %>%
                        mutate(ventilator_restriction=if_else(!is.na(ventilator_restriction),'ventilator_restricted','not_ventilator_restricted'))

vacc_data <- vaccine_raw %>%
  rename(real_ssn =`Kóði lögleg kennitala`, patient_id=`Person Key`, vacc_number=`Fjöldi bólusetninga`, date_first_dose=`1. Bólusetningardagur`, date_second_dose=`2. Bólusetningardagur`, vacc_name=`1. Heiti bóluefnis`) %>%
  #rename(real_ssn=`Kóði.lögleg.kennitala`, patient_id=`Person.Key`, vacc_number=`Fjöldi.bólusetninga`, date_first_dose=`X1..Bólusetningardagur`, date_second_dose=`X2..Bólusetningardagur`, vacc_name=`X1..Heiti.bóluefnis`) %>%
  select(real_ssn,patient_id, vacc_number, date_first_dose, date_second_dose, vacc_name) %>%
  mutate(vacc_bool=if_else(vacc_number>=2 | vacc_name=="Janssen", TRUE, FALSE)) %>%
  mutate(date_first_dose=as.Date(date_first_dose), date_second_dose=as.Date(date_second_dose)) %>%
  mutate(date_fully_vaccinated=if_else(vacc_name=="Janssen", date_first_dose+14, if_else(vacc_bool, date_second_dose+14, NULL))) %>%
  mutate(vacc_bool=if_else(date_fully_vaccinated<=date_prediction, TRUE, FALSE)) %>%
  select(real_ssn,patient_id, vacc_bool, date_fully_vaccinated) %>%
  mutate(patient_id=as.numeric(patient_id)) %>%
  filter(patient_id!=0) %>%
  filter(!is.na(patient_id)) %>%
  distinct()

test_cleaning()

################## ----- Data processing ----- ##############################################################

# fix to remove redundancies due to simple classification
#filter out units that are not predicted and relabel the units in the terms to be predicted.Also relabel text_out


hospital_isolations_filtered <- inner_join(hospital_isolations,select(unit_categories,unit_category_raw,unit_category),by=c('unit_in'='unit_category_raw')) %>%
                                filter(unit_category!='home') %>%
                                mutate(unit_in=unit_category) %>%
                                filter(isolation_category!='covid19_quarantine') %>%
                                group_by(patient_id) %>%
                                arrange(date_time_in) %>%
                                mutate(state_block_nr=get_hospital_state_block_numbers(unit_in,date_in,date_time_in,date_out,date_time_out)) %>%
                                group_by(patient_id,state_block_nr,unit_in) %>%
                                arrange(date_time_in) %>%
                                summarize(isolation_category=tail(isolation_category,1),
                                          date_time_in=head(date_time_in,1),date_in=head(date_in,1),
                                          date_time_out=tail(date_time_out,1),date_out=tail(date_out,1)) %>%
                                ungroup()

hospital_visits_filtered <- inner_join(hospital_visits,select(unit_categories,unit_category_raw,unit_category),by=c('unit_in'='unit_category_raw')) %>%
                            filter(unit_category!='home') %>%
                            mutate(unit_in=unit_category) %>%
                            group_by(patient_id) %>%
                            arrange(date_time_in) %>%
                            mutate(state_block_nr=get_hospital_state_block_numbers(unit_in,date_in,date_time_in,date_out,date_time_out)) %>%
                            group_by(patient_id,state_block_nr,unit_in) %>%
                            arrange(date_time_in) %>%
                            summarize(date_time_in=head(date_time_in,1),date_in=head(date_in,1),
                                      date_time_out=tail(date_time_out,1),date_out=tail(date_out,1),
                                      text_out=tail(text_out,1)) %>%
                            ungroup() %>%
                            full_join(.,hospital_isolations_filtered,by=c('patient_id','state_block_nr','unit_in'),suffix=c('','_isolation')) %>%
                            left_join(interview_last,by='patient_id') %>%
                            rename(date_recovered=date_clinical_assessment) %>%
                            #mutate(date_recovered=ifelse(patient_id %in% hospital_isolations_covid_now$patient_id,as.Date(NA),date_recovered)) %>% #bradabirgdalausn fyrir folk i einangrun
                            filter(if_else(!is.na(date_recovered),date_in<=date_recovered,TRUE)) %>% # remove hospital visits after recovery. If recovery date is NA, keep entry
                            mutate(in_hospital_before=if_else(!is.na(date_in),(date_in_isolation-date_in)>1,FALSE)) %>% # check if in hospital before becoming infected
                            mutate(date_in=if_else(is.na(date_in_isolation),date_in,date_in_isolation),
                                   date_time_in=if_else(is.na(date_time_in_isolation),date_time_in,date_time_in_isolation),
                                   date_out=if_else(is.na(date_out_isolation),date_out,date_out_isolation),
                                   date_time_out=if_else(is.na(date_time_out_isolation),date_time_out,date_time_out_isolation)
                                   ) %>%
                            inner_join(select(individs,patient_id),by='patient_id') %>%
                            arrange(patient_id,date_time_in) %>%
                            select(patient_id,unit_in,date_in,date_time_in,date_out,date_time_out,text_out,in_hospital_before)


hospital_visit_first_date <- group_by(hospital_visits_filtered,patient_id) %>%
                              arrange(date_time_in) %>%
                              summarize(date_diagnosis_hospital=min(date_in,na.rm=TRUE)) %>%
                              ungroup()
                           
hospital_outcomes <- group_by(hospital_visits_filtered,patient_id) %>%
                       summarize(.,outcome_hospital=if_else(any(grepl('death',text_out)),'death',NULL),
                                   date_outcome_hospital=Min(date_out[grepl('death',text_out)],na.rm=TRUE)) %>%
                       ungroup()

#TODO: consider choosing maximum priority (i.e. max priority order)
interview_extra_priority <- group_by(interview_extra,patient_id) %>%
                              summarize(priority_interview_extra=Min(priority,na.rm=TRUE)) %>%
                              ungroup()

#Find latest interview for each patient
interview_last_date <- bind_rows(select(interview_first,patient_id,date_diagnosis_interview) %>% rename(date_clinical_assessment=date_diagnosis_interview),
                                 select(interview_follow_up,patient_id,date_clinical_assessment),
                                 select(interview_extra,patient_id,date_clinical_assessment)) %>%
                        filter(date_clinical_assessment<=date_data) %>% # We have some future dates - check.
                        group_by(.,patient_id) %>%
                        summarize(.,date_last_known=max(date_clinical_assessment,na.rm=TRUE)) %>%
                        ungroup() %>% 
                        left_join(.,interview_last,by='patient_id') %>% 
                        mutate(date_last_interview=if_else(is.finite(date_clinical_assessment),date_clinical_assessment,date_last_known))

individs_extended <- inner_join(individs,covid_diagnosis,by='patient_id') %>%
                      left_join(.,hospital_visit_first_date,by='patient_id') %>%
                      mutate(.,date_diagnosis_tmp=if_else(!is.na(date_diagnosis_hospital),pmin(date_diagnosis_pcr,date_diagnosis_hospital),date_diagnosis_pcr)) %>%
                      left_join(.,interview_first,by='patient_id') %>%
                      mutate(.,date_diagnosis=if_else(!is.na(date_diagnosis_interview),pmin(date_diagnosis_tmp,date_diagnosis_interview),date_diagnosis_tmp)) %>%
                      left_join(.,interview_extra_priority,by='patient_id') %>%
                      mutate(.,priority=ifelse(is.na(priority),priority_interview_extra,priority)) %>%
                      mutate(outcome=ifelse(recovered,'recovered','in_hospital_system')) %>%
                      left_join(.,hospital_outcomes,by='patient_id') %>%
                      mutate(outcome=if_else(!is.na(outcome_hospital),outcome_hospital,outcome)) %>%
                      left_join(.,interview_last_date,by='patient_id') %>%
                      mutate(.,date_outcome=if_else(outcome=='in_hospital_system',as.Date(NA),if_else(outcome=='recovered',date_last_interview,date_outcome_hospital))) %>%
                      filter(if_else(is.finite(date_outcome) & outcome=='recovered',(date_outcome-date_diagnosis)>0,TRUE)) %>%
                      mutate(.,priority=impute_priority(priority,age,num_comorbidity)) %>%
                      left_join(.,select(treatment_contraints,patient_id,intensive_care_unit_restriction),by='patient_id') %>%
                      mutate(intensive_care_unit_restriction=if_else(is.na(intensive_care_unit_restriction),'not_icu_restricted',intensive_care_unit_restriction)) %>%
                      select(.,patient_id,zip_code,age,sex,priority,num_comorbidity,intensive_care_unit_restriction,date_first_symptoms,date_diagnosis,outcome,date_outcome) %>%
                      mutate(date_first_symptoms=if_else(is.na(date_first_symptoms), date_diagnosis, date_first_symptoms)) %>%
                      left_join(vacc_data,by=c('patient_id'='patient_id')) %>%
                      mutate(real_ssn=if_else(is.na(real_ssn),as.integer(1),as.integer(real_ssn))) %>%
                      mutate(vacc_at_diagnosis=if_else(date_diagnosis>=date_fully_vaccinated,TRUE,FALSE)) %>%
                      filter(date_diagnosis<=date_last_known_state) %>% #ath til ad virki med gognum dagsins i dag
                      mutate(vacc_at_diagnosis=if_else(is.na(vacc_at_diagnosis),FALSE,vacc_at_diagnosis)) #%>%
                      #mutate(vacc_at_diagnosis=if_else(real_ssn==0&is.na(date_fully_vaccinated),TRUE,vacc_at_diagnosis)) %>% #utlendingar bolusettir
                      #select(.,-real_ssn)
                       
# skoda <- individs_extended %>%
#   filter(date_diagnosis>=ymd("2021-07-01")) %>%
#   mutate(vacc_at_diagnosis=if_else(vacc_at_diagnosis,'bolusett','obolusett'))
# smit <- skoda %>% group_by(vacc_at_diagnosis) %>% summarise(sum=n())
# smit <- smit %>% mutate(prop=sum/sum(sum))
# innlagnir <- skoda %>% mutate(hospital=if_else(state_worst=='home','no_hospital','hospital')) %>%
#   group_by(hospital,vacc_at_diagnosis) %>%
#   summarise(sum=n()) %>%
#   ungroup() %>%
#   group_by(vacc_at_diagnosis) %>%
#   mutate(prop=sum/sum(sum))
# icu <- skoda %>% mutate(icu=if_else(state_worst=='intensive_care_unit','icu','no_icu')) %>%
#   group_by(icu,vacc_at_diagnosis) %>%
#   summarise(sum=n()) %>%
#   ungroup() %>%
#   group_by(vacc_at_diagnosis) %>%
#   mutate(prop=sum/sum(sum))
# individs_extended$vacc<- runif(nrow(individs_extended),0,1)
# individs_extended<- individs_extended %>%mutate(vacc=if_else(vacc>=0.3,FALSE,TRUE)) %>%
#   mutate(vacc_at_diagnosis=if_else(date_diagnosis<=ymd("2021-06-01"),FALSE,vacc))

#patient transitions.
#Start by assuming everybody is at home from the time diagnosed to today
#Note: patients diagnosed on date_data have a known state on that date, but others do not. Applies both for dates_home and dates_hospital

dates_home <- lapply(1:nrow(individs_extended),function(i){  
    #date_home_latest_imputed <- if_else(individs_extended$date_diagnosis[i]==date_data,date_data, date_last_known_state)
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
                                       rename(interview_first,date_clinical_assessment=date_diagnosis_interview) %>% select(patient_id,date_clinical_assessment,clinical_assessment),
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
                    return(tmp)}) %>% 
                    bind_rows() %>%
                    group_by(.,patient_id,date) %>%
                    summarize(state=tail(state,1)) %>%
                    ungroup()

outpatient_BB_extended <- left_join(dates_clinical_assessment, select(outpatient_BB, patient_id, date_in, date_symptoms, outpatient), by=c("patient_id", "date"="date_in")) %>%
                          mutate(outpatient=if_else(is.na(outpatient), FALSE, outpatient)) %>% 
                          filter(!is.na(date)) %>%
                          left_join(., select(individs_extended, patient_id, age, sex, priority, num_comorbidity, date_diagnosis, outcome, date_outcome, date_first_symptoms), by="patient_id") %>%
                          mutate(date_first_symptoms=if_else(is.na(date_first_symptoms), date_symptoms, date_first_symptoms)) %>%
                          select(., -date_symptoms)

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
                                mutate(state_tomorrow=if_else(outcome=='recovered' & date_tomorrow==date_outcome & date_outcome<=date_last_known_state,'recovered',state_tomorrow),
                                      severity_tomorrow=if_else(outcome %in% c('death','recovered') & date_tomorrow==date_outcome & date_outcome<=date_last_known_state,NA_character_,severity_tomorrow)) %>%
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
  
  
individs_splitting_variables <- select(individs_extended,patient_id,age,sex,priority,state_first,intensive_care_unit_restriction,vacc_at_diagnosis) %>%
                                left_join(age_groups,by='age') %>%
                                mutate_at(vars(matches('age_'),-matches('order')),list( ~paste0('age_',.))) %>%
                                left_join(priority_categories,by=c('priority'='priority_all')) %>%
                                rename(priority_all=priority) %>%
                                mutate_at(vars(matches('priority'),-matches('order')),list( ~paste0('priority_',.))) %>%
                                select(-matches('raw')) %>%
                                mutate(point_of_diagnosis=if_else(state_first=='home','outpatient','inpatient')) %>%
                                mutate(point_of_diagnosis_order=if_else(point_of_diagnosis=='outpatient',1,2)) %>%
                                mutate(age_simple_point_of_diagnosis=if_else(age_simple=='age_0-50',age_simple,paste(age_simple,point_of_diagnosis,sep='_'))) %>%
                                mutate(age_simple_point_of_diagnosis_order=if_else(age_simple=='age_0-50',1,if_else(point_of_diagnosis=='outpatient',2,3))) %>%
                                mutate(age_simple_intensive_care_unit_restriction=if_else(age_simple=='age_0-50',age_simple,paste(age_simple,intensive_care_unit_restriction,sep='_'))) %>%
                                mutate(age_simple_intensive_care_unit_restriction_order=if_else(age_simple=='age_0-50',1,if_else(intensive_care_unit_restriction=='not_icu_restricted',2,3))) %>%
                                mutate(age_simple_sex=if_else(age_simple=='age_0-50',age_simple,paste(age_simple,sex,sep='_'))) %>%
                                mutate(age_simple_sex_order=if_else(age_simple=='age_0-50',1,if_else(sex=='Kona',2,3))) %>%
                                mutate(age_simple_vaccinated=paste(age_simple,vacc_at_diagnosis,sep='_')) %>%
                                mutate(age_simple_vaccinated_order=if_else(age_simple_vaccinated=='age_0-50_FALSE',1,if_else(age_simple_vaccinated=='age_0-50_TRUE',2,if_else(age_simple_vaccinated=='age_51+_FALSE',3,4))))


#summarize state blocks, extracting min and max date to calculate length of each state. Note:states entered yesterday are not used to estimate length of stay
patient_transitions_state_blocks <- group_by(patient_transitions,patient_id) %>%
                                    mutate(state_block_nr=get_state_block_numbers(state),
                                           state_with_severity_block_nr=get_state_block_numbers(paste0(state,severity))) %>%
                                    group_by(.,patient_id,state_block_nr,state_with_severity_block_nr,state,severity) %>% arrange(.,date) %>% 
                                    summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)],severity_next=severity_tomorrow[which.max(date)]) %>%
                                    mutate(censored=(is.na(state_next))) %>%
                                    mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
                                    ungroup()
#Fyrir sankey í dashboard
patient_transitions_state_blocks_base <- group_by(patient_transitions,patient_id) %>%
  mutate(state_block_nr=get_state_block_numbers(state)) %>%
  group_by(.,patient_id,state_block_nr,state) %>% arrange(.,date) %>% 
  summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)]) %>%
  mutate(censored=(is.na(state_next))) %>%
  mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
  ungroup() %>%
  select(patient_id, state, date_start = 'state_block_nr_start', date_end = 'state_block_nr_end', state_next, censored)

# individs_extended_forsendur <- individs_extended %>% select(-patient_id)
# ids_in_now <- patient_transitions_state_blocks_base %>% filter(censored) %>% select(patient_id)
# legutimar_forsendur <- patient_transitions_state_blocks_base %>%
#   left_join(select(individs_extended, patient_id, vacc_at_diagnosis, age)) %>%
#   filter(state=="inpatient_ward" | state=="intensive_care_unit") %>%
#   filter(!(patient_id %in% ids_in_now$patient_id)) %>%
#   mutate(length_of_stay_tmp=as.numeric(date_end-date_start)) %>%
#   filter(length_of_stay_tmp<=59) %>%
#   group_by(patient_id, state) %>%
#   mutate(length_of_stay=sum(length_of_stay_tmp)) %>%
#   ungroup() %>%
#   distinct(patient_id, state, vacc_at_diagnosis, age, length_of_stay, .keep_all = T) %>%
#   group_by(patient_id) %>%
#   mutate(length_of_stay_hospital=sum(length_of_stay)) %>%
#   ungroup() %>%
#   select(-patient_id, -censored)

#### Lagað fyrir hermun{ (kannski til betri leið til að gera þetta, tekur fólkið líka út aftur í tímann)
# Tökum út þá sem eru lengur en 59 daga í stöðu, veldur vandræðum í gögnum eftir ákveðinn tíma
flawed_state_duration_ids <- patient_transitions_state_blocks %>% filter(state_duration>59) %>% select(patient_id) #| state_block_nr_start<ymd("2021-06-01")
environment_objects <- objects()

dfs <- list()

#Finnur allt í environment sem inniheldur þessi id
for(i in environment_objects){
  x=get(i)
  if(is.data.frame(x)) {
    if("patient_id" %in% colnames(x)){
      dfs[[i]] <- get(i)
    }
  }
}

filter_bad_LOS <- function(dataframe){
  dataframe <- dataframe %>% filter(!(patient_id %in% flawed_state_duration_ids$patient_id))
}

dfs <- within(dfs, rm(flawed_state_duration_ids)) 

#Tökum út raðir með þessi id
dfs <- lapply(dfs, filter_bad_LOS)

#Yfirskrifum í environment
list2env(dfs, env=globalenv())
rm(dfs, flawed_state_duration_ids, environment_objects)

#### }Lagað fyrir hermun

# TODO: fix tests
#test_data_processing()

################# ----- Historical data ------ ###############
historical_expanded <- expand_grid(date=seq(min(patient_transitions$date),max(patient_transitions$date),by=1),state=get_states_in_order('base',active=TRUE))
historical_data <- group_by(patient_transitions,date,state) %>% 
                    summarise(count=n()) %>%
                    right_join(.,historical_expanded,by=c('date','state')) %>%
                    mutate(count=if_else(is.na(count),0,as.numeric(count)))
historical_data_children <- patient_transitions %>%
                    left_join(select(individs, patient_id, age), by="patient_id") %>%
                    filter(age<=15) %>%
                    group_by(date,state) %>% 
                    summarise(count=n()) %>%
                    right_join(.,historical_expanded,by=c('date','state')) %>%
                    mutate(count=if_else(is.na(count),0,as.numeric(count)))
historical_turnover <- get_historical_turnover()
historical_state_sequences_base <- get_state_sequences(model='base',seq_type = 'finished')
historical_state_sequences_extended <- get_state_sequences(model='extended',seq_type = 'finished')
historical_infections_per_date <- covid_diagnosis %>% 
  group_by(date_diagnosis_pcr) %>% 
  rename(date=date_diagnosis_pcr) %>%
  summarize(count=n())
################# ----- Predicted number of infections ------ ##############################
if(forecast=='hi'){
  infections_predicted_per_date <- get_infections_predicted_per_date(source='hi', date_prediction)
}
if(forecast=='from_file'){
  infections_predicted_per_date <- get_infections_predicted_per_date(source='from_file', date_prediction)
}
if(forecast=='manual'){
  stikar_manual <- read.csv(paste0(path_tables, "stikar_manual.csv"))
  infections_predicted_per_date <- get_infections_predicted_per_date(source='manual', date_prediction, stikar_manual$alpha, stikar_manual$beta, stikar_manual$S, stikar_manual$nu, stikar_manual$day_n)
}

##### Auka til þess að lengja spá aftur í tímann með sögulegum gögnum ######
if(min(infections_predicted_per_date$date)>date_data-7){
  date_data_end<-date_data
  day1 <- infections_predicted_per_date %>% filter(date==min(date))
  n <- nrow(day1)
  firstday <- min(infections_predicted_per_date$date)-20
  day_n <- date_data_end-firstday
  new_cases_manual <- data.frame("date"=rep(seq.Date(from=firstday, length.out = day_n, by=1), each=n),
                               "new_cases"=rep(0:(n-1),times=day_n))
  infections_per_date<-covid_diagnosis %>% group_by(date_diagnosis_pcr) %>% summarize(n=n())
  new_cases_manual<-left_join(new_cases_manual,infections_per_date,by=c(date="date_diagnosis_pcr"))

  new_cases_manual<-new_cases_manual %>% mutate(count=if_else(n==new_cases, 8000, 0)) %>% select(date,new_cases,count)
  new_cases_manual<-new_cases_manual %>% mutate(count = replace_na(count, 0))
  new_cases_predicted <- infections_predicted_per_date %>% filter(date>(date_data_end-1))
  infections_predicted_per_date <- rbind(new_cases_manual,new_cases_predicted) %>% 
    group_by(date)
}
################# ----- proportion of patients going to outpatient clinic ------ #################
prop_outpatient_clinic <- get_prop_outpatient_clinic(historical_data)


if(write_tables){
  write.table(historical_data, file = paste0(path_tables,date_data,'_historical_data.csv'), quote = F,row.names=F,sep=',')
  write.table(historical_turnover, file = paste0(path_tables,date_data,'_historical_turnover.csv'), quote = F,row.names=F,sep=',')
  write.table(prop_outpatient_clinic, file=paste0(path_outpatient_clinic,date_data,'_prop_outpatient_clinic.csv'), quote = F,row.names=F,sep=',')
  write.table(infections_predicted_per_date, file = paste0(path_tables,date_data,'_infections_predicted.csv'), quote = F,row.names=F,sep=',')
  write.table(historical_infections_per_date, file = paste0(path_tables,'infections_historical.csv'), quote = F,row.names=F,sep=',')
  write.table(historical_data_children, file = paste0(path_tables,'infections_historical_children.csv'), quote = F,row.names=F,sep=',')
}

################# ----- Transition summary and length of stay distribution for all experiments ------ ##############################
run_info <- get_run_info(run_id) 

filter_date <- ymd("2021-06-01")
filter_date_los <- filter_date
filter_date_transition <- filter_date
filter_date_first_state <- filter_date

if(write_tables){
  write.table(run_info, file = paste0(path_tables,date_data,'_',run_id,'_run_info.csv'), quote = F,row.names=F,sep='\t',col.names=F)
}
dates_observed <- seq(date_observed_start,date_last_known_state,by=1) 
for(id in run_info$experiment_id){
  experiment_table_list <- get_tables_for_experiment(id,dates_observed,filter_date_transition,filter_date_first_state,filter_date_los)
  if(write_tables){
    write.table(experiment_table_list$current_state_per_date,file=paste0(path_sensitive_tables,date_data,'_',id,'_current_state_per_date.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$current_state_per_date_filtered,file=paste0(path_sensitive_tables,date_data,'_',id,'_current_state_per_date_filtered.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$first_state,file=paste0(path_tables,date_data,'_',id,'_first_state.csv'),sep=',',row.names=F,quote=F)
    write.table(experiment_table_list$transition_summary,file=paste0(path_tables,date_data,'_',id,'_transition_summary.csv'),sep=',',row.names=FALSE,quote=FALSE)
    write.table(experiment_table_list$length_of_stay,file=paste0(path_tables,date_data,'_',id,'_length_of_stay.csv'),sep=',',row.names=F,quote=F)
  }
}

