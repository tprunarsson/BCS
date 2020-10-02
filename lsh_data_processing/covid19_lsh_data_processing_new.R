#!/usr/bin/env Rscript
invisible(Sys.setlocale("LC_ALL","IS_is"))
suppressPackageStartupMessages({
  suppressWarnings({
  library(optparse)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  })
})
options(dplyr.summarise.inform = FALSE) #Óþolandi skilaboð

cat("Processing data...\n")

source('create_input_for_simulation.R')
source('help_functions.R')
#source('functions_kt.R')

date_data_tmp <- as.Date('2020-05-05','%Y-%m-%d')
date_prediction_tmp <- as.Date('2020-04-20','%Y-%m-%d')
date_observed_start_tmp <- date_data_tmp-3
path_to_lsh_data_tmp <- '../lsh_data_new/'
path_sensitive_tables <- '../lsh_data/'
path_tables <- '../input/'
write_tables_tmp <- TRUE
run_id_tmp <- 17

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
              help="path to data from LSH", metavar="character")
)

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['date_data']])){
  date_data <- date_data_tmp 
  #warning(paste0('You did not provide a current date. ',date_data,' will be used'))
}else{
  date_data <- as.Date(as.character(opt[['date_data']]),'%Y-%m-%d')
}

if(is.null(opt[['date_prediction']])){
  date_prediction <- date_prediction_tmp 
  #warning(paste0('You did not provide a prediction date. ',date_prediction,' will be used'))
}else{
  date_prediction <- as.Date(as.character(opt[['date_prediction']]),'%Y-%m-%d')
}

if(is.null(opt[['date_observed_start']])){
  date_observed_start <- date_observed_start_tmp 
  #warning(paste0('You did not provide date observed start. ',date_observed_start,' will be used'))
}else{
  date_observed_start <- as.Date(as.character(opt[['date_observed_start']]),'%Y-%m-%d')
}

if(is.null(opt[['path_to_lsh_data']])){
  path_to_data <- path_to_lsh_data_tmp
}else{
  path_to_data <- opt[['path_to_lsh_data']]
}

if(is.null(opt[['run_id']])){
  run_id=run_id_tmp
  #warning(paste0('You did not provide a run_id. ',run_id_tmp,' will be used'))
}else{
  run_id <- opt[['run_id']]
}

if (length(opt)>1){
  write_tables <- TRUE
}else{
  write_tables <- write_tables_tmp
}

#we assume we only know the state of patient at midnight before date_data
date_last_known_state <- date_data-1

##### ----- Read data ----- #####
# Paths
file_path_bb_data <- paste0(path_to_data,'df_bb_2020-05-05.csv')
file_path_lsh_data <- paste0(path_to_data,'df_lsh_2020-05-05.csv')
file_path_pcr_data <- paste0(path_to_data,'df_pcr_2020-05-09.csv')
file_path_phone_data <- paste0(path_to_data,'df_phone_2020-05-13.csv')
file_path_covid_id <- paste0(path_to_data,'2020-06-12_covid_id_conversion.xlsx')
path_outpatient_clinic <- '../outpatient_clinic_history/'
path_to_lsh_data <- '../lsh_data/'
file_path_coding <- 'lsh_coding.xlsx'
file_path_experiment_template <- 'experiment_template.xlsx'
file_path_priors <- 'priors.xlsx'

#Initialize progress bar
pb <- txtProgressBar(title="Example progress bar", label="0% done", min=0, max=100, initial=0, style = 3)
setTxtProgressBar(pb, 2)

# Raw data
data_bb_raw  <- suppressMessages(read_csv(file_path_bb_data))
data_lsh_raw  <- suppressMessages(read_csv(file_path_lsh_data))
data_pcr_raw  <- suppressMessages(read_csv(file_path_pcr_data))
data_phone_raw  <- suppressMessages(read_csv(file_path_phone_data, col_types = cols(freetext_immunosuppression='c')))

setTxtProgressBar(pb, 4)

# Gögnin að ofan innihalda ekki upplýsingar um einangrun og því notum við þessi gögn:
file_name_lsh_data <- paste0('2020-05-07','_lsh_covid_data.xlsx')
file_path_data <- paste0(path_to_lsh_data,file_name_lsh_data)
hospital_isolations_raw <- read_excel(file_path_data,sheet ='Einangrun-soguleg gogn', skip=3)

data_covid_id  <- read_excel(file_path_covid_id, skip = 3)
covid_id <- rename(data_covid_id , kt = `Kennitala/gervikennitala`, patient_id = `Person Key`)
experiment_description <- read_excel(file_path_experiment_template,sheet='experiment_description')
experiment_specification <- read_excel(file_path_experiment_template,sheet='experiment_specification')
run_description <- read_excel(file_path_experiment_template,sheet='run_description')
run_specification <- read_excel(file_path_experiment_template,sheet='run_specification')
heuristics_description <- read_excel(file_path_experiment_template,sheet='heuristics_description')
prior_transitions <- read_excel(file_path_priors,sheet = 'transitions')

setTxtProgressBar(pb, 6)

# Categories töflur
priority_categories <- read_excel(file_path_coding,sheet = 'priority_categories')
age_groups <- read_excel(file_path_coding,sheet = 'age_groups')
unit_categories <- read_excel(file_path_coding,sheet = 'lsh_unit_categories') %>%
  mutate(unit_category=!!as.name(paste0('unit_category_',unit_category_type)),
         unit_category_order=!!as.name(paste0('unit_category_order_',unit_category_type)))
isolation_categories <- read_excel(file_path_coding,sheet = 'lsh_isolation_categories') %>%
  mutate(isolation_category=!!as.name(paste0('isolation_category_',isolation_category_type)))
clinical_assessment_categories <- read_excel(file_path_coding,sheet = 'clinical_assessment_categories') %>%
  mutate(clinical_assessment_category=!!as.name(paste0('clinical_assessment_category_',clinical_assessment_category_type)),
         clinical_assessment_category_order=!!as.name(paste0('clinical_assessment_category_order_',clinical_assessment_category_type)))
length_of_stay_categories <- read_excel(file_path_coding,sheet = 'length_of_stay_categories') 

setTxtProgressBar(pb, 8)

##### -----Cleaning----- #####
#Tengjum kennitölur og patient_id saman
data_bb_raw  <- left_join(data_bb_raw,covid_id, by ='kt') 
data_lsh_raw  <- left_join(data_lsh_raw,covid_id, by ='kt') 
data_pcr_raw  <- left_join(data_pcr_raw,covid_id, by ='kt') 
data_phone_raw  <- left_join(data_phone_raw,covid_id, by ='kt') 

#laga þrjár vitlausar dagsetningar
for(i in data_lsh_raw$patient_id) if(i %in% 428857) data_lsh_raw$date_discharge[data_lsh_raw$date_discharge == as.Date('2020-03-16',"%Y-%m-%d")] <- as.Date('2020-04-16',"%Y-%m-%d")
for(i in data_lsh_raw$patient_id) if(i %in% 405117) data_lsh_raw$date_admission[data_lsh_raw$date_admission == as.Date('2020-04-18',"%Y-%m-%d")] <- as.Date('2020-03-18',"%Y-%m-%d")
for(i in data_lsh_raw$patient_id) if(i %in% 410748) data_lsh_raw$date_admission[data_lsh_raw$date_admission == as.Date('2020-04-27',"%Y-%m-%d")] <- as.Date('2020-03-27',"%Y-%m-%d")

setTxtProgressBar(pb, 10)

data_bb <- select(data_bb_raw,-kt) 
data_bb <- data_bb[c(ncol(data_bb),1:(ncol(data_bb)-1))]
data_lsh <- select(data_lsh_raw,-kt)
data_lsh <- data_lsh[c(ncol(data_lsh),1:(ncol(data_lsh)-1))]
data_pcr <- select(data_pcr_raw,-kt)
data_pcr <- data_pcr[c(ncol(data_pcr),1:(ncol(data_pcr)-1))]
data_phone <- select(data_phone_raw,-kt)
data_phone <- data_phone[c(ncol(data_phone),1:(ncol(data_phone)-1))]

data_phone <- mutate(data_phone,state=ifelse(green==1,'green',
                                             ifelse(yellow==1,'yellow',
                                                    ifelse(red==1,'red',
                                                           ifelse(blue==1,'blue',NA)))))

# Bæta við töflunni yfir einangranir þar sem þau gögn eru ekki í nýju gögnunum
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
  select(patient_id,isolation_category,unit_in,date_time_in,date_in,date_time_out,date_out)

setTxtProgressBar(pb, 12)

##### -----Processing----- #####

individs_extended <-select(data_pcr, patient_id, sex, age, test_nr, date, result) %>%
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

setTxtProgressBar(pb, 14)

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

setTxtProgressBar(pb, 16)

#####--- Hospital_visits_filtered búið til ---#####
tmp1 <- data_lsh %>% 
  select(patient_id, date_mort, mortality, date_admission, date_discharge, date_start_icu, date_end_icu, icu_given, admitted_from) %>%
  distinct() %>%
  filter_at(vars(-patient_id), any_vars(!is.na(.))) %>%
  mutate(icu_given=if_else(is.na(icu_given), "Nei", icu_given)) %>%
  mutate(mortality=if_else(is.na(mortality), "Nei", mortality)) %>%
  mutate(date_discharge=if_else(!is.na(date_mort), date_mort, date_discharge))

tmp2 <- tibble("patient_id"=rep(NA, nrow(tmp1)*3), "unit_in"=rep(NA, nrow(tmp1)*3), "date_in"=rep(as.Date("01-01-20", "%Y-%m-%d"), nrow(tmp1)*3), "date_out"=rep(as.Date("01-01-20", "%Y-%m-%d"), nrow(tmp1)*3), "text_out"=rep(NA, nrow(tmp1)*3), "date_admission"=rep(as.Date(NA, "%Y-%m-%d")))

i <- 1 # fyrir tmp1
k <- 1 # fyrir tmp2

setTxtProgressBar(pb, 18)

while(i <= nrow(tmp1)){
  tmp2$patient_id[k] <- tmp1$patient_id[i]
  tmp2$date_admission[k] <- tmp1$date_admission[i] #einn með tvær heimsóknir, þurfum, þess vegna að hafa þetta með í bili
  if(tmp1$icu_given[i]=="Já"){ # Fer aðilinn í ICU?
    if(tmp1$date_start_icu[i]>tmp1$date_admission[i]){ # Fer aðilinn fyrst á legudeild?
      tmp2$unit_in[k] <- "inpatient_ward"
      tmp2$date_in[k] <- tmp1$date_admission[i]
      tmp2$date_out[k] <- tmp1$date_start_icu[i]
      tmp2$text_out[k] <- "at_hospital"
      k <- k+1
      tmp2$unit_in[k] <- "intensive_care_unit"
      tmp2$date_in[k] <- tmp1$date_start_icu[i]
      tmp2$date_out[k] <- tmp1$date_end_icu[i]
      if(tmp1$mortality[i]!="Já"){ # Ef aðilinn deyr ekki fer hann aftur á legudeild
        tmp2$text_out[k] <- "at_hospital"
        k <- k+1
        tmp2$unit_in[k] <- "inpatient_ward"
        tmp2$date_in[k] <- tmp1$date_end_icu[i]
        tmp2$date_out[k] <- tmp1$date_discharge[i]
      }
      else if(tmp1$date_end_icu[i]<tmp1$date_discharge[i] | is.na(tmp1$date_end_icu[i])){ # Athugum dagsetningar hjá þeim sem deyja, fara þeir aftur á legudeild?
        tmp2$text_out[k] <- "at_hospital"
        k <- k+1
        tmp2$unit_in[k] <- "inpatient_ward"
        tmp2$date_in[k] <- tmp1$date_end_icu[i]
        tmp2$date_out[k] <- tmp1$date_discharge[i]
        tmp2$text_out[k] <- "death"
      }
      else{
        tmp2$text_out[k] <- "death"
      }
    }
    else{ 
      tmp2$unit_in[k] <- "intensive_care_unit"
      tmp2$date_in[k] <- tmp1$date_start_icu[i]
      tmp2$date_out[k] <- tmp1$date_end_icu[i]
      if(tmp1$mortality[i]!="Já"){ # Ef aðilinn deyr ekki fer hann næst á legudeild
        tmp2$text_out[k] <- "at_hospital"
        k <- k+1
        tmp2$unit_in[k] <- "inpatient_ward"
        tmp2$date_in[k] <- tmp1$date_end_icu[i]
        tmp2$date_out[k] <- tmp1$date_discharge[i]
        tmp2$text_out[k] <- "home"
      }
      else if(tmp1$date_end_icu[i]<tmp1$date_discharge[i] | is.na(tmp1$date_end_icu[i])){ # Athugum dagsetningar hjá þeim sem deyja
        tmp2$text_out[k] <- "at_hospital"
        k <- k+1
        tmp2$unit_in[k] <- "inpatient_ward"
        tmp2$date_in[k] <- tmp1$date_end_icu[i]
        tmp2$date_out[k] <- tmp1$date_discharge[i]
        tmp2$text_out[k] <- "death"
      }
      else{
        tmp2$text_out[k] <- "death"
      }
    }
  }
  else{
    tmp2$unit_in[k] <- "inpatient_ward"
    tmp2$date_in[k] <- tmp1$date_admission[i]
    tmp2$date_out[k] <- tmp1$date_discharge[i]
    if(tmp1$mortality[i]=="Já"){
      tmp2$text_out[k] <- "death"
    }
    else{
      tmp2$text_out[k] <- "home"
    }
  }
  
  if(i != nrow(tmp1)){
    k <- k+1
  }
  i=i+1
}

setTxtProgressBar(pb, 20)

tmp2 <- tmp2 %>% fill(patient_id, date_admission) %>%
  filter(., row_number()<=k) %>%
  filter(., !(patient_id==1244828 & is.na(date_in))) %>% 
  mutate(text_out=if_else(patient_id==1244828 & is.na(date_out), "death", text_out))

hospital_visits_filtered <- tmp2 %>%
  left_join(., select(tmp1, patient_id, admitted_from, mortality, date_discharge, date_admission, date_mort), by=c("patient_id", "date_admission")) %>%
  mutate(., in_hospital_before=admitted_from %in% c("Annarri deild"), text_out=ifelse(!is.finite(date_discharge), "at_hospital", text_out)) %>%
  mutate(., outcome=if_else(mortality=="Já", "death", if_else(text_out=="at_hospital", "in_hospital_system", "recovered"))) %>%
  select(-admitted_from, -mortality) %>%
  group_by(patient_id) %>%
  arrange(date_in) %>%
  mutate(state_block_nr=get_hospital_state_block_numbers(unit_in,date_in,date_in,date_out,date_out)) %>%
  group_by(patient_id, unit_in, outcome, in_hospital_before, state_block_nr, date_mort) %>%
  arrange(date_in) %>%
  summarize(date_in=head(date_in,1), date_out=tail(date_out,1), text_out=tail(text_out,1)) %>%
  ungroup() %>%
  # isolations gögnin innihalda stundum betri dagsetningar
  full_join(.,hospital_isolations_filtered,by=c('patient_id','state_block_nr','unit_in'),suffix=c('','_isolation')) %>%
  mutate(date_in=if_else((abs(difftime(date_in, date_in_isolation, "days"))>1 & !is.na(date_in_isolation)) | is.na(date_in), date_in_isolation, date_in),
         date_out=if_else((abs(difftime(date_out, date_out_isolation, "days"))>1 & !is.na(date_out_isolation)) | is.na(date_out), date_out_isolation, date_out)) %>%
  select(-date_time_in, -date_time_out, -date_in_isolation, -date_out_isolation, -isolation_category)

rm(tmp1)
rm(tmp2)

setTxtProgressBar(pb, 22)

# skoða reglu með state_first=home
individs_extended <- left_join(individs_extended, select(hospital_visits_filtered, patient_id, outcome, unit_in, date_in, in_hospital_before, date_mort), by="patient_id") %>%
  left_join(., select(unit_categories, unit_category, unit_category_order), by=c("unit_in" = "unit_category")) %>%
  mutate(state_first=if_else(!in_hospital_before | is.na(in_hospital_before), "home", "inpatient_ward"), date_outcome=date_mort) %>% # Vitum bara dags hjá þeim sem deyja eins og er
  group_by(patient_id) %>%
  mutate(state_worst_order=max(unit_category_order)) %>%
  left_join(., select(unit_categories, unit_category, unit_category_order), by=c("state_worst_order"="unit_category_order")) %>%
  select(., -state_worst_order, -unit_in, -date_in, -in_hospital_before, -unit_category_order, -date_mort) %>%
  distinct() %>%
  group_by(patient_id) %>%
  slice(1) %>% #ein manneskja með tvær færslur (tvær heimsóknir á lsh)
  distinct() %>%
  rename(.,state_worst=unit_category) %>%
  left_join(., select(data_lsh , patient_id, risk), by="patient_id") %>%
  rename(priority=`risk`) %>%
  mutate(priority=gsub('\\s.*','', priority)) %>%
  left_join(.,select(priority_categories,priority_raw,priority_all,priority_all_order),by=c('priority'='priority_raw')) %>%
  mutate(.,priority=priority_all, priority_order=priority_all_order) %>%
  select(-priority_all,-priority_all_order, -priority_order) %>%
  ungroup() %>%
  distinct() %>%
  group_by(patient_id) %>%
  mutate(nrows=n()) %>%
  filter(!(nrows>1 & is.na(priority))) %>% #Einn með tvö priority
  select(-nrows)

setTxtProgressBar(pb, 24)

#hafa recovered í outcome úr data_phone 
recovered_phone <- select(data_phone,patient_id,call_nr,date_discharge) %>%
  group_by(.,patient_id) %>%
  filter(call_nr == min(call_nr)) %>%
  filter(!is.na(patient_id)) %>%
  mutate(.,outcome_phone=if_else(date_discharge>=date_data | is.na(date_discharge),"in_hospital_sysyem","recovered")) %>%
  ungroup()

individs_extended <- left_join(individs_extended,select(recovered_phone,patient_id,outcome_phone),by='patient_id') %>%
  mutate(.,outcome_lagad=if_else(is.na(outcome),outcome_phone,outcome)) %>%
  select(.,patient_id, age, sex, date_diagnosis, state_first, outcome_lagad, state_worst, date_outcome, priority) %>%
  rename(outcome = outcome_lagad)

date_discharge_phone <- data_phone  %>%
  group_by(.,patient_id) %>%
  filter(call_nr==min(call_nr)) %>%
  mutate(.,date_discharge=as.Date(date_discharge,"%Y-%m-%d")) %>%
  filter(!is.na(patient_id)) %>% #tokum bara ut thar sem vantar patient_id, i lagi?
  ungroup() 

setTxtProgressBar(pb, 26)

individs_extended <- left_join(individs_extended,select(date_discharge_phone,patient_id,date_discharge), by= 'patient_id') #%>% 

date_symptoms_phone <- data_phone  %>%
  group_by(.,patient_id) %>%
  filter(call_nr==min(call_nr)) %>%
  mutate(.,date_symptoms=as.Date(date_symptoms,"%Y-%m-%d")) %>%
  filter(!is.na(patient_id)) %>% # aftur..
  ungroup()

individs_extended <- left_join(individs_extended,select(date_symptoms_phone,patient_id,date_symptoms), by= 'patient_id') #%>%

setTxtProgressBar(pb, 28)

#Undirliggjandi sjukdomar ur data_phone 
comorbidities_table <- select(data_phone ,patient_id,call_nr,date,dm_i,dm_ii,cardiovascular_disease,hypertension,pulmonary_disease,chronic_kidney_disease,current_cancer) 
comorbidities_phone <- comorbidities_table %>%
  mutate(.,n_comorbidity=rowSums(comorbidities_table[,-c(1,2,3)],na.rm=FALSE)) %>%
  group_by(.,patient_id,n_comorbidity) %>%
  filter(call_nr==min(call_nr)) %>%
  filter(!is.na(patient_id)) %>%
  ungroup()

individs_extended <- left_join(individs_extended,select(comorbidities_phone,patient_id,n_comorbidity), by= 'patient_id') %>%
  unique()

individs_extended <- mutate(individs_extended, date_outcome=if_else(is.na(date_outcome), date_discharge, date_outcome)) %>%
  select(-date_discharge) %>%
  mutate(zip_code=109) %>%
  mutate(intensive_care_unit_restriction='not_icu_restricted') %>% #höfum ekki upplýsingar um icu restriciton
  mutate(date_symptoms=if_else(is.na(date_symptoms), date_diagnosis, date_symptoms)) %>% #symptoms model
  rename(date_first_symptoms=date_symptoms)
  
setTxtProgressBar(pb, 30)

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

setTxtProgressBar(pb, 31)

#for symptoms model
dates_home_symptoms <- lapply(1:nrow(individs_extended),function(i){  
  #date_home_latest_imputed <- if_else(individs_extended$date_diagnosis[i]==date_data,date_data, date_last_known_state)
  return(tibble(patient_id=individs_extended$patient_id[i],state='home',date=seq(individs_extended$date_first_symptoms[i],date_last_known_state,by=1))) 
}) %>% 
  bind_rows() %>%
  left_join(.,select(individs_extended,patient_id,date_outcome),by='patient_id') %>%
  group_by(patient_id) %>%
  filter(!is.finite(date_outcome) | date<=date_outcome) %>%
  select(-date_outcome) %>%
  ungroup()

setTxtProgressBar(pb, 32)

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

setTxtProgressBar(pb, 34)

interview_extra <- select(individs_extended,patient_id) %>%
  mutate(priority='medium', date_clinical_assessment=date_data,clinical_assessment='green')
interview_follow_up <- select(individs_extended,patient_id) %>%
  mutate(date_clinical_assessment=date_data,clinical_assessment='green') 
interview_first <- select(individs_extended,patient_id) %>%
  mutate(date_diagnosis_interview=date_data,clinical_assessment='green')
NEWS_score <- select(individs_extended,patient_id) %>%
  mutate(date=date_data,NEWS_score='green')
ventilator_times <- select(individs_extended,patient_id) %>%
  mutate(date=date_data,ventilator='green',unit_in='intensive_care_unit')

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

setTxtProgressBar(pb, 36)

patient_transitions_symptoms <- right_join(dates_hospital,dates_home_symptoms,by=c('patient_id','date'),suffix=c('_hospital','_home')) %>%
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

setTxtProgressBar(pb, 38)

state_worst_case_per_date <- filter(unit_categories,!duplicated(unit_category)) %>% inner_join(patient_transitions,.,by=c('state'='unit_category')) %>%
  inner_join(.,filter(clinical_assessment_categories,!duplicated(clinical_assessment_category)),by=c('severity'='clinical_assessment_category')) %>%
  group_by(.,patient_id) %>%
  arrange(.,date) %>%
  mutate(state_worst=get_state_worst(paste0(state,'-',severity),paste0(unit_category_order,clinical_assessment_category_order))) %>%
  separate(.,state_worst,into=c('state_worst','state_worst_severity'),sep='-') %>%
  ungroup() %>%
  select(patient_id,date,state_worst,state_worst_severity)

state_worst_case <- group_by(state_worst_case_per_date,patient_id) %>% arrange(date) %>% slice(n()) %>% select(-date)

state_worst_case_per_date_symptoms <- filter(unit_categories,!duplicated(unit_category)) %>% inner_join(patient_transitions_symptoms,.,by=c('state'='unit_category')) %>%
  inner_join(.,filter(clinical_assessment_categories,!duplicated(clinical_assessment_category)),by=c('severity'='clinical_assessment_category')) %>%
  group_by(.,patient_id) %>%
  arrange(.,date) %>%
  mutate(state_worst=get_state_worst(paste0(state,'-',severity),paste0(unit_category_order,clinical_assessment_category_order))) %>%
  separate(.,state_worst,into=c('state_worst','state_worst_severity'),sep='-') %>%
  ungroup() %>%
  select(patient_id,date,state_worst,state_worst_severity)

individs_extended <- left_join(select(individs_extended, -state_worst), state_worst_case,by='patient_id', suffix) %>% filter(!is.na(patient_id))
rm(state_worst_case)

setTxtProgressBar(pb, 39)

individs_splitting_variables <- select(individs_extended,patient_id,age,sex,priority,state_first,intensive_care_unit_restriction) %>%
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
  mutate(age_simple_sex_order=if_else(age_simple=='age_0-50',1,if_else(sex=='Kona',2,3)))

setTxtProgressBar(pb, 40)

patient_transitions_state_blocks <- group_by(patient_transitions,patient_id) %>%
  mutate(state_block_nr=get_state_block_numbers(state),
         state_with_severity_block_nr=get_state_block_numbers(paste0(state,severity))) %>%
  group_by(.,patient_id,state_block_nr,state_with_severity_block_nr,state,severity) %>% arrange(.,date) %>% 
  summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)],severity_next=severity_tomorrow[which.max(date)]) %>%
  mutate(censored=(is.na(state_next))) %>%
  mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
  ungroup()

#Fyrir sankey í dashboard
patient_transitions_state_blocks2 <- group_by(patient_transitions,patient_id) %>%
  mutate(state_block_nr=get_state_block_numbers(state)) %>%
  group_by(.,patient_id,state_block_nr,state) %>% arrange(.,date) %>% 
  summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)]) %>%
  mutate(censored=(is.na(state_next))) %>%
  mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
  ungroup() %>%
  select(patient_id, state, date_start = 'state_block_nr_start', date_end = 'state_block_nr_end', state_next)

patient_transitions_state_blocks_symptoms <- group_by(patient_transitions_symptoms,patient_id) %>%
  mutate(state_block_nr=get_state_block_numbers(state),
         state_with_severity_block_nr=get_state_block_numbers(paste0(state,severity))) %>%
  group_by(.,patient_id,state_block_nr,state_with_severity_block_nr,state,severity) %>% arrange(.,date) %>% 
  summarize(state_block_nr_start=min(date),state_block_nr_end=max(date),state_next=state_tomorrow[which.max(date)],severity_next=severity_tomorrow[which.max(date)]) %>%
  mutate(censored=(is.na(state_next))) %>%
  mutate(state_duration=as.numeric(state_block_nr_end-state_block_nr_start)+1) %>%
  ungroup()

historical_expanded <- expand_grid(date=seq(min(patient_transitions$date),max(patient_transitions$date),by=1),
                                   state=get_states_in_order('base',active=TRUE))

historical_expanded_symptoms <- expand_grid(date=seq(min(patient_transitions_symptoms$date),max(patient_transitions_symptoms$date),by=1),
                                   state=get_states_in_order('base',active=TRUE))

historical_data <- group_by(patient_transitions,date,state) %>% 
  summarise(count=n()) %>%
  right_join(.,historical_expanded,by=c('date','state')) %>%
  mutate(count=if_else(is.na(count),0,as.numeric(count)))

setTxtProgressBar(pb, 42)

historical_data_symptoms <- group_by(patient_transitions_symptoms,date,state) %>% 
  summarise(count=n()) %>%
  right_join(.,historical_expanded,by=c('date','state')) %>%
  mutate(count=if_else(is.na(count),0,as.numeric(count)))

historical_turnover <- get_historical_turnover()
historical_state_sequences_base <- get_state_sequences(model='base',seq_type = 'finished')
historical_state_sequences_extended <- get_state_sequences(model='extended',seq_type = 'finished')
historical_state_sequences_symptoms <- get_state_sequences(model='symptoms',seq_type = 'finished')

historical_turnover_symptoms <- get_historical_turnover_symptoms()
################# ----- Predicted number of infections ------ ##############################
infections_predicted_per_date <- get_infections_predicted_per_date(source='hi',date_prediction)

################# ----- proportion of patients going to outpatient clinic ------ #################

setTxtProgressBar(pb, 44)

## ath eitthvad bogid
get_prop_outpatient_clinic <- function(current_state_per_date_summary,window_size=7){
  nr_at_home_per_day <- filter(current_state_per_date_summary,state=='home') %>% rename(nr_at_home=count)
  outpatient_clinic_visits_per_day <- data_bb %>%
    select(patient_id,date_bb) %>%
    arrange(patient_id,date_bb) %>%
    group_by(.,date_bb) %>%
    summarize(nr_visits=n()) %>% ungroup() %>%
    left_join(.,nr_at_home_per_day,by=c('date_bb'='date')) %>%
    mutate(prop_visits=nr_visits/nr_at_home)
  
  date_for_calculation <- date_data-window_size
  prop_outpatient_clinic_per_window <- filter(outpatient_clinic_visits_per_day, date_bb >= date_for_calculation) %>%
    summarize(prop_visits_per_window=sum(prop_visits)/window_size)
  return(prop_outpatient_clinic_per_window)
}

setTxtProgressBar(pb, 46)

prop_outpatient_clinic <- get_prop_outpatient_clinic(historical_data)
prop_outpatient_clinic_symptoms <- get_prop_outpatient_clinic(historical_data_symptoms)

################# ----- writing tables ------ #################
run_info <- get_run_info(run_id)

if(write_tables){
  write.table(historical_data, file = paste0(path_tables,date_data,'_historical_data.csv'), quote = F,row.names=F,sep=',')
  setTxtProgressBar(pb, 47)
  write.table(historical_turnover, file = paste0(path_tables,date_data,'_historical_turnover.csv'), quote = F,row.names=F,sep=',')
  setTxtProgressBar(pb, 48)
  write.table(prop_outpatient_clinic, file=paste0(path_outpatient_clinic,date_data,'_prop_outpatient_clinic.csv'), quote = F,row.names=F,sep=',')
  setTxtProgressBar(pb, 49)
  write.table(infections_predicted_per_date, file = paste0(path_tables,date_data,'_infections_predicted.csv'), quote = F,row.names=F,sep=',')
  setTxtProgressBar(pb, 50)
}

if(write_tables){
  write.table(run_info, file = paste0(path_tables,date_data,'_',run_id,'_run_info.csv'), quote = F,row.names=F,sep='\t',col.names=F)
  setTxtProgressBar(pb, 52)
}

dates_observed <- seq(date_observed_start,date_last_known_state,by=1) 
pb_i <- 0

for(id in run_info$experiment_id){
  experiment_table_list <- get_tables_for_experiment(id,dates_observed)
  pb_i <- pb_i+4
  setTxtProgressBar(pb, 52 + pb_i)
  if(write_tables){
    write.table(experiment_table_list$current_state_per_date,file=paste0(path_sensitive_tables,date_data,'_',id,'_current_state_per_date.csv'),sep=',',row.names=FALSE,quote=FALSE)
    pb_i <- pb_i+1
    setTxtProgressBar(pb, 52 + pb_i)
    write.table(experiment_table_list$current_state_per_date_filtered,file=paste0(path_sensitive_tables,date_data,'_',id,'_current_state_per_date_filtered.csv'),sep=',',row.names=FALSE,quote=FALSE)
    pb_i <- pb_i+1
    setTxtProgressBar(pb, 52 + pb_i)
    write.table(experiment_table_list$first_state,file=paste0(path_tables,date_data,'_',id,'_first_state.csv'),sep=',',row.names=F,quote=F)
    pb_i <- pb_i+1
    setTxtProgressBar(pb, 52 + pb_i)
    write.table(experiment_table_list$transition_summary,file=paste0(path_tables,date_data,'_',id,'_transition_summary.csv'),sep=',',row.names=FALSE,quote=FALSE)
    pb_i <- pb_i+4
    setTxtProgressBar(pb, 52 + pb_i)
    write.table(experiment_table_list$length_of_stay,file=paste0(path_tables,date_data,'_',id,'_length_of_stay.csv'),sep=',',row.names=F,quote=F)
    pb_i <- pb_i+1
    setTxtProgressBar(pb, 52 + pb_i)
  }
}

setTxtProgressBar(pb, 100)
close(pb)
cat("Data processing complete\n")

