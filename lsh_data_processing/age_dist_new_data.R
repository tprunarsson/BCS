invisible(Sys.setlocale("LC_ALL","IS_is"))
suppressPackageStartupMessages({
  library(optparse)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
})

age_dist_new_data <- function(){
date_data <- ymd("2020-08-17")
wave_2_start <- ymd("2020-07-23")
path_to_lsh_data <- '~/projects/covid/BCS/lsh_data/'
file_name_lsh_data <- paste0(date_data,'_lsh_covid_data.xlsx')
file_path_data <- paste0(path_to_lsh_data,file_name_lsh_data)
file_path_coding <- 'lsh_coding.xlsx'

age_groups <- read_excel(file_path_coding,sheet = 'age_groups')

individs_raw <- read_excel(file_path_data, sheet = 'Einstaklingar', skip=3)
covid_diagnosis_raw <- read_excel(file_path_data,sheet ='Dag. greiningar frá veirurannsó', skip=6)


individs <- rename(individs_raw,patient_id=`Person Key`,age=`Aldur heil ár`, sex=`Yfirfl. kyns`,zip_code=`Póstnúmer`,
                   covid_group_raw=`Heiti sjúklingahóps`) %>%
  filter_at(vars(-patient_id),any_vars(!is.na(.))) %>%
  filter(patient_id != 0) %>%
  group_by(.,patient_id) %>%
  summarise(zip_code=min(zip_code,na.rm=T),
            age=min(age,na.rm=T),
            sex=min(sex,na.rm=T)) %>%
  ungroup()

covid_diagnosis <- rename(covid_diagnosis_raw,patient_id=`Person Key`,date_time_diagnosis_pcr=`Dagsetning greiningar`) %>%
  separate(.,col='date_time_diagnosis_pcr',into=c('date_diagnosis_pcr','time_diagnosis_pcr'),sep=' ',remove=FALSE) %>%
  mutate(.,date_diagnosis_pcr=as.Date(date_diagnosis_pcr,"%Y-%m-%d")) %>%
  select(.,patient_id, date_diagnosis_pcr)

aldur_faraldur2_logistic <- left_join(individs, covid_diagnosis, by="patient_id") %>%
  filter(date_diagnosis_pcr>wave_2_start, zip_code!="999") %>%
  left_join(., select(age_groups, age, age_official), by="age") %>%
  select(., age_official) %>% 
  count(age_official) %>%
  mutate(prop=n/sum(n)) %>% 
  select(-n) %>% 
  rename(splitting_variable=age_official) %>%
  mutate(splitting_variable=paste0("age_", splitting_variable)) %>%
  #rbind(c("age_0-9", 0), .) %>%
  mutate(prop=as.numeric(prop))

return(aldur_faraldur2_logistic)
}
