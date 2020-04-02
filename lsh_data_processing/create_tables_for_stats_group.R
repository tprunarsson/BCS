source('covid19_lsh_data_preprocessing.R')
path_tables='../output_stats_group/'
#Create table of new hospital cases,new icu,out of hospital and out of icu per day
finished_states <- inner_join(select(individs_extended,patient_id,age_group_std),
                              select(patient_transitions_state_blocks_summary,-censored,-state_duration),
                              by='patient_id')%>%
    filter(state!=state_next) %>%
    mutate(date=state_block_nr_end+1) %>% 
    select(patient_id,date,age_group_std,state,state_next)

first_states_hospitals <- inner_join(select(individs_extended,patient_id,age_group_std),
                                     select(patient_transitions_state_blocks_summary,-censored,-state_duration),
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

write.table(states_by_date_and_age_std,paste0('../output/events_per_date_and_age_',current_date,'.csv'),sep=',')

#hospital and icu distributions for Brynjólfur
group_by(hospital_visits_filtered,patient_id) %>% summarise(icu=any(grepl('intensive_care_unit',unit_in))) %>%
    ungroup() %>% left_join(select(individs_extended,patient_id,age_group_std),.,by='patient_id') %>%
    group_by(age_group_std) %>% summarise(fj_smitadra=n(),fj_spitala=sum(!is.na(icu)),fj_icu=sum(icu,na.rm=T)) %>% 
    ungroup() %>% arrange(age_group_std) %>% write.table(paste0('../output/hospital_and_icu_distr_',current_date,'.csv'),row.names=F,quote=F,sep=',')

