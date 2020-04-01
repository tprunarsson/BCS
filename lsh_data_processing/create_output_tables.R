source('covid19_lsh_data_preprocessing.R')

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

write.table(patient_transition_counts_matrix_all,file=paste0(path_to_output,current_date,'_transition_matrix','.csv'),sep=',',row.names=FALSE,col.names=states,quote=FALSE)

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

write.table(patient_transition_counts_matrix_age_simple_under_50,file=paste0(path_to_output,current_date,'_transition_matrix_under_50','.csv'),sep=',',row.names=F,col.names=states,quote=F)
write.table(patient_transition_counts_matrix_age_simple_over_50,file=paste0(path_to_output,current_date,'_transition_matrix_over_50','.csv'),sep=',',row.names=F,col.names=states,quote=F)


#current state

current_state <-  filter(patient_transitions_state_blocks,date==date_last_known_state-1) %>%
                    inner_join(.,select(patient_transitions_state_blocks_summary,patient_id,state_block_nr,state_block_nr_start),by=c('patient_id','state_block_nr')) %>%
                    filter(!(state_tomorrow %in% c('recovered','death'))) %>%
                    mutate(days_in_state=if_else(state==state_tomorrow,as.numeric(current_date-state_block_nr_start),1)) %>%
                    inner_join(individs_extended,.,by='patient_id') %>%
                    mutate(days_from_diagnosis=as.numeric(current_date-date_diagnosis)) %>%
                    select(patient_id,age,sex,state,days_in_state,days_from_diagnosis,state_worst)

current_state_newly_diagnosed <- anti_join(individs_extended,select(current_state,patient_id),by='patient_id') %>%
    filter(.,outcome=='in_hospital_system') %>%
    mutate(.,state='home') %>%
    left_join(.,select(hospital_visits_filtered,patient_id,unit_in,date_time_in),by='patient_id') %>%
    mutate(state=if_else(is.na(unit_in),state,unit_in)) %>%
    group_by(.,patient_id) %>% arrange(date_time_in) %>%
    summarize(.,state=tail(state,1)) %>%
    ungroup() %>%
    inner_join(individs_extended,.,by='patient_id') %>%
    mutate(.,days_from_diagnosis=as.numeric(current_date-date_diagnosis)) %>%
    mutate(.,days_in_state=days_from_diagnosis) %>%
    select(.,patient_id,age,sex,state,days_in_state,days_from_diagnosis,state_worst)

current_state <- bind_rows(current_state,current_state_newly_diagnosed)


write.table(current_state,file=paste0(path_to_output,current_date,'_current_state','.csv'),sep=',',row.names=F,quote=F)

#length of stay by age 

state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                           select(patient_transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                           by='patient_id')

state_blocks_with_age_imputed <- filter(state_blocks_with_age,censored==T)

length_of_stay_by_age_simple <- group_by(state_blocks_with_age_imputed,state,age_group_simple,state_duration) %>%
                                summarise(count=n()) %>%
                                arrange(state,age_group_simple)

write.table(length_of_stay_by_age_simple,file=paste0(path_to_output,current_date,'_length_of_stay','.csv'),sep=',',row.names=F,quote=F)

#first state of all patients that have been diagnosed

first_state <- group_by(hospital_visits_filtered,patient_id) %>%
    summarize(initial_state_hospital=unit_in[which.min(date_time_in)],min_date_in=min(date_in,na.rm=TRUE)) %>%
    right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
    mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
    select(age,sex,initial_state)


write.table(first_state,file=paste0(path_to_output,current_date,'_first_state','.csv'),sep=',',row.names=F,quote=F)

#get predictions from covid hi model

hi_predictions <- filter(hi_predictions_raw,name=='cases',type=='new',age=='total') %>%
    select(date,median,upper)


#write
write.table(hi_predictions,file=paste0(path_to_output,prediction_date,'_hi_predictions','.csv'),sep=',',row.names=F,quote=F)

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
