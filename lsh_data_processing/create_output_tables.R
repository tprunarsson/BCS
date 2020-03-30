path_to_root <- '~/projects/covid/BCS/'
source(paste0(path_to_root,'/lsh_data_processing/covid19_lsh_data_preprocessing.R'))
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

write.table(patient_transition_counts_matrix_all,file=paste0(path_to_output,'transition_matrix_',current_date,'.csv'),sep=',',row.names=FALSE,col.names=states,quote=FALSE)

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

write.table(patient_transition_counts_matrix_age_simple_under_50,file=paste0(path_to_output,'transition_matrix_under_50_',current_date,'.csv'),sep=',',row.names=F,col.names=states,quote=F)
write.table(patient_transition_counts_matrix_age_simple_over_50,file=paste0(path_to_output,'transition_matrix_over_50_',current_date,'.csv'),sep=',',row.names=F,col.names=states,quote=F)


#current state

current_state <-  filter(patient_transitions_state_blocks,date==current_date) %>%
    inner_join(.,select(patient_transitions_state_blocks_summary,patient_id,state_block_nr,state_block_nr_start),by=c('patient_id','state_block_nr')) %>%
    mutate(days_in_state=as.numeric(current_date-state_block_nr_start)) %>%
    inner_join(individs_extended,.,by='patient_id') %>%
    mutate(days_from_diagnosis=as.numeric(current_date-date_diagnosis)) %>%
    select(patient_id,age,sex,state,days_in_state,days_from_diagnosis,state_worst)


write.table(current_state,file=paste0(path_to_output,'current_state_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#length of stay by age 

state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                           select(patient_transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                           by='patient_id')

state_blocks_with_age_imputed <- tomas(state_blocks_with_age)

length_of_stay_by_age_simple <- group_by(state_blocks_with_age_imputed,state,age_group_simple,state_duration) %>%
                                summarise(count=n()) %>%
                                arrange(state,age_group_simple)

write.table(length_of_stay_by_age_simple,file=paste0(path_to_output,'length_of_stay_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#first state of all patients that have been diagnosed

first_state <- group_by(hospital_visits_filtered,patient_id) %>%
    summarize(initial_state_hospital=unit_in[which.min(date_time_in)],min_date_in=min(date_in,na.rm=TRUE)) %>%
    right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
    mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
    select(age,sex,initial_state)


write.table(first_state,file=paste0(path_to_output,'first_state_',current_date,'.csv'),sep=',',row.names=F,quote=F)

#get predictions from covid hi model

hi_predictions <- filter(hi_predictions_raw,name=='cases',type=='new',age=='total') %>%
    select(date,median,upper)


#write
write.table(hi_predictions,file=paste0(path_to_output,'hi_predictions_','2020-03-27','.csv'),sep=',',row.names=F,quote=F)

#get length of stay distribution for those finishing home state
length_of_stay_updated <- inner_join(select(patient_transitions_state_blocks_summary,patient_id,state,state_block_nr,state_duration),
                                     select(individs_extended,patient_id,age_group_simple,outcome),by='patient_id') %>%
    left_join(.,patients_finished_home,by=c('patient_id','state')) %>%
    filter(!(state=='home'&!is.finite(date))) %>%
    select(-date,-state_tomorrow) %>%
    group_by(state,age_group_simple,state_duration) %>%
    summarise(count=n()) %>%
    arrange(state,age_group_simple) %>%
    ungroup() %>%
    filter(!(state=='home' & state_duration==0))