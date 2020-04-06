

############################## ------ Create input for simulation ----- ##############################


############################## -----extract current state of patients in hospital system at any date
get_current_state_per_date <- function(type=''){
    if(type=='extended'){
        transitions=patient_transitions_extended
    }else{
        transitions <- patient_transitions
    }
    state_newly_diagnosed <- anti_join(individs_extended,select(transitions,patient_id),by='patient_id') %>%
        filter(.,outcome=='in_hospital_system') %>%
        mutate(.,state='home') %>%
        left_join(.,select(hospital_visits_filtered,patient_id,unit_in,date_time_in),by='patient_id') %>%
        mutate(state=if_else(is.na(unit_in),state,unit_in)) %>%
        group_by(.,patient_id) %>% arrange(date_time_in) %>%
        summarize(.,date=min(date_diagnosis,na.rm=T),state=tail(state,1)) %>%
        ungroup()
    
    state_date_last_known <- filter(transitions,date==(date_last_known_state-1)) %>%
        mutate(.,state=state_tomorrow,date=date+1) %>%
        filter(state!='recovered') %>%
        select(.,patient_id,date,state) %>%
        bind_rows(.,state_newly_diagnosed)
    
    current_state_per_date <- select(transitions,-state_tomorrow) %>%
        bind_rows(.,state_date_last_known) %>%
        group_by(.,date,state) %>%
        summarise(count=n())
    return(current_state_per_date)
}


############### ---- Transition matrices ---- ###################################
get_states_in_order <- function(type=''){
    states_active <- distinct(unit_categories,unit_category,unit_category_order) %>%
        arrange(unit_category_order) %>%
        select(unit_category) %>%
        unlist() %>%
        unname() 
    if(type=='extended'){
        clinical_assessment_active <- c('green','red')
        states_active <- sapply(states_active,function(x) paste0(x,'_',clinical_assessment_active)) %>% c()
    }
    
    states_end <- c('death','recovered')
    return(c(states_active,states_end))
}

get_transition_matrix_all <- function(type=''){
    if(type=='extended'){
        transitions=patient_transitions_extended
    }else{
        transitions <- patient_transitions
    }
    states <- get_states_in_order(type)
    #all age groups
    state_transitions_all <- expand.grid(states,states,stringsAsFactors = FALSE) %>%
        rename(state=Var1,state_tomorrow=Var2) %>%
        as_tibble()
    transition_counts_all <- group_by(transitions,state,state_tomorrow) %>% summarise(count=as.numeric(n())) %>% 
        right_join(.,state_transitions_all,by=c('state','state_tomorrow')) %>%
        mutate(count=if_else(is.na(count),0,count))
    transition_counts_matrix_all <- matrix(transition_counts_all$count,ncol=length(states),nrow=length(states))
    colnames(transition_counts_matrix_all) <- states
    return(transition_counts_matrix_all)
}
#simple age groups
get_transition_matrix_by_age <- function(type=''){
    if(type=='extended'){
        transitions=patient_transitions_extended
    }else{
        transitions <- patient_transitions
    }
    states <- get_states_in_order(type)
    age_group_simple=c('0-50','51+')
    state_transitions_age_simple <- expand.grid(age_group_simple,states,states,stringsAsFactors = FALSE) %>%
        rename(age_group_simple=Var1,state=Var2,state_tomorrow=Var3) %>%
        as_tibble()
    transition_counts_age_simple <- inner_join(select(individs_extended,patient_id,age_group_simple),transitions,by='patient_id') %>%
        group_by(.,age_group_simple,state,state_tomorrow) %>%
        summarize(count=as.numeric(n())) %>%
        group_by(age_group_simple) %>%
        right_join(.,state_transitions_age_simple,by=c('age_group_simple','state','state_tomorrow')) %>%
        mutate(count=if_else(is.na(count),0,count)) %>%
        ungroup()
    
    transition_counts_matrix_age_simple_under_50 <- filter(transition_counts_age_simple,age_group_simple=='0-50') %>% 
        select(count) %>% unlist() %>% 
        matrix(.,ncol=length(states),nrow=length(states))
    transition_counts_matrix_age_simple_over_50 <- filter(transition_counts_age_simple,age_group_simple=='51+') %>% 
        select(count) %>% 
        unlist() %>% 
        matrix(.,ncol=length(states),nrow=length(states))
    colnames(transition_counts_matrix_age_simple_under_50) <- states
    colnames(transition_counts_matrix_age_simple_over_50) <- states
    return(list('under_50'=transition_counts_matrix_age_simple_under_50,'over_50'=transition_counts_matrix_age_simple_over_50))
}
############### ---- Current state of patients in hospital system ---- ###################################
get_current_state <- function(type=''){
    if(type=='extended'){
        transitions=patient_transitions_extended
    }else{
        transitions <- patient_transitions
    }
    current_state <-  filter(transitions,date==date_last_known_state-1) %>%
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
}

############## ----- Length of stay distribution by state and age ----- ############## 
get_length_of_stay_by_age_simple <- function(type=''){
    if(type=='extended'){
        transitions_state_blocks_summary <- patient_transitions_state_blocks_extended
    }else{
        transitions_state_blocks_summary <- patient_transitions_state_blocks
    }
    state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                        select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                        by='patient_id')
    #theta_ward = fitlognormal(state_blocks_with_age, "inpatient_ward")
    theta_icu = fitlognormal(state_blocks_with_age, "intensive_care_unit")
    state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age, "home_green")
    state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age, "home_red")
    state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age_imputed, "inpatient_ward_green")
    state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age_imputed, "inpatient_ward_red")
    #state_blocks_with_age_imputed <- impute_lognormal(state_blocks_with_age_imputed, "intensive_care_unit", theta_icu)
    
    length_of_stay_by_age_simple <- group_by(state_blocks_with_age_imputed,state,age_group_simple,state_duration) %>%
        summarise(count=n()) %>%
        arrange(state,age_group_simple)
    return(length_of_stay_by_age_simple)
}


################# ----- First state of individuals diagnosed with COVID-19 ---- #############
get_first_state <- function(type=''){
    first_state <- group_by(hospital_visits_filtered,patient_id,date_in) %>%
        summarize(state_hospital_before_midnight=unit_in[which.max(date_time_in)]) %>%
        group_by(.,patient_id) %>%
        summarize(initial_state_hospital=state_hospital_before_midnight[which.min(date_in)],min_date_in=which.min(date_in)) %>%
        right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
        mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
        select(age,sex,initial_state)
    return(first_state)
}



