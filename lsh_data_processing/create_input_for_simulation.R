

############################## ------ Create input for simulation ----- ##############################


############################## -----extract current state of patients in hospital system at any date
get_current_state_per_date <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
                        )
    }else{
        transitions <- patient_transitions
    }
    
    current_state_per_date <- select(transitions,-state_tomorrow) %>%
        group_by(.,date,state) %>%
        summarise(count=n())
    return(current_state_per_date)
}


############### ---- Transition matrices ---- ###################################
get_states_in_order <- function(type='',active=F){
    states_active <- distinct(unit_categories,unit_category,unit_category_order) %>%
        arrange(unit_category_order) %>%
        select(unit_category) %>%
        unlist() %>%
        unname() 
    if(type=='clinical_assessment_included'){
        clinical_assessment_active <- c('green','red')
        states_active <- sapply(states_active,function(x) paste0(x,'-',clinical_assessment_active)) %>% c()
    }
    if(active){
       states <- states_active 
    }else{
        states_end <- c('death','recovered')
        states <- c(states_active,states_end)
    }
    return(states)
}

get_transition_matrix_all <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        )
    }else{
        transitions <- patient_transitions
    }
    states <- get_states_in_order(type)
    #all age groups
    state_transitions_all <- expand.grid(states,states,stringsAsFactors = FALSE) %>%
                                rename(state=Var1,state_tomorrow=Var2) %>%
                                as_tibble()
    transition_counts_all <- filter(transitions,!is.na(state_tomorrow)) %>% group_by(.,state,state_tomorrow) %>%
                                summarise(count=as.numeric(n())) %>% 
                                right_join(.,state_transitions_all,by=c('state','state_tomorrow')) %>%
                                mutate(count=if_else(is.na(count),0,count))
    transition_counts_matrix_all <- matrix(transition_counts_all$count,ncol=length(states),nrow=length(states))
    colnames(transition_counts_matrix_all) <- states
    return(transition_counts_matrix_all)
}
#simple age groups
get_transition_matrix_by_age <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
                        )
    }else{
        transitions <- patient_transitions
    }
    states <- get_states_in_order(type)
    age_group_simple=c('0-50','51+')
    state_transitions_age_simple <- expand.grid(age_group_simple,states,states,stringsAsFactors = FALSE) %>%
        rename(age_group_simple=Var1,state=Var2,state_tomorrow=Var3) %>%
        as_tibble()
    transition_counts_age_simple <- filter(transitions,!is.na(state_tomorrow)) %>% inner_join(select(individs_extended,patient_id,age_group_simple),.,by='patient_id') %>%
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
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                                                  state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                                           is.na(severity_tomorrow) ~ state_tomorrow,
                                                                           TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
                       ) %>%
                       group_by(.,patient_id) %>%
                       mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
                       ungroup()
        state_block_starts <- select(patient_transitions_state_blocks,patient_id,state_block_nr,state_block_nr_start)
    }else{
        transitions <- group_by(patient_transitions,patient_id) %>% mutate(state_block_nr=get_state_block_numbers(state)) %>% ungroup()
        state_block_starts <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr) %>%
                                summarize(state_block_nr_start=min(state_block_nr_start)) %>%
                                ungroup()
    }
    
    current_state <- filter(transitions,date==date_last_known_state) %>%
        inner_join(.,state_block_starts,by=c('patient_id','state_block_nr')) %>%
        mutate(days_in_state=as.numeric(current_date-state_block_nr_start)) %>%
        inner_join(individs_extended,.,by='patient_id') %>%
        mutate(days_from_diagnosis=as.numeric(current_date-date_diagnosis)) %>%
        mutate(state_worst=if(type=='clinical_assessment_included') paste0(state_worst,'-',state_worst_severity) else state_worst) %>%
        select(patient_id,age,sex,state,days_in_state,days_from_diagnosis,state_worst)

    return(current_state)
}

############## ----- Length of stay distribution by state and age ----- ############## 
get_length_of_stay_by_age_simple <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
                                            select(.,patient_id,state_block_nr,state,censored,state_duration)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
                                            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
                                            ungroup()
    }
    state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                        select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                        by='patient_id')
    #theta_ward = fitlognormal(state_blocks_with_age, "inpatient_ward")
    
    
    
    states_active <- get_states_in_order(type,active=T)
    #for now exclude intensive care unit, missing data on severity in that state at the moment
    states_active <- states_active[!grepl('intensive_care_unit',states_active)]
    state_blocks_with_age_imputed <- state_blocks_with_age
    for(i in 1:length(states_active)){
        state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age, states_active[i])
    }
    theta_icu = fitlognormal(state_blocks_with_age, "intensive_care_unit")
    state_blocks_with_age_imputed <- impute_lognormal(state_blocks_with_age_imputed, "intensive_care_unit", theta_icu)
    
    length_of_stay_by_age_simple <- group_by(state_blocks_with_age_imputed,state,age_group_simple,state_duration) %>%
        summarise(count=n()) %>%
        arrange(state,age_group_simple)
    return(length_of_stay_by_age_simple)
}


################# ----- First state of individuals diagnosed with COVID-19 ---- #############
get_first_state <- function(type=''){
    # first_state <- group_by(hospital_visits_filtered,patient_id,date_in) %>%
    #     summarize(state_hospital_before_midnight=unit_in[which.max(date_time_in)]) %>%
    #     group_by(.,patient_id) %>%
    #     summarize(initial_state_hospital=state_hospital_before_midnight[which.min(date_in)],min_date_in=which.min(date_in)) %>%
    #     right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
    #     mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
    #     select(age,sex,initial_state)
    first_state <- inner_join(select(individs_extended,patient_id,age,sex),patient_transitions,by='patient_id') %>%
    group_by(patient_id,age,sex) %>%
    summarise(date=min(date),state=state[which.min(date)])
    return(first_state)
}



