

############################## ------ Create input for simulation ----- ##############################


############################## -----extract current state of patients in hospital system at any date
get_current_state_per_date <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
                        ) %>%
                        group_by(.,patient_id) %>%
                        mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
                        ungroup()
        state_block_starts <- select(patient_transitions_state_blocks,patient_id,state_with_severity_block_nr,state_block_nr_start) %>%
                                rename(state_block_nr=state_with_severity_block_nr)
    }else{
        transitions <- group_by(patient_transitions,patient_id) %>% mutate(state_block_nr=get_state_block_numbers(state)) %>% ungroup()
        state_block_starts <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr) %>%
            summarize(state_block_nr_start=min(state_block_nr_start)) %>%
            ungroup()
    }
    
    current_state_per_date <- inner_join(transitions,state_block_starts,by=c('patient_id','state_block_nr')) %>%
        mutate(days_in_state=as.numeric(date-state_block_nr_start)+1) %>%
        inner_join(select(individs_extended,patient_id,age,sex,date_diagnosis),.,by='patient_id') %>%
        mutate(days_from_diagnosis=as.numeric(date-date_diagnosis)+1) %>%
        inner_join(.,state_worst_case_per_date,by=c('patient_id','date')) %>%
        mutate(state_worst=if(type=='clinical_assessment_included') paste0(state_worst,'-',state_worst_severity) else state_worst) %>%
        select(patient_id,date,age,sex,state,days_in_state,days_from_diagnosis,state_worst)
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

get_transition_matrix_all <- function(type='',recovered_imputed){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        ) %>% select(.,patient_id,date,state,state_tomorrow)
        
    }else{
        transitions <- select(patient_transitions,patient_id,date,state,state_tomorrow)
    }
    states <- get_states_in_order(type)
    #all age groups
    state_transitions_all <- expand.grid(states,states,stringsAsFactors = FALSE) %>%
                                rename(state=Var1,state_tomorrow=Var2) %>%
                                as_tibble()
    transition_counts_all <- bind_rows(transitions,recovered_imputed) %>% 
                                filter(.,!is.na(state_tomorrow)) %>% group_by(.,state,state_tomorrow) %>%
                                summarise(count=as.numeric(n())) %>% 
                                right_join(.,state_transitions_all,by=c('state','state_tomorrow')) %>%
                                mutate(count=if_else(is.na(count),0,count))
    transition_counts_matrix_all <- matrix(transition_counts_all$count,ncol=length(states),nrow=length(states))
    colnames(transition_counts_matrix_all) <- states
    return(transition_counts_matrix_all)
}
#simple age groups
get_transition_matrix_by_age <- function(type='',recovered_imputed_by_age){
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
                                        bind_rows(.,recovered_imputed_by_age) %>%
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


############## ----- Length of stay distribution by state and age ----- ############## 
get_length_of_stay_predicted_by_age_simple <- function(type='',states_to_predict,max_num_days_vec,states_to_predict_labels=states_to_predict){
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
    age_groups <- sort(unique(state_blocks_with_age$age_group_simple))
    length_of_stay_samples <- lapply(1:length(states_to_predict),function(i){
        x <- filter(state_blocks_with_age,state==states_to_predict[i] & !censored) %>% select(state_duration) %>% unlist() %>% unname()
        x_c <- filter(state_blocks_with_age,state==states_to_predict[i] & censored) %>% select(state_duration) %>% unlist() %>% unname()
        sample_from_lognormal(x,x_c,states_to_predict_labels[i],max_num_days_vec[i],age_groups)
    }) %>% bind_rows()
    
    length_of_stay_predicted_by_age_simple <- filter(state_blocks_with_age,grepl('home',state)) %>%
        filter(!censored) %>%
        group_by(.,state,age_group_simple,state_duration) %>%
        summarise(count=n()) %>%
        ungroup() %>%
        bind_rows(.,length_of_stay_samples) 

    return(length_of_stay_predicted_by_age_simple)
}




get_length_of_stay_empirical_by_age_simple <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
            select(.,patient_id,state_with_severity_block_nr,state,censored,state_duration) %>%
            rename(state_block_nr=state_with_severity_block_nr)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
            ungroup()
    }
    state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                        select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                        by='patient_id')
    length_of_stay_empirical_by_age_simple <- group_by(state_blocks_with_age,state,age_group_simple,censored,state_duration) %>%
        summarise(count=n()) %>%
        arrange(state,censored,age_group_simple) %>%
        ungroup()
    return(length_of_stay_empirical_by_age_simple)
}

################# ----- First state of individuals diagnosed with COVID-19 ---- #############
get_first_state <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        )
    }else{
        transitions <- patient_transitions
    }
    first_state <- inner_join(select(individs_extended,patient_id,age,sex),transitions,by='patient_id') %>%
    group_by(patient_id,age,sex) %>%
    summarise(date_diagnosis=min(date),initial_state=state[which.min(date)]) %>%
    ungroup()
    return(first_state)
}