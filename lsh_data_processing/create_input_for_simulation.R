

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
        inner_join(select(individs_extended,patient_id,splitting_variable,date_diagnosis),.,by='patient_id') %>%
        mutate(days_from_diagnosis=as.numeric(date-date_diagnosis)+1) %>%
        inner_join(.,state_worst_case_per_date,by=c('patient_id','date')) %>%
        mutate(state_worst=if(type=='clinical_assessment_included') paste0(state_worst,'-',state_worst_severity) else state_worst) %>%
        select(patient_id,date,splitting_variable,state,days_in_state,days_from_diagnosis,state_worst)
    return(current_state_per_date)
}

############### ---- Transition matrices ---- ##################################

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
    state_transitions_all <- expand_grid(state=states,state_tomorrow=states) %>%
                                as_tibble()
    transition_counts_all <- bind_rows(transitions,recovered_imputed) %>% 
                                filter(.,!is.na(state_tomorrow)) %>%
                                group_by(.,state,state_tomorrow) %>%
                                summarize(count=as.numeric(n())) %>%
                                ungroup() %>%
                                right_join(.,state_transitions_all,by=c('state','state_tomorrow')) %>% 
                                mutate(count=if_else(is.na(count),0,count)) %>%
                                mutate(state=factor(state,levels=states,labels=states),
                                       state_tomorrow=factor(state_tomorrow,levels=states,labels=states)) %>%
                                arrange(state,state_tomorrow)
    transition_counts_matrix_all <- matrix(transition_counts_all$count,ncol=length(states),nrow=length(states),byrow=T)
    colnames(transition_counts_matrix_all) <- states
    return(transition_counts_matrix_all)
}

get_transition_matrix_by_splitting_variable <- function(type='',recovered_imputed,splitting_variable_name){
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
    splitting_variable_values <- get_splitting_variable_values_in_order(splitting_variable_name)
    state_transitions_all_by_splitting_variable <- expand_grid(splitting_variable=splitting_variable_values,state=states,state_tomorrow=states) %>%
                                mutate(splitting_variable=factor(splitting_variable,levels=splitting_variable_values,labels=splitting_variable_values))
    transition_counts_by_splitting_variable <- filter(transitions,!is.na(state_tomorrow)) %>%
                            inner_join(select(individs_extended,patient_id,splitting_variable),.,by='patient_id') %>%
                            bind_rows(.,recovered_imputed) %>%
                            group_by(.,splitting_variable,state,state_tomorrow) %>%
                            summarize(count=as.numeric(n())) %>%
                            ungroup() %>%
                            right_join(.,state_transitions_all_by_splitting_variable,by=c('splitting_variable','state','state_tomorrow')) %>%
                            mutate(count=if_else(is.na(count),0,count)) %>%
                            mutate(state=factor(state,levels=states,labels=states),
                                   state_tomorrow=factor(state_tomorrow,levels=states,labels=states)) %>%
                            arrange(state,state_tomorrow)
                                        
    
    transition_matrix_list <- lapply(splitting_variable_values,function(x){
        mat <- filter(transition_counts_by_splitting_variable,splitting_variable==x) %>% 
            select(count) %>% unlist() %>% 
            matrix(.,ncol=length(states),nrow=length(states),byrow=T)
        colnames(mat) <- states
        return(mat)
    })
    names(transition_matrix_list) <- splitting_variable_values
    return(transition_matrix_list)
}

get_transition_summary <- function(type='',recovered_imputed,splitting_variable_name){
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
    splitting_variable_values <- get_splitting_variable_values_in_order(splitting_variable_name)
    state_transitions_all_by_splitting_variable <- expand_grid(splitting_variable=splitting_variable_values,state=states,state_tomorrow=states) %>%
        mutate(splitting_variable=factor(splitting_variable,levels=splitting_variable_values,labels=splitting_variable_values))
    transition_counts_by_splitting_variable <- filter(transitions,!is.na(state_tomorrow)) %>%
        inner_join(select(individs_extended,patient_id,splitting_variable),.,by='patient_id') %>%
        bind_rows(.,recovered_imputed) %>%
        group_by(.,splitting_variable,state,state_tomorrow) %>%
        summarize(count=as.numeric(n())) %>%
        ungroup() %>%
        right_join(.,state_transitions_all_by_splitting_variable,by=c('splitting_variable','state','state_tomorrow')) %>%
        mutate(count=if_else(is.na(count),0,count)) %>%
        mutate(state=factor(state,levels=states,labels=states),
               state_tomorrow=factor(state_tomorrow,levels=states,labels=states)) %>%
        arrange(splitting_variable,state,state_tomorrow) %>%
        filter(state!=state_tomorrow)
    
    return(transition_counts_by_splitting_variable)
}


############## ----- Length of stay distribution by state and splitting variable ----- ############## 
get_length_of_stay_empirical <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
            select(.,patient_id,state_with_severity_block_nr,state,censored,state_duration) %>%
            rename(state_block_nr=state_with_severity_block_nr)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
            ungroup()
    }
    state_blocks_with_splitting_variable <- inner_join(select(individs_extended,patient_id,splitting_variable),
                                        select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                        by='patient_id')
    length_of_stay_empirical <- group_by(state_blocks_with_splitting_variable,state,splitting_variable,censored,state_duration) %>%
        summarize(count=n()) %>%
        arrange(state,censored,splitting_variable) %>%
        ungroup()
    return(length_of_stay_empirical)
}

get_length_of_stay_predicted <- function(type='',states_to_predict,max_num_days_vec,splitting_variable_name,states_to_predict_labels=states_to_predict){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
                                            select(.,patient_id,state_block_nr,state,censored,state_duration)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
                                            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
                                            ungroup()
    }
    state_blocks_with_splitting_variable <- inner_join(select(individs_extended,patient_id,splitting_variable),
                                                       select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                                       by='patient_id')
    splitting_variable_values <- get_splitting_variable_values_in_order(splitting_variable_name)
    length_of_stay_samples <- lapply(1:length(states_to_predict),function(i){
        x <- filter(state_blocks_with_splitting_variable,state==states_to_predict[i] & !censored) %>% select(state_duration) %>% unlist() %>% unname()
        x_c <- filter(state_blocks_with_splitting_variable,state==states_to_predict[i] & censored) %>% select(state_duration) %>% unlist() %>% unname()
        sample_from_beta(x,x_c,states_to_predict_labels[i],max_num_days_vec[i],splitting_variable_values)
    }) %>% bind_rows()
    
    length_of_stay_predicted <- filter(state_blocks_with_splitting_variable,grepl('home',state)) %>%
        filter(!censored) %>%
        group_by(.,state,splitting_variable,state_duration) %>%
        summarize(count=n()) %>%
        ungroup() %>%
        bind_rows(.,length_of_stay_samples) %>%
        arrange(state,splitting_variable,state_duration)

    return(length_of_stay_predicted)
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
    first_state <- inner_join(select(individs_extended,patient_id,splitting_variable),transitions,by='patient_id') %>%
    group_by(patient_id,splitting_variable) %>%
    summarize(date_diagnosis=min(date),initial_state=state[which.min(date)]) %>%
    ungroup()
    return(first_state)
}