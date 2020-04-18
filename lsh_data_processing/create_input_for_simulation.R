################# ----- Current state of individuals with ongoing COVID-19 infectoin ---- #############
get_current_state_per_date <- function(model){
    if(type=='extended'){
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
        inner_join(select(individs_extended,patient_id,date_diagnosis),.,by='patient_id') %>%
        mutate(days_from_diagnosis=as.numeric(date-date_diagnosis)+1) %>%
        inner_join(.,state_worst_case_per_date,by=c('patient_id','date')) %>%
        mutate(state_worst=if(type=='extended') paste0(state_worst,'-',state_worst_severity) else state_worst) %>%
        inner_join(.,individs_splitting_variables,by='patient_id') %>%
        select(-state_tomorrow,-date_diagnosis,-matches('severity'),-matches('state_block_nr'),-matches('order'))
    return(current_state_per_date)
}

################# ----- First state of individuals diagnosed with COVID-19 infection ---- #############
get_first_state <- function(model){
    if(type=='extended'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        )
    }else{
        transitions <- patient_transitions
    }
    first_state <- group_by(transitions,patient_id) %>%
    summarize(date_diagnosis=min(date),initial_state=state[which.min(date)]) %>%
    ungroup() %>%
    inner_join(.,individs_splitting_variables,by='patient_id') %>%
    select(-matches('severity'),-matches('state_block_nr'),-matches('order'))
    
    return(first_state)
}


write_tables_for_experiment(){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
                                            select(.,patient_id,state_block_nr,state,censored,state_duration)
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
                        ) %>%
                        select(.,patient_id,date,state,state_tomorrow)
    }else{
        transitions <- select(patient_transitions,patient_id,date,state,state_tomorrow)
    }
    transitions <- bind_rows(transitions,recovered_imputed)
    all_states <- get_states_in_order(type,active=F)
    all_active_states <- get_states_in_order(type,active=T)
    max_splitting_name <- select(individs_splitting_variables,matches(paste(unique(splitting_variable_names),sep='|')),-matches('order')) %>%
        summarise_all(.,~length(unique(.))) %>%
        gather(key='splitting_variable',value='nr_levels') %>%
        slice(which.max(nr_levels)) %>%
        select(splitting_variable) %>%
        unlist() %>%
        unname()
    max_splitting_values <- mutate(individs_splitting_variables,
                                   max_splitting=!!as.name(max_splitting_name),
                                   max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
        filter(.,!duplicated(max_splitting)) %>%
        arrange(max_splitting_order) %>%
        select(max_splitting,-matches('order')) %>%
        unlist() %>%
        unname()
    transition_summary_list <- list()
}

################# ----- Transition counts between states ---- #############
get_transition_summary <- function(type='',recovered_imputed,states,splitting_variable_names){
    if(type=='clinical_assessment_included'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        ) %>%
            select(.,patient_id,date,state,state_tomorrow)
    }else{
        transitions <- select(patient_transitions,patient_id,date,state,state_tomorrow)
    }
    transitions <- bind_rows(transitions,recovered_imputed)
    all_states <- get_states_in_order(type,active=F)
    all_active_states <- get_states_in_order(type,active=T)
    #full_patient_transition_experiment_dat <- tibble(state=all_active_states)%>%
    #                                        left_join(.,patient_transition_experiment_dat,by='state') %>%
    #                                        mutate(value=if_else(is.na(value),'none',value))
    #states <- full_patient_transition_experiment_dat$state
    #splitting_variable_names <- full_patient_transition_experiment_dat$value
    max_splitting_name <- select(individs_splitting_variables,matches(paste(unique(splitting_variable_names),sep='|')),-matches('order')) %>%
        summarise_all(.,~length(unique(.))) %>%
        gather(key='splitting_variable',value='nr_levels') %>%
        slice(which.max(nr_levels)) %>%
        select(splitting_variable) %>%
        unlist() %>%
        unname()
    max_splitting_values <- mutate(individs_splitting_variables,
                                   max_splitting=!!as.name(max_splitting_name),
                                   max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
        filter(.,!duplicated(max_splitting)) %>%
        arrange(max_splitting_order) %>%
        select(max_splitting,-matches('order')) %>%
        unlist() %>%
        unname()
    transition_summary_list <- list()
    for(i in 1:length(states)){
        transitions_with_splitting_variable <- filter(transitions,grepl(states[i],state)) %>%
            inner_join(.,select(individs_splitting_variables,patient_id,matches(splitting_variable_names[i])),by='patient_id') %>%
            mutate(state=factor(state,levels=all_active_states),state_tomorrow=factor(state_tomorrow,levels=all_states)) %>%
            mutate(general_state=states[i]) 
        states_mapping <- filter(transitions_with_splitting_variable,!duplicated(state)) %>%
            select(general_state,state) %>%
            mutate(state=factor(state,levels=all_active_states)) %>%
            arrange(state) 
        state_values=states_mapping$state
        if(splitting_variable_names[i] %in% names(transitions_with_splitting_variable)){
            splitting_variable_mapping <- mutate(individs_splitting_variables,
                                                 max_splitting=!!as.name(max_splitting_name),
                                                 splitting_variable=!!as.name(splitting_variable_names[i]),
                                                 max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
                filter(.,!duplicated(max_splitting)) %>%
                arrange(max_splitting_order) %>%
                select(splitting_variable,max_splitting,-matches('order'))
            
            splitting_variable_values <- mutate(individs_splitting_variables,
                                                splitting_variable=!!as.name(splitting_variable_names[i]),
                                                splitting_variable_order=!!as.name(paste0(splitting_variable_names[i],'_order'))) %>%
                filter(.,!duplicated(splitting_variable)) %>%
                arrange(splitting_variable_order) %>%
                select(splitting_variable,-matches('order')) %>%
                unlist() %>%
                unname()
            transitions_with_splitting_variable <- mutate(transitions_with_splitting_variable,
                                                          splitting_variable=factor(!!as.name(splitting_variable_names[i]),levels=splitting_variable_values)) %>%
                select(patient_id,date,splitting_variable,general_state,state,state_tomorrow)
        }else{
            splitting_variable_mapping <- mutate(individs_splitting_variables,
                                                 max_splitting=!!as.name(max_splitting_name),
                                                 max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
                filter(.,!duplicated(max_splitting)) %>%
                arrange(max_splitting_order) %>%
                mutate(splitting_variable=factor('none',levels='none')) %>%
                select(splitting_variable,max_splitting,-matches('order'))
            splitting_variable_values <- 'none'
            transitions_with_splitting_variable <- mutate(transitions_with_splitting_variable,
                                                          splitting_variable=factor('none',levels='none')) %>%
                select(patient_id,date,splitting_variable,general_state,state,state_tomorrow)
        }
        
        all_transitions_by_splitting_variable <- expand_grid(max_splitting=splitting_variable_mapping$max_splitting,general_state=states[i],state=state_values,state_tomorrow=all_states[!grepl(states[i],all_states)]) %>%
            inner_join(.,splitting_variable_mapping,by='max_splitting') %>%
            mutate(splitting_variable=factor(splitting_variable,levels=splitting_variable_values),
                   max_splitting=factor(max_splitting,levels=max_splitting_values),
                   state=factor(state,levels=all_active_states),
                   state_tomorrow=factor(state_tomorrow,levels=all_states))
        transition_summary_list[[states[i]]] <- filter(transitions_with_splitting_variable,!is.na(state_tomorrow)) %>%
            group_by(.,splitting_variable,general_state,state_tomorrow) %>%
            summarize(count=as.numeric(n())) %>%
            ungroup() %>%
            right_join(.,all_transitions_by_splitting_variable,by=c('splitting_variable','general_state','state_tomorrow')) %>%
            mutate(count=if_else(is.na(count),0,count)) %>%
            select(max_splitting,state,state_tomorrow,count) %>%
            rename(splitting_variable=max_splitting)
        
        
    }
    return(bind_rows(transition_summary_list) %>% arrange(splitting_variable,state,state_tomorrow))
}

################# ----- Statistical inference of length of stay distributions ---- #############
get_length_of_stay_predicted <- function(type='',states,splitting_variable_names,distr,max_num_days){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
            select(.,patient_id,state_block_nr,state,censored,state_duration)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
            ungroup()
    }
    all_states <- get_states_in_order(type,active=F)
    all_active_states <- get_states_in_order(type,active=T)
    length_of_stay_list <- list()
    max_splitting_name <- select(individs_splitting_variables,matches(paste(unique(splitting_variable_names),sep='|')),-matches('order')) %>%
        summarise_all(.,~length(unique(.))) %>%
        gather(key='splitting_variable',value='nr_levels') %>%
        slice(which.max(nr_levels)) %>%
        select(splitting_variable) %>%
        unlist() %>%
        unname()
    max_splitting_values <- mutate(individs_splitting_variables,
                                   max_splitting=!!as.name(max_splitting_name),
                                   max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
        filter(.,!duplicated(max_splitting)) %>%
        arrange(max_splitting_order) %>%
        select(max_splitting,-matches('order')) %>%
        unlist() %>%
        unname()
    for(i in 1:length(states)){
        state_blocks_with_splitting_variable <- inner_join(select(individs_splitting_variables,patient_id,matches(splitting_variable_names[i])),
                                                           select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                                           by='patient_id') %>% filter(grepl(states[i],state)) %>% mutate(general_state=states[i])
        if(splitting_variable_names[i] %in% names(state_blocks_with_splitting_variable)){
            splitting_variable_mapping <- mutate(individs_splitting_variables,
                                                 max_splitting=!!as.name(max_splitting_name),
                                                 splitting_variable=!!as.name(splitting_variable_names[i]),
                                                 max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
                filter(.,!duplicated(max_splitting)) %>%
                arrange(max_splitting_order) %>%
                select(splitting_variable,max_splitting,-matches('order'))
            
            splitting_variable_values <- mutate(individs_splitting_variables,
                                                splitting_variable=!!as.name(splitting_variable_names[i]),
                                                splitting_variable_order=!!as.name(paste0(splitting_variable_names[i],'_order'))) %>%
                filter(.,!duplicated(splitting_variable)) %>%
                arrange(splitting_variable_order) %>%
                select(splitting_variable,-matches('order')) %>%
                unlist() %>%
                unname()
            state_blocks_with_splitting_variable <- mutate(state_blocks_with_splitting_variable,
                                                           splitting_variable=factor(!!as.name(splitting_variable_names[i]),levels=splitting_variable_values)) %>%
                select(patient_id,splitting_variable,general_state,state,censored,state_duration)
            
        }else{
            splitting_variable_mapping <- mutate(individs_splitting_variables,
                                                 max_splitting=!!as.name(max_splitting_name),
                                                 max_splitting_order=!!as.name(paste0(max_splitting_name,'_order'))) %>%
                filter(.,!duplicated(max_splitting)) %>%
                arrange(max_splitting_order) %>%
                mutate(splitting_variable=factor('none',levels='none')) %>%
                select(splitting_variable,max_splitting,-matches('order'))
            splitting_variable_values <- 'none'
            state_blocks_with_splitting_variable <- mutate(state_blocks_with_splitting_variable,
                                                           splitting_variable='none') %>%
                select(patient_id,splitting_variable,general_state,state,censored,state_duration)
            
        }
        all_states_in_order <- get_states_in_order(type)
        states_mapping <- filter(state_blocks_with_splitting_variable,!duplicated(state)) %>% select(general_state,state) %>% mutate(state=factor(state,levels=all_states_in_order)) %>% arrange(state) 
        state_values=states_mapping$state
        
        if(grepl('home',states[i])){
            length_of_stay_extended <- expand_grid(max_splitting=splitting_variable_mapping$max_splitting,general_state=states[i],state=state_values) %>%
                inner_join(.,splitting_variable_mapping,by='max_splitting') %>%
                mutate(splitting_variable=factor(splitting_variable,levels=splitting_variable_values),
                       max_splitting=factor(max_splitting,levels=max_splitting_values),
                       state=factor(state,levels=all_active_states))
            length_of_stay_list[[states[i]]] <- filter(state_blocks_with_splitting_variable,!censored) %>%
                group_by(.,general_state,splitting_variable,state_duration) %>%
                summarize(count=n()) %>%
                ungroup() %>%
                inner_join(.,length_of_stay_extended,by=c('splitting_variable','general_state')) %>%
                select(state,max_splitting,state_duration,count) %>%
                rename(splitting_variable=max_splitting)
            
        }else{
            length_of_stay_list[[states[i]]]  <- lapply(1:length(splitting_variable_values),function(j){
                x <- filter(state_blocks_with_splitting_variable,splitting_variable==splitting_variable_values[j] & general_state==states[i] & !censored) %>% select(state_duration) %>% unlist() %>% unname()
                x_c <- filter(state_blocks_with_splitting_variable,splitting_variable==splitting_variable_values[j] & general_state==states[i] & censored) %>% select(state_duration) %>% unlist() %>% unname()
                not_to_be_split <- filter(splitting_variable_mapping,splitting_variable==splitting_variable_values[j]) %>% select(.,max_splitting) %>% unlist() %>% unname()  
                if(distr=='beta'){
                    sample_from_beta(x,x_c,state_values,max_num_days,not_to_be_split)
                }else{
                    sample_from_lognormal(x,x_c,state_values,max_num_days,not_to_be_split)
                }
            }) %>% bind_rows()
        }
    }
    return(bind_rows(length_of_stay_list) %>% arrange(state,splitting_variable,state_duration))
}


################# ----- Extract CDF from posterior predictive distribution from the stats group model of number infected
get_infections_predicted_per_date <- function(source,date_prediction){
    if(source=='hi'){
        hi_posterior_predictive_distr <- read_csv(paste0('https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Iceland_Posterior/Iceland_Posterior_',date_prediction,'.csv'))
        dates <- unique(hi_posterior_predictive_distr$date)
        max_new_cases = max(hi_posterior_predictive_distr$new_cases)
        #note plus 1 to include the possibility of 0 new cases 
        hi_mat_CDF <- matrix(rep(0,length(dates)*(max_new_cases+1)), nrow = length(dates), ncol=(max_new_cases+1))
        rownames(hi_mat_CDF) <- as.character(dates)
        colnames(hi_mat_CDF) <- c(0:max_new_cases)
        for (i in 1:length(dates)) {
            new_cases <- table(filter(hi_posterior_predictive_distr,date==dates[i])$new_cases)
            hi_mat_CDF[i,names(new_cases)] = cumsum(new_cases)/sum(new_cases)
        }
        return(hi_mat_CDF)
    }
    return()
}

# recovered_imputed <- anti_join(select(current_state,patient_id,state),select(current_state_write,patient_id)) %>%
#     mutate(date=current_date,state_tomorrow='recovered') %>%
#     select(.,patient_id,date,state,state_tomorrow)

# recovered_imputed_extended <- anti_join(select(current_state_extended,patient_id,state),select(current_state_extended_write,patient_id)) %>%
#     mutate(date=current_date,state_tomorrow='recovered') %>%
#     select(.,patient_id,date,state,state_tomorrow)

# first_state_per_date_extended_summary_splitting_variable <- inner_join(first_state_extended,select(individs_extended,patient_id,splitting_variable),by=c('patient_id','splitting_variable')) %>%
#     select(date_diagnosis,splitting_variable,initial_state) %>%
#     group_by(date_diagnosis,initial_state,splitting_variable) %>%
#     summarize(count=n())

# first_state_per_date_summary_splitting_variable <- inner_join(first_state,select(individs_extended,patient_id,splitting_variable),by=c('patient_id','splitting_variable')) %>%
#     select(date_diagnosis,splitting_variable,initial_state) %>%
#     group_by(date_diagnosis,initial_state,splitting_variable) %>%
#     summarize(count=n())

#first_state_per_date_summary <- group_by(first_state,date_diagnosis,initial_state) %>% summarize(count=n())

#current_state_per_date_summary <- group_by(current_state_per_date,date,state) %>% summarize(count=n())
