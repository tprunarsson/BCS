path_experiments <- '../experiments/'
experiment_file_name <- 'experiment_template.xlsx'
experiment_description <- read_excel(paste0(path_experiments,experiment_file_name),sheet='experiment-description')
experiment_specification <- read_excel(paste0(path_experiments,experiment_file_name),sheet='experiment-specification')
run_description <- read_excel(paste0(path_experiments,experiment_file_name),sheet='run-description')
run_specification <- read_excel(paste0(path_experiments,experiment_file_name),sheet='run-specification')

models <- experiment_description$model
experiment_ids <- experiment_description$experiment_id
write_experiment_run <- function(experiment_specification,experiment_ids,models){
    
    lapply(1:length(experiment_ids),function(id){
        get_output_table(models[i],filter(experiment_specification,experiment_id==experiment_ids[i]))
    })
    
    if(write_tables_for_simulation){
        write.table(patient_transition_summary,file=paste0(path_tables,current_date,'_transition_summary_',splitting_variable_write,'.csv'),sep=',',row.names=FALSE,quote=FALSE)
        write.table(length_of_stay_predicted,file=paste0(path_tables,current_date,'_length_of_stay_',splitting_variable_write,'.csv'),sep=',',row.names=F,quote=F)
    }
}

get_output_tables <- function(model,specification_dat){
    if(model=='clinical_assessment_included'){
        recovered_imputed_model <- recovered_imputed_extended
    }else{
        recovered_imputed_model <- recovered_imputed
    }
    patient_transition_experiment_dat <- filter(specification_dat,type=='transition')
    patient_transition_summary <- get_transition_summary(model,recovered_imputed_model,patient_transition_experiment_dat$state,patient_transition_experiment_dat$value)
    length_of_stay_experiment_dat <- filter(specification_dat,type=='length_of_stay')
    length_of_stay_predicted <- get_length_of_stay_predicted(model,length_of_stay_experiment_dat$state,length_of_stay_experiment_dat$value,distr='lognormal',max_num_days=28) 
    test_tables_for_simulation()
}


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
