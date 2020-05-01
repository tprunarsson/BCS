################# ----- Current state of individuals with ongoing COVID-19 infectoin ---- #############
get_current_state_per_date <- function(model,max_splitting_dat){
    if(model=='extended'){
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
        mutate(state_worst=if(model=='extended') paste0(state_worst,'-',state_worst_severity) else state_worst) %>%
        inner_join(.,select(max_splitting_dat,patient_id,max_splitting_values) %>% rename(splitting_variable=max_splitting_values),by='patient_id') %>%
        select(-state_tomorrow,-date_diagnosis,-matches('severity'),-matches('state_block_nr')) %>%
        select(patient_id,date,splitting_variable,state,days_in_state,days_from_diagnosis,state_worst)
    return(current_state_per_date)
}

################# ----- First state of individuals diagnosed with COVID-19 infection ---- #############
get_first_state <- function(model,max_splitting_dat){
    if(model=='extended'){
        transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),
                              state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,
                                                       is.na(severity_tomorrow) ~ state_tomorrow,
                                                       TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow))
        )
    }else{
        transitions <- patient_transitions
    }
    active_states <- factor(get_states_in_order(model,active=T))
    max_splitting_values <- filter(max_splitting_dat,!duplicated(max_splitting_values)) %>%
                            arrange(max_splitting_order) %>%
                            select(max_splitting_values) %>%
                            unlist() %>%
                            unname() %>%
                            factor(.,levels=.,labels=.)
    first_state_expanded<- expand_grid(initial_state=active_states,splitting_variable=max_splitting_values)
    
    first_state <- group_by(transitions,patient_id) %>%
    summarize(date_diagnosis=min(date),initial_state=state[which.min(date)]) %>%
    ungroup() %>%
    inner_join(.,select(max_splitting_dat,patient_id,max_splitting_values) %>% rename(splitting_variable=max_splitting_values),by='patient_id') %>%
    select(-matches('severity'),-matches('state_block_nr')) %>%
    mutate(initial_state=factor(initial_state,levels=active_states),
           splitting_variable=factor(splitting_variable,levels=max_splitting_values)) %>%
    group_by(initial_state,splitting_variable) %>%
    summarize(count=n()) %>%
    right_join(.,first_state_expanded,by=c('initial_state','splitting_variable')) %>%
    mutate(count=if_else(is.na(count),0,as.numeric(count)))
    return(first_state)
}

################# ----- Transition counts between states ---- #############
get_transition_summary <- function(model,recovered_imputed,s,splitting_variable_name,max_splitting_mapping,time_splitting_variable_name){
    if(model=='extended'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,
                                                   state=paste0(state,'-',severity),
                                                   state_next=case_when(is.na(state_next) & is.na(severity_next) ~ NA_character_,
                                                                        !is.na(state_next) & is.na(severity_next) ~ state_next,
                                                                        TRUE ~ paste0(state_next,'-',severity_next))
                                                                        ) %>%
                                            filter(.,!censored,grepl(s,state)) %>%
                                            select(.,patient_id,state_block_nr,state,state_next,state_duration)
                                            
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
                                            summarize(.,state_next=state_next[which.max(state_with_severity_block_nr)],
                                                      censored=censored[which.max(state_with_severity_block_nr)],
                                                      state_duration=sum(state_duration)) %>%
                                            filter(.,!censored,grepl(s,state)) %>%
                                            select(.,patient_id,state_block_nr,state,state_next,state_duration) %>%
                                            ungroup()
    }
    all_states <- get_states_in_order(model,active=F)
    all_active_states <- get_states_in_order(model,active=T)
    if(splitting_variable_name %in% names(individs_splitting_variables)){
        transitions_state_blocks_summary <- inner_join(transitions_state_blocks_summary,
                                                       select(individs_splitting_variables,patient_id,matches(splitting_variable_name),-matches('order')),
                                                       by='patient_id') %>%
                                            rename(splitting_variable=!!splitting_variable_name)
    }else{
        transitions_state_blocks_summary <- mutate(transitions_state_blocks_summary,splitting_variable='none')
    }
    splitting_variable_mapping <- get_splitting_variable_mapping(splitting_variable_name,max_splitting_mapping)
    splitting_variable_values <- unique(splitting_variable_mapping$splitting_variable)
    
    if(time_splitting_variable_name %in% names(length_of_stay_categories)){
        transitions_state_blocks_summary <- inner_join(transitions_state_blocks_summary,
                                                       select(length_of_stay_categories,length_of_stay,matches(time_splitting_variable_name),-matches('order')),
                                                       by=c('state_duration'='length_of_stay')) %>%
                                            rename(time_splitting_variable=!!time_splitting_variable_name)
    }else{
        transitions_state_blocks_summary <- mutate(transitions_state_blocks_summary,time_splitting_variable='none')
    }
    length_of_stay_categories <- mutate(length_of_stay_categories,none='none')
    transition_summary_extended <- expand_grid(max_splitting_values=splitting_variable_mapping$max_splitting_values,
                                               state=s,
                                               state_next=all_states[!grepl(s,all_states)],
                                               max_time_splitting_values=length_of_stay_categories$length_of_stay) %>%
                                    inner_join(.,splitting_variable_mapping,by='max_splitting_values') %>%
                                    inner_join(.,rename(length_of_stay_categories,time_splitting_variable=!!time_splitting_variable_name),by=c('max_time_splitting_values'='length_of_stay'))
    
    transition_summary_state <- group_by(transitions_state_blocks_summary,splitting_variable,state,state_next,time_splitting_variable) %>%
        summarize(count=as.numeric(n())) %>%
        ungroup() %>%
        right_join(.,transition_summary_extended,by=c('splitting_variable','state','state_next','time_splitting_variable')) %>%
        mutate(count=if_else(is.na(count),0,count)) %>%
        mutate(state=factor(state,levels=all_active_states),
               state_next=factor(state_next,levels=all_states)) %>%
        select(max_splitting_values,state,state_next,max_time_splitting_values,count) %>%
        rename(splitting_variable=max_splitting_values,
               time_splitting_variable=max_time_splitting_values)
    return(transition_summary_state)
}




################# ----- Statistical inference of length of stay distributions ---- #############
get_length_of_stay_predicted <- function(model,s,splitting_variable_name,max_splitting_mapping,distr='lognormal',max_num_days=35){
    if(model=='extended'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
            select(.,patient_id,state_block_nr,state,censored,state_duration)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
            summarize(censored=censored[which.max(state_with_severity_block_nr)],
                      state_duration=sum(state_duration)) %>%
            ungroup()
    }
    all_active_states <- get_states_in_order(model,active=T)
    splitting_variable_mapping <- get_splitting_variable_mapping(splitting_variable_name,max_splitting_mapping)
    splitting_variable_values <- unique(splitting_variable_mapping$splitting_variable)
    if(splitting_variable_name %in% names(individs_splitting_variables)){
        state_blocks_with_splitting_variable <- filter(transitions_state_blocks_summary,grepl(s,state)) %>%
                                                inner_join(.,select(individs_splitting_variables,patient_id,matches(splitting_variable_name),-matches('order')),by='patient_id') %>%
                                                mutate(general_state=s) %>%
                                                rename(splitting_variable=!!splitting_variable_name)
    }else{
        state_blocks_with_splitting_variable <- filter(transitions_state_blocks_summary,grepl(s,state)) %>%
                                                mutate(.,splitting_variable='none') %>%
                                                mutate(general_state=s)
    }
    states_mapping <- filter(state_blocks_with_splitting_variable,!duplicated(state)) %>% select(general_state,state) %>% mutate(state=factor(state,levels=all_active_states)) %>% arrange(state) 
    state_values=states_mapping$state
    if(grepl('home',s)){
        length_of_stay_extended <- expand_grid(max_splitting_values=splitting_variable_mapping$max_splitting_values,general_state=s,state=state_values) %>%
            inner_join(.,splitting_variable_mapping,by='max_splitting_values')
        length_of_stay_in_state <- filter(state_blocks_with_splitting_variable,!censored) %>%
            group_by(.,general_state,splitting_variable,state_duration) %>%
            summarize(count=n()) %>%
            ungroup() %>%
            inner_join(.,length_of_stay_extended,by=c('splitting_variable','general_state')) %>%
            select(state,max_splitting_values,state_duration,count) %>%
            rename(splitting_variable=max_splitting_values)
        
    }else{
        length_of_stay_in_state  <- lapply(1:length(splitting_variable_values),function(j){
                                        x <- filter(state_blocks_with_splitting_variable,splitting_variable==splitting_variable_values[j] & !censored) %>% select(state_duration) %>% unlist() %>% unname()
                                        x_c <- filter(state_blocks_with_splitting_variable,splitting_variable==splitting_variable_values[j] & censored) %>% select(state_duration) %>% unlist() %>% unname()
                                        not_to_be_split <- filter(splitting_variable_mapping,splitting_variable==splitting_variable_values[j]) %>% select(.,max_splitting_values) %>% unlist() %>% unname()  
                                        length_of_stay_expanded <- expand_grid(state=state_values,splitting_variable=not_to_be_split,state_duration=1:max_num_days)
                                        length_of_stay_samples <- sample_from_distr(x,x_c,max_num_days,distr)
                                        length_of_stay_splitting_variable <- inner_join(length_of_stay_expanded,length_of_stay_samples,by=c('state_duration'))
                                        return(length_of_stay_splitting_variable)
                                    }) %>% bind_rows()
    }
    return(length_of_stay_in_state)
}


################# ----- Extract CDF from posterior predictive distribution from the stats group model of number infected
get_infections_predicted_per_date <- function(source,date_prediction){
    if(source=='hi'){
        hi_posterior_predictive_distr <- read_csv(paste0('https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Iceland_Posterior/Iceland_Posterior_',date_prediction,'.csv'))
        hi_mat_CDF_expanded <- expand_grid(date=unique(hi_posterior_predictive_distr$date),new_cases=0:max(hi_posterior_predictive_distr$new_cases))
        hi_mat_CDF <- group_by(hi_posterior_predictive_distr,date,new_cases) %>%
                        summarise(count=n()) %>%
                        ungroup() %>%
                        right_join(hi_mat_CDF_expanded,by=c('date','new_cases')) %>%
                        mutate(count=if_else(is.na(count),0,as.numeric(count)))# %>%
                        #get_cdf(.,num_groups=1)
        return(hi_mat_CDF)
    }
    return()
}

################# ----- Get information on the current run of a batch of experiments --------
get_run_info <- function(current_run_id){
    experiment_ids <- filter(run_specification,run_id==current_run_id) %>%
                        inner_join(.,experiment_description,by='experiment_id') %>%
                        select(experiment_id) %>%
                        unlist() %>%
                        unname()
    run_info <- lapply(experiment_ids,function(id){
        splitting_variable_names <- filter(experiment_specification,experiment_id==id,
                                              type %in% c('transition','length_of_stay'),
                                              type_attribute=='time_independent') %>%
                                    select(value) %>%
                                    unlist() %>%
                                    unname()
        max_splitting_info <- get_max_splitting_dat(splitting_variable_names) %>%
                                filter(.,!duplicated(max_splitting_values)) %>%
                                arrange(max_splitting_order) %>%
                                select(max_splitting_name,max_splitting_values) %>%
                                group_by(max_splitting_name) %>%
                                summarize(max_splitting_values=paste(max_splitting_values,collapse=';')) %>%
                                mutate(experiment_id=id) %>%
                                select(experiment_id,max_splitting_name,max_splitting_values)
        
        heuristics_info <- filter(experiment_specification,experiment_id==id,type=='heuristic') %>%
                            select(-type,-state) %>%
                            right_join(heuristics_description,by=c('value'='heuristic_id')) %>%
                            mutate(heuristic_in_use=as.numeric(!is.na(experiment_id))) %>%
                            mutate(experiment_id=id) %>%
                            group_by(experiment_id) %>%
                            summarize(heuristic_string=paste(heuristic_in_use,collapse=';'))
        
        filter(experiment_description,experiment_id==id) %>%
            select(experiment_id,model) %>%
            inner_join(max_splitting_info,by='experiment_id') %>%
            inner_join(heuristics_info,by='experiment_id')
    }) %>% bind_rows()
    return(run_info)
}


############### ------- get tables specific for experiment ------------------
get_tables_for_experiment <- function(id){
    experiment_info <- filter(run_info,experiment_id==id)
    model <- experiment_info$model
    max_splitting_names <- unlist(strsplit(experiment_info$max_splitting_name,split=':'))
    max_splitting_dat <- get_max_splitting_dat(max_splitting_names)
    max_splitting_mapping <- filter(max_splitting_dat,!duplicated(max_splitting_values)) %>%
                                    arrange(max_splitting_order) %>%
                                    select(-patient_id)
    
    experiment <- filter(experiment_specification,experiment_id==id)
    length_of_stay_states <- filter(experiment,type=='length_of_stay')$state
    length_of_stay_splitting_variable_names <- filter(experiment,type=='length_of_stay')$value
    transition_states <- filter(experiment,type=='transition',type_attribute=='time_independent')$state
    transition_splitting_variable_names <- filter(experiment,type=='transition',type_attribute=='time_independent')$value
    transition_time_splitting_variable_names <- filter(experiment,type=='transition',type_attribute=='time_dependent')$value

    current_state_per_date <- get_current_state_per_date(model,max_splitting_dat)
    current_state <- filter(current_state_per_date,date==date_last_known_state) %>% select(-date)
    current_state_filtered <- filter(current_state,!(days_from_diagnosis > 14 & state_worst == 'home'))
    current_state_from_start <- filter(current_state_per_date,date==(start_date-1)) %>% filter(!(days_from_diagnosis > 14 & state_worst == 'home')) %>% select(-date)
    recovered_imputed <- anti_join(select(current_state,patient_id,state),select(current_state_filtered,patient_id),by='patient_id') %>%
                                    mutate(date=current_date,state_tomorrow='recovered') %>%
                                    select(.,patient_id,date,state,state_tomorrow)
    first_state <- get_first_state(model,max_splitting_dat)# %>% get_cdf(.,num_groups = 1)
    transition_summary <-lapply(1:length(transition_states),function(i){
        get_transition_summary(model,recovered_imputed,transition_states[i],transition_splitting_variable_names[i],max_splitting_mapping,transition_time_splitting_variable_names[i])
    }) %>% bind_rows() %>% arrange(splitting_variable,state,state_next)# %>% get_cdf(.,num_groups = 2)
    length_of_stay <-lapply(1:length(length_of_stay_states),function(i){
        get_length_of_stay_predicted(model,length_of_stay_states[i],length_of_stay_splitting_variable_names[i],max_splitting_mapping,distr='lognormal',max_num_days = 35)
    }) %>% bind_rows() %>% arrange(state,splitting_variable)# %>% get_cdf(.,num_groups = 2)

    
    
    return(list('length_of_stay'=length_of_stay,
                'transition_summary'=transition_summary,
                'current_state_per_date'=current_state_per_date,
                'current_state_filtered'=current_state_filtered,
                'current_state_from_start'=current_state_from_start,
                'first_state'=first_state))
}
