################# ----- Current state of individuals with ongoing COVID-19 infectoin ---- #############
get_current_state_per_date <- function(model,date_observed,max_splitting_dat){
    transitions <- get_patient_transitions_at_date(model,date_observed) %>%
                    group_by(.,patient_id) %>%
                    mutate(.,state_block_nr=get_state_block_numbers(state)) %>%
                    ungroup()
    state_block_starts <- get_patient_transitions_state_blocks(transitions,model)
    if(model=='extended'){
        state_block_starts <- rename(state_block_starts,state_block_nr=state_with_severity_block_nr)
    }
    state_block_starts <- select(state_block_starts,patient_id,state_block_nr,state_block_nr_start)
                            
    #TODO: decide how date variable is thought, that is whether we represent the state in which an id was in before or after midnight of the date
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
get_first_state <- function(model,date_observed,max_splitting_dat){
    transitions <- get_patient_transitions_at_date(model,date_observed)
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
get_transition_summary <- function(model,date_observed,recovered_imputed_date_observed,s,splitting_variable_name,max_splitting_mapping,time_splitting_variable_name,prior_info){
    splitting_variable_mapping <- get_splitting_variable_mapping(splitting_variable_name,max_splitting_mapping)
    splitting_variable_values <- unique(splitting_variable_mapping$splitting_variable)
    if(model=='ferguson'){
        return(get_ferguson_transition_summary(s,splitting_variable_name,splitting_variable_mapping,time_splitting_variable_name,prior_info))
    }
    transitions <- get_patient_transitions_at_date(model,date_observed) %>%
                    select(patient_id,date,state,severity,state_tomorrow,severity_tomorrow)
    transitions_state_blocks <- get_patient_transitions_state_blocks(transitions,model) %>%
                                filter(.,!censored,grepl(s,state)) %>%
                                select(.,patient_id,state_block_nr,state,state_next,state_duration)
    all_states <- get_states_in_order(model,active=F)
    all_active_states <- get_states_in_order(model,active=T)
    if(splitting_variable_name %in% names(individs_splitting_variables)){
        transitions_state_blocks <- inner_join(transitions_state_blocks,
                                    select(individs_splitting_variables,patient_id,matches(splitting_variable_name),-matches('order')),
                                       by='patient_id') %>%
                                    rename(splitting_variable=!!splitting_variable_name)
    }else{
        transitions_state_blocks <- mutate(transitions_state_blocks,splitting_variable='none')
    }
    if(time_splitting_variable_name %in% names(length_of_stay_categories)){
        transitions_state_blocks <- inner_join(transitions_state_blocks,
                                                       select(length_of_stay_categories,length_of_stay,matches(time_splitting_variable_name),-matches('order')),
                                                       by=c('state_duration'='length_of_stay')) %>%
                                            rename(time_splitting_variable=!!time_splitting_variable_name)
    }else{
        transitions_state_blocks <- mutate(transitions_state_blocks,time_splitting_variable='none')
    }
    length_of_stay_categories <- mutate(length_of_stay_categories,none='none')
    transition_summary_extended <- expand_grid(max_splitting_values=splitting_variable_mapping$max_splitting_values,
                                               state=s,
                                               max_time_splitting_values=length_of_stay_categories$length_of_stay,
                                               state_next=all_states[!grepl(s,all_states)]) %>%
                                    inner_join(.,splitting_variable_mapping,by='max_splitting_values') %>%
                                    inner_join(.,rename(length_of_stay_categories,time_splitting_variable=!!time_splitting_variable_name),by=c('max_time_splitting_values'='length_of_stay'))
    
    transition_summary_state <- group_by(transitions_state_blocks,splitting_variable,state,time_splitting_variable,state_next) %>%
        summarize(count=as.numeric(n())) %>%
        ungroup() %>%
        right_join(.,transition_summary_extended,by=c('splitting_variable','state','time_splitting_variable','state_next')) %>%
        mutate(count=if_else(is.na(count),0,count)) %>%
        mutate(state=factor(state,levels=all_active_states),
               state_next=factor(state_next,levels=all_states)) %>%
        select(max_splitting_values,state,max_time_splitting_values,state_next,count) %>%
        rename(splitting_variable=max_splitting_values,
               time_splitting_variable=max_time_splitting_values)
    return(transition_summary_state)
}




################# ----- Statistical inference of length of stay distributions ---- #############
get_length_of_stay_predicted <- function(model,date_observed,s,splitting_variable_name,max_splitting_mapping,distr='lognormal',max_num_days=35,prior_info){
    splitting_variable_mapping <- get_splitting_variable_mapping(splitting_variable_name,max_splitting_mapping)
    splitting_variable_values <- unique(splitting_variable_mapping$splitting_variable)
    if(model=='ferguson'){
        return(get_ferguson_length_of_stay(s,splitting_variable_name,splitting_variable_mapping,prior_info))
    }
    transitions <- get_patient_transitions_at_date(model,date_observed)
    transitions_state_blocks <- get_patient_transitions_state_blocks(transitions,model)
    all_active_states <- get_states_in_order(model,active=T)
    if(splitting_variable_name %in% names(individs_splitting_variables)){
        state_blocks_with_splitting_variable <- filter(transitions_state_blocks,grepl(s,state)) %>%
                                                inner_join(.,select(individs_splitting_variables,patient_id,matches(splitting_variable_name),-matches('order')),by='patient_id') %>%
                                                mutate(general_state=s) %>%
                                                rename(splitting_variable=!!splitting_variable_name)
    }else{
        state_blocks_with_splitting_variable <- filter(transitions_state_blocks,grepl(s,state)) %>%
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

get_prior_dat <- function(splitting_variable_name,splitting_variable_mapping,prior_info){
    prior_info <- strsplit(prior_info,split=';') %>% unlist()
    prior_dat <- filter(prior_transitions,location==prior_info[1]) %>%
        inner_join(age_groups,by='age_official') %>%
        mutate(splitting_variable=if(splitting_variable_name=='none') 'none' else paste0('age_',!!as.name(splitting_variable_name))) %>%
        group_by(location,splitting_variable) %>%
        summarise(inpatient_ward=sum(infected*inpatient_ward)/sum(infected),
                  intensive_care_unit=sum(infected*intensive_care_unit)/sum(infected)) %>%
        ungroup() %>%
        pivot_longer(cols=c('inpatient_ward','intensive_care_unit'),names_to='state_next',values_to='p') %>%
        mutate(state=if_else(state_next=='inpatient_ward','home','inpatient_ward')) %>%
        mutate(p_complement=1-p) %>%
        pivot_longer(cols=c('p','p_complement'),names_to='prob_type',values_to='prob')%>%
        mutate(state_next=if_else(prob_type=='p_complement','recovered',state_next)) %>%
        inner_join(filter(prior_length_of_stay,location==prior_info[2]),by=c('state','state_next')) %>%
        select(splitting_variable,state,state_next,prob,state_duration) %>%
        bind_rows(.,expand_grid(splitting_variable=unique(.$splitting_variable),
                                state='intensive_care_unit',
                                state_next='inpatient_ward',
                                prob=1,
                                state_duration=10)) %>% 
        inner_join(splitting_variable_mapping,by='splitting_variable') %>%
        select(max_splitting_values,state,state_next,prob,state_duration) %>%
        rename(splitting_variable=max_splitting_values)
    return(prior_dat)
}

get_ferguson_transition_summary <- function(s,splitting_variable_name,splitting_variable_mapping,time_splitting_variable_name,prior_info){
    prior_dat <- get_prior_dat(splitting_variable_name,splitting_variable_mapping,prior_info)
    
    state_transitions_expanded <- expand_grid(splitting_variable=unique(splitting_variable_mapping$max_splitting_values),
                                              state=s,
                                              state_next=get_states_in_order('base'))
    
    state_transitions_with_time_splitting_expanded <- expand_grid(splitting_variable=unique(splitting_variable_mapping$max_splitting_values),
                                                                  state=s,
                                                                  state_next=get_states_in_order('base'),
                                                                  time_splitting_variable=if(time_splitting_variable_name=='none') paste('none',length_of_stay_categories$length_of_stay,sep=';')
                                                                  else paste(length_of_stay_categories[,time_splitting_variable_name,drop=T],length_of_stay_categories$length_of_stay,sep=';')) %>%
        separate(time_splitting_variable,sep=';',into = c('time_splitting_variable','max_time_splitting_variable')) %>%
        mutate(max_time_splitting_variable=as.numeric(max_time_splitting_variable)) %>%
        filter(state!=state_next)
    
    transition_summary <- right_join(prior_dat,state_transitions_expanded,by=c('splitting_variable','state','state_next')) %>%
        mutate(prob=if_else(is.na(prob),0,prob)) %>%
        mutate(state_duration=if_else(is.na(state_duration),0,state_duration)) %>%
        left_join(.,select(length_of_stay_categories,length_of_stay,matches(paste0('^',time_splitting_variable_name,'$'))),
                  by=c('state_duration'='length_of_stay')) %>%
        mutate(time_splitting_variable=if(time_splitting_variable_name=='none') 'none' else !!as.name(time_splitting_variable_name)) %>%
        select(splitting_variable,state,state_next,prob,state_duration,time_splitting_variable) %>%
        left_join(state_transitions_with_time_splitting_expanded,.,by=c('splitting_variable','state','state_next','time_splitting_variable')) %>%
        mutate(prob=if_else(is.na(prob),0,prob),
               state_duration=if_else(is.na(state_duration),0,state_duration)) %>%
        mutate(.,prob=if_else(prob>0,1,0)) %>%
        select(splitting_variable,state,max_time_splitting_variable,state_next,prob) %>%
        rename(time_splitting_variable=max_time_splitting_variable,count=prob)
    
    return(transition_summary)
}

get_ferguson_length_of_stay <- function(s,splitting_variable_name,splitting_variable_mapping,prior_info){
    prior_dat <- get_prior_dat(splitting_variable_name,splitting_variable_mapping,prior_info)
    length_of_stay_expanded <- expand_grid(splitting_variable=unique(splitting_variable_mapping$max_splitting_values),
                                           state=s,
                                           state_duration=length_of_stay_categories$length_of_stay)
    length_of_stay <- mutate(prior_dat,count=1000*prob) %>%
        select(splitting_variable,state,state_duration,count) %>%
        left_join(length_of_stay_expanded,.,by=c('splitting_variable','state','state_duration')) %>%
        mutate(count=if_else(is.na(count),0,count)) %>%
        select(state,splitting_variable,state_duration,count)
    return(length_of_stay)
}



################# ----- Extract CDF from posterior predictive distribution from the stats group model of number infected
get_infections_predicted_per_date <- function(source,date_prediction){
    if(source=='hi'){
        hi_posterior_predictive_distr <- read_csv(paste0('https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Iceland_Posterior/Iceland_Posterior_',date_prediction,'.csv'),col_types=cols())
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
        splitting_variable_names <- filter(experiment_specification,experiment_id==id) %>%
                                    pivot_longer(cols=c('transition_time_independent_splitting','length_of_stay_splitting'),names_to = 'type',values_to = 'value') %>%
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
        
        heuristics_info <- filter(experiment_description,experiment_id==id) %>%
                            separate_rows(heuristics,sep=';') %>%
                            mutate(heuristics=as.numeric(heuristics)) %>%
                            right_join(heuristics_description,by=c('heuristics'='heuristic_id')) %>%
                            mutate(heuristic_in_use=as.numeric(!is.na(experiment_id))) %>%
                            mutate(experiment_id=id) %>%
                            group_by(experiment_id) %>%
                            summarize(heuristic_string=paste(heuristic_in_use,collapse=';'))
        
        filter(experiment_description,experiment_id==id) %>%
            mutate(prior_info=paste(prior_transition,prior_los,sep=';')) %>%
            select(experiment_id,model,prior_info) %>%
            inner_join(max_splitting_info,by='experiment_id') %>%
            inner_join(heuristics_info,by='experiment_id')
            
    }) %>% bind_rows()
    return(run_info)
}


############### ------- get tables specific for experiment ------------------
get_tables_for_experiment <- function(id,dates_observed){
    experiment_info <- filter(run_info,experiment_id==id)
    model <- experiment_info$model
    prior_info <- experiment_info$prior_info
    max_splitting_names <- unlist(strsplit(experiment_info$max_splitting_name,split=':'))
    max_splitting_dat <- get_max_splitting_dat(max_splitting_names)
    max_splitting_mapping <- filter(max_splitting_dat,!duplicated(max_splitting_values)) %>%
                                arrange(max_splitting_order) %>%
                                select(-patient_id)
    
    experiment <- filter(experiment_specification,experiment_id==id)
    states <- experiment$state
    length_of_stay_splitting_variable_names <- experiment$length_of_stay_splitting
    transition_splitting_variable_names <- experiment$transition_time_independent_splitting
    transition_time_splitting_variable_names <- experiment$transition_time_dependent_splitting
    if(model=='ferguson'){
        current_state_per_date <- tibble(patient_id=numeric(0),date=as.Date(x = integer(0), origin = "1970-01-01"),splitting_variable=character(0),state=character(0),days_in_state=numeric(0),days_from_diagnosis=numeric(0),state_worst=character(0))
        current_state_per_date_filtered <- tibble(date=as.Date(x = integer(0), origin = "1970-01-01"),patient_id=numeric(0),splitting_variable=character(0),state=character(0),days_in_state=numeric(0),days_from_diagnosis=numeric(0),state_worst=character(0))
        recovered_imputed <- tibble(date=as.Date(x = integer(0), origin = "1970-01-01"),patient_id=integer(0),state=character(0),state_tomorrow=character(0))
    }else{
        current_state_per_date <- get_current_state_per_date(model,date_last_known_state,max_splitting_dat)
        current_state_per_date_filtered <- filter(current_state_per_date,!(days_from_diagnosis >= 14 & state_worst == 'home')) %>%
            select(date,patient_id,everything()) %>%
            arrange(date,patient_id)
        recovered_imputed <- anti_join(select(current_state_per_date,date,patient_id,state),
                                       select(current_state_per_date_filtered,date,patient_id),
                                       by=c('date','patient_id')) %>%
            mutate(state_tomorrow='recovered') %>%
            arrange(date,patient_id) %>%
            select(.,date,patient_id,state,state_tomorrow)
    }
    first_state_list <- list()
    transition_summary_list <- list()
    length_of_stay_list <- list()
    for(date_observed in dates_observed){ 
        first_state_list[[date_observed]] <- get_first_state(model,date_observed,max_splitting_dat)# %>% get_cdf(.,num_groups = 1)
        transition_summary_list[[date_observed]] <-lapply(1:length(states),function(i){
            get_transition_summary(model,date_observed,filter(recovered_imputed,date==date_observed),states[i],transition_splitting_variable_names[i],max_splitting_mapping,transition_time_splitting_variable_names[i],prior_info)
        }) %>% bind_rows() %>% arrange(splitting_variable,state,time_splitting_variable,state_next)# %>% get_cdf(.,num_groups = 2)
        length_of_stay_list[[date_observed]] <-lapply(1:length(states),function(i){
            get_length_of_stay_predicted(model,date_observed,states[i],length_of_stay_splitting_variable_names[i],max_splitting_mapping,distr='lognormal',max_num_days = 42,prior_info)
        }) %>% bind_rows() %>% arrange(state,splitting_variable)# %>% get_cdf(.,num_groups = 2)
    }
    first_state <- bind_rows(first_state_list,.id='date') %>% mutate(date=dates_observed[as.numeric(date)])
    transition_summary <- bind_rows(transition_summary_list,.id='date') %>% mutate(date=dates_observed[as.numeric(date)])
    length_of_stay <- bind_rows(length_of_stay_list,.id='date') %>% mutate(date=dates_observed[as.numeric(date)])

    return(list('length_of_stay'=length_of_stay,
                'transition_summary'=transition_summary,
                'current_state_per_date'=current_state_per_date,
                'current_state_per_date_filtered'=current_state_per_date_filtered,
                'first_state'=first_state))
}
