source('covid19_lsh_data_preprocessing.R')
source('impute_length_of_stay.R')

############### ---- Transition matrices ---- ###################################
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

write.table(patient_transition_counts_matrix_all,file=paste0(path_tables,current_date,'_transition_matrix','.csv'),sep=',',row.names=FALSE,col.names=states,quote=FALSE)

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

write.table(patient_transition_counts_matrix_age_simple_under_50,file=paste0(path_tables,current_date,'_transition_matrix_under_50','.csv'),sep=',',row.names=F,col.names=states,quote=F)
write.table(patient_transition_counts_matrix_age_simple_over_50,file=paste0(path_tables,current_date,'_transition_matrix_over_50','.csv'),sep=',',row.names=F,col.names=states,quote=F)


############### ---- Current state of patients in hospital system ---- ###################################

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
current_state_write <- filter(current_state, !(days_from_diagnosis > 14 & state == 'home') )

write.table(current_state_write,file=paste0(path_data,current_date,'_current_state','.csv'),sep=',',row.names=F,quote=F)

############## ----- Length of stay distribution by state and age ----- ############## 

state_blocks_with_age <- inner_join(select(individs_extended,patient_id,age_group_simple),
                                    select(patient_transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                    by='patient_id')

theta_ward = fitlognormal(state_blocks_with_age, "inpatient_ward")
theta_icu = fitlognormal(state_blocks_with_age, "intensive_care_unit")
state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age, "home")
state_blocks_with_age_imputed <- impute_empirical(state_blocks_with_age_imputed, "inpatient_ward")
state_blocks_with_age_imputed <- impute_lognormal(state_blocks_with_age_imputed, "intensive_care_unit", theta_icu)

length_of_stay_by_age_simple <- group_by(state_blocks_with_age_imputed,state,age_group_simple,state_duration) %>%
    summarise(count=n()) %>%
    arrange(state,age_group_simple)

write.table(length_of_stay_by_age_simple,file=paste0(path_tables,current_date,'_length_of_stay','.csv'),sep=',',row.names=F,quote=F)

################# ----- First state of individuals diagnosed with COVID-19 ---- #############

first_state <- group_by(hospital_visits_filtered,patient_id) %>%
    summarize(initial_state_hospital=unit_in[which.min(date_time_in)],min_date_in=min(date_in,na.rm=TRUE)) %>%
    right_join(.,select(individs_extended,patient_id,age,sex,date_diagnosis),by='patient_id') %>%
    mutate(initial_state=if_else(is.na(initial_state_hospital),'home',if_else(min_date_in==date_diagnosis,initial_state_hospital,'home'))) %>%
    select(age,sex,initial_state)


write.table(first_state,file=paste0(path_data,current_date,'_first_state','.csv'),sep=',',row.names=F,quote=F)

################# ----- Extract CDF from posterior predictive distribution from the stats group model of number infected
hi_posterior_predictive_distr <- read_csv(paste0('https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/Iceland_Posterior/Iceland_Posterior_',prediction_date,'.csv'))
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
write.table(hi_mat_CDF, file = paste0(path_tables,current_date,'_iceland_posterior.csv'), quote = F,sep=',')

