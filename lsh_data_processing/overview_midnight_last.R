# Overview at midnight (current state)
age_groups <- read_excel(file_path_coding,sheet = "age_groups")

add_sum_row <- function(tibble_display,name_display=names(tibble_display)[1]){
  names(tibble_display)=c(name_display,names(tibble_display)[2:ncol(tibble_display)])
  sr <- tibble('Total',summarize_if(tibble_display[2:ncol(tibble_display)],is.numeric, sum, na.rm=TRUE)) 
  names(sr)=c(name_display, names(tibble_display)[2:ncol(tibble_display)])
  return(bind_rows(tibble_display,sr))
}

name_display_wards='Inpatient Ward'
name_display_icu='Intensive Care Unit'
name_display_home='Home'
name_display_out ='Released'

#all
covid_all <- select(individs_extended, patient_id, outcome) %>% group_by(outcome) %>% summarize(cnt=n()) %>% add_sum_row('All')
hospital <- filter(hospital_visits, !is.finite(date_out)) %>% left_join(., unit_categories, by=c('unit_in'='unit_category_raw')) %>%
            select(patient_id, unit_in, unit_category) 
#home
recovered_imputed <- mutate(recovered_imputed_by_age, state='imputed as recovered') %>% select(patient_id,state)
home <- select(current_state_write, patient_id, state) %>% filter(state=='home') %>% bind_rows(recovered_imputed) %>%
         group_by(state) %>% summarize(cnt=n()) %>% add_sum_row(.,name_display_home)

# ward
ward <- filter(hospital, unit_category=='inpatient_ward') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_wards)
#icu
icu <- filter(hospital, unit_category=='intensive_care_unit') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_icu)
#released
released <- filter(individs_extended, outcome!='in_hospital_system') %>% select(patient_id, outcome) %>% group_by(outcome) %>% 
            summarize(Count=n()) %>% add_sum_row(.,name_display_out)

#current state details
#total and by age
sim_age_total <- select(current_state_write,patient_id,state) %>% group_by(state) %>% summarize(All=n()) %>% add_sum_row()
sim_age_groups <- left_join(current_state_write,age_groups,by='age') %>% select(-age_group_official,-age_group_simple) %>% 
  group_by(state,age_group_three) %>% summarize(cnt=n()) %>%
  pivot_wider(names_from='age_group_three',values_from='cnt') %>% mutate_at(c(2:4),~replace(., is.na(.), 0)) %>%
  ungroup() %>% add_sum_row(.,'Age')
sim_total_age_groups <- bind_cols(sim_age_total,sim_age_groups[2:ncol(sim_age_groups)])
# worst state
sim_state_worst <- select(current_state_write,patient_id,state,state_worst) %>% group_by(state,state_worst) %>% summarize(cnt=n()) %>%
  pivot_wider(names_from='state_worst', values_from='cnt') %>% mutate_at(c(2:4),~replace(., is.na(.), 0)) %>%
  ungroup() %>% add_sum_row(.,'Worst State')
sim_total_state_worst <- bind_cols(sim_age_total,sim_state_worst[2:ncol(sim_state_worst)])
#Days from diagnosis and days in state
sim_dfd_dis <- select(current_state_write, patient_id, state, days_from_diagnosis, days_in_state) %>% group_by(state) %>% 
  summarize_at(vars(days_from_diagnosis,days_in_state),funs(min,median,max)) %>%
  rename(dfd_min=days_from_diagnosis_min,dfd_median=days_from_diagnosis_median, dfd_max=days_from_diagnosis_max,
         dis_min=days_in_state_min,dis_median=days_in_state_median, dis_max=days_in_state_max) %>%
  select(state,dfd_min,dfd_median,dfd_max,dis_min,dis_median,dis_max)

print(covid_all)
print(home)
print(ward)
print(icu)
print(released)
print(sim_total_age_groups)
print(sim_total_state_worst)
print(sim_dfd_dis)

# hospital <- filter(hospital_visits, !is.finite(date_out)) %>% left_join(., unit_categories, by=c('unit_in'='unit_category_raw')) %>%
#             select(patient_id, unit_in, unit_category) 
# ward <- filter(hospital, unit_category=='inpatient_ward') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_wards)
# icu <- filter(hospital, unit_category=='intensive_care_unit') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_wards)
# recovered_imputed <- mutate(recovered_imputed_by_age, imputed=T, label='home') %>% select(patient_id,label,imputed)
# home <- filter(individs_extended,outcome=='in_hospital_system') %>% select(patient_id) %>%
#         left_join(., recovered_imputed, by='patient_id') %>% mutate(imputed=if_else(is.na(imputed), F, imputed), 
#                                                                     label=if_else(imputed==F, 'recovered_imputed', label)) %>%
#         group_by(label) %>% summarize(n()) %>% add_sum_row(.,name_display_home)
# out <-  filter(individs_extended,outcome!='in_hospital_system') %>% select(patient_id, outcome) %>% group_by(outcome) %>% 
#         summarize(Count=n()) %>% add_sum_row(.,name_display_out)
# 
# sim_age_groups <- left_join(current_state,age_groups,by='age') %>% select(-age_group_official,-age_group_simple) %>% 
#                   group_by(state,age_group_three) %>% summarize(cnt=n()) %>%
#                   pivot_wider(names_from='age_group_three',values_from='cnt') %>% mutate_at(c(2:4),~replace(., is.na(.), 0)) %>%
#                   ungroup() %>% add_sum_row()
# 
# sim_state_worst <- select(current_state,patient_id,state,state_worst) %>% group_by(state,state_worst) %>% summarize(cnt=n()) %>%
#                     pivot_wider(names_from='state_worst', values_from='cnt') %>% mutate_at(c(2:4),~replace(., is.na(.), 0)) %>%
#                     ungroup() %>% add_sum_row()
# 
# sim_los <- select(current_state, patient_id, state, days_from_diagnosis, days_in_state) %>% group_by(state) %>% 
#             summarize_at(vars(days_from_diagnosis,days_in_state),funs(min,median,max))






