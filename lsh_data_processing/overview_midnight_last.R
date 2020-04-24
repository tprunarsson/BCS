# Overview at midnight (current state)
age_groups <- read_excel(file_path_coding,sheet = "age_groups")

add_sum_row <- function(tibble_display,name_display=names(tibble_display)[1]){
  names(tibble_display)=c(name_display,names(tibble_display)[2:ncol(tibble_display)])
  sr <- tibble('Total',summarize_if(tibble_display[2:ncol(tibble_display)],is.numeric, sum, na.rm=TRUE)) 
  names(sr)=c(name_display, names(tibble_display)[2:ncol(tibble_display)])
  return(bind_rows(tibble_display,sr))
}

overview_midnight_path='../dashboard/input/overview_midnight.RData'
name_display_wards='Inpatient Ward'
name_display_icu='Intensive Care Unit'
name_display_home='Home'
name_display_out ='Released'

#all
sys_covid_all <- select(individs_extended, patient_id, outcome) %>% group_by(outcome) %>% summarize(cnt=n()) %>% add_sum_row('All')
hospital_current <- filter(hospital_visits, !is.finite(date_out)) %>% left_join(., unit_categories, by=c('unit_in'='unit_category_raw')) %>%
            select(patient_id, unit_in, unit_category)
hospital_all <- left_join(hospital_visits, unit_categories, by=c('unit_in'='unit_category_raw')) %>%
                select(patient_id, date_in, date_out, unit_in, unit_category)
#home
sim_recovered_imputed <- mutate(recovered_imputed, state='imputed as recovered') %>% select(patient_id,state)
sim_home <- select(current_state_write, patient_id, state) %>% filter(state=='home') %>% bind_rows(sim_recovered_imputed) %>%
            group_by(state) %>% summarize(cnt=n()) %>% add_sum_row(.,name_display_home)

# ward
sim_ward <- filter(hospital_current, unit_category=='inpatient_ward') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_wards)
#icu
sim_icu <- filter(hospital_current, unit_category=='intensive_care_unit') %>% group_by(unit_in) %>% summarize(Count=n()) %>% add_sum_row(.,name_display_icu)
#released
sys_released <- filter(individs_extended, outcome!='in_hospital_system') %>% select(patient_id, outcome) %>% group_by(outcome) %>% 
            summarize(Count=n()) %>% add_sum_row(.,name_display_out)

#current state details
#total and by age
sim_age_total <- select(current_state_write,patient_id,state) %>% group_by(state) %>% summarize(All=n()) %>% add_sum_row()
 sim_age_groups <- left_join(current_state_write, individs_extended, by='patient_id') %>% select(patient_id, state, age) %>%
                  left_join(age_groups,by='age') %>% select(patient_id,state,age_group_three) %>% 
                  group_by(state,age_group_three) %>% summarize(cnt=n()) %>%
                  pivot_wider(names_from='age_group_three',values_from='cnt') %>% 
                  mutate_at(c(2:4),~replace(., is.na(.), 0)) %>%
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

trans_raw=c('incoming','transfer','outgoing')
trans_txt=c('Incomeing','Transfer','Outgoing')
trans_labels <- tibble(trans_raw,trans_txt)

sim_hosp_turn <- select(hospital_visits,patient_id, unit_in, unit_category_all,date_in, date_out ) %>%
                      filter(!(unit_category_all=='outpatient_clinic' | unit_category_all=='emergency_room') &
                               (date_in==date_last_known_state | date_out==date_last_known_state)) %>%
              mutate_at(vars(date_in), ~replace(., date_in!=date_last_known_state, NA)) %>%
              mutate(transition=if_else(is.finite(date_in),'incoming','outgoing')) %>%
              group_by(patient_id) %>% mutate_at(vars(transition),~replace(., n()>1, 'transfer')) %>%
              ungroup() %>% mutate(value=1) %>%
              right_join(.,trans_labels,by=c('transition'='trans_raw')) %>%
              mutate_at(vars(value),~replace(.,(transition=='transfer' & is.finite(date_out)),-1)) %>%
              pivot_wider(names_from='transition', values_from = 'value') %>%
              select(unit_category_all, incoming, transfer,outgoing) %>%
              group_by(unit_category_all) %>% summarize_all(sum, na.rm=T) %>%
              mutate(all=incoming+transfer-outgoing) %>% ungroup() %>% filter(!is.na(unit_category_all)) %>% 
              add_sum_row('Turnover at hospital')

# outcome_raw <- c('diagnosed','recovered','deceased')
# outcome_text <- c('Diagnosed','Recovered','Deceased')
# outcome_labels <- tibble(outcome_raw,outcome_text)
# sys_turn <- filter(individs_extended, (date_outcome==date_last_known_state) | (date_diagnosis==date_last_known_state)) %>%
#                 select(patient_id, date_diagnosis, outcome) %>%
#                 mutate_at(vars(outcome), ~replace(.,date_diagnosis==date_last_known_state,'diagnosed')) %>%
#                 right_join(.,outcome_labels,by=c('outcome'='outcome_raw')) %>%
#                 group_by(outcome) %>% summarize(cnt=n()) %>% ungroup() %>%
#                 mutate(cnt=if_else(outcome!='diagnosed',-cnt,cnt)) %>%
#                 add_sum_row('Turnover in system')

#DfD when arriving at hospital
hosp_arr_dfd <- filter(hospital_all, unit_category=='inpatient_ward' | unit_category=='intensive_care_unit') %>%
                left_join(.,individs_extended, by='patient_id') %>% 
                select(patient_id,unit_category, date_in, date_diagnosis) %>%
                mutate(dfd=date_in-date_diagnosis) %>% group_by(unit_category) %>% 
                summarize_at(vars(dfd),funs(min,median,max))
        
#Printing
print(sys_covid_all)
print(sim_home)
print(sim_ward)
print(sim_icu)
print(sys_released)
print('Age groups')
print(sim_total_age_groups)
print('Worst State')
print(sim_total_state_worst)
print("Days from diagnosis (dfd) and days in state (dis)")
print(sim_dfd_dis)
print('Turnover from the day before')
#print(sys_turn)
print(sim_hosp_turn)

#save(sys_covid_all,sim_home,sim_ward,sim_icu,sys_released,sim_total_age_groups,sim_total_state_worst,sim_dfd_dis,sim_hosp_turn,file=overview_midnight_path)

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






