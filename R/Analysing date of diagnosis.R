# need to run lsh_covid19_lsh_data_processing before

tmp <- left_join(individs,hospital_visit_first_date,by='patient_id') %>%
  mutate(source_date_diagnosis='hospital_visit_first') %>%
  left_join(.,interview_extra_first_date,by='patient_id') %>%
  mutate(.,date_diagnosis_tmp=pmin(min_date_in,min_date_clincial_assessment,na.rm=TRUE)) %>%
  mutate_at(vars(source_date_diagnosis), 
            ~replace(.,date_diagnosis_tmp==min_date_clincial_assessment,'interview_extra_first')) %>%
  select(.,-min_date_clincial_assessment,-min_date_in) %>%
  left_join(.,select(interview_first,-date_clinical_assessment,-clinical_assessment),by='patient_id') %>%
  mutate(.,date_diagnosis=pmin(date_diagnosis_tmp,date_diagnosis,na.rm=TRUE)) %>%
  mutate_at(vars(source_date_diagnosis),~replace(.,date_diagnosis!=date_diagnosis_tmp,'interview_first')) %>%
  mutate_at(vars(source_date_diagnosis), ~replace(.,!is.finite(date_diagnosis),'missing'))

cmp_with_interview_first <- left_join(tmp, interview_first,by='patient_id') %>% 
                                  select(patient_id, source_date_diagnosis, date_diagnosis.x, date_diagnosis.y) %>% 
                                  filter(date_diagnosis.x!=date_diagnosis.y) %>% 
                                  group_by(source_date_diagnosis) %>%
                                  summarize(cnt=n())