############ ------------- Help functions ########################
write_data_health_info <- function(){
    #date of diagnosis
    interview_first_filtered <- filter(interview_first, is.finite(date_diagnosis)) 
    date_diagnosis_last_info <- sprintf(paste0("Most recent date of diagnosis in forms: ", max(interview_first_filtered$date_diagnosis,na.rm=T)))
    
    interview_last_filtered <- filter(interview_last, is.finite(date_clinical_assessment)) 
    date_interview_last_info <- sprintf(paste0("Most recent date of last interview in forms: ", max(interview_last_filtered$date_clinical_assessment,na.rm=T)))
    
    num_with_date_diagnosis <- left_join(individs,interview_first, by='patient_id') %>% filter(is.finite(date_diagnosis)) %>% summarize(n()) %>% unlist()
    date_diagnosis_info <- sprintf("Number of individs missing date of diagnosis in forms: %.0f (%.1f%%)", nrow(individs)-num_with_date_diagnosis,100*(nrow(individs)-num_with_date_diagnosis)/nrow(individs))
    
    patients_hospital <- distinct(hospital_visits_filtered,patient_id)
    
    #clinical assessment
    num_with_clinical_assessment <- left_join(individs,interview_follow_up,by='patient_id') %>% filter(is.finite(date_clinical_assessment)) %>% distinct(patient_id) %>% summarize(n()) %>% unlist()
    clinical_assessment_info=sprintf("Number of individs missing clinical assessment in forms: %.0f (%.1f%%)", nrow(individs)-num_with_clinical_assessment,100*(nrow(individs)-num_with_clinical_assessment)/nrow(individs))
    
    #recovered with a final interview
    num_recovered_with_last_interview <- left_join(individs,interview_last, by='patient_id') %>% filter(covid_group=='recovered' & is.finite(date_clinical_assessment)) %>% summarize(n()) %>% unlist()
    num_recovered <- filter(individs,covid_group=='recovered') %>% summarize(n()) %>% unlist()
    recovered_info <- sprintf("Number of recovered individs missing date of last interview in forms: %.0f (%.1f%%)", num_recovered-num_recovered_with_last_interview,100*(num_recovered-num_recovered_with_last_interview)/num_recovered)
    
    #
    potential_wrong_date_diagnosis <- mutate(interview_first,diff_call=date_clinical_assessment-date_diagnosis) %>%
                                      filter(is.finite(diff_call)) %>%
                                      filter(diff_call<0) %>%
                                      select(patient_id) %>%
                                      mutate(explanation='Eftirfylgni hefst fyrir dagsetningu greiningar')
    
    potential_wrong_date_diagnosis_info <- sprintf("Number of individs with date of follow-up before date of diagnosis: %.0f (%.1f%%)",nrow(potential_wrong_date_diagnosis),100*nrow(potential_wrong_date_diagnosis)/nrow(individs))
    
    
    missing_info_interview_first <- left_join(select(individs,patient_id),select(hospital_visits_filtered,patient_id,text_out), by='patient_id') %>%
                                                                        filter(!(text_out %in% c('at_hospital','death'))) %>%
                                                                        select(-text_out) %>%
                                                                        left_join(.,interview_first,by='patient_id') %>%
                                                                        filter_at(vars(-patient_id), any_vars(is.na(.))) %>% 
                                                                        distinct(patient_id) %>%
                                                                        mutate(explanation="Vantar upplýsingar í fyrsta viðtal")
    
    missing_info_interview_first_info <- sprintf("Number of individs with missing info in first interview: %.0f (%.1f%%)",nrow(missing_info_interview_first),100*nrow(missing_info_interview_first)/nrow(individs))
    
                                    
    output_string <- cat('Data health information:','\n\t-',
                         date_diagnosis_last_info,'\n\t-',
                         date_diagnosis_info,'\n\t-',
                         potential_wrong_date_diagnosis_info,'\n\t-',
                         missing_info_interview_first_info,'\n\t-',
                         clinical_assessment_info,'\n\t-',
                         date_interview_last_info, '\n\t-',
                         recovered_info,'\n')
    return(output_string)
}

test_unique_id_date <- function(dat){
    return(nrow(distinct(dat,patient_id,date))==nrow(dat))
}

test_interview_unique_id_date <- function(dat){
    return(nrow(distinct(dat,patient_id,date_clinical_assessment))==nrow(dat))
}

test_unique_id <- function(dat){
    return(nrow(distinct(dat,patient_id))==nrow(dat))
}

test_transition_matrices <- function(){
    #test if transitions from home to recovered are as many as recovered in individs_extended
    return((filter(patient_transition_counts_all,state=='home',state_tomorrow=='recovered') %>% ungroup() %>% select(count) %>% unlist() %>%unname())==nrow(filter(individs_extended,outcome=='recovered')))
}


test_current_state <- function(){
    #test if number in current state is the same as registred in hospital system in indvidis_extended
    return(nrow(current_state)==nrow(filter(individs_extended,outcome=='in_hospital_system')))
}

test_length_of_stay <- function(){
    
}

test_first_state <- function(){
    
}

test_posterior_predictive_distr <- function(){
    
}

display_warning_if_items_not_found <- function(items,text){
  if (nrow(items)>0) {
    warning(text)
    do.call(cat,c(items,sep='\n'))
  }
}

list_hash <- function(dat,cols=names(dat),verbose=FALSE){     
    out_vec=vector('character',length=length(cols))     
    for(i in 1:length(cols)){         
         out_vec[i] <- md4(paste(dat[,cols[i]],collapse=''))     
     }     
     if(verbose){         
        return(out_vec)     
     }else{         
        return(md4(paste(out_vec,collapse='')))      
     }
}
################ ---- Test functions ---- ##################

test_lsh_data_file <- function(){
    #Check if new sheets or changes to sheet names
    distinct_s_data <- distinct(tibble(excel_sheets(file_path_data)) %>% rename(sheet_name_raw=`excel_sheets(file_path_data)`))
    distinct_s_coding <- distinct(sheet_names,sheet_name_raw)

    if ((distinct_s_data %>% summarize(n()) %>% unlist()) >
        (distinct_s_coding %>% summarize(n()) %>% unlist())) {
        warning('BCS:New sheet(s) have been added to the LSH data file')
        do.call(cat,c(setdiff(distinct_s_data,distinct_s_coding),sep='\n'))
    }

    if (!identical(sort(distinct_s_data$sheet_name_raw),sort(sheet_names$sheet_name_raw))) {
        warning('BCS: Sheet names have changed')
    }

    #Check if new unit categories
    distinct_uc_data <- distinct(rename(hospital_visits_raw, unit_category_raw=`Deild Heiti`),unit_category_raw) %>% filter(!is.na(unit_category_raw))
    distinct_uc_coding <- distinct(unit_categories,unit_category_raw)
    not_found_in_data <- setdiff(distinct_uc_data,distinct_uc_coding)
    warning_text <- 'BCS:New unit categories have been added to the LSH data file'
    display_warning_if_items_not_found(not_found_in_data,warning_text)

    #Check if new text out categories
    distinct_toc_data <- distinct(rename(hospital_visits_raw, text_out_category_raw=`Heiti afdrifa`),text_out_category_raw) %>% filter(!is.na(text_out_category_raw))
    distinct_toc_coding <- distinct(text_out_categories,text_out_category_raw)
    not_found_in_data <- setdiff(distinct_toc_data,distinct_toc_coding)
    warning_text <- 'BCS:New text out categories have been added to the LSH data file'
    display_warning_if_items_not_found(not_found_in_data,warning_text)

    #Check if new Covid groups
    distinct_cg_data <- distinct(rename(individs_raw, covid_group_raw=`Heiti sjúklingahóps`),covid_group_raw) %>% filter(!is.na(covid_group_raw))
    distinct_cg_coding <- distinct(covid_groups,covid_group_raw)
    not_found_in_data <- setdiff(distinct_cg_data,distinct_cg_coding)
    warning_text <- 'BCS:New COVID19 groups have been added to the LSH data file'
    display_warning_if_items_not_found(not_found_in_data,warning_text)
    
    return('Finished testing data files')
}

get_sequences <- function(){
  pt <- filter(patient_transitions,!is.na(state_tomorrow))
  
  allsequences = NULL
  for (p in unique(patient_transitions$patient_id)) {
    trans <- subset(patient_transitions,p==patient_transitions$patient_id)
    trans <- trans[order(trans$date,trans$state_tomorrow),]
    n = nrow(trans)
    if (n > 1) {
      idx = trans$state_tomorrow[1:n-1]!=trans$state_tomorrow[2:n]
      if (sum(idx, na.rm = T) >= 1) {
        sequence = trans$state_tomorrow[idx]
        if (sequence[length(sequence)] != trans$state_tomorrow[n])
          sequence = c(sequence,trans$state_tomorrow[n])
        allsequences = c(allsequences, paste(sequence, collapse = "->"))
      }
    }
  }
  state_sequences_in_data <- tibble(allsequences) %>% group_by(allsequences) %>% 
                              summarize(count=n()) %>% arrange(desc(count))
  return(state_sequences_in_data)
  
  # Reading same information from patient_transitions_state_blocks_summary
  # dat2 <- patient_transitions_state_blocks_summary %>% group_by(patient_id) %>% 
  #         summarise(state_trajectory=ifelse(sum(censored)==0,paste0(c(state,tail(state_next,1)),collapse='->'),
  #                                           paste0(state,collapse='->'))) %>% group_by(state_trajectory) %>% 
  #         summarise(count=n())
}

get_sequences_extended <- function(){
  patient_transitions <- mutate(patient_transitions,state=paste0(state,'-',severity),                       
                                state_tomorrow=case_when(is.na(state_tomorrow) ~ NA_character_,                                                
                                                         is.na(severity_tomorrow) ~ state_tomorrow,                                                
                                                         TRUE ~ paste0(state_tomorrow,'-',severity_tomorrow)) ) %>%     
    group_by(.,patient_id) %>% mutate(.,state_block_nr=get_state_block_numbers(state)) %>%     
    ungroup() %>%     
    filter(!is.na(state_tomorrow))
  
  allsequences = NULL
  for (p in unique(patient_transitions$patient_id)) {
    trans <- subset(patient_transitions,p==patient_transitions$patient_id)
    trans <- trans[order(trans$date,trans$state_tomorrow),]
    n = nrow(trans)
    if (n > 1) {
      idx = trans$state_tomorrow[1:n-1]!=trans$state_tomorrow[2:n]
      if (sum(idx, na.rm = T) >= 1) {
        sequence = trans$state_tomorrow[idx]
        if (sequence[length(sequence)] != trans$state_tomorrow[n])
          sequence = c(sequence,trans$state_tomorrow[n])
        allsequences = c(allsequences, paste(sequence, collapse = "->"))
      }
    }
  }
  state_sequences_in_data <- tibble(allsequences) %>% group_by(allsequences) %>% 
                              summarize(count=n()) %>% arrange(desc(count))
  return(state_sequences_in_data)
}
test_cleaning <- function(){
  test_unique_id(interview_first)
  test_unique_id(interview_last)
  test_interview_unique_id_date(interview_follow_up)
  test_interview_unique_id_date(interview_extra)
  # compare individs$patient_id and individs_raw$`Person Key`
  # ...
  return('Success')
}

test_data_processing <- function(){
 
     #Write out data health information
    invisible(write_data_health_info())
    #check uniqueness of various tables

    test_unique_id(individs_extended)
    test_unique_id_date(dates_home)
    test_unique_id_date(dates_hospital)
#    test_unique_id_date(recovered_transitions)
    test_unique_id_date(patient_transitions)
    #check if state worst in individs_extended is correct
    return('Finished testing data processing')
}

test_tables_for_simulation <- function(){
    #test output tables
#    test_transition_matrices()
    test_current_state()
    test_length_of_stay()
    test_first_state()
    test_posterior_predictive_distr()
    return('Success')
}

