############ ------------- Help functions ########################
write_data_health_info <- function(){
    #date of diagnosis
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
    output_string <- cat('Data health information:','\n\t-',date_diagnosis_info,'\n\t-',clinical_assessment_info,'\n\t-',recovered_info,'\n')
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

    if (nrow(setdiff(distinct_uc_data,distinct_uc_coding)>0)) {
        warning('BCS:New unit categories have been added to the LSH data file')
        do.call(cat,c(setdiff(distinct_uc_data,distinct_uc_coding),sep='\n'))
    }

    #Check if new text out categories
    distinct_toc_data <- distinct(rename(hospital_visits_raw, text_out_category_raw=`Heiti afdrifa`),text_out_category_raw) %>% filter(!is.na(text_out_category_raw))
    distinct_toc_coding <- distinct(text_out_categories,text_out_category_raw)

    if (nrow(setdiff(distinct_toc_data,distinct_toc_coding))>0) {
        warning('BCS:New text out categories have been added to the LSH data file')
        do.call(cat,c(setdiff(distinct_toc_data,distinct_toc_coding),sep='\n'))
    }

    #Check if new Covid groups
    distinct_cg_data <- distinct(rename(individs_raw, covid_group_raw=`Heiti sjúklingahóps`),covid_group_raw) %>% filter(!is.na(covid_group_raw))
    distinct_cg_coding <- distinct(covid_groups,covid_group_raw)

    if (nrow(setdiff(distinct_cg_data,distinct_cg_coding))>0) {
        warning('BCS:New COVID19 groups have been added to the LSH data file')
        do.call(cat,c(setdiff(distinct_cg_data,distinct_cg_coding),sep='\n'))
    }
    return('Finished testing data files')
}

test_cleaning <- function(){
    # compare individs$patient_id and individs_raw$`Person Key`
    # ...
    return('Success')
}

test_data_processing <- function(){
 
     #Write out data health information
    invisible(write_data_health_info())
    #check uniqueness of various tables
    test_unique_id(interview_first)
    test_unique_id(interview_last)
    test_interview_unique_id_date(interview_follow_up)
    test_interview_unique_id_date(interview_extra)
    test_unique_id(individs_extended)
    test_unique_id_date(dates_home)
    test_unique_id_date(dates_hospital)
    test_unique_id_date(recovered_transitions)
    test_unique_id_date(patient_transitions)
    #check if state worst in individs_extended is correct
    return('Finished testing data processing')
}

test_tables_for_simulation <- function(){
    #test output tables
    test_transition_matrices()
    test_current_state()
    test_length_of_stay()
    test_first_state()
    test_posterior_predictive_distr()
    return('Success')
}

