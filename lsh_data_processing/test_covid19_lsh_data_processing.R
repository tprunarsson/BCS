cmp_vectors <- function(x1,x2){
    return(all(sort(unique(x1))==sort(unique(x2))))
}


test_lsh_data_file <- function(){
    # #Check if new sheets or changes to sheet names
    # distinct_s_data <- distinct(tibble(excel_sheets(file_path_data)) %>% rename(sheet_name_raw=`excel_sheets(file_path_data)`))
    # distinct_s_coding <- distinct(sheet_names,sheet_name_raw)
    # 
    # if ((distinct_s_data %>% summarize(n()) %>% unlist()) > 
    #     (distinct_s_coding %>% summarize(n()) %>% unlist())) {
    #     warning('BCS:New sheet(s) have been added to the LSH data file')
    #     do.call(cat,c(setdiff(distinct_s_data,distinct_s_coding),sep='\n'))
    # }
    # 
    # if (identical(sheets_in_datafile,sheet_names)) {
    #     warning('BCS: Sheet names have changed')
    # }
    # 
    # #Check if new unit categories
    # distinct_uc_data <- distinct(rename(hospital_visits_raw, unit_category_raw=`Deild Heiti`),unit_category_raw) %>% filter(!is.na(unit_category_raw))
    # distinct_uc_coding <- distinct(unit_categories,unit_category_raw)
    # 
    # if ((distinct_uc_data %>% summarize(n()) %>% unlist()) > 
    #     (distinct_uc_coding %>% summarize(n()) %>% unlist())) {
    #     warning('BCS:New unit categories have been added to the LSH data file')
    #     do.call(cat,c(setdiff(distinct_uc_data,distinct_uc_coding),sep='\n'))
    # }
    # 
    # #Check if new text out categories
    # distinct_toc_data <- distinct(rename(hospital_visits_raw, text_out_category_raw=`Heiti afdrifa`),text_out_category_raw) %>% filter(!is.na(text_out_category_raw))
    # distinct_toc_coding <- distinct(text_out_categories,text_out_category_raw)
    # 
    # if ((distinct_toc_data %>% summarize(n()) %>% unlist()) > 
    #     (distinct_toc_coding %>% summarize(n()) %>% unlist())) {
    #     warning('BCS:New text out categories have been added to the LSH data file')
    #     do.call(cat,c(setdiff(distinct_toc_data,distinct_toc_coding),sep='\n'))
    # }
    # 
    # #Check if new Covid groups
    # distinct_cg_data <- distinct(rename(individs_raw, covid_group_raw=`Heiti sjúklingahóps`),covid_group_raw) %>% filter(!is.na(covid_group_raw))
    # distinct_cg_coding <- distinct(covid_groups,covid_group_raw)
    # 
    # if ((distinct_cg_data %>% summarize(n()) %>% unlist()) > 
    #     (distinct_cg_coding %>% summarize(n()) %>% unlist())) {
    #     warning('BCS:New COVID19 groups have been added to the LSH data file')
    #     do.call(cat,c(setdiff(distinct_cg_data,distinct_cg_coding),sep='\n'))
    # }
}

test_cleaning <- function(){
    cmp_vectors(individs$patient_id,individs_raw$`Person Key`)
    #test if 
}

test_data_processing <- function(){
    # 
    # cmp_vectors(patient_transitions$patient_id,individs_extended[individs_extended$date_diagnosis<=date_last_known_state,'patient_id',drop=T])
    # #check if there are recovered patients were date of recovery is less than two weeks from diagnosis
    # recovered_too_soon <- filter(individs_extended,if_else(!is.na(date_outcome),outcome=='recovered' & (date_outcome-date_diagnosis) < 14,FALSE))
    # if(nrow(recovered_too_soon)!=0){
    #     
    # }
}

test_transition_matrices <- function(){
    
}

test_current_state <- function(){
    # if(nrow(filter(individs_extended,outcome=='in_hospital_system'))!=nrow(current_state)){
    #     warning('')
    # }
    # all(sort(unique(patient_transitions$patient_id))==sort(unique(current_state$patient_id)))
}

test_length_of_stay <- function(){
    
}

test_first_state <- function(){
    
}

test_posterior_predictive_distr <- function(){
    
}

#check if state worst is correct