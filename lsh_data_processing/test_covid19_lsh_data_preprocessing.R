cmp_vectors <- function(x1,x2){
    return(all(sort(unique(x1))==sort(unique(x2))))
}


test_lsh_data_file <- function(){
    
}

test_cleaning <- function(){
    cmp_vectors(individs$patient_id,individs_raw$`Person Key`)
    #test if 
}

test_data_processing <- function(){
    
    cmp_vectors(patient_transitions$patient_id,individs_extended[individs_extended$date_diagnosis<=date_last_known_state,'patient_id',drop=T])
    #check if there are recovered patients were date of recovery is less than two weeks from diagnosis
    recovered_too_soon <- filter(individs_extended,if_else(!is.na(date_outcome),outcome=='recovered' & (date_outcome-date_diagnosis) < 14,FALSE))
    if(nrow(recovered_too_soon)!=0){
        
    }
}

test_transition_matrices <- function(){
    
}

test_current_state <- function(){
    if(nrow(filter(individs_extended,outcome=='in_hospital_system'))!=nrow(current_state)){
        warning('')
    }
    all(sort(unique(patient_transitions$patient_id))==sort(unique(current_state$patient_id)))
}

test_length_of_stay <- function(){
    
}

test_first_state <- function(){
    
}

test_posterior_predictive_distr <- function(){
    
}

#check if state worst is correct