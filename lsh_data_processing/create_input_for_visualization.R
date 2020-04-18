############## ----- Length of stay distribution by state and splitting variable ----- ############## 
get_length_of_stay_empirical <- function(type=''){
    if(type=='clinical_assessment_included'){
        transitions_state_blocks_summary <- mutate(patient_transitions_state_blocks,state=paste0(state,'-',severity)) %>%
            select(.,patient_id,state_with_severity_block_nr,state,censored,state_duration) %>%
            rename(state_block_nr=state_with_severity_block_nr)
    }else{
        transitions_state_blocks_summary <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
            summarize(censored=censored[which.max(state_with_severity_block_nr)],state_duration=sum(state_duration)) %>%
            ungroup()
    }
    state_blocks_with_splitting_variable <- inner_join(select(individs_extended,patient_id,splitting_variable),
                                                       select(transitions_state_blocks_summary,patient_id,state,censored,state_duration),
                                                       by='patient_id')
    length_of_stay_empirical <- group_by(state_blocks_with_splitting_variable,state,splitting_variable,censored,state_duration) %>%
        summarize(count=n()) %>%
        arrange(state,censored,splitting_variable) %>%
        ungroup()
    return(length_of_stay_empirical)
}