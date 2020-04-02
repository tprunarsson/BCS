nrow(filter(individs_extended,outcome=='in_hospital_system'))==nrow(current_state)
nrow(distinct(patient_transitions,patient_id))==nrow(current_state)
