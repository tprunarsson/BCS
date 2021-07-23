library(stringr)
palette1 <- c("#E69F00", "#56B4E9", "#CC79A7", "#009E73",
              '#999999', "#F0E442", "#0072B2", "#D55E00")

#' Load csv data from github with particular date format in name
#'
#' @param type the name of dataset afer the date, e.g. length_of_stay_empirical
#' @param data_folder the path of the folder containing the data, default 'dashboard/input'
#' @param start_date the earliest date of the data available, default '2020-04-02'
#'
#' @return
#' @export
load_git_data <- function(type = c('length_of_stay_with_imputed', 'length_of_stay_with_imputed_age_simple', 'length_of_stay_empirical', 'transition_matrix', 'covid_simulation', 'prop_outpatient_clinic'),
                          data_folder = 'dashboard/input', 
                          start_date = '2020-04-02') {
    # Create URLs to csv files
    root_path <- 'https://raw.githubusercontent.com/tprunarsson/BCS/master'
    files <- sprintf('%s_%s.csv', seq.Date(from = as.Date(start_date), to = Sys.Date(), by = 1), type)
    
    #Get csv data to a list and row-bind all non-null results
    data_list <- list()
    for (file in files) {
        github_url <- file.path(root_path, data_folder, file)
        if (RCurl::url.exists(github_url)) {
            data_list[[file]] <- try(read.csv(github_url), silent = TRUE)
        } else {
            next
        }
        # Add state column for transition data
        if (grepl('transition', type)) {
            data_list[[file]]$state <- names(data_list[[file]])
        }
        # Add date column if file existed. Remove from list if file didn't exist
        if (typeof(data_list[[file]]) == 'list') {
            data_list[[file]]$date <- as.Date(str_extract(file, '\\d{4}-\\d{2}-\\d{2}'))
        } else {
            data_list[[file]] <- NULL
        }
    }
    bind_rows(data_list)
}


#' Recode the state column to have Icelandic names
#'
#' @param df_with_state the dataframe containing the state column
#'
#' @return
#' @export
recode_states <- function(df_with_state) {
    # stopifnot(nrow(df_with_state)>0)
    # stopifnot('state' %in% names(df_with_state))
    try(
        df_with_state %>% 
            mutate(state = state %>% recode(home='Heimaeinangrun',
                                            outpatient='Göngudeild',
                                            inpatient_ward='Legudeild',
                                            intensive_care_unit='Gjörgæsla')),
    silent = T)
}


initial_get_and_save_data <- function() {
    bcs_data <- list()
    bcs_data$data_length_of_stay <- load_git_data(
        'length_of_stay_empirical', 
        start_date = '2020-04-08'
    ) %>% 
        recode_states
    
    bcs_data$data_all_simulation <- load_git_data(
        'covid_simulation',
        data_folder = 'simulation_history',
        start_date = '2020-04-09')
    
    # bcs_data$data_transition <- load_git_data('transition_matrix', data_folder = 'input')
    
    saveRDS(bcs_data, 'input/bcs_data.rds')
}


get_and_save_new_data <- function(bcs_data) {
    max_date_los <- max(bcs_data$data_length_of_stay$date)
    max_date_sim <- max(bcs_data$data_all_simulation$date)
    # showNotification('Sæki ný gögn...', duration = 4, id = 'msg_saeki', type = 'message')
    
    data_simulation_new <- data.frame()
    if (max_date_sim < Sys.Date()) {
        showNotification('Sæki simulation history...', duration = 2, id = 'msg_sim', type = 'message')
        data_simulation_new <- load_git_data(
            'covid_simulation',
            data_folder = 'simulation_history',
            start_date = max_date_sim + 1)
    }
    if (nrow(data_simulation_new) > 0) {
        bcs_data$data_all_simulation <- bind_rows(bcs_data$data_all_simulation,
                                                  data_simulation_new)
        
    }

    data_los_new <- data.frame()
    if (max_date_los < Sys.Date()) {
        showNotification('Sæki length of stay...', duration = 2, id = 'msg_sim', type = 'message')
        data_los_new <- load_git_data(
            'length_of_stay_empirical_age_simple', 
            start_date = max_date_los + 1)
        if (nrow(data_los_new) > 0) {
            data_los_new <- data_los_new %>% 
                recode_states()%>% 
                mutate(age_group_simple = gsub('age_', '', splitting_variable)) %>% 
                select(-splitting_variable)
            bcs_data$data_length_of_stay <- bind_rows(bcs_data$data_length_of_stay,
                                                      data_los_new)
        }
    }
    if (any(nrow(data_simulation_new) > 0, 
            nrow(data_los_new) > 0)) {
        saveRDS(bcs_data, 'input/bcs_data.rds')
        msg <- 'Ný gögn hafa verið vistuð'
    } else {
        msg <- 'Engin nýrri gögn fundust'
    }
    message(msg)
    return(msg)
}


join_prop_outpatient_to_sim <- function(window_size = 7) {
    df_with_props <- bcs_data$data_all_simulation %>% 
        mutate(date_prop = date - window_size) %>% 
        dplyr::full_join(bcs_data$prop_visits, by = c('date_prop' = 'date')) %>% 
        mutate(outpatient = home * prop_visits_last_week)
}

##For Ferguson simulation
run_ferguson_simulation <- function(splitting_variable_name,infected_distr,transitions_location='wuhan',los_location='wuhan'){
    if(transitions_location=='wuhan'){
        transition_prob <- filter(prior_transitions,location=='wuhan') %>%
            inner_join(select(age_groups,age_official,matches(paste0('^',splitting_variable_name,'$'))),by='age_official') %>%
            rename(splitting_variable=!!splitting_variable_name) %>%
            group_by(splitting_variable) %>%
            summarise(p_hospital=sum(infected*inpatient_ward)/sum(infected),
                      p_icu=sum(infected*intensive_care_unit)/sum(infected),
                      p_death=sum(infected*deceased)/sum(infected))
    }else{
        transition_prob <- patient_transitions_state_blocks %>%
            inner_join(select(individs_splitting_variables,patient_id,matches(paste0('^',splitting_variable_name,'$'))),by='patient_id') %>%
            rename(splitting_variable=!!splitting_variable_name) %>%
            group_by(patient_id,splitting_variable) %>%
            summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
            group_by(splitting_variable) %>%
            summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
            ungroup() %>%
            mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu)))
    }
    
    splitting_distribution <- get_patient_transitions_at_date('base',date_observed = as.Date('2020-05-08')) %>%
        distinct(patient_id) %>%
        inner_join(individs_splitting_variables,by='patient_id') %>%
        rename(splitting_variable=!!splitting_variable_name) %>%
        group_by(splitting_variable) %>%
        summarise(prop=n()) %>%
        ungroup() %>%
        mutate(prop=prop/sum(prop))
    dates <- seq(min(infected_distr$dags), max(infected_distr$dags), by=1)
    #dates <- seq(min(infected_distr$date),max(infected_distr$date),by=1)
    active_cases <- matrix(0,ncol=length(dates)+21,nrow=1000) 
    hospital_cases <- matrix(0,ncol=length(dates)+21,nrow=1000)
    icu_cases <- matrix(0,ncol=length(dates)+21,nrow=1000)
    for(i in 1:length(dates)){
        new_cases <- rep(infected_distr$new_cases[infected_distr$dags==dates[i]],1000)
        # new_cases <- sample(infected_distr$new_cases[infected_distr$date==dates[i]],
        #                     size = 1000,
        #                     prob=infected_distr$prob[infected_distr$date==dates[i]],
        #                     replace = T)
        active_cases[,i:(i+21-1)] <-active_cases[,i:(i+21-1)] + matrix(rep(new_cases,21),ncol=21)
        splitting_new_cases <- sapply(new_cases,function(x){
            splitting_samples <- sample(1:nrow(splitting_distribution),size=x,replace=T,prob=splitting_distribution$prop)
            splitting_samples_summary <- rep(0,nrow(splitting_distribution))
            for(s in splitting_samples){
                splitting_samples_summary[s] <- splitting_samples_summary[s]+1 
            }
            return(splitting_samples_summary)
        }) %>% t()
        hospital_cases_per_splitting <- matrix(0,nrow=nrow(splitting_new_cases),ncol=ncol(splitting_new_cases))
        for(j in 1:ncol(splitting_new_cases)){
            hospital_cases_per_splitting[,j] <- rbinom(size=splitting_new_cases[,j],n=nrow(splitting_new_cases),prob=transition_prob$p_hospital[j])
        }
        hospital_cases[,(i+7):(i+21-1)] <- hospital_cases[,(i+7):(i+21-1)] + matrix(rep(rowSums(hospital_cases_per_splitting),14),ncol=14) 
        icu_cases_per_splitting <- matrix(0,nrow=nrow(hospital_cases_per_splitting),ncol=ncol(hospital_cases_per_splitting))
        for(j in 1:ncol(hospital_cases_per_splitting)){
            icu_cases_per_splitting[,j] <- rbinom(size=hospital_cases_per_splitting[,j],n=nrow(hospital_cases_per_splitting),prob=transition_prob$p_icu[j])
        }
        icu_cases[,(i+10):(i+10+10-1)] <- icu_cases[,(i+10):(i+10+10-1)] + matrix(rep(rowSums(icu_cases_per_splitting),10),ncol=10)
    }
    hospital_dat <- tibble(date=dates,
                           state='hospital',
                           median=apply(hospital_cases,2,quantile,probs=0.5)[1:length(dates)],
                           lower=apply(hospital_cases,2,quantile,probs=0.025)[1:length(dates)],
                           upper=apply(hospital_cases,2,quantile,probs=0.975)[1:length(dates)])
    icu_dat <- tibble(date=dates,
                      state='intensive_care_unit',
                      median=apply(icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                      lower=apply(icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                      upper=apply(icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    return(bind_rows(hospital_dat,icu_dat))
}

#Nýtt Ferguson simulation með best LOS

run_ferguson_simulation_new <- function(infected_distr, los, splitting_distribution, hospital_prob=NULL){
    transition_prob <- patient_transitions_state_blocks %>%
        inner_join(select(individs_splitting_variables,patient_id,matches(paste0('^','age_official','$'))),by='patient_id') %>%
        rename(splitting_variable=!!'age_official') %>%
        group_by(patient_id,splitting_variable) %>%
        summarise(hospital=any(state!='home'),icu=any(state=='intensive_care_unit'),death=any(if_else(is.na(state_next),FALSE,state_next=='death'))) %>%
        group_by(splitting_variable) %>%
        summarise(p_hospital=sum(hospital)/n(),p_icu=sum(icu)/sum(hospital),p_death=sum(death)/n()) %>%
        ungroup() %>%
        mutate(p_icu=if_else(is.na(p_icu),0,as.numeric(p_icu)))
    
    if(!is.null(hospital_prob)){
        transition_prob <- transition_prob %>%
            mutate(p_hospital=if_else(p_hospital>hospital_prob, hospital_prob, p_hospital))
    }

    dates <- seq(min(infected_distr$date), max(infected_distr$date),by=1)
    
    active_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000) 
    hospital_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000)
    icu_cases <- matrix(0,ncol=length(dates)+los$vd,nrow=1000)
    for(i in 1:length(dates)){
        new_cases <- rep(infected_distr$new_cases[infected_distr$date==dates[i]],1000)
        # new_cases <- sample(infected_distr$new_cases[infected_distr$date==dates[i]],
        #                     size = 1000,
        #                     prob=infected_distr$prob[infected_distr$date==dates[i]],
        #                     replace = T)
        active_cases[,i:(i+los$vd-1)] <-active_cases[,i:(i+los$vd-1)] + matrix(rep(new_cases,los$vd),ncol=los$vd)
        splitting_new_cases <- sapply(new_cases,function(x){
            splitting_samples <- sample(1:nrow(splitting_distribution),size=x,replace=T,prob=splitting_distribution$prop)
            splitting_samples_summary <- rep(0,nrow(splitting_distribution))
            for(s in splitting_samples){
                splitting_samples_summary[s] <- splitting_samples_summary[s]+1 
            }
            return(splitting_samples_summary)
        }) %>% t()
        hospital_cases_per_splitting <- matrix(0,nrow=nrow(splitting_new_cases),ncol=ncol(splitting_new_cases))
        for(j in 1:ncol(splitting_new_cases)){
            hospital_cases_per_splitting[,j] <- rbinom(size=splitting_new_cases[,j],n=nrow(splitting_new_cases),prob=transition_prob$p_hospital[j])
        }
        hospital_cases[,(i+los$pre_lega):(i+los$vd-1)] <- hospital_cases[,(i+los$pre_lega):(i+los$vd-1)] + matrix(rep(rowSums(hospital_cases_per_splitting),los$ld),ncol=los$ld) 
        icu_cases_per_splitting <- matrix(0,nrow=nrow(hospital_cases_per_splitting),ncol=ncol(hospital_cases_per_splitting))
        for(j in 1:ncol(hospital_cases_per_splitting)){
            icu_cases_per_splitting[,j] <- rbinom(size=hospital_cases_per_splitting[,j],n=nrow(hospital_cases_per_splitting),prob=transition_prob$p_icu[j])
        }
        icu_cases[,(i+los$pre_lega+los$pre_icu):(i+los$pre_lega+los$pre_icu+los$gd-1)] <- icu_cases[,(i+los$pre_lega+los$pre_icu):(i+los$pre_lega+los$pre_icu+los$gd-1)] + matrix(rep(rowSums(icu_cases_per_splitting),los$gd),ncol=los$gd)
    }
    home_dat <- tibble(date=dates,
                       state='home',
                       median=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                       lower=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                       upper=apply(active_cases-hospital_cases-icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    hospital_dat <- tibble(date=dates,
                           state='hospital',
                           median=apply(hospital_cases,2,quantile,probs=0.5)[1:length(dates)],
                           lower=apply(hospital_cases,2,quantile,probs=0.025)[1:length(dates)],
                           upper=apply(hospital_cases,2,quantile,probs=0.975)[1:length(dates)])
    icu_dat <- tibble(date=dates,
                      state='intensive_care_unit',
                      median=apply(icu_cases,2,quantile,probs=0.5)[1:length(dates)],
                      lower=apply(icu_cases,2,quantile,probs=0.025)[1:length(dates)],
                      upper=apply(icu_cases,2,quantile,probs=0.975)[1:length(dates)])
    return(bind_rows(home_dat,hospital_dat,icu_dat))
}

openHTML <- function(file_name){
    browseURL(paste0('file://', file.path(getwd(), file_name)))
}

# Plot functions
plot_ferguson_prediction_state <- function(state_f, prediction_dat, color_state){
    if(nrow(prediction_dat[prediction_dat$state==state_f, ]) < 1) {
        ggplot() +
            geom_text(aes(x=10, y = 10, label = 'Spá vantar')) +
            lims(x=c(0,30), y=c(0,100)) +
            theme_minimal()
        stop()
    }
    prediction_dat <- gather(prediction_dat, key, value, median, upper) %>%
        mutate(key = key %>% recode(median = "Líkleg spá",
                                    upper = "Svartsýn spá"))
    ggplot(filter(prediction_dat, state==state_f), aes(x=date, y=value)) + 
        geom_line(aes(lty=key, alpha=key), col = color_state) + 
        scale_linetype_manual(values=c("solid", "dashed")) +
        labs(x = '', y = 'Fjöldi', linetype = '', alpha = '') +
        scale_alpha_manual(values = c(1, 0.7)) +
        theme_minimal()
}

plot_age_distribution <- function(age_distribution){
    age_distribution <- age_distribution %>% 
        mutate(splitting_variable=str_replace(splitting_variable, "age_", "")) %>% 
        mutate(prop=100*round(prop, 3)) %>%
        rename(Aldur=splitting_variable, Prósenta=prop)
    
    ggplot(data=age_distribution, aes(x=Aldur, y=Prósenta)) + geom_col(col="#84a9ac", fill="#84a9ac") + 
        labs(x=NULL, y="Prósenta") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
}

plot_skyrsla <- function(state_f, prediction_dat, historical_dat, color_state){
    if(nrow(prediction_dat[prediction_dat$state==state_f, ]) < 1) {
        ggplot() +
            geom_text(aes(x=10, y = 10, label = 'Spá vantar')) +
            lims(x=c(0,30), y=c(0,100)) +
            theme_minimal()
        stop()
    }
    prediction_dat <- gather(prediction_dat, key, value, median, upper, lower) %>%
        mutate(key = key %>% recode(median = "Líkleg spá",
                                    upper = "Svartsýn spá",
                                    lower = "Bjartsýn spá")) %>%
        mutate(value=round(value))
    prediction_dat$key <- factor(prediction_dat$key, levels = c("Svartsýn spá", "Líkleg spá", "Bjartsýn spá"))
    ggplot(filter(prediction_dat, state==state_f), aes(x=date, y=value)) + 
        geom_line(aes(lty=key, alpha=key), col = color_state) + 
        scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
        labs(x = '', y = 'Fjöldi', linetype = '', alpha = '') +
        scale_alpha_manual(values = c(0.7, 1, 0.7)) +
        theme_minimal() + 
        #labs(subtitle = prediction_dat$best_alpha[1]) + 
        geom_point(data=filter(historical_dat, state==state_f), aes(y=count, label="Söguleg gögn")) + 
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
}

plot_outpatient <- function(prediction_dat, state, color_state, historical_dat, date_history_filter){
    dat <- prediction_dat %>% filter(key==state) %>% 
        mutate(quantile = quantile %>% recode(median = "Líkleg spá",
                                    upper = "Svartsýn spá",
                                    lower = "Bjartsýn spá")) %>%
        mutate(value=round(value))
    dat$quantile <- factor(dat$quantile, levels = c("Svartsýn spá", "Líkleg spá", "Bjartsýn spá"))
    historical_dat <- historical_dat %>% filter(date>date_history_filter)
    ggplot(dat, aes(x=date, y=value)) + 
        geom_line(aes(lty=quantile, alpha=quantile), col = color_state) + 
        scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
        labs(x = '', y = 'Fjöldi', linetype = '', alpha = '') +
        scale_alpha_manual(values = c(0.7, 1, 0.7)) +
        theme_minimal() + 
        geom_point(data=filter(historical_dat, key==state), aes(y=count, label="Söguleg gögn")) + 
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
}
