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
