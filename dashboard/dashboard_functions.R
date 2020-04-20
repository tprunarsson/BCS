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
load_git_data <- function(type = c('length_of_stay_with_imputed', 'length_of_stay_empirical', 'transition_matrix', 'covid_simulation', 'prop_outpatient_clinic'),
                          data_folder = 'dashboard/input', 
                          start_date = '2020-04-02') {
    # Create URLs to csv files
    root_path <- 'https://raw.githubusercontent.com/tprunarsson/BCS/master'
    files <- sprintf('%s_%s.csv', seq.Date(from = as.Date(start_date), to = Sys.Date(), by = 1), type)
    
    #Get csv data to a list and row-bind all non-null results
    data_list <- list()
    for (file in files) {
        data_list[[file]] <- try(
            read.csv(file.path(root_path, data_folder, file)), silent = TRUE
        )
        # Add state column for transition data
        if (grepl('transition', type)) {
            data_list[[file]]$state <- names(data_list[[file]])
        }
        # Add date column if file existed. Remove crom list if file didn't exist
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
    stopifnot('state' %in% names(df_with_state))
    df_with_state %>% 
        mutate(state = state %>% recode(home='Heimaeinangrun',
                                        inpatient_ward='Legudeild',
                                        intensive_care_unit='Gjörgæsla'))
}



get_and_save_data <- function() {
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


get_and_save_if_new_data <- function(bcs_data = NULL) {
    max_date_los <- coalesce(max(bcs_data$data_length_of_stay$date,'2020-04-08'))
    max_date_sim <- coalesce(max(bcs_data$data_all_simulation$date,'2020-04-09'))
    if (max_date_sim < Sys.Date()) {
        data_simulation_new <- load_git_data(
            'covid_simulation',
            data_folder = 'simulation_history',
            start_date = max_date_sim + 1)
        bcs_data$data_all_simulation <- bind_rows(bcs_data$data_all_simulation,
                                                  data_simulation_new)
    }
    if (max_date_los < Sys.Date()) {
        data_los_new <- load_git_data(
            'length_of_stay_empirical', 
            start_date = max_date_los + 1
        ) %>% 
            recode_states
        bcs_data$data_length_of_stay <- bind_rows(bcs_data$data_length_of_stay,
                                                  data_los_new)
        
    }
    if (any(exists('data_simulation_new'), exists('data_los_new'))) {
        saveRDS(bcs_data, 'input/bcs_data.rds')
        message('New data has been saved')
    } else {
        message('Data is already up to date')
    }
}
