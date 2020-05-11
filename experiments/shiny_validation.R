#!/usr/bin/env Rscript
invisible(Sys.setlocale("LC_ALL","IS_is"))
suppressPackageStartupMessages({
    require(optparse)
    require(readxl)
    require(dplyr)
    require(tidyr)
    require(readr)
})

option_list <-  list(
    make_option(c("-s", "--date_start"), type="character", default=NULL, 
                help="start date of simulation", metavar="character"),
    make_option(c("-r", "--run_id"), type="integer", default=NULL, 
                help="run_id to identify run of a set of experiments", metavar="integer"),
    make_option(c("-a", "--use_scenario"),action='store_true', default=FALSE, 
                help="scenario used or not"),
    make_option(c("-p", "--path"), type="character", default=NULL, 
                help="path to root", metavar="character")
    
)

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['date_start']])){
    stop(paste0('Error: You must provide a start date. '))
}else{
    date_start <- opt[['date_start']]
}
if(is.null(opt[['run_id']])){
    stop(paste0('Error: You must provide a run_id'))
}else{
    run_id <- 1
}
if(is.null(opt[['use_scenario']])){
    stop(paste0('use_scenario flag must be active'))
}
if(is.null(opt[['path']])){
    stop(paste0('You must provide a path'))
}else{
    path_to_root <- opt[['path']]
}
scenario_first_state_filename <- paste0(date_start,'_',run_id,'_scenario_first_state.csv')
scenario_infected_filename <- paste0(date_start,'_',run_id,'_scenario_infected.csv')
path_input <- paste0(path_to_root,'/input/')
if(!(scenario_first_state_filename %in% list.files(path_input))){
    stop('Error: scenario first state file does not exist')
}else if(!(scenario_infected_filename %in% list.files(path_input))){
    stop('Error: scenario infected file does not exist')
}else{
    scenario_first_state <- read_csv(paste0(path_input,scenario_first_state_filename),col_types=cols())
    scenario_infected <- read_csv(paste0(path_input,scenario_infected_filename),col_types=cols())
}


###### scenario_infected tests ##################
if(class(scenario_infected$date_diagnosis)!='Date'){
    stop('Error: date_diagnosis column in scenario_infected must be of class Date')
}else if(any(scenario_infected$count%%1!=0)){
    stop('Error: count column in scenario_infected must be an integer')
}

if(min(scenario_infected$date_diagnosis)!=date_start){
    stop('Error: start date column in scenario_infected file must match date_start input argument') 
}

###### scenario_first_state tests ##################
if(class(scenario_first_state$initial_state)!='character'){
    stop('Error: initial_state column in scenario_first_state must be of class character')
}else if(class(scenario_first_state$splitting_variable)!='character'){
    stop('Error: splitting_variable column in scenario_first_state must be of class character')
}else if(any(scenario_first_state$count%%1!=0)){
    stop('Error: count column in scenario_infected must be an integer')
}

if(any(scenario_first_state$initial_state!=c('home','home','inpatient_ward','inpatient_ward','intensive_care_unit','intensive_care_unit'))){
    stop('Error: initial_state column in scenario_first_state file is in wrong format') 
}else if(any(scenario_first_state$splitting_variable!=c('age_0-50','age_51+','age_0-50','age_51+','age_0-50','age_51+'))){
    stop('Error: splitting_variable column in scenario_first_state file is in wrong format')
}

print('tests_passed')






