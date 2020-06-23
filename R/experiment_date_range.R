library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gganimate)

date_data_tmp <- as.Date('2020-05-08','%Y-%m-%d')
date_observed_start_tmp <- as.Date('2020-03-27')
date_start <- as.Date('2020-03-02')
experiment_id_tmp <- 1
fixed <- FALSE
display_tmp <- FALSE

option_list <-  list(
    make_option(c("-e", "--experiment_id"), type="character", default=NULL, 
                help="run_id of run", metavar="character"),
    make_option(c("-d", "--date_data"), type="character", default=NULL, 
                help="date of data", metavar="character"),
    make_option(c("-o", "--date_observed_start"), type="character", default=NULL, 
                help="start date of simulation", metavar="character"),
    make_option(c("-t", "--tracking"), type="integer", default=NULL, 
                help="should tracking info be read?", metavar="integer")
)

opt_parser <-  OptionParser(option_list=option_list)
opt <-  parse_args(opt_parser)

if(is.null(opt[['experiment_id']])){
    experiment_id <- experiment_id_tmp
    warning(paste0('You did not provide an experiment ID. ',experiment_id,' will be used'))
}else{
    experiment_id <- opt[['experiment_id']]
}

if(is.null(opt[['date_data']])){
    date_data <- date_data_tmp
    warning(paste0('You did not provide date of data. ',date_data,' will be used'))
}else{
    date_data <- as.Date(as.character(opt[['date_data']]),'%Y-%m-%d')
}

if(is.null(opt[['date_observed_start']])){
    date_observed_start <- date_observed_start_tmp
    warning(paste0('You did not provide date observed start. ',date_observed_start,' will be used'))
}else{
    date_observed_start <- as.Date(as.character(opt[['date_observed_start']]),'%Y-%m-%d')
}

if (length(opt)>1){
    display <- TRUE
}else{
    display <- display_tmp
}
states_in_order <- c('home','inpatient_ward','intensive_care_unit')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla')
path_historical_data <- paste0('../input/',date_data,'_historical_data.csv')
historical_data <- read_csv(path_historical_data,col_types=cols()) %>% mutate(.,date=date,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

simulation_summary <- tibble(date_observed=as.Date(x = integer(0), origin = "1970-01-01"),date=as.Date(x = integer(0), origin = "1970-01-01"),state=factor(character(0),levels=states_in_order,labels=states_labels_in_order),median=numeric(),lower=numeric(),upper=numeric(),historical_value=numeric())
dates_observed <- seq(date_observed_start,date_data-2,by=1)
dates_start <- if(fixed) rep(date_start,length(dates_observed)) else seq(date_observed_start+1,date_data-1,by=1)
dates_expanded <- expand_grid(date_observed=seq(date_observed_start,date_data-1,by=1),date=seq(date_start,date_data-1,by=1),state=factor(states_in_order,levels=states_in_order,labels=states_labels_in_order))
for(i in seq_along(dates_observed)){
    path_simulation_data <- paste0('../output/',dates_start[i],'_',experiment_id,'_',dates_observed[i],'_covid_simulation.csv')
    simulation_all <- read_csv(file = path_simulation_data,col_types=cols()) %>%
                        mutate(.,date=as.Date(dates_start[i]+day)) %>%
                        pivot_longer(.,-matches('date|day'),names_to='state',values_to ='count') %>%
                        filter(.,state %in% c('home','inpatient_ward','intensive_care_unit')) %>%
                        mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order)) %>%
                        mutate(date_observed=dates_observed[i])
    
    simulation_summary <- inner_join(simulation_all,historical_data,by=c('date','state'),suffix=c('_sim','_historical')) %>%
                            group_by(.,date_observed,date,state) %>%
                            summarize(median=median(count_sim),
                                      lower=quantile(count_sim,probs=0.025),
                                      upper=quantile(count_sim,probs=0.975),
                                      historical_value=unique(count_historical)) %>%
                            ungroup() %>%
                            select(date_observed,date,state,median,lower,upper,historical_value) %>%
                            bind_rows(simulation_summary,.)
}

simulation_summary <- left_join(dates_expanded,simulation_summary,by=c('date_observed','date','state')) %>%
                        distinct() %>%
                        inner_join(historical_data,by=c('date','state')) %>%
                        mutate(historical_value=count) %>%
                        mutate(median=if_else(is.na(median),0,median),
                               lower=if_else(is.na(lower),0,lower),
                               upper=if_else(is.na(upper),0,upper)) %>%
                        select(date_observed,date,state,median,lower,upper,historical_value)
if(fixed){
    pred_animation <- ggplot(data=simulation_summary) +
        geom_line(aes(x=date,y=median)) +
        geom_line(aes(x=date,y=lower),linetype = "dashed") +
        geom_line(aes(x=date,y=upper),linetype = "dashed") +
        geom_point(data=historical_data,aes(date,count)) +
        geom_vline(aes_(xintercept=quote(date_observed)),linetype='dashed',color='red') +
        facet_wrap(~state,scales='free') + 
        theme_bw() +
        labs(title = 'date_observed: {frame_time}', x = '', y = 'Fjöldi') +
        transition_time(as.Date(date_observed))
}else{
    pred_animation <- ggplot(data=simulation_summary) +
        geom_line(aes(x=date,y=median)) +
        geom_line(aes(x=date,y=lower),linetype = "dashed") +
        geom_line(aes(x=date,y=upper),linetype = "dashed") +
        geom_point(data=historical_data,aes(date,count)) +
        geom_vline(aes_(xintercept=quote(date_observed+1)),linetype='dashed',color='red') +
        facet_wrap(~state,scales='free') + 
        theme_bw() +
        labs(title = 'date_observed: {frame_time}', x = '', y = 'Fjöldi') +
        transition_time(as.Date(date_observed))    
}

animate(pred_animation, height = 800, width = 1600,fps=3)
anim_save(paste0("~/Downloads/varying_start_prediction_",experiment_id,".gif"), animation = last_animation())
