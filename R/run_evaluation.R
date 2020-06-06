library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


date_data_tmp <- as.Date('2020-05-08','%Y-%m-%d')
date_start_tmp <- as.Date('2020-03-02','%Y-%m-%d')
run_id_tmp <- 8
tracking_tmp <- FALSE
display_tmp <- FALSE

option_list <-  list(
    make_option(c("-r", "--run_id"), type="character", default=NULL, 
                help="run_id of run", metavar="character"),
    make_option(c("-d", "--date_data"), type="character", default=NULL, 
                help="date of data", metavar="character"),
    make_option(c("-s", "--date_start"), type="character", default=NULL, 
                help="start date of simulation", metavar="character"),
    make_option(c("-t", "--tracking"), type="integer", default=NULL, 
                help="should tracking info be read?", metavar="integer")
)

opt_parser <-  OptionParser(option_list=option_list)
opt <-  parse_args(opt_parser)

if(is.null(opt[['run_id']])){
    run_id <- run_id_tmp
    warning(paste0('You did not provide a current date. ',run_id,' will be used'))
}else{
    run_id <- opt[['run_id']]
}

if(is.null(opt[['date_data']])){
    date_data <- date_data_tmp
    warning(paste0('You did not provide the date of data. ',date_data,' will be used'))
}else{
    date_data <- as.Date(as.character(opt[['date_data']]),'%Y-%m-%d')
}

if(is.null(opt[['date_start']])){
    date_start<- date_start_tmp
    warning(paste0('You did not provide the date of data. ',date_start,' will be used'))
}else{
    date_start <- as.Date(as.character(opt[['date_start']]),'%Y-%m-%d')
}

if(is.null(opt[['tracking']])){
    tracking <- tracking_tmp
    warning(paste0('You did not provide action regarding tracking. Tracking = ',tracking_tmp, ' will not be used'))
}else if(opt[['tracking']]==0){
    tracking <- tracking_tmp
}else{
    tracking <- TRUE
}

if (length(opt)>1){
    display <- TRUE
}else{
    display <- display_tmp
}
path_run_info <- paste0('../input/',date_data,'_',run_id,'_run_info.csv')
run_info <- read_tsv(path_run_info,col_names = c('experiment_id','model','prior_info','splitting_variable_name','splitting_variable_values','heuristic_string'),col_types = cols())

states_in_order <- c('home','inpatient_ward','intensive_care_unit')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla')
path_historical_data <- paste0('../input/',date_data,'_historical_data.csv')
historical_data <- read_csv(path_historical_data,col_types=cols()) %>% mutate(.,date=date-1,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

simulation_summary=tibble(experiment_id=factor(character(0),levels=as.character(run_info$experiment_id)),date=as.Date(x = integer(0), origin = "1970-01-01"),state=factor(character(0),levels=states_in_order,labels=states_labels_in_order),median=numeric(),lower=numeric(),upper=numeric(),historical_value=numeric(),historical_quantile=numeric())
performance_data_list <- list()
paths_data_list <- list()
turnover_plot_list <- list()
for(id in run_info$experiment_id){
    path_simulation_data <- paste0('../output/',date_start,'_',id,'_',date_data-1,'_covid_simulation.csv')
    simulation_all <- read_csv(file = path_simulation_data,col_types=cols()) %>%
                        mutate(.,date=as.Date(date_start+day)) %>%
                        pivot_longer(.,-matches('date|day'),names_to='state',values_to ='count') %>%
                        filter(.,state %in% c('home','inpatient_ward','intensive_care_unit')) %>%
                        mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))
    
    simulation_summary <- inner_join(simulation_all,historical_data,by=c('date','state'),suffix=c('_sim','_historical')) %>%
                            group_by(.,date,state) %>%
                            summarize(median=median(count_sim),
                                      lower=quantile(count_sim,probs=0.025),
                                      upper=quantile(count_sim,probs=0.975),
                                      historical_value=unique(count_historical),
                                      historical_quantile=ecdf(count_sim)(historical_value)) %>%
                            ungroup() %>%
                            mutate(experiment_id=factor(as.character(id),levels=as.character(run_info$experiment_id))) %>%
                            select(experiment_id,date,state,median,lower,upper,historical_value,historical_quantile) %>%
                            bind_rows(simulation_summary,.)
}
if(tracking){
    for(id in run_info$experiment_id){
        path_to_tracking_data <- paste0('../output/',date_start,'_',id,'_tracking_info.csv')
        tracking_info <- read_csv(path_to_tracking_data,col_types=cols())
        paths_data_list[[id]] <- group_by(tracking_info,person_id,sim_no) %>%
            filter(transition!='state_current' & (transition != 'state_out' | (transition == 'state_out' & date==min(date[transition=='state_out'])))) %>%
            filter(tail(state,1) %in% c('recovered','death')) %>%
            summarise(path=paste(state,collapse=' -> ')) %>%
            ungroup() %>%
            mutate(experiment_id=id) %>%
            select(experiment_id,person_id,sim_no,path)
        turnover <- filter(tracking_info,transition!='state_current') %>%
            group_by(sim_no,date,transition,state) %>%
            summarise(count=n()) %>%
            group_by(date,transition,state) %>%
            summarise(median=median(count),lower=quantile(count,probs=0.025),upper=quantile(count,probs=0.975))%>%
            ungroup() %>%
            mutate(experiment_id=id) %>%
            select(experiment_id,date,state,transition,median,lower,upper)
        turnover_plot_list[[id]] <- ggplot(turnover,aes(date,median,fill=transition)) + geom_col(position='dodge') + facet_wrap(~state,scales='free')
        
    }
}

performance_dat <- group_by(simulation_summary,experiment_id,state) %>%
                        summarise(mse=mean((median-historical_value)^2),
                                  days_from_peak=date[which.max(median)]-date[which.max(historical_value)],
                                  peak_diff=max(median)-max(historical_value)) %>%
                        ungroup() %>%
                        select(experiment_id,state,mse,days_from_peak,peak_diff)

ggplot(data=simulation_summary) +
    geom_line(aes(x=date,y=median,group=experiment_id,color=experiment_id)) +
    geom_line(aes(x=date,y=lower,group=experiment_id,color=experiment_id),linetype = "dashed") +
    geom_line(aes(x=date,y=upper,group=experiment_id,color=experiment_id),linetype = "dashed") +
    geom_point(data=historical_data,aes(date,count)) +
    facet_wrap(~state,scales='free') + 
    theme_bw()
if(display){
    ggsave(paste0('../output/run_',run_id,'.png'),device='png',width=16,height=10)
    system(paste0('open ../output/run_',run_id,'.png'))
}
#quantile errors
ggplot(simulation_summary,aes(date,historical_quantile)) +
    geom_point() +
    geom_abline(slope=0,intercept=0.5) +
    geom_abline(slope=0,intercept=0.975,linetype='dashed') +
    geom_abline(slope=0,intercept=0.025,linetype='dashed') +
    facet_wrap(~state)
ggplot(simulation_summary,aes(historical_quantile)) +
    geom_histogram() +
    facet_wrap(~state)


