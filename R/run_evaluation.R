oldw <- getOption("warn")
options(warn = -1)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
options(warn = oldw)

options(dplyr.summarise.inform = FALSE) #Óþolandi skilaboð

date_data_tmp <- as.Date('2020-09-30','%Y-%m-%d') #BREYTA með nýjum lsh gögnum
date_start_tmp <- as.Date('2020-09-30','%Y-%m-%d')
run_id_tmp <- 17
tracking_tmp <- FALSE
display_tmp <- TRUE
wave_tmp <- 1

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
    warning(paste0('You did not provide a run id. ',run_id,' will be used'))
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
    warning(paste0('You did not provide action regarding tracking. Tracking = ',tracking_tmp, ' will be used'))
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

if(date_start>ymd("2020-07-23")){
    wave <- 2
} else{
    wave <- wave_tmp
}

display <- FALSE

path_run_info <- paste0('../input/',date_data,'_',run_id,'_run_info.csv')
run_info <- read_tsv(path_run_info,col_names = c('experiment_id','model','prior_info','splitting_variable_name','splitting_variable_values','heuristic_string'),col_types = cols())
path_experiment_description <- '../lsh_data_processing/experiment_template.xlsx'
path_historical_data <- paste0('../input/',date_data,'_historical_data.csv')

states_in_order <- c('home','inpatient_ward','intensive_care_unit')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla')

historical_data <- read_csv(path_historical_data,col_types=cols()) %>% mutate(.,date=date-1,state=factor(state,levels=states_in_order,labels=states_labels_in_order))
experiment_description <- read_excel('../lsh_data_processing/experiment_template.xlsx',sheet='experiment_description') %>%
                            mutate(label_icelandic=if_else(is.na(label_icelandic),'dummy',label_icelandic)) %>%
                            filter(experiment_id %in% run_info$experiment_id)

simulation_summary=tibble(experiment_id=factor(character(0),levels=as.character(run_info$experiment_id)),date=as.Date(x = integer(0), origin = "1970-01-01"),state=factor(character(0),levels=states_in_order,labels=states_labels_in_order),median=numeric(),lower=numeric(),upper=numeric(),historical_value=numeric(),historical_quantile=numeric())
performance_data_list <- list()
paths_data_list <- list()
turnover_plot_list <- list()
for(id in run_info$experiment_id){
    experiment_name <- experiment_description$label_icelandic[experiment_description$experiment_id==id]
    path_simulation_data <- paste0('../output/',date_start,'_',id,'_',date_data-1,'_covid_simulation.csv')
    simulation_all <- read_csv(file = path_simulation_data,col_types=cols()) %>%
                        mutate(.,date=as.Date(date_start+day)) %>%
                        pivot_longer(.,-matches('date|day'),names_to='state',values_to ='count') %>%
                        filter(.,state %in% c('home','inpatient_ward','intensive_care_unit')) %>%
                        mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))
    
    ## breytti inner_join í left_join fyrir bylgju 2
    simulation_summary <- left_join(simulation_all,historical_data,by=c('date','state'),suffix=c('_sim','_historical')) %>%
    #simulation_summary <- inner_join(simulation_all,historical_data,by=c('date','state'),suffix=c('_sim','_historical')) %>%
                            group_by(.,date,state) %>%
                            summarize(median=median(count_sim),
                                      lower=quantile(count_sim,probs=0.025),
                                      upper=quantile(count_sim,probs=0.975),
                                      historical_value=unique(count_historical),
                                      historical_quantile=ecdf(count_sim)(historical_value)) %>%
                            ungroup() %>%
                            mutate(experiment_id=factor(as.character(id),levels=as.character(run_info$experiment_id)),
                                   experiment_name=factor(as.character(experiment_name),levels=unique(experiment_description$label_icelandic))) %>%
                            select(experiment_id,experiment_name,date,state,median,lower,upper,historical_value,historical_quantile) %>%
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

#2. byglja
if(wave==2){
    historical_data <- historical_data %>% filter(date>ymd("2020-07-23")) 
}
if(wave==1){
    historical_data <- historical_data %>% filter(date<ymd("2020-07-23"))
}

ggplot(data=simulation_summary) +
    geom_line(aes(x=date,y=median,group=experiment_id,color=experiment_name)) +
    geom_line(aes(x=date,y=lower,group=experiment_id,color=experiment_name),linetype = "dashed") +
    geom_line(aes(x=date,y=upper,group=experiment_id,color=experiment_name),linetype = "dashed") +
    geom_point(data=historical_data,aes(date,count)) +
    #geom_point(data=historical_data_filtered,aes(date,count)) +
    facet_wrap(~state,scales='free') +
    xlab('Dagsetning') + 
    ylab('Fjöldi')+
    theme_bw()+
    theme(legend.title=element_blank())
if(display){
    ggsave(paste0('../output/run_',run_id,'.png'),device='png',width=16,height=10)
    system(paste0('open ../output/run_',run_id,'.png'))
}

path_simulation_summary_output <- paste0("../output/", date_data, "_", date_start, "_", run_id, "_simulation_summary.csv")
write.table(simulation_summary, path_simulation_summary_output, sep=',',row.names=FALSE,quote=FALSE)
#quantile errors
# ggplot(simulation_summary,aes(date,historical_quantile)) +
#     geom_point() +
#     geom_abline(slope=0,intercept=0.5) +
#     geom_abline(slope=0,intercept=0.975,linetype='dashed') +
#     geom_abline(slope=0,intercept=0.025,linetype='dashed') +
#     facet_wrap(~state)
# ggplot(simulation_summary,aes(historical_quantile)) +
#     geom_histogram() +
#     facet_wrap(~state)

simulation_summary_home <- filter(simulation_summary, state=="Heimaeinangrun")
simulation_summary_inpatient <- filter(simulation_summary, state=="Legudeild")
simulation_summary_icu <- filter(simulation_summary, state=="Gjörgæsla")

MSE_home <- mean((simulation_summary_home$historical_value - simulation_summary_home$median)^2, na.rm = TRUE)
cat(paste("MSE home:", round(MSE_home, 2), "\n"))
MSE_inpatient <- mean((simulation_summary_inpatient$historical_value - simulation_summary_inpatient$median)^2, na.rm = TRUE)
cat(paste("MSE inpatient ward:", round(MSE_inpatient,2), "\n"))
MSE_icu <- mean((simulation_summary_icu$historical_value - simulation_summary_icu$median)^2, na.rm = TRUE)
cat(paste("MSE icu:", round(MSE_icu, 2), "\n"))

# tmp <- run_ferguson_simulation2(infected_distr, dagar) %>%
#   mutate(., state=if_else(state=="hospital", "inpatient_ward", state))
# tmp2 <- inner_join(tmp,historical_data,by=c('date','state'),suffix=c('_sim','_historical'))
# tmp2_home <- filter(tmp2, state=="home")
# tmp2_inpatient <- filter(tmp2, state=="inpatient_ward")
# tmp2_icu <- filter(tmp2, state=="intensive_care_unit")
# MSE_home <- mean((tmp2_home$median-tmp2_home$count)^2)
# MSE_inpatient <- mean((tmp2_inpatient$median-tmp2_inpatient$count)^2)
# MSE_icu <- mean((tmp2_icu$median-tmp2_icu$count)^2)
# MSE <- mean((tmp2$median-tmp2$count)^2)

