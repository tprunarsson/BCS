library(readxl)
library(optparse)
library(dplyr)
library(tidyr)
library(readr)

current_date_tmp <- as.Date('2020-10-02','%Y-%m-%d')
path_simulation_tmp <- '~/projects/covid/BCS/output/'
path_summary_hospital_tmp <- '~/projects/covid/BCS/output_lsh/'
  
option_list <-  list(
  make_option(c("-c", "--current_date"), type="character", default=NULL, 
              help="current date of data being used", metavar="character")
)

opt_parser <-  OptionParser(option_list=option_list);
opt <-  parse_args(opt_parser);

if(is.null(opt[['current_date']])){
  current_date <- current_date_tmp
  path_simulation <- file_path_simulation_tmp
  path_summary_hospital <- file_path_summary_hospital_tmp
  warning(paste0('You did not provide a current date. ',current_date,' will be used'))
}else{
  current_date <- as.Date(as.character(opt[['current_date']]),'%Y-%m-%d')
  path_simulation <- "../output/"
  path_summary_hospital <- "../output_lsh/"
}

file_path_simulation <- paste0(path_simulation,current_date,'_4_', current_date-1, '_covid_simulation.csv')
#file_path_simulation <- paste0(path_simulation,current_date,'_covid_simulation.csv')
file_path_summary_hospital <- paste0(path_summary_hospital,current_date,'_covid_simulation_summary_hospital.csv')
#file_path_summary_hospital_all <- paste0('~/projects/covid/BCS/output/',current_date_tmp,'_covid_simulation_summary_all.csv')

states_in_order <- c('home','inpatient_ward','intensive_care_unit','death','recovered')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla','Andlát','Batnað')

simulation <- read_csv(file_path_simulation) %>%
              mutate(.,date=as.factor(current_date+day)) %>%
              gather(.,key='state',value='count',-date,-day) %>%
              mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

summary_all <- group_by(simulation,date,state) %>% summarize(median=round(median(count)),
                                              quantile25=round(quantile(count,probs=0.25)),
                                              quantile75=round(quantile(count,probs=0.75)),
                                              quanitle975=round(quantile(count,probs=0.975)))

summary_hospital <- filter(summary_all,state=='Legudeild' | state=='Gjörgæsla' )

# NOTE: Need to change order before writing to file

write.table(summary_hospital,file_path_summary_hospital,sep=',',row.names=F,quote=F)
#write.table(summary_hospital_all,file_path_summary_hospital_all,sep=',',row.names=F,quote=F)