library(readxl)

current_date_tmp <- as.Date('2020-04-03','%Y-%m-%d')
file_path_simulation <- paste0('~/projects/covid/BCS/output/',current_date_tmp,'_covid_simulation.csv')
file_path_summary_hospital <- paste0('~/projects/covid/BCS/output_lsh/',current_date_tmp,'_covid_simulation_summary_hospital.csv')
file_path_summary_hospital_all <- paste0('~/projects/covid/BCS/output/',current_date_tmp,'_covid_simulation_summary_all.csv')

states_in_order <- c('home','inpatient_ward','intensive_care_unit','death','recovered')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla','Andlát','Batnað')

simulation <- read_csv(file_path_simulation) %>%
              mutate(.,date=as.factor(current_date+day)) %>%
              gather(.,key='state',value='count',-date,-day) %>%
              mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

    
summary_all <- group_by(simulation,date,state) %>% summarize(median=round(median(count)),
                                              quantile25=round(quantile(count,probs=0.25)),
                                              quantile75=round(quantile(count,probs=0.75)),
                                              quanitle95=round(quantile(count,probs=0.95)))

summary_hospital <- filter(summary_all,state=='inpatient_ward' | state=='intensive_care_unit' )

write.table(summary_hospital,file_path_summary_hospital,sep=',',row.names=F,col.names=states,quote=F)
write.table(summary_hospital_all,file_path_summary_hospital_all,sep=',',row.names=F,col.names=states,quote=F)
