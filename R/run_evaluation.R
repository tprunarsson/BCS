library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
current_date=as.Date('2020-04-02','%Y-%m-%d')
path_simulation_data <- paste0('../output/',current_date,'_covid_simulation.csv')
path_historical_data <- paste0('../input/',current_date,'_current_date_per_date_summary.csv')

states_in_order <- c('home','inpatient_ward','intensive_care_unit')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla')

simulation_all <- read_csv(file = path_simulation_data ) %>%
    mutate(.,date=as.Date(current_date+day)) %>%
    pivot_longer(.,-matches('date|day'),names_to='state',values_to ='count') %>%
    filter(.,state %in% c('home','inpatient_ward','intensive_care_unit')) %>%
    mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

simulation_summary <- group_by(simulation_all,day,date,state) %>%
                        summarize(median=median(count),lower=quantile(count,probs=0.025),upper=quantile(count,probs=0.975))

historical_data <- mutate(current_state_per_date,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

ggplot(data=simulation_summary,aes(x=date)) +
    geom_line(aes(x=date,y=median)) +
    geom_line(aes(x=date,y=lower),linetype = "dashed") +
    geom_line(aes(x=date,y=upper),linetype = "dashed") +
    facet_wrap(~state,scales='free') + 
    theme_bw()
