library(ggplot2)
library(readr)
current_date=as.Date('2020-03-30','%Y-%m-%d')
path_data_upper <-'covid_run_0_2020-03-30.csv'
states_in_order <- c('home','inpatient_ward','intensive_care_unit','death','recovered')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla','Andlát','Batnað')
upper_run <- read_csv(file = path_data_upper) %>%
                mutate(.,date=as.factor(current_date+day)) %>%
                gather(.,key='state',value='count',-date,-day) %>%
                mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

ggplot(upper_run, aes(x = date, y = count)) + geom_boxplot(col='blue4') + facet_wrap(~state,scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.background = element_rect(fill='white',colour='black'),
            panel.grid.major = element_line(colour='gray95'),panel.grid.minor = element_line(colour='gray95')) + xlab('') + ylab('')
ggsave(filename='simulations_from_upper_prediction.png',device='png',width=18,height=11)

path_data_median <-'covid_run_1_2020-03-30.csv'
median_run <- read_csv(file = path_data_median) %>%
    mutate(.,date=as.factor(current_date+day)) %>%
    gather(.,key='state',value='count',-date,-day) %>%
    mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

ggplot(median_run, aes(x = date, y = count)) + geom_boxplot(col='blue4') + facet_wrap(~state,scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),panel.grid.minor = element_line(colour='gray95')) + xlab('') + ylab('')
ggsave(filename='simulations_from_median_prediction.png',device='png',width=18,height=11)



# ggplot(upper_run, aes(x = date, y = inpatient_ward)) + geom_boxplot()
# ggplot(upper_run, aes(x = date, y = intensive_care_unit)) + geom_boxplot()
# ggplot(upper_run, aes(x = date, y = death)) + geom_boxplot()
# ggplot(upper_run, aes(x = date, y = recovered)) + geom_boxplot()
# 
# path_data <-'covid_run_1_2020-03-30.csv'
# median_run <- read.csv2(file = path_data, sep=",")
# median_run$day = as.factor(median_run$day)
# ggplot(median_run, aes(x = day, y = home)) + geom_boxplot()
# ggplot(median_run, aes(x = day, y = inpatient_ward)) + geom_boxplot()
# ggplot(median_run, aes(x = day, y = intensive_care_unit)) + geom_boxplot()
# ggplot(median_run, aes(x = day, y = death)) + geom_boxplot()
# ggplot(median_run, aes(x = day, y = recovered)) + geom_boxplot()
