
library(ggplot2)
library(readr)
current_date=as.Date('2020-03-02','%Y-%m-%d')
path_data_upper <- paste0('../output/',current_date,'_covid_simulation.csv')
states_in_order <- c('home','inpatient_ward','intensive_care_unit')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla')

simulation_run <- read_csv(file = path_data_upper) %>%
                mutate(.,date=as.factor(current_date+day)) %>%
                gather(.,key='state',value='count',-date,-day) %>%
                filter(.,state %in% c('home','inpatient_ward','intensive_care_unit')) %>%
                mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order))

f <- function(x) {
    r <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}

df <- current_state_per_date %>% ungroup() %>% mutate(.,date=as.factor(date), state=factor(state,levels=states_in_order,labels=states_labels_in_order))

#brk <- levels(simulation_run$date); brk[seq(2,length(brk),2)] <- " "; 
brk[seq(2,length(brk),3)] <- " "; brk[seq(3,length(brk),3)] <- " "
brk <- c(brk," ")
p = ggplot(simulation_run, aes(x = date, y = count)) + stat_summary(fun.data = f, geom="boxplot") + geom_boxplot(color = "gray", alpha = 0.3, outlier.shape = NA)  +
    geom_point(data=df, aes(x = date, y = count, color = "black"), color = "red", shape = 23, show.legend = FALSE) +
    facet_wrap(~state,scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.background = element_rect(fill='white',colour='black'),
            panel.grid.major = element_line(colour='gray95'),panel.grid.minor = element_line(colour='gray95')) + xlab('') + ylab('') +
    scale_x_discrete(labels=brk)

ggsave(filename='simulations_performance.png',device='png',width=18,height=11)
