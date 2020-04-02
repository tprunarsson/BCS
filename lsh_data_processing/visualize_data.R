source('covid19_lsh_data_preprocessing.R')
state_newly_diagnosed <- anti_join(individs_extended,select(patient_transitions,patient_id),by='patient_id') %>%
    filter(.,outcome=='in_hospital_system') %>%
    mutate(.,state='home') %>%
    left_join(.,select(hospital_visits_filtered,patient_id,unit_in,date_time_in),by='patient_id') %>%
    mutate(state=if_else(is.na(unit_in),state,unit_in)) %>%
    group_by(.,patient_id) %>% arrange(date_time_in) %>%
    summarize(.,date=min(date_diagnosis,na.rm=T),state=tail(state,1)) %>%
    ungroup()

state_date_last_known <- filter(patient_transitions,date==(date_last_known_state-1)) %>%
    mutate(.,state=state_tomorrow,date=date+1) %>%
    filter(state!='recovered') %>%
    select(.,patient_id,date,state) %>%
    bind_rows(.,state_newly_diagnosed)

current_state_per_date <- select(patient_transitions,-state_tomorrow) %>%
    bind_rows(.,state_date_last_known) %>%
    group_by(.,date,state) %>%
    summarise(count=n()) 
write.table(paste0('../output/states_in_hospital_system_num_per_date_',current_date,'.csv'),row.names=F,quote=F,sep=',')

#table for emergency room,outpatient clinic visited by id and day
#Note edge case for id 100672
#get nr of individuals in each state per date

window_size <- 7
nr_at_home_per_day <- filter(current_state_per_date,state=='home') %>% rename(nr_at_home=count)
emergency_room_visits_per_day <- filter(hospital_visits,unit_category_all=='emergency_room') %>%
    left_join(.,select(individs_extended,patient_id,date_diagnosis),by='patient_id') %>%
    mutate(emergency_room_type=if_else(date_diagnosis == date_in,'new_infection','other')) %>%
    distinct(patient_id,date_in,emergency_room_type) %>%
    select(patient_id,date_in,emergency_room_type) %>%
    arrange(patient_id,date_in) %>%
    group_by(.,date_in,emergency_room_type) %>%
    summarise(nr_visits=n()) %>% ungroup() %>%
    left_join(.,nr_at_home_per_day,by=c('date_in'='date')) %>%
    mutate(prop_visits=nr_visits/nr_at_home)

calc_sliding_p <- function(p_vec,window_size){
    p_sliding=rep(0,length(p_vec))
    for(i in 1:length(p_vec)){
        if(i<window_size){
            p_sliding[i] <- mean(p_vec[1:i])
        }else{
            p_sliding[i] <- mean(p_vec[(i-6):i])
        }
    }
    return(p_sliding)
}

date_all <- seq(min(emergency_room_visits_per_day$date_in),max(emergency_room_visits_per_day$date_in),by=1)
emergency_room_types=c('new_infection','other')
date_expanded <- expand_grid(date_all,emergency_room_types) %>% rename(date_in=date_all,emergency_room_type=emergency_room_types)  
prop_emergency_room_last_week <- left_join(date_expanded,select(emergency_room_visits_per_day,date_in,emergency_room_type,prop_visits),by=c('date_in','emergency_room_type')) %>%
    mutate(prop_visits=if_else(is.na(prop_visits),0,prop_visits)) %>%
    group_by(.,emergency_room_type) %>%
    arrange(date_in) %>%
    mutate(prop_visits_last_week=calc_sliding_p(prop_visits,window_size)) %>%
    ungroup() %>%
    filter(.,date_in>=(min(date_in)+7)) %>%
    mutate(emergency_room_type=factor(emergency_room_type,levels=emergency_room_types,labels=c('Nýtt smit','Annað'))) %>% 
    rename(`Tegund komu`=emergency_room_type)

ggplot(prop_emergency_room_last_week,aes(date_in,prop_visits*100,col=`Tegund komu`)) +
    geom_point() + xlab('Dagsetning') + ylab('Hlutfall(%)')  +
    ggtitle('Hlutfall smitaðra í heimaeinangrun sem koma á bráðamóttöku skipt eftir tegund komu') +
    scale_color_manual(values=c('blue4','gold2'))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=10))

ggsave(filename='../R/2020-04-01_emergency_prop_by_type.png',device='png',width=8,height=6)


ggplot(group_by(prop_emergency_room_last_week,date_in) %>% summarise(prop_visits_last_week=sum(prop_visits)*100) %>% ungroup(),aes(date_in,prop_visits_last_week)) +
    geom_point(col='forestgreen') + xlab('Dagsetning') + ylab('Hlutfall síðustu viku (%)')  +
    ggtitle('Hlutfall smitaðra í heimaeinangrun sem koma á bráðamóttöku') +
    theme(legend.position='none') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=10))
ggsave(filename='../R/2020-04-01_emergency_sliding_prop_by_type.png',device='png',width=8,height=6)


ggplot(group_by(prop_emergency_room_last_week,date_in) %>% summarise(prop_visits=sum(prop_visits)*100) %>% ungroup(),aes(date_in,prop_visits)) +
    geom_point(col='forestgreen') + xlab('Dagsetning') + ylab('Hlutfall(%)')  +
    ggtitle('Hlutfall smitaðra í heimaeinangrun sem koma á bráðamóttöku') +
    theme(legend.position='none') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=10))
ggsave(filename='../R/2020-04-01_emergency_prop.png',device='png',width=8,height=6)

ggplot(group_by(prop_emergency_room_last_week,date_in) %>% summarise(prop_visits_last_week=sum(prop_visits_last_week)*100) %>% ungroup(),aes(date_in,prop_visits_last_week)) +
    geom_point(col='forestgreen') + xlab('Dagsetning') + ylab('Hlutfall síðustu viku (%)')  +
    ggtitle('Hlutfall smitaðra í heimaeinangrun sem koma á bráðamóttöku') +
    theme(legend.position='none') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=10))
ggsave(filename='../R/2020-04-01_emergency_sliding_prop.png',device='png',width=8,height=6)

outpatient_clinic_visits_per_day <- filter(hospital_visits,unit_category_all=='outpatient_clinic') %>%
    select(patient_id,date_in) %>%
    arrange(patient_id,date_in) %>%
    group_by(.,date_in) %>%
    summarise(nr_visits=n()) %>% ungroup() %>%
    left_join(.,nr_at_home_per_day,by=c('date_in'='date')) %>%
    mutate(prop_visits=nr_visits/nr_at_home)

prop_outpatient_clinic_last_week <- filter(outpatient_clinic_visits_per_day,date_in>=current_date-window_size) %>%
    summarise(prop_visits_last_week=sum(prop_visits)/window_size)


sliding_p_by_group <- filter(prop_emergency_room_last_week,date_in=='2020-03-30')$prop_visits_last_week
current_date=as.Date('2020-04-01','%Y-%m-%d')
path_data <-'~/Downloads/2020-04-01_covid_simulation_all.csv'
states_in_order <- c('home','inpatient_ward','intensive_care_unit','death','recovered')
states_labels_in_order <- c('Heimaeinangrun','Legudeild','Gjörgæsla','Andlát','Batnað')
run_home <- read_csv(file = path_data) %>%
    mutate(.,date=as.factor(current_date+day)) %>%
    gather(.,key='state',value='count',-date,-day) %>%
    filter(.,state=='home') %>%
    mutate(.,state=factor(state,levels=states_in_order,labels=states_labels_in_order)) %>%
    mutate(.,count_all=sum(sliding_p_by_group)*count,count_new_infection=sliding_p_by_group[1]*count,count_other=sliding_p_by_group[2]*count)


#Compute medians to display on plot
medians <- group_by(run_home,date) %>% summarise(med_all=median(count_all),med_new_infection=median(count_new_infection),med_other=median(count_other))

group_plot_dat <- gather(run_home,key='type',value='count',count_new_infection,count_other) %>% mutate(type=factor(type,c('count_new_infection','count_other'),c('Nýtt smit','Annað')))
group_plot_median_dat <- gather(medians,key='type',value='med',med_new_infection,med_other) %>% mutate(type=factor(type,c('count_new_infection','count_other'),c('Nýtt smit','Annað')))
ggplot(group_plot_dat, aes(x = date, y = count)) + 
    geom_boxplot(col='blue4') + facet_wrap(~type) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=20)) + 
    xlab('Dagsetning') + 
    ylab('Fjöldi') +
    ggtitle('Fjöldi koma á bráðamóttöku á hverjum degi skipt eftir tegund komu')
ggsave(filename='../R/2020-04-01_emergency_room_prediction_by_type.png',device='png',width=16,height=10)

ggplot(run_home, aes(x = date, y = count_all)) + 
    geom_boxplot(col='blue4') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill='white',colour='black'),
          panel.grid.major = element_line(colour='gray95'),
          panel.grid.minor = element_line(colour='gray95'),
          text = element_text(size=20)) + 
    xlab('Dagsetning') + 
    ylab('Fjöldi') +
    ggtitle('Heildarfjöldi koma á bráðamóttöku á hverjum degi')
ggsave(filename='../R/2020-04-01_emergency_room_prediction_all.png',device='png',width=16,height=12)
