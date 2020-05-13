
trck=read_csv("../output/2020-03-02_7_tracking_info.csv")
curr <- read_csv("../lsh_data/2020-04-20_2_current_state_per_date.csv")

arrival_state_per_date <- filter(curr,days_in_state==1) %>% group_by(date,state) %>% summarize(cnt=n())
#current_state_per_date <- filter(curr,days_in_state>1) %>% group_by(date,state) %>% summarize(cnt=n())

arrival_sim_state_per_date <- arrival_sim_cnt <- filter(trck,sim_no==0,transition=="state_first") %>% group_by(date,state) %>% summarize(cnt=n())
#current_sim_state_per_date <- arrival_sim_cnt <- filter(trck,sim_no==0,transition=="state_current") %>% group_by(date,state) %>% summarize(cnt=n())

arrival_cnt_per_date <- curr %>% group_by(patient_id) %>% summarize(date=min(as.Date(date))) %>% ungroup() %>% group_by(date) %>% summarize(cnt=n())
#current_cnt_per_date <- filter(curr,days_in_state>1) %>% group_by(date) %>% summarize(cnt=n())

arrival_sim_cnt_per_date <- arrival_sim_cnt <- filter(trck,sim_no==0,transition=="state_first") %>% group_by(date) %>% summarize(cnt=n())
#current_sim_cnt_per_date <- arrival_sim_cnt <- filter(trck,sim_no==0,transition=="state_current") %>% group_by(date) %>% summarize(cnt=n())

diff_arrival <- inner_join(arrival_cnt_per_date,arrival_sim_cnt_per_date, by='date') %>% filter(cnt.x != cnt.y)
#diff_current <- inner_join(current_cnt_per_date,current_sim_cnt_per_date, by='date') %>% filter(cnt.x != cnt.y)
