get_states_in_order <- function(model='',active=F){
  states_active <- distinct(unit_categories,unit_category,unit_category_order) %>%
    arrange(unit_category_order) %>%
    select(unit_category) %>%
    unlist() %>%
    unname() 
  if(model=='extended'){
    clinical_assessment_active <- c('green','red')
    states_active <- sapply(states_active,function(x) paste0(x,'-',clinical_assessment_active)) %>% c()
  }
  if(active){
    states <- states_active 
  }else{
    states_end <- c('death','recovered')
    states <- c(states_active,states_end)
  }
  return(states)
}

get_splitting_variable_values_in_order <- function(splitting_variable_name){
  if(splitting_variable_name=='age'){
    values <- distinct(age_groups,age_group,age_group_order) %>%
      arrange(age_group_order) %>%
      select(age_group) %>%
      mutate(age_group=paste0('age_',age_group)) %>%
      unlist() %>%
      unname() 
  }else if(splitting_variable_name=='priority'){
    values <- distinct(priority_categories,priority_category,priority_category_order) %>%
      arrange(priority_category_order) %>%
      select(priority_category) %>%
      mutate(priority_category=paste0('priority_',priority_category)) %>%
      unlist() %>%
      unname() 
  }else{
    stop('splitting variable not yet defined')
  }
  return(values)
}


get_splitting_variable <- function(dat,variable_name){
  splitting_variable <- c()
  if(variable_name=='age'){
    if(!('age' %in% names(dat))){
      stop("age must be a variable in dat")   
    }
    splitting_variable <- select(dat,age) %>% inner_join(age_groups,by='age') %>% mutate(age_group=paste0('age_',age_group)) %>% select(age_group) %>% unlist() %>% unname()
  }else if(variable_name=='priority'){
    if(!('priority' %in% names(dat))){
      stop("priority must be a variable in dat")   
    }
    splitting_variable <- select(dat,priority) %>% mutate(priority=paste0('priority_',priority)) %>% unlist() %>% unname()
  }else{
    stop("splitting variable type not yet defined") 
  }
  splitting_variable_values_in_order <- get_splitting_variable_values_in_order(variable_name)
  return(factor(splitting_variable,levels=splitting_variable_values_in_order,labels=splitting_variable_values_in_order))
}

impute_priority <- function(dat){
  
}

impute_severity <- function(state,severity_vec){
  output_vec <- vector('character',length = length(severity_vec))
  if(length(severity_vec)>0){
    if(all(is.na(severity_vec))){
        output_vec[1] <- 'green'
    }else{
      first_non_NA <- min(which(!is.na(severity_vec)))
      output_vec[1] <- severity_vec[first_non_NA]
    }
    if(length(severity_vec)>1){
      current_severity <- output_vec[1]
      for(i in 2:length(severity_vec)){
        if(is.na(severity_vec[i])){
          output_vec[i] <- current_severity
        }else{
          output_vec[i] <- severity_vec[i]
          current_severity <- output_vec[i]
        }
      }
    }
  }
  return(output_vec)
}

#identify and sequencially number blocks of states for each patient_id 
get_state_block_numbers <- function(state_vec){
  state_nr=1
  state_block_numbers <- c()
  
  if(length(state_vec)==0){
    return(state_block_numbers)
  }
  
  state_block_numbers[1]=state_nr=1
  
  if(length(state_vec)==1){
    return(state_block_numbers)
  }
  for(i in 2:length(state_vec)){
    if(state_vec[i]!=state_vec[i-1]){
      state_nr <- state_nr+1
    }
    state_block_numbers[i] <- state_nr
  }
  return(state_block_numbers)
}

get_state_worst <- function(state_vec,order_vec){
  state_worst_vec <- vector('character',length=length(state_vec))
  if(length(state_vec)==0){
    return(state_worst_vec)
  }
  state_worst=state_vec[1]
  state_worst_order=order_vec[1]
  for(i in 1:length(state_vec)){
    if(order_vec[i]>state_worst_order){
      state_worst <- state_vec[i]
      state_worst_order <- order_vec[i]
      state_worst_vec[i] <- state_vec[i]
    }else{
      state_worst_vec[i] <- state_worst
    }
  }
  return(state_worst_vec)
}

fit_lognormal <- function(x,x_c,max_num_days) {
  n <- length(x)+length(x_c)
  objective_function <- function(theta){
    L=sum(log(plnorm(x+0.5,theta[1],theta[2])-plnorm(x-0.5,theta[1],theta[2]))) + sum(log(plnorm(max_num_days+0.5,theta[1],theta[2])-plnorm(x_c,theta[1],theta[2])))-n*log(plnorm(max_num_days+0.5,theta[1],theta[2]))
    return(-L)
  }
  theta_init <- c(1,1)
  theta <- optim(theta_init,objective_function,method='L-BFGS-B',lower = theta_init)$par
  return(theta)
}


sample_from_lognormal <- function(x,x_c,max_num_days,nr_samples=1e6){
  theta_s = fit_lognormal(x,x_c,max_num_days=max_num_days_inpatient_ward)
  length_of_stay_samples <- rlnorm(nr_samples,theta_s[1],theta_s[2]) %>%
                            round() %>%
                            table() %>%
                            as.numeric() %>%
                            tibble(state_duration=0:(length(.)-1),count=.) %>%
                            filter(state_duration>0 & state_duration<=max_num_days) %>%
                            select(state_duration,count)
  return(length_of_stay_samples)
}

fit_beta <- function(x,x_c,max_num_days) {
  objective_function <- function(theta){
    L=sum(log(pbeta(x/max_num_days,theta[1],theta[2])-pbeta((x-1)/max_num_days,theta[1],theta[2]))) + sum(log(1-pbeta((x_c-0.5)/max_num_days,theta[1],theta[2])))
    return(-L)
  }
  theta_init <- c(1,3.5)
  theta <- optim(theta_init,objective_function,method='L-BFGS-B',lower = c(0,0))$par
  return(theta)
}

sample_from_beta <- function(x,x_c,max_num_days,nr_samples=1e6){
  theta_s = fit_beta(x,x_c,max_num_days=max_num_days)
  length_of_stay_samples <- (max_num_days*(rbeta(nr_samples,theta_s[1],theta_s[2])+0.5)) %>%
                              round() %>%
                              table() %>%
                              as.numeric() %>%
                              tibble(state_duration=0:(length(.)-1),count=.) %>%
                              filter(state_duration>0 & state_duration<=max_num_days)%>%
                              select(state_duration,count)
  return(length_of_stay_samples)
}

get_max_splitting_dat <- function(splitting_variable_names){
  max_splitting_dat <- select(individs_splitting_variables,patient_id,matches(paste(unique(splitting_variable_names),sep='|'))) %>%
    pivot_longer(.,-matches('patient_id|order'),names_to='splitting_variable',values_to='value') %>%
    pivot_longer(.,matches('order'),names_to='splitting_variable_order_name',values_to='splitting_variable_order') %>%
    filter(.,paste0(splitting_variable,'_order')==splitting_variable_order_name) %>%
    group_by(patient_id) %>%
    arrange(splitting_variable) %>%
    mutate(max_splitting_name=paste(splitting_variable,collapse=':'),
           max_splitting_values=paste(value,collapse=':'),
           max_splitting_order=paste(splitting_variable_order,collapse=':')) %>%
    ungroup() %>%
    pivot_wider(.,id_cols = c('patient_id','max_splitting_name','max_splitting_values','max_splitting_order'),names_from = 'splitting_variable',values_from = 'value') 
  return(max_splitting_dat)
}

get_splitting_variable_mapping <- function(splitting_variable_name,max_splitting_dat){
  if(splitting_variable_name %in% names(max_splitting_dat)){
    splitting_variable_mapping <- select(max_splitting_dat,matches(splitting_variable_name),max_splitting_values) %>%
      rename(splitting_variable=!!splitting_variable_name)
  }else{
    splitting_variable_mapping <- mutate(max_splitting_dat,splitting_variable='none') %>% select(.,splitting_variable,max_splitting_values)
  }
  splitting_variable_mapping <- mutate(splitting_variable_mapping,max_splitting_values=factor(max_splitting_values,levels=max_splitting_values))
  return(splitting_variable_mapping)
}


#analysis
# theta_ward = fitlognormal(state_blocks_with_age, "inpatient_ward",max_num_days=21)
# theta_icu = fitlognormal(state_blocks_with_age, "intensive_care_unit",max_num_days=28)
# 
# theta_ward_data <- tibble(state_duration=seq(0,21,length.out = 100)) %>%
#   mutate(inpatient_ward=dlnorm(state_duration,theta_ward[1],theta_ward[2])/plnorm(21,theta_ward[1],theta_ward[2])) %>%
#   gather(.,key='state','density',inpatient_ward)
# 
# theta_icu_data <- tibble(state_duration=seq(0,28,length.out = 100)) %>%
#   mutate(intensive_care_unit=dlnorm(state_duration,theta_icu[1],theta_icu[2])/plnorm(28,theta_icu[1],theta_icu[2])) %>%
#   gather(.,key='state','density',intensive_care_unit)
# 
# theta_data <- bind_rows(theta_ward_data,theta_icu_data)
# plot_data <- filter(state_blocks_with_age,state!='home') %>% group_by(state,censored,state_duration) %>% summarise(count=n()) %>% group_by(state) %>% mutate(density=count/sum(count))
# ggplot(data=plot_data,aes(state_duration,density)) + geom_col(aes(fill=censored)) + geom_line(data=theta_data,aes(x=state_duration,y=density)) + facet_wrap(~state)
# inpatient_ward_gof_dat <- filter(state_blocks_with_age,state=='inpatient_ward') %>% dplyr::select(state_duration,censored)
# intensive_care_unit_gof_dat <- filter(state_blocks_with_age,state=='intensive_care_unit') %>% dplyr::select(state_duration,censored)
# gofTestCensored(inpatient_ward_gof_dat$state_duration,inpatient_ward_gof_dat$censored, censoring.side = "right", test = "sf", distribution = "lnorm")
# gofTestCensored(intensive_care_unit_gof_dat$state_duration,intensive_care_unit_gof_dat$censored, censoring.side = "right", test = "sf", distribution = "lnorm")

# beta distribution analysis
# ward_x <- filter(state_blocks_with_splitting_variable,state=='inpatient_ward' & !censored) %>% select(state_duration) %>% unlist() %>% unname()
# ward_x_c <- filter(state_blocks_with_splitting_variable,state=='inpatient_ward' & censored) %>% select(state_duration) %>% unlist() %>% unname()
# theta_ward = fit_beta(ward_x,ward_x_c,max_num_days=28)
# icu_x <- filter(state_blocks_with_splitting_variable,state=='intensive_care_unit' & !censored) %>% select(state_duration) %>% unlist() %>% unname()
# icu_x_c <- filter(state_blocks_with_splitting_variable,state=='intensive_care_unit' & censored) %>% select(state_duration) %>% unlist() %>% unname()
# theta_icu = fit_beta(icu_x,icu_x_c,max_num_days=28)
# 
# theta_ward_data <- tibble(state_duration=seq(0.01,1,length.out = 100)) %>%
#   mutate(inpatient_ward=dbeta(state_duration,theta_ward[1],theta_ward[2])/28) %>%
#   mutate(state_duration=(state_duration+0.5/28)*28) %>%
#   pivot_longer(.,inpatient_ward,names_to='state',values_to='density')
# 
# theta_icu_data <- tibble(state_duration=seq(0.01,1,length.out = 100)) %>%
#   mutate(intensive_care_unit=dbeta(state_duration,theta_icu[1],theta_icu[2])/28) %>%
#   mutate(state_duration=(state_duration+0.5/28)*28) %>%
#   pivot_longer(.,intensive_care_unit,names_to='state',values_to='density')
# 
# theta_data <- bind_rows(theta_ward_data,theta_icu_data)
# plot_data <- filter(state_blocks_with_splitting_variable,state!='home') %>% group_by(state,censored,state_duration) %>% summarise(count=n()) %>% group_by(state) %>% mutate(density=count/sum(count))
# ggplot(data=plot_data,aes(state_duration,density)) + geom_col(aes(fill=censored),position='dodge') + geom_line(data=theta_data,aes(x=state_duration,y=density)) + facet_wrap(~state)
# inpatient_ward_gof_dat <- filter(state_blocks_with_age,state=='inpatient_ward') %>% dplyr::select(state_duration,censored)
# intensive_care_unit_gof_dat <- filter(state_blocks_with_age,state=='intensive_care_unit') %>% dplyr::select(state_duration,censored)
# gofTestCensored(inpatient_ward_gof_dat$state_duration,inpatient_ward_gof_dat$censored, censoring.side = "right", test = "sf", distribution = "lnorm")
# gofTestCensored(intensive_care_unit_gof_dat$state_duration,intensive_care_unit_gof_dat$censored, censoring.side = "right", test = "sf", distribution = "lnorm")


#Length of stay experiment
# nr_samples <- 1e6
# censored_ward=current_state %>% filter(state=='inpatient_ward')
# 
# samples=matrix(0,ncol=nrow(censored_ward),nrow=nr_samples)
# for(i in 1:nrow(censored_ward)){
#   idv_distr=filter(length_of_stay_predicted,splitting_variable=='age_51+',state=='inpatient_ward',state_duration>=censored_ward$days_in_state[i]) %>%
#     mutate(prob=count/sum(count)) %>%
#     select(prob) %>% unlist() %>% unname()
#     samples[,i] <- sample(0:(length(idv_distr)-1),size=nr_samples,prob=idv_distr,replace=T)
# }
# median(apply(samples,1,function(x) sum(x==0)))
# 
# distr_table <- lapply(1:nrow(censored_ward),function(i){
#   distr=filter(length_of_stay_predicted,splitting_variable=='age_51+',state=='inpatient_ward',state_duration>=censored_ward$days_in_state[i]) %>%
#     mutate(patient_id=censored_ward$patient_id[i],prob=count/sum(count)) %>%
#     select(patient_id,state_duration,prob)
# }) %>% bind_rows()
# 
# group_by(distr_table,patient_id) %>% summarise(n=n()) %>% ungroup() %>% mutate(not_zero=(n-1)/n,zero=1/n) %>% summarise(exp_nr_zeros=sum(zero),prob_no_zero=prod(not_zero))
# 
# prop red at home
# home_first_without_children <- group_by(patient_transitions_state_blocks,patient_id,state_block_nr,state) %>%
#   summarize(censored=censored[which.max(state_with_severity_block_nr)],state_block_nr_start=min(state_block_nr_start),state_block_nr_end=max(state_block_nr_end)+1,state_duration=sum(state_duration),state_next=tail(state_next,1)) %>%
#   ungroup() %>% inner_join(select(individs_extended,patient_id,age,splitting_variable),.,by='patient_id') %>%
#   mutate(state_next=if_else(state_next=='recovered',state_next,'not_recovered')) %>%
#   filter(!censored,state_block_nr==1,state=='home',age>18)
# #filter(!censored,state=='home')
# 
# 
# LOS_plot_dat <- group_by(home_first_without_children,state,state_next,state_duration) %>%
#   summarize(count=n()) %>%
#   arrange(state,state_next,state_duration)
# ggplot(LOS_plot_dat,aes(state_duration,count,fill=state_next)) + geom_col(position='dodge')
# 
# 
# dates_vec <- seq(min(home_first_without_children$state_block_nr_end),max(home_first_without_children$state_block_nr_end),by=1)
# state_next_vec <- c('not_recovered','recovered')
# splitting_variable_values <- get_splitting_variable_values_in_order('age')
# prop_red_plot_dat <- group_by(home_first_without_children,state_block_nr_end,state_next,splitting_variable) %>%
#   summarise(count=n()) %>%
#   left_join(expand_grid(date=dates_vec,outcome=state_next_vec,splitting_variable=splitting_variable_values),.,by=c('date'='state_block_nr_end','outcome'='state_next','splitting_variable')) %>%
#   mutate(count=if_else(is.na(count),0,as.numeric(count))) %>%
#   group_by(outcome,splitting_variable) %>%
#   mutate(cumulative_counts=cumsum(count)) %>%
#   group_by(date,splitting_variable) %>%
#   arrange(outcome) %>%
#   summarise(prop_red=cumulative_counts[1]/(cumulative_counts[1]+cumulative_counts[2]))
# ggplot(prop_red_plot_dat,aes(date,prop_red)) + geom_line() + ylim(0,1) + facet_wrap(~splitting_variable)