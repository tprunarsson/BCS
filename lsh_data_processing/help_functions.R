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

fitlognormal <- function(x,x_c,max_num_days) {
  n <- length(x)+length(x_c)
  objective_function <- function(theta){
    L=sum(log(plnorm(x+0.5,theta[1],theta[2])-plnorm(x-0.5,theta[1],theta[2]))) + sum(log(plnorm(max_num_days+0.5,theta[1],theta[2])-plnorm(x_c,theta[1],theta[2])))-n*log(plnorm(max_num_days+0.5,theta[1],theta[2]))
    return(-L)
  }
  theta_init <- c(1,1)
  theta <- optim(theta_init,objective_function,method='L-BFGS-B',lower = theta_init)$par
  return(theta)
}

sample_from_lognormal <- function(x,x_c,s,max_num_days,age_groups,nr_samples=1e6){
  theta_s = fitlognormal(x,x_c,max_num_days=max_num_days_inpatient_ward)
  length_of_stay_s_expanded <- expand_grid(age_group_simple=age_groups,state_duration=1:max_num_days) %>%
    mutate(state=s)
  length_of_stay_s <- rlnorm(nr_samples,theta_s[1],theta_s[2]) %>%
    round() %>%
    table() %>%
    as.numeric() %>%
    tibble(state=s,state_duration=0:(length(.)-1),count=.) %>%
    filter(state_duration>0 & state_duration<=max_num_days) %>%
    inner_join(.,length_of_stay_s_expanded,by=c('state','state_duration')) %>%
    select(state,age_group_simple,state_duration,count)
  return(length_of_stay_s)
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
