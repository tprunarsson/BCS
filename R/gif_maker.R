oldw <- getOption("warn")
options(warn = -1)
library(tidyverse)
library(lubridate)
library(gdata)
options(warn = oldw)

# Hafa inni ef ekki er kallað úr BCS_gagnabord2
setwd("../lsh_data_processing")
source("covid19_lsh_data_processing.R") #Þurfum nokkra hluti úr þessari skrá. 
gdata::keep(patient_transitions_state_blocks, individs_splitting_variables, get_patient_transitions_at_date, patient_transitions, infections_predicted_per_date, historical_data, path_tables, sure = TRUE)

dates <- seq.Date(from=ymd("2020-03-08"), to=ymd("2020-05-02"), by=1)
los <- data.frame("vd"=21, "ld"=14, "gd"=10, "pre_lega"=7, "pre_icu"=3)
date_prediction <- ymd("2020-04-20")
setwd("../R")
source('best_weighted.R')

alphas <- data.frame("date"=dates, alpha=rep(NA, length(dates)))
iter=1

for(i in 38:length(dates)){
  day <- dates[i]
  df <- best_weighted_function(day, los, 'hi', date_prediction)
  alphas[i, 2] <- df$best_alpha[1]
}

# Setja inn í best_weighted_function():
#   
# plot <- ggplot(data=combined_model_today, aes(x=date)) +
#   geom_line(aes(y=median), color="dodgerblue3") + 
#   geom_line(aes(y=lower), linetype="dashed", color="dodgerblue3") + 
#   geom_line(aes(y=upper), linetype="dashed", color="dodgerblue3") + 
#   #geom_point(aes(y=count_historical)) +
#   geom_vline(xintercept = ymd(today), linetype="dotted", color="indianred3") +
#   geom_point(data=historical_data_filtered, aes(y=count)) +
#   geom_point(data=historical_data, aes(y=count), alpha=0.4) +
#   facet_wrap(~state,scales='free') +
#   labs(x="Dagsetning", y="Fjöldi", title = today, subtitle = paste("alpha =", best_alpha)) + 
#   theme_bw() +
#   theme(legend.title=element_blank())
# 
# setwd("../graphs")
# ggsave(paste0(today, "_graph_weighted2.png"), plot, device='png', width=16, height=10)

alpha_mynd_best <- alphas %>% ggplot(aes(x=date, y=alpha)) + geom_line(color="#84a9ac") + geom_point(color="#3b6978") + labs(title = "Alpha", subtitle = "Sameining tveggja spálíkana, hærra alpha þýðir að IC er meira notað")
alpha_mynd_best
ggsave("alpha_mynd_best.png", alpha_mynd_best, device='png', width=16, height=10)
write.table(alphas, file = "alphas.csv", quote = F,row.names=F,sep=',')
