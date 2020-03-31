library(ggplot2)

path_data <-'covid_run_0_2020-03-30.csv'
upper_run <- read.csv2(file = path_data, sep=",")
upper_run$day = as.factor(median_run$day)
ggplot(upper_run, aes(x = day, y = home)) + geom_boxplot()
ggplot(upper_run, aes(x = day, y = inpatient_ward)) + geom_boxplot()
ggplot(upper_run, aes(x = day, y = intensive_care_unit)) + geom_boxplot()
ggplot(upper_run, aes(x = day, y = death)) + geom_boxplot()
ggplot(upper_run, aes(x = day, y = recovered)) + geom_boxplot()

path_data <-'covid_run_1_2020-03-30.csv'
median_run <- read.csv2(file = path_data, sep=",")
median_run$day = as.factor(median_run$day)
ggplot(median_run, aes(x = day, y = home)) + geom_boxplot()
ggplot(median_run, aes(x = day, y = inpatient_ward)) + geom_boxplot()
ggplot(median_run, aes(x = day, y = intensive_care_unit)) + geom_boxplot()
ggplot(median_run, aes(x = day, y = death)) + geom_boxplot()
ggplot(median_run, aes(x = day, y = recovered)) + geom_boxplot()
