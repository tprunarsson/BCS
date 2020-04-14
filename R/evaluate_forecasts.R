forecasts_raw <- read_excel('~/projects/covid/BCS/dashboard/input/forecast_evaluation.xlsx',sheet = "forecasts")
settings_raw <- read_excel('~/projects/covid/BCS/dashboard/input/forecast_evaluation.xlsx',sheet = "settings")

forecasts <- mutate(forecasts_raw,date=as.Date(date,"%Y-%m-%d"))
forecasts_ward <- filter(forecasts,state=='ward')
real_ward <- filter(current_state_per_date_summary,state=='inpatient_ward' & date>=as.Date("2020-03-25"))
forecasts_icu <- filter(forecasts,state=='icu')
real_icu <- filter(current_state_per_date_summary,state=='intensive_care_unit' & date>=as.Date("2020-03-25"))

ggplot() +
        geom_line(data=real_ward,aes(x=date,y=count,color='red')) +
        geom_line(data=forecasts_ward,aes(x=date,y=`0`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`1`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`2`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`3`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`4`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`5`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`7`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`9`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`11`)) +
        geom_line(data=forecasts_ward,aes(x=date,y=`13`))

ggplot() +
  geom_line(data=real_icu,aes(x=date,y=count,color='red')) +
  geom_line(data=forecasts_icu,aes(x=date,y=`0`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`1`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`2`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`3`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`4`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`5`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`7`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`9`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`11`)) +
  geom_line(data=forecasts_icu,aes(x=date,y=`13`))
