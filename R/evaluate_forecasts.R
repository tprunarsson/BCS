Sys.setlocale(locale = "is_IS.UTF-8")
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(here)
library(hrbrthemes)

forecasts_raw <- read_excel(here('../dashboard/input/forecast_evaluation.xlsx'), sheet = "forecasts")
settings_raw <- read_excel(here('../dashboard/input/forecast_evaluation.xlsx'), sheet = "settings")
current_state_per_date_summary <- readr::read_csv('~/Downloads/2020-04-15_current_state_per_date_summary.csv')

real <- current_state_per_date_summary %>% 
  filter(date >= '2020-03-25',
         state %in% c('inpatient_ward', 'intensive_care_unit')) %>% 
  mutate(state = state %>% factor(levels = c('inpatient_ward', 'intensive_care_unit'), 
                                  labels = c('Legudeild', 'Gjörgæsla')))

forecasts <- forecasts_raw %>% 
  # filter states and translate to Icelandic
  filter(state %in% c('ward', 'icu')) %>% 
  mutate(date = as.Date(date),
         state = state %>% factor(levels = c('ward', 'icu'), 
                                  labels = c('Legudeild', 'Gjörgæsla'))) %>% 
  # Make data long
  tidyr::pivot_longer(cols = c(-date, -state), names_to = 'spa_nr', values_to = 'count') %>% 
  filter(spa_nr %in% c(0, seq(1, max(spa_nr), 2)),
         !is.na(count)) %>% 
  # Add coulmn with initial value of each forecast for either state
  group_by(spa_nr) %>%
  mutate(first_date_count = ifelse(date == min(date), count, NA))

p <- ggplot(data=real, aes(x = date, y=count)) +
  #plot real data
  geom_line(color='red', alpha = 0.3) +
  geom_point(color='red', shape = 18) +
  #plot forecast and initial value
  geom_line(data = forecasts, aes(group = spa_nr), 
            alpha = 0.65) +
  geom_point(data = forecasts, aes(y = first_date_count), 
             alpha = 0.4, size = 0.8, shape = 21) +
  #formats
  facet_wrap(~state, ncol = 2, as.table = F, scales = 'free') +
  labs(x = '', y = 'Fjöldi') +
  scale_y_continuous(limits=c(0,70)) +
  scale_x_date(date_labels = "%e %b") +
  hrbrthemes::theme_ipsum()
p
ggsave('~/Downloads/real_vs_forecast.png', p, height = 5.5, width = 12)

## Test animations, by date or by spa_nr
# p_anim <- p + 
  # geom_segment(data = forecasts,
  #              aes(xend = as.Date('2020-05-12'), yend = count, group = spa_nr), 
  #              linetype = 2, colour = 'black') +
  # geom_text(data = forecasts, 
  #           aes(x = as.Date('2020-05-15', group = spa_nr), label = spa_nr), 
  #           hjust = 0) +
  # geom_segment(data = real,
  #              aes(xend = as.Date('2020-05-12'), yend = count), 
  #              linetype = 2, colour = 'red') +
  # geom_text(data = real, 
  #           aes(x = as.Date('2020-05-10'), label = 'Raungögn'), 
  #           hjust = 0, col = 'red') +
  # transition_reveal(as.numeric(spa_nr))
  # transition_reveal(date)
# p_anim
# anim_save('~/Downloads/anim_spa_vs_real.gif', p_anim, rewind = F, duration = 6)
