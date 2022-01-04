# Load libraries ----------------------------------------------------------
library(tidyverse)

# Data Reading and Wrangling ----------------------------------------------

# NY TIMES Data 
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
usa_states <- read_csv(here::here("2022_w1","usa_states_population.csv")) %>% 
  select(State, Pop)

usa_state_geom <- tigris::states() %>% 
  tigris::shift_geometry()

covid_cases <- covid_data %>% 
  group_by(state, fips) %>% 
  arrange(date) %>% 
  mutate(
    daily_cases = c(cases[1], diff(cases, lag = 1))
  ) %>% 
  ungroup() %>% 
  arrange(state, date) 
  

glimpse(usa_state_geom)
covid_cases_rm <- covid_cases %>% 
  mutate(roll_cases = zoo::rollmean(daily_cases, k = 5, fill = NA)) %>% 
  # Select 2021 Data
  filter(lubridate::year(date) == 2021) %>% 
  left_join(usa_states, by = c("state" = "State")) %>% 
  drop_na(Pop) %>% 
  mutate(incidence_rate = 10^5 *roll_cases / Pop) 


usa_state_geom %>% 
  left_join(covid_cases_rm, by = c("NAME" = "state")) %>% 
  filter(between(date, as.Date("2021-05-10"),as.Date("2021-05-17"))) %>% 
  ggplot() + 
  geom_sf() + 
  facet_wrap(vars(date))
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
