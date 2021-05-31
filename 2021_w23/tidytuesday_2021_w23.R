
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(glue)

# Read & Data  Wrangling --------------------------------------------------
summary_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
challenges_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/challenges.csv')
castaways_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

# Mean Castaways 
(castaways_df %>% 
    group_by(season_name, season) %>% 
    summarise(
      n_p = n(),
      mean_age = mean(age)
    ) %>% 
    arrange(season)
    )

(
  castaways_df <- castaways_df %>% 
    distinct(season_name,season, full_name, castaway, age,.keep_all = T)
)
# https://flowingdata.com/2013/11/18/regional-names/ Goat tables
(goats_df <- challenges_df %>% 
    group_by(season_name, winners, challenge_type) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(challenge_type) %>% 
    pivot_wider(names_from = "challenge_type", values_from = "n") %>% 
    arrange(-immunity, -reward)
    # slice_max(order_by = n , n = 50) %>% 
)

states <- tibble(
  state = state.name, 
  state_abb = state.abb
)

(summary_df <- summary_df %>% 
    mutate(year = year(premiered))
)
concatenate <- function(words) {
  words <- as.character(words)
  words <- unique(words)
  if (length(words) > 1) {
  str_c(c(str_c(head(words,length(words)-1), collapse = ", "), last(words)), collapse = " and ")
  }
  else {
    words
  }
}

concatenate(c("a"))
states 
(goats_final_df <- goats_df %>% 
    left_join(castaways_df, by = c("season_name", "winners" = "castaway")) %>% 
    left_join(states) %>% 
    left_join(select(summary_df, season_name,year)) %>% 
    mutate(city = glue("{city} · {state_abb}"),
           season_name = glue("{season_name} · {year}"))  %>% 
    select(season_name:personality_type, -state) %>%
    filter(!is.na(full_name)) %>%
    mutate(season_name = str_remove(season_name, "Survivor: ")) %>%
  
    group_by(full_name) %>%
    arrange(age) %>% 
    summarise(
      across(immunity:reward, sum, na.rm =T, .names = "{.col}_wins"),
      across(c(season_name,age:personality_type), list(collapse = ~concatenate(.x)), .names = "{.col}s"),
    ) %>%
    ungroup() %>%
    arrange(-immunity_wins, -reward_wins)
)
