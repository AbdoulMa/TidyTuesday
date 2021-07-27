# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggstream)


# Data reading and Wrangling ----------------------------------------------
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
olympics <- olympics %>% 
  mutate(team = case_when(
    team == "Soviet Union" ~ "Russia",
    team == "East Germany" ~ "Germany",
    team == "West Germany" ~ "Germany",
    team == "United States-1" ~ "United States",
    team == "China-1" ~ "China",
    team == "Australasia" ~ "Australia",
    TRUE ~ team
  ))




(summer_olympics <- olympics %>% 
  filter(season == "Summer") %>% 
  distinct(year, city) %>% 
  mutate(edition = glue::glue("{city} \n {year}")) %>% 
  filter(!(year == 1906 & city == "Athina"),!(year == 1956 & city == "Stockholm")) %>% 
    arrange(year) %>% 
    rowid_to_column() 
)

top_federations <- olympics %>% 
  filter(season == "Summer", medal == "Gold") %>%
  count(team, sort = T) %>% 
  head(15) %>% 
  pull(team)
top_federations

og_segments <-  tibble (x = c(seq(1896,1912 , by = 4),seq(1920,1936 , by = 4),seq(1948,2016 , by = 4)), y = 600)

gold_medalists <- olympics %>% 
  mutate(
    team = fct_other(team,keep = top_federations, other_level = "Other countries")) %>% 
  filter(season == "Summer", medal == "Gold") %>% 
  count(team,  year) %>% 
  arrange(year) 

gold_medalists
gold_medalists %>% 
  ggplot() +
  geom_segment(data = og_segments , aes(x =x  , xend =x, y = y, yend = -y), size = 0.5, color = "grey60")+ 
  geom_text( data = summer_olympics, aes(x = year, y =-630, label = edition), lineheight = .90, angle = 45) +
  geom_text( data = summer_olympics, aes(x = year, y = 630, label = edition), lineheight = .90, angle = 45,) +
  geom_stream(aes(year, n, fill = team),color = "black", size = .25) +
  geom_stream_label(aes(x=year,y = n ,fill = team, label = team)) + 
  scale_fill_manual(
    values =c("United States" = "grey60", 
              "China" = "grey35",
              "Russia" = "red"
          ),
    na.value = "grey90"
  )
  # guides(fill = "none")


