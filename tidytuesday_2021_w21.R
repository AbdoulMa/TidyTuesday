
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(sf)
library(albersusa)
library(datasets)
# Read Data & Wrangling ---------------------------------------------------
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey_deg_collapse <- survey %>% 
  mutate(
    education_level = fct_collapse(highest_level_of_education_completed,
      normal_sup_education = c("College degree","Master's degree"),
      long_sup_education = c("Professional degree (MD, JD, etc.)","PhD")
    )
  )

# Try with degree 
degree_comparison <- survey_deg_collapse %>% 
  filter(
    !str_detect(state, ","),
    education_level %in% c("normal_sup_education","long_sup_education"),
    !is.na(state)
  ) %>% 
  group_by(state,education_level) %>% 
  summarise(
    avg_annual_salary = mean(annual_salary)
  )


degree_comparison <- degree_comparison %>% 
  filter(! state %in% c("Alaska","Hawaii") )


states <- tibble(
  state = state.name, 
  state_abb = state.abb,
  longitude = pluck(state.center,"x"),
  latitude = pluck(state.center,"y")
)

normal_education_color <- "#00496f"
long_education_color <- "#dd4124"
degree_earning_map <- degree_comparison %>% 
  left_join(states, by = "state") %>% 
  mutate (
  longitude = case_when(
    state_abb == "RI" ~ longitude + 1.8,
    state_abb == "NH" ~ longitude + 1.05,
    state_abb == "NJ" ~ longitude + .2,
    state_abb == "MA" ~ longitude + 0.2,
    TRUE ~ longitude),
  latitude = case_when(
    state_abb == "VT" ~ latitude + 1., 
    state_abb == "NC" ~ latitude - .3, 
    state_abb == "NH" ~ latitude + .4, 
    TRUE ~ latitude
  ), 
  state = case_when(state_abb %in% c("PA", "NJ","MA","NH","MD","CT") ~ state_abb, 
                    TRUE ~ state)
)
us_states <- map_data("state")
ggplot() +
  # plot states ----
geom_polygon(
  data = us_states,
  aes(
    x     = long, 
    y     = lat, 
    group = group
  ),
  fill  = "grey95",
  size = .75,
  color = rgb(151,151,151,50,maxColorValue=255)
)  + 
  geom_text(
    data = degree_earning_map, 
    aes(
      x = longitude,
      y = latitude,
      label = state
    ),
    vjust = 1.2,
    hjust = 0.2,
    family = "Source Sans Pro",
    fontface = "bold",
    size = 3.5
    
  ) + 
  geom_segment(
    data = filter(degree_earning_map, education_level == "normal_sup_education"),
    aes(
      x = longitude, 
      xend = longitude,
      y = latitude, 
      yend = latitude + round(avg_annual_salary) / 70000 
    ),
    size = 4,
    color = "#00496f"
  ) + 
  geom_segment(
    data = filter(degree_earning_map, education_level == "long_sup_education"),
    aes(
      x = longitude+0.5, 
      xend = longitude+0.5,
      y = latitude, 
      yend = latitude + round(avg_annual_salary) / 70000 
    ),
    size = 4,
    color = "#dd4124"
  ) +
  geom_text(
    data = filter(degree_earning_map, education_level == "normal_sup_education"),
    aes(
      x = longitude, 
      y = latitude + round(avg_annual_salary) / 70000,
      label=   paste0(round(avg_annual_salary/1000),"K")
    ),
    vjust = -.3,
    size = 2.8,
    color = "black",
    family = "Inconsolata"
  ) + 
  geom_text(
    data = filter(degree_earning_map, education_level == "long_sup_education"),
    aes(
      x = longitude+0.8, 
      y = latitude + round(avg_annual_salary) / 70000,
      label=   paste0(round(avg_annual_salary/1000),"K")
    ),
    hjust = .6,
    vjust = -.3,
    size = 2.8,
    color = "black",
    family = "Inconsolata"
  ) + 
  annotate(
    geom = "richtext", 
    x = -118, y = 30, 
    family = "Source Sans Pro",
    label = "Data from : Ask a manger survey.<br>
    Tidytuesday Week-21 2021 | <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**",
    fill = NA, label.color = NA,
    hjust = 0
  ) + 
  annotate(
    geom = "richtext",
    x = -95, y = 51,
    family = "Inconsolata",
    label = '<span style= "font-size:25px;"> COMPUTING OR TECH INDUSTRY</span><br><br> Difference of average annual salary  between people with <span style="color:#00496f">College or Master\'s degrees</span> <br>
    and people with <span style = "color:#dd4124">PhD or Professional degrees</span> by state
    according to \"Ask a Manager Survey\"',
    fill = NA, label.color = NA,
    size = 4.5,
    fontface = "bold"
  )  +
   theme_void() + 
  theme(
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(t = 20, b= 10)
  )

ggsave(here::here("Outputs","tidytuesday_2021_w21.png"), width = 18, height = 11, dpi= 300)


