# Load libraries ----------------------------------------------------------
library(tidyverse)
Sys.setlocale(locale = "en_US.UTF-8")

# Data Reading and Wrangling ----------------------------------------------

# NY TIMES Data 
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
usa_states <- read_csv(here::here("2022_w1","usa_states_population.csv")) %>% 
  select(State, Pop)

# States Geometries
usa_states_geom <- albersusa::usa_sf() %>% 
  sf::st_transform(albersusa::us_laea_proj) %>%
  select(name)

covid_cases <- covid_data %>% 
  group_by(state, fips) %>% 
  arrange(date) %>% 
  # DON'T USE DIFF - somedays data  are incoherent 
  mutate(
    pd_cases = lag(cases) # Previous Day Cases
  ) %>% 
  replace_na(list(pd_cases = 0)) %>% 
  mutate(
    daily_cases = case_when(cases > pd_cases ~ cases - pd_cases, 
                            TRUE ~ 0)
  ) %>%
  ungroup() %>% 
  arrange(state, date) 

# Roll Mean Computing
covid_cases_rm <- covid_cases %>% 
  mutate(roll_cases = zoo::rollmean(daily_cases, k = 5, fill = NA)) %>% 
  # Select 2021 Data
  filter(lubridate::year(date) == 2021) %>% 
  left_join(usa_states, by = c("state" = "State")) %>% 
  drop_na(Pop) %>% 
  mutate(incidence_rate = 10^5 *roll_cases / Pop) %>% 
  mutate(incidence_rate  = cut(incidence_rate, breaks = c(seq(0,50,5), Inf), include.lowest = T) %>% 
           factor(labels = paste0(">", seq(0,50, 5)))) 


# Graphic -----------------------------------------------------------------
bg_color <- "#e5e4e2"
font_family <- "MTGB1"
caption <- "Incidence rate are calculated for 100,000 people in each state.
Inspired from a graphic in the DIE ZEIT newspaper of November 18, 2021.
Data from NY Times · Tidytuesday Week-52 2021 · Abdoul ISSA BIDA."

# Plot 
covid_evolution_plot <- usa_states_geom %>% 
  left_join(covid_cases_rm, by = c("name" = "state")) %>% 
  mutate(fancy_date = fct_inorder(format(date, "%b. %d"))) %>% 
  ggplot() + 
  geom_sf(aes(fill = incidence_rate), size = .05, color = "grey55") + 
  facet_wrap(vars(fancy_date), strip.position = "bottom")+ 
  colorspace::scale_fill_discrete_sequential(name = str_to_upper("COVID-19 Incidence Rate"),
                                             palette = "Rocket", rev = T, 
                                             guide = guide_legend(
                                               nrow = 1,
                                               keyheight = unit(.75, "cm"),
                                               keywidth = unit(.75, "cm"),
                                               label.position = "right",
                                               label.theme = element_text(family = font_family, size = rel(15), margin = margin(r = 15)),
                                               title.theme = element_text(family = font_family, size = rel(18),margin = margin(b = .5, unit = "cm")),
                                               title.position = "top",
                                               title.hjust = .5
                                             )) +
  theme_minimal() + 
  theme(
    text = element_text(family = font_family, color = "#111111"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text( size = rel(1.125), face = "bold"),
    legend.position = "top",
    legend.text =  element_text(family = font_family),
    plot.background = element_rect(fill = bg_color , color = NA)
  )

# Plot with title and Caption 
(final_plot <- cowplot::ggdraw(covid_evolution_plot) + 
  labs(title = "2021 · A pandemic year",
       caption = caption) + 
  theme(
    plot.title = element_text(color = "#111111", family = font_family, size = rel(5), face = "bold", margin = margin(t = .5, b = .5, unit = 'cm')),
    plot.caption = element_text(family = font_family, size = rel(1.25), hjust = .5, face = "bold", margin = margin(t = .25, b = .25, unit = 'cm')),
    plot.background = element_rect(fill = bg_color, color = NA),
    plot.margin = margin(t = .5, r= .5, b = .5, l=.5, unit = "cm"),
    legend.box.spacing = unit(.5, "cm")
  )
)

# Saving ------------------------------------------------------------------
path <- here::here("2022_w1", "tidytuesday_2022_w1")
ggsave(filename = glue::glue("{path}.png"), width = 25, height = 25, device = ragg::agg_png, dpi = 320)
