
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(sf)

# Data Wrangling ----------------------------------------------------------
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

glimpse(stressor)

colony <- colony %>% 
  rowwise() %>% 
  mutate(
    colony_evolution_pct = 100*(colony_added - colony_lost)/ colony_n
  ) 


unique(colony$state)
other_stressor <- stressor %>% 
  filter(stressor != "Varroa mites") %>% 
  group_by(year, months, state) %>% 
  slice_max(order_by = stress_pct, n = 1) %>% 
  ungroup() %>% 
  arrange(state)

colony_with_stressor <- colony %>% 
  left_join(other_stressor, by = c("year", "months", "state"))


us_states <- tigris::states() %>% 
  filter(!NAME %in% c("Commonwealth of the Northern Mariana Islands","United States Virgin Islands", "Guam","American Samoa", "Puerto Rico")) %>% 
  tigris::shift_geometry()

small_states_names <- c("VT","MA","CT", "NJ", "MD") #"RI","DE", "DC"
us_states <- us_states %>% 
  mutate(state_center = sf::st_centroid(geometry),
         long = sf::st_coordinates(state_center)[,1],
         lat = sf::st_coordinates(state_center)[,2]
  )  %>% 
  
  # small_states <- us_states %>% 
  #   filter(STUSPS %in% small_states_names)
  # 
  # small_states <- small_states %>% 
  mutate(
    ctl_pt1_long  = case_when(
      STUSPS == "DC" ~ long + 5e5,
      STUSPS == "MD" ~ long + 5e5,
      STUSPS == "DE" ~ long + 2.75e5,
      STUSPS == "NJ" ~ long + 2.85e5,
      STUSPS == "CT" ~ long + 2.5e5,
      STUSPS == "RI" ~ long + 2.5e5,
      STUSPS == "MA" ~ long + 3e5,
      STUSPS == "VT" ~ long - .5e5,
      TRUE ~ long),
    ctl_pt1_lat  = case_when(
      STUSPS == "DC" ~ lat - 8.5e5,
      STUSPS == "MD" ~ lat - 6.25e5,
      STUSPS == "DE" ~ lat - 3.5e5,
      STUSPS == "NJ" ~ lat - 2.5e5,
      STUSPS == "CT" ~ lat - 1.5e5,
      STUSPS == "RI" ~ lat + 1.5e5,
      STUSPS == "RI" ~ lat  + 2.5e5,
      STUSPS == "MA" ~ lat + 5e5,
      STUSPS == "VT" ~ lat + 4.5e5,
      TRUE ~ lat), 
    ctl_pt2_long = case_when(
      STUSPS == "VT" ~ long - 2e5,
      STUSPS == "DC" ~ long + 8e5,
      STUSPS == "MD" ~ long + 8e5,
      STUSPS == "DE" ~ long + 7e5,
      STUSPS == "NJ" ~ long + 6e5,
      STUSPS == "CT" ~ long + 6e5,
      STUSPS == "RI" ~ long + 6e5,
      STUSPS == "MA" ~ long + 4e5,
      TRUE ~ long + 8e5
    ),
    ctl_pt2_lat = ctl_pt1_lat
  ) 

month_data <- us_states %>% 
  left_join(colony_with_stressor, by = c("NAME" = "state")) %>%
  filter(months == "January-March", year == 2017) 

glimpse(month_data)
month_data %>%
  ggplot() + 
  geom_sf(data = us_states, fill = "grey65",size = 0.25) + 
  geom_sf(aes(fill = stressor) ) + 
  geom_point(data = filter(us_states, STUSPS %in% small_states_names), aes(long, lat)) + 
  geom_segment(data = filter(us_states, STUSPS %in% small_states_names), aes(x = long, y = lat, xend = ctl_pt1_long, yend = ctl_pt1_lat)) + 
  geom_segment(data = filter(us_states, STUSPS %in% small_states_names), aes(x = ctl_pt1_long, y = ctl_pt1_lat, xend = ctl_pt2_long, yend = ctl_pt2_lat)) + 
  geom_richtext(data =  filter(month_data, !STUSPS %in% small_states_names), aes(x = long, y = lat, label = STUSPS)) +
  geom_richtext(data =  filter(month_data, STUSPS %in% small_states_names), aes(x = ctl_pt2_long, y = ctl_pt2_lat, 
                                                                                label = STUSPS,
                                                                                hjust = ifelse(STUSPS == "VT", 1.1,  -.1))) 


# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w2", "tidytuesday_2022_w2")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

