
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(tigris)
library(cowplot)
library(patchwork)

# Data Wrangling ----------------------------------------------------------
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations <- stations %>% 
  drop_na(EV_NETWORK) %>% 
  relocate(c("LONGITUDE","LATITUDE"), .before = 1L)

ommitted_states <- c("AK","VI","AS","GU","MP","PR","HI")

states <- tigris::states() %>% 
  st_transform("ESRI:102003") %>% 
  shift_geometry() %>% 
  filter(!STUSPS %in% ommitted_states)


stations <- stations %>% 
  filter(LONGITUDE != 0,!STATE %in% ommitted_states) %>% 
  mutate(
    LONGITUDE = case_when(ZIP == 98345 ~ -122.6255, 
                          TRUE ~ LONGITUDE),
    LATITUDE = case_when(ZIP == 98345 ~ 47.698, 
                         TRUE ~ LATITUDE),
    EV_NETWORK = fct_lump_n(EV_NETWORK, n = 9, other_level = "Other Networks")) 

stations_pos <- stations %>% 
  mutate(
    circle_long = plyr::round_any(LONGITUDE, .25, ceiling),
    circle_lat = plyr::round_any(LATITUDE, .25, ceiling), 
    .before = 1L
  ) %>% 
  group_by(circle_long, circle_lat, EV_NETWORK) %>% 
  summarise(
    nb_stations = n()
  ) %>% 
  ungroup() %>% 
  group_by(circle_long, circle_lat) %>% 
  slice_max(order_by = nb_stations, n = 1) %>% 
  ungroup() 

# stations_pos %>% 
#   ggplot() + 
#   geom_point(aes(circle_long, circle_lat, color = EV_NETWORK))
glimpse(stations_pos)
stations_geoms <- stations_pos %>% 
  st_as_sf(coords = c("circle_long", "circle_lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform("ESRI:102003") %>%
  shift_geometry()


(plot <- ggplot() + 
    geom_sf(data = filter(states, !STUSPS %in% c("HI","PR")), fill = NA, size = .1) +
    geom_sf(data = stations_geoms, aes(color = EV_NETWORK), size = .9) + 
    theme_minimal()+ 
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      legend.title = element_blank(), 
      axis.text = element_blank()
    ))

networks <- levels(stations_geoms$EV_NETWORK)
# 1 deg -> 54.6 miles
# .25 de -> 54.6/4 = 13.5 miles 
# 10 / 54.6 = .18 deg -> 10 miles 
(legend_plot <- ggplot() + 
    geom_text(data = tibble(networks), aes(x = 1:10, y = 1, label = networks)) + 
    geom_point(aes(x = 1:10, y = 0, color = networks), size = 3.5) + 
    coord_cartesian(clip = "off") + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none", 
    )
)


combine_plot <- legend_plot + plot +
  plot_layout(ncol = 1,heights = c(.05,.95))


caption <- "Data from US DOT by way of Data is Plural. \n\nTidytuesday Week-8 2022 \n Abdoul ISSA BIDA"
ggdraw(combine_plot) + 
  draw_label(x = .15, y = .2, 
             label = caption,
             hjust = 0
             ) + 
  labs(title = "Nearest Stations", 
       subtitle = "The most common alternative transportation fuel stations\n within a 10 miles radius") + 
  theme_minimal() + 
  theme()
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w9", "tidytuesday_2022_w9")
ggsave(filename = glue::glue("{path}.png"), width = 11, height = 8.5, device = ragg::agg_png, dpi = 300)



