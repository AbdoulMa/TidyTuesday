
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(cowplot)
library(patchwork)

# Data Wrangling ----------------------------------------------------------
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations <- stations %>% 
  drop_na(EV_NETWORK) %>% 
  relocate(c("LONGITUDE","LATITUDE"), .before = 1L)

# States to omit
ommitted_states <- c("AK","VI","AS","GU","MP","PR","HI")

# Load states shapefile
states <- tigris::states() %>% 
  st_transform("ESRI:102003") %>% 
  filter(!STUSPS %in% ommitted_states)

stations <- stations %>% 
  filter(LONGITUDE != 0,!STATE %in% ommitted_states, 
         !CITY %in% c("Guaynabo","San Juan","Rio Grande","Carolina"),
         STATION_NAME != "Niumamua Office Building") %>% 
  mutate(
    LONGITUDE = case_when(ZIP == 98345 ~ -122.6255, 
                          TRUE ~ LONGITUDE),
    LATITUDE = case_when(ZIP == 98345 ~ 47.698, 
                         TRUE ~ LATITUDE),
    EV_NETWORK = fct_lump_n(EV_NETWORK, n = 9, other_level = "Other Networks")) 

# Stations positions approximations to circle centers
stations_pos <- stations %>% 
  mutate(
    # 1 deg -> 54.6 miles
    # .35 deg -> 54.6*.35 = 13.5 miles 
    # 10 / 54.6 = .18 deg -> 10 miles 
    circle_long = plyr::round_any(LONGITUDE, .35, ceiling),
    circle_lat = plyr::round_any(LATITUDE, .35, ceiling), 
    .before = 1L
  ) %>% 
  group_by(circle_long, circle_lat, EV_NETWORK) %>% 
  summarise(
    nb_stations = n()
  ) %>% 
  ungroup() %>% 
  group_by(circle_long, circle_lat) %>% 
  slice_max(order_by = nb_stations, n = 1,with_ties = F) %>% 
  ungroup() 

# Convert points to geoms and edit projection 
stations_geoms <- stations_pos %>% 
  st_as_sf(coords = c("circle_long", "circle_lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform("ESRI:102003") 

# Graphic -----------------------------------------------------------------
colors_palette <- c(
  "Blink Network" = "#5d2d91",
  "ChargePoint Network" = "#e72522",
  "EV Connect" = "#01aef0",
  "Greenlots"      = "#84bc41",
  "Non-Networked"  = "#005db5",
  "SemaCharge Network" = "#00a6c0",
  "Tesla" = "#ffcc06",            
  "Tesla Destination"  = "#be1a8b", 
  "Volta" = "#111111",         
  "Other Networks" = "#02954e" 
)

# Points Map Plot
plot <- ggplot() + 
  geom_sf(data = filter(states, !STUSPS %in% c("HI","PR")), fill = NA, size = .1) +
  geom_sf(data = stations_geoms, aes(color = EV_NETWORK), size = 1.125) + 
  scale_color_manual(
    values = colors_palette
  ) + 
  theme_minimal()+ 
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank(), 
    axis.text = element_blank()
  )

# Stations networks Legend
networks <- levels(stations_geoms$EV_NETWORK)
legend_plot <- ggplot() + 
  geom_text(data = tibble(networks), aes(x = 1:10, y = 1, label = networks),
            size = 2.35, family = "Go Medium") + 
  geom_point(aes(x = 1:10, y = 0, color = networks), size = 3.5) + 
  scale_color_manual(
    values = colors_palette
  ) + 
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )

combine_plot <- legend_plot + plot +
  plot_layout(ncol = 1,heights = c(.035,.965)) & 
  theme(
    plot.background = element_rect(fill = "#b4ad96", color = NA)
  )

# Final Plot : Combine Plot + Caption Annotation 
caption <- "*Keeps only continental\n states stations.\nData from US DOT \nby way of Data is Plural. \n \nTidytuesday Week-8 2022 \nAbdoul ISSA BIDA\ninspired by Nathan YAU."
ggdraw(combine_plot) + 
  draw_label(x = .1, y = .15, 
             label = caption,
             size = 12.5,
             hjust = 0,
             fontfamily = "NY Bold"
  ) + 
  labs(title = "Nearest Stations", 
       subtitle = "The most common alternative transportation\n fuel stations within a 20 miles radius") + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#b4ad96", color = NA),
    plot.margin = margin(t = 1, r= .5, b=.5, l=.5, unit = "cm"),
    plot.title = element_text(family = "Go Black", size = rel(3.5)),
    plot.subtitle = element_text(family = "NY Bold", size = rel(2))
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w9", "tidytuesday_2022_w9")
ggsave(filename = glue::glue("{path}.png"), width = 10.5, height = 8, device = ragg::agg_png, dpi = 300)

