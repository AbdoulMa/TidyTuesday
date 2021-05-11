library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(sf)
library(albersusa)
library(PNWColors)
library(patchwork)
# https://rud.is/books/30-day-map-challenge/rural-01.html
# Load Data & Wrangling ---------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 20)
broadband <- pluck(tuesdata, "broadband")

broadband <- broadband %>% 
  mutate(across(4:5, parse_double)) 

states_broadband_mean <- broadband %>% 
  group_by(ST) %>%
  summarise(across(3:4, ~mean(.x, na.rm =T))) %>% 
  select(
    ST,
    mean_bb_per_ffc = `BROADBAND AVAILABILITY PER FCC`,
    mean_bb_usage = `BROADBAND USAGE`
  )  

broadband <-  broadband %>%
  mutate(fips = `COUNTY ID`,
         bb_per_ffc = `BROADBAND AVAILABILITY PER FCC`,
         bb_usage = `BROADBAND USAGE`) %>% 
  mutate(
    fips = case_when(fips < 9999 ~ str_c("0",as.character(fips)), 
                     TRUE ~ as.character(fips))
  )
  
counties <- counties_sf()
counties <- counties %>% 
  mutate(fips = as.character(fips))
  
broadband_sf <- counties %>%  
  left_join(broadband, by = "fips")

my_theme <-  theme_minimal(base_family = "Source Sans Pro") + 
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text =  element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 20,r =10,b = 20,l = 20),
    plot.title = element_text(size = rel(1.5), face = "bold", hjust = .5),
  )
  
map <- broadband_sf %>%  
  ggplot() +
  geom_sf( aes(fill = bb_usage),color = "#b2b2b2", size = 0.1) +
  labs(
    title = "Map Internet Access"
  ) + 
  scale_fill_gradientn(colours = pnw_palette("Bay",100), guide = "coloursteps") +
  coord_sf(crs = "ESRI:102003") +
  my_theme +
  theme(legend.position = c(0.985, 0.5))



distribution <- states_broadband_mean  %>%
  arrange(mean_bb_per_ffc) %>%
  mutate(
    mean_bb_per_ffc = round(mean_bb_per_ffc*100,0)
  ) %>% 
  group_by(mean_bb_per_ffc) %>% 
  summarize(
    states = str_c(ST, collapse = ",")
  ) %>% 
  mutate(
    r_num = row_number(),
    vpos = case_when(r_num %% 2 == 0 ~ 0.95,
                     TRUE ~ 1.05),
    vjust = case_when(r_num %% 2 == 0 ~ 1,
                      TRUE ~ 0),
    label = glue::glue( '<span style = "color : black;"><span>{states}</span><br><b><span style = "font-size:12px;">{mean_bb_per_ffc}%</span></b></span>')
  ) %>% 
  ggplot() + 
  annotate(geom = "segment", x = 55, xend = 105, y = 1, yend = 1, size = 1.1, color = "grey75") +
  geom_richtext(aes( mean_bb_per_ffc, y = vpos,label = label, vjust = vjust ), fill = NA, label.colour = NA, family = "Source Sans Pro") +
  geom_segment(aes(x = mean_bb_per_ffc, xend = mean_bb_per_ffc, y = 1, yend = vpos ), color = "grey60") +
  geom_point(aes(x = mean_bb_per_ffc, y = 1), size = 4, color = "#DD4124") + 
  labs(
    title = "Average of people per state with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps"
  ) + 
  scale_y_continuous(
  name = NULL,
  limits = c(0.45,1.45)
  ) + 
  my_theme 

map / distribution + 
  plot_layout(nrow = 2, heights = c(5,2))
