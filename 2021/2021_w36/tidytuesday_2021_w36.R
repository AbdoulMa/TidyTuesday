# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(glue)
library(patchwork)
library(ggtext)
library(ragg)

# Data Reading and Wrangling ----------------------------------------------
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

(regions <- bird_baths %>% 
  filter(bird_count != 0, !is.na(bioregions)) %>% 
  group_by(bioregions) %>% 
  summarise(
    n_types = n_distinct(bird_type)
  ) %>% 
    arrange(n_types)
)  

# https://data.gov.au/data/dataset/70bb7ab7-e8a9-4be5-aa73-85bb22c2cb88 Download SF
aus_bioregions <- read_sf("2021_w36/IBRA/IBRA7_regions.shp")

# Joint
bioregions_birds <- aus_bioregions %>% 
  left_join(regions, by = c("REG_NAME_7" = "bioregions")) %>% 
    filter(!is.na(n_types))

# Compute centroid for padding
bioregions_birds <- bioregions_birds %>% 
  mutate(centroid = st_centroid(geometry))

# Graphic -----------------------------------------------------------------
# Function to draw 
graph <- function(geometry, centroid, REG_NAME_7, n_types){
  padding_y <- case_when(
    REG_NAME_7 %in% c("Victorian Midlands","South East Coastal Plain") ~ 1.5, 
    REG_NAME_7 %in% c("South Eastern Queensland","South Eastern Highlands","NSW South Western Slopes","NSW North Coast","Flinders Lofty Block") ~ 3.5, 
    REG_NAME_7 %in% c("Sydney Basin") ~ 2.5, 
    TRUE ~ 6
  )
  padding_x <- case_when(
    REG_NAME_7 %in% c("Victorian Midlands","South East Coastal Plain") ~ 4.5, 
    REG_NAME_7 %in% c("Sydney Basin","South Eastern Queensland","South Eastern Highlands","NSW South Western Slopes","NSW North Coast","Flinders Lofty Block") ~ padding_y, 
    TRUE ~ 6
  )
  ggplot2::ggplot(geometry) +
    geom_sf(color = "#212121", fill = "#228833", size = .25) +
    coord_sf(xlim = c(pluck(centroid,1)-padding_x ,
                      pluck(centroid,1)+padding_x),
             ylim = c(pluck(centroid,2)-padding_y ,
                      pluck(centroid,2)+padding_y),
             expand = FALSE) + 
    labs(subtitle = str_to_upper(glue("**{REG_NAME_7}** <br> {n_types} species"))) + 
    theme_void() + 
    theme(
      plot.subtitle = element_markdown(family = "Inconsolata",hjust = .5, size = rel(1.15))
    )
}


plots <- bioregions_birds %>% 
  select(geometry, centroid, REG_NAME_7, n_types) %>%
  arrange(desc(n_types)) %>% 
  pmap(graph)

(plots[[1]] + plots[[2]] + plots[[3]]) /
  (plots[[4]] + plots[[5]] + plots[[6]]) / 
  (plots[[7]] + plots[[8]] + plots[[9]]) + 
  plot_annotation(
    title = "Birds Diversity in Australian Biogeographic Regions", 
    caption = "Data from Cleary et al, 2016, suggested by Alison Hill.
       Tidytuesday Week-36 2021 - Abdoul ISSA BIDA."
  ) & 
  theme(
    plot.title = element_text(family = "Lato Black",hjust = .5, size = rel(1.8)),
    plot.caption =  element_text(family = "Lato Semibold",color = "grey15", face = "italic",size = rel(1.2), hjust = .5, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F6F7', color = NA),
    plot.margin = margin(t = 15, b = 10, r = 25)
  )

# Saving ------------------------------------------------------------------
ggsave("2021_w36/tidytuesday_2021_w36.png", width = 12, height = 9, device = agg_png, dpi = 320)
