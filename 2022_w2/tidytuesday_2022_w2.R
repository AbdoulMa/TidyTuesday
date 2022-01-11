
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(sf)

systemfonts::register_font(
  name = "GNB",
  plain = "/home/abdoul-ma/Téléchargements/Fonts/Gotham Narrow/GothamNarrow-Bold.otf"
)

# Data Wrangling ----------------------------------------------------------
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# Compute colony annual evolution 
colony <- colony %>% 
  filter(months != "2019") %>% 
  replace_na(list(colony_added = 0)) %>% 
  rowwise() %>% 
  mutate(
    colony_evolution_pct = 100*(colony_added - colony_lost)/ colony_n
  )  %>% 
  drop_na(colony_evolution_pct)

# Get the main stressor for each state, each period and each year 
other_stressor <- stressor %>% 
  filter(stressor != "Varroa mites") %>% 
  group_by(year, months, state) %>% 
  slice_max(order_by = stress_pct, n = 1) %>% 
  ungroup() %>% 
  arrange(state)

# Join colony evolution with the stressors data 
colony_with_stressor <- colony %>% 
  left_join(other_stressor, by = c("year", "months", "state"))


# American states geometry
us_states <- tigris::states() %>% 
  filter(!NAME %in% c("Commonwealth of the Northern Mariana Islands","United States Virgin Islands", "Guam","American Samoa", "Puerto Rico")) %>% 
  tigris::shift_geometry()

small_states_names <- c("VT","MA","CT", "NJ", "MD") #"RI","DE", "DC"
us_states <- us_states %>% 
  # compute states centers 
  mutate(state_center = sf::st_centroid(geometry),
         long = sf::st_coordinates(state_center)[,1],
         lat = sf::st_coordinates(state_center)[,2])%>% 
  # Own repels 
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

monthly_data <- us_states %>% 
  left_join(colony_with_stressor, by = c("NAME" = "state")) 


# Graphic -----------------------------------------------------------------
years <- 2015:2016
periods <- c("January-March","April-June","July-September","October-December")
seasons <- c("Winter", "Spring", "Summer", "Autumn")

animation::saveGIF(
  {  
    for (y in years) { # years loop
      for (period in periods) { # period/ season loop 
        if (y != 2019 & period != "April-June") {
          season <-  seasons[which(periods == period)]
          period_data <- monthly_data %>% 
            filter(year == y & months == period) %>% 
            mutate(
              fancy_evolution = case_when(
                colony_evolution_pct > 0 ~ glue::glue("<span>+{round(colony_evolution_pct, 1)}%</span>"),
                TRUE ~ glue::glue("<span>{round(colony_evolution_pct, 1)}%</span>")
              ),
              fancy_label = glue::glue("{STUSPS}<br>{fancy_evolution}")
            )
          
          plot <- period_data %>%
            ggplot() + 
            geom_sf(data = us_states, fill = "grey65",size = 0.25) + 
            geom_sf(aes(fill = stressor), size = 0.25, color = "white") + 
            geom_point(data = filter(us_states, STUSPS %in% small_states_names), aes(long, lat), color = "white", size = .25) + 
            geom_segment(data = filter(us_states, STUSPS %in% small_states_names), aes(x = long, y = lat, xend = ctl_pt1_long, yend = ctl_pt1_lat), color = "white",size = .125) + 
            geom_segment(data = filter(us_states, STUSPS %in% small_states_names), aes(x = ctl_pt1_long, y = ctl_pt1_lat, xend = ctl_pt2_long, yend = ctl_pt2_lat), color ="white", size = .125) + 
            geom_richtext(data =  filter(period_data, !STUSPS %in% small_states_names), 
                          aes(x = long, y = lat, label = fancy_label),
                          family = "Gotham Medium",
                          size = 2,
                          fontface = "bold",
                          fill = NA,
                          label.color = NA,
                          color = "white",
                          label.size = 0) +
            geom_richtext(data =  filter(period_data, STUSPS %in% small_states_names), 
                          aes(x = ctl_pt2_long, y = ctl_pt2_lat, 
                              label = fancy_label,
                              hjust = ifelse(STUSPS == "VT", 1,  0)),
                          family = "Gotham Medium",
                          size = 2,
                          fontface = "bold",
                          fill = NA, 
                          label.color = NA, 
                          color = "white",
                          label.size = 0
            ) + 
            labs(
              title = "Bee colonies",
              subtitle = glue::glue("{season} {y}"), 
              caption = 
                "*Varroa mites which is the principal stressor has been omited.
      Data from USDA with Georgios Karamanis contribution.
     Tidytuesday Week-2 2022 · Abdoul ISSA BIDA."
            ) + 
            scale_fill_manual(
              name = "Stressors",
              values = c(
                "Disesases" = "#F7DC05FF",
                "Other" = "#3D98D3FF",
                "Other pests/parasites" = "#EC0B88FF",
                "Pesticides" = "#F9791EFF",
                "Unknown" = "#3DD378FF",
                "NA" = "#DDDDDD"
              ),
              na.value = "#DDDDDD",
              drop = FALSE
            ) +
            theme_minimal() +
            theme(
              text = element_text(color = "white", family = "Gotham Medium"),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              legend.position = c(.15, .75),
              legend.title.align = .5,
              legend.text = element_text( size = rel(.9)),
              legend.title = element_text( size = rel(2), face = "bold"),
              plot.title = element_text(size = rel(3.5),  family = "Mercury", face = "bold", hjust = .5),
              plot.subtitle = element_text(size = rel(2), face = "bold", hjust = .5),
              plot.caption = element_text(size = rel(1.05)),
              plot.margin = margin(t = .5, b = .5, r = .5, unit = "cm")
            )
          
          plot <- cowplot::ggdraw(plot) + 
            theme(plot.background = element_rect(fill = "#111111", color = NA))
          print(plot)
        }
      }
    }
  }, movie.name =  here::here("2022_w2", "tidytuesday_2022_w2.gif"), inteval = 1, ani.width = 1250, ani.height = 900, ani.res = 160)



