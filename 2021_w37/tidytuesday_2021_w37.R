# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
# Data Reading and Wrangling ----------------------------------------------
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')

# Graphic -----------------------------------------------------------------
season_lst_races <- races %>% 
  group_by(year) %>%
  slice_max(date, n = 1)

# Season Winners
season_lst_races %>% 
  left_join(driver_standings, by = "raceId") %>% 
  filter(position == 1) %>% 
  left_join(drivers, by = "driverId") 

# Constructors Winners
season_lst_races %>% 
  left_join(constructor_standings, by = "raceId") %>% 
  filter(position == 1) %>% 
  left_join(constructors, by = "constructorId") 


gp_winners <- results %>% 
  filter(position == 1) %>% 
  left_join(races, by= "raceId") %>% 
  left_join(drivers, by = "driverId") %>% 
  left_join(constructors, by = "constructorId", suffix = c("_other","_constructor" )) %>% 
  mutate(name_other = str_replace(name_other, "Grand Prix", "GP"))

gp_winners_10_20 <-  gp_winners %>% 
  filter(between(year, 2010, 2020)) %>% 
  add_row(year = 2010:2019) %>% 
  arrange(year) 
  
nb_gps <- nrow(gp_winners_10_20)
gp_winners_10_20 <- gp_winners_10_20 %>% 
  mutate(
    len = seq(pi, 0, length.out = nb_gps),
    x = cos(len), 
    y = sin(len),
    label_y = sin(len) * 3, 
    label_angle = seq(180, 0, length.out = nb_gps)
  ) %>% 
  mutate(
    driver_CTC = case_when(nationality_other == "Spanish" ~ "ESP",
                           nationality_other == "British" ~ "GBR",
                           nationality_other == "German" ~ "GER",
                           nationality_other == "Australian" ~ "AUS",
                           nationality_other == "Venezuelan" ~ "VNZ",
                           nationality_other == "Finnish" ~ "FIN",
                           nationality_other == "Dutch" ~ "NED",
                           nationality_other == "Monegasque" ~ "MON",
                           nationality_other == "French" ~ "ESP",
                           nationality_other == "Mexican" ~ "MEX",
                           is.na(nationality_other) ~ ""
                           ) 
    
  ) %>% 
  mutate(
    fancy_winner = glue::glue("<span style='color:grey75'>{name_other}<span> &bull; <span style='font-size:11px;font-weight:bolder;'>{forename} {surname}</span> <span style='font-size: 6px; color: grey55;'>{driver_CTC}</span> <span>({name_constructor})</span>")
  )

(
  season_lst_races %>% 
    first()
)

(grid_positions <- gp_winners_10_20 %>% 
  select(x, y, grid) %>% 
    slice(rep(1:n(), each = 22)) %>% 
    mutate(
      grid_position = rep(1:22, nb_gps),
      c = rep(seq(1.8,1.4, length.out = 22), nb_gps),
           x = c *x,
           y = c*y)
  
)

  ggplot() + 
  # geom_segment(data = filter(gp_winners_00_20, !is.na(resultId)), aes(x = x, y = y,
  #                  xend = xend, yend = yend),
  #              size = .75,
  #              )  +
  geom_point(data = filter(grid_positions, !grid_position == grid), aes(x = x, y =y),
               size = .55,
             alpha = .3,
             color = "white"
               )  +  
  geom_point(data = filter(grid_positions, grid_position == grid), aes(x = x, y =y),
               size = .85,
             alpha = 1, pch = 21, 
             
               )  +
  # Dividers
  geom_segment(data = filter(gp_winners_10_20, is.na(resultId)), aes(x = 1.35*x, y = 1.35*y,
                                                                      xend = 2.75*x, yend = 2.75*y),
               size = .5,
               color = "white"
               ) + 
    geom_richtext(data = filter(gp_winners_10_20, !is.na(resultId)),aes(cos(len)*2.35,sin(len)*2.35, label = fancy_winner, angle = label_angle),
                  fill = "transparent",
                  label.color = NA,
                  size = 2.5,
                  color = "white"
    ) + 
    theme_minimal() + 
    theme(
      plot.background = element_rect(fill = "#212121", color = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
    
  
# Title
  plot_ratio <- pi
  tibble(theta = seq(0, 2 * pi, length.out = 13),
         y = sin(theta),
         tangent_slope = cos(theta) * plot_ratio,
         text_angle = atan(tangent_slope)) %>%
  ggplot(aes(theta, y)) +
  geom_richtext(aes(label = round(y, 2),
                    angle = text_angle * 180 / pi),
                vjust = 0,
                label.color = NA,
                label.padding = unit(1, "pt"),
                label.margin = unit(2, "pt"),
                size = 4) +
  geom_point() +
  stat_function(fun = sin) +
  scale_x_continuous(expression(theta),
                     breaks = seq(0, 2 * pi, length.out = 13),
                     minor_breaks = NULL,
                     labels = function(x) round(x * 180 / pi)) +
  scale_y_continuous(expression(sin(theta))) +
  coord_fixed(ratio = plot_ratio, clip = "off") +
  theme_minimal(base_size = 16) + 
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )  
# Saving ------------------------------------------------------------------
