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
seasons_winners <- season_lst_races %>% 
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
    fancy_winner = glue::glue("<span style='font-size:8.5px;color:grey7'>{name_other}<span> &bull; <span style='color: #000000;font-size:11px;font-weight:bolder;'>{forename} {surname}</span> <span style='font-size: 6px; color: grey7;'>{driver_CTC}</span> <span>({name_constructor})</span>")
  )

(
  seasons_winners <- seasons_winners %>% 
    ungroup() %>% 
    filter(between(year,2010,2020)) %>% 
    mutate(
      fancy_season_winner = glue::glue("{year} <br> {forename} {str_to_upper(surname)}"),
      theta = seq(pi,  0, length.out = 11),
      x = cos(theta),
      y = sin(theta),
      # tangent_slope = cos(theta) * plot_ratio,
      # text_angle = atan(tangent_slope)
    )
)

(grid_positions <- gp_winners_10_20 %>% 
    select(x, y, grid) %>% 
    slice(rep(1:n(), each = 22)) %>% 
    mutate(
      grid_position = rep(1:22, nb_gps),
      c = rep(seq(2,1.6, length.out = 22), nb_gps),
      x = c *x,
      y = c*y)
  
)

ggplot() +
  geom_point(data = filter(grid_positions, !grid_position == grid), aes(x = x, y =y),
             size = .55,
             alpha = .3,
             # color = "white"
  )  +  
  geom_point(data = filter(grid_positions, grid_position == grid), aes(x = x, y =y),
             pch = 21, 
             color = "black",
             fill = "white",
             stroke = .85
             
  )  +
  geom_richtext(data = filter(gp_winners_10_20, !is.na(resultId)),aes(cos(len)*2.55,sin(len)*2.55, label = fancy_winner, angle = label_angle),
                fill = "transparent",
                label.color = NA,
                size = 2.5,
                family = "Bahnschrift"
  ) +
  geom_richtext(data =seasons_winners, aes(x= 3.2*x, y =3.2*y,label = fancy_season_winner
  ),
  vjust = 0,
  fill = NA,
  label.color = NA,
  family = "Forza Black",
  label.padding = unit(1, "pt"),
  label.margin = unit(2, "pt"),
  size = 5) + 
  annotate(geom="text", x = 1.8 ,y= -0.25, label = "GRID POSITIONS",color = "#151515")+ 
  annotate(geom="text", x = -1.8 ,y= -0.25, label = "GRID POSITIONS",color = "#151515")+
  annotate(geom="text", x = 2 ,y= -0.05, label = "1st", angle = 270, color = "#151515", size = 3, hjust = 0)+
  annotate(geom="text", x = 1.6 ,y= -0.05, label = "22nd", angle = 270, color = "#151515",size = 3,hjust = 0)+
  annotate(geom="text", x = -2 ,y= -0.05, label = "1st", angle = 90, color = "#151515", size = 3, hjust = 1)+
  annotate(geom="text", x = -1.6 ,y= -0.05, label = "22nd", angle = 90, color = "#151515",size = 3,hjust = 1)+
  annotate(geom= "segment", x = -.5, xend = .5, y = 3.5, yend = 3.5 ,color="#FF1801", size = 2) +
  annotate(geom= "segment", x = -.5, xend = .5, y = 3.9, yend = 3.9,color="#FF1801",size = 2) +
  annotate(geom = "text", x = 0, y = 3.7, label = "2010-2020", family = "Gotham Medium", fontface = "bold", size= 15) + 
  annotate(geom = "richtext", x = 0, y = .5, label = "<span style='font-size:145px;'><span>F</span><span style='color:#FF1801;'>1</span></span><br><span>Formula</span><span style='color:#FF1801;'>1</span>",
           fill = NA, label.color = NA,label.padding = unit(0,"cm"),
           family = "Forza Black", fontface = "bold.italic") + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#F5F5F5", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
coord_cartesian()


# Saving ------------------------------------------------------------------
file_name = here::here("2021_w37/tidytuesday_2021_w37")

ggsave(glue("{filename}.pdf"), width = 9, height = 7.5, device = cairo_pdf)