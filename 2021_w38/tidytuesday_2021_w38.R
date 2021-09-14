# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(grid)
# Data Reading and Wrangling ----------------------------------------------
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio_features <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

madonna <- billboard %>% 
  filter(str_detect(performer, 'Madonna')) %>% 
  mutate(week = lubridate::mdy(week_id),
         year = lubridate::year(week)) %>% 
  distinct(song, .keep_all = T) %>% 
  left_join(audio_features, by = "song_id", suffix = c("_billboard","_features")) %>% 
  drop_na(spotify_track_id) %>% 
  arrange(week)

madonna  %>% 
  filter(year == 1997) %>% 
  view()
madonna_years <- madonna %>% 
  group_by(year = week) %>% 
  summarise(
    across(danceability: spotify_track_popularity, mean)
  ) 


# Graphic -----------------------------------------------------------------
img <- png::readPNG(here("2021_w38/madonna.png"))
g_pic <- rasterGrob(img, interpolate = T)
annotation_1992  <-  "<span style='color : grey35;'>***'This Used To Be My Playground', 'Erotica' and <br>'Deeper And Deeper'***</span> were released.<br>
She can't help falling in love, so <br>
She felt deeper and deeper the further She went."
annotation_1996 <-  "<span style='color : grey35;'>***'Love Don't Live Here Anymore'***</span> <br>enters in Billboard Magazine <br>on 04/20/1996 
at the 91th place."
annotation_1997 <- " In 1997, She didn't want \n Argentina to cry for her."
annotation_middle <- "Queen of Pop kept doing what \nshe is the best at, Pop Music."
annotation_2012 <- "<span style='color : grey35;'>***'Give Me All Your Luvin'***</span> featuring<br> **Nicki Minaj** & **M.I.A.** in 2012 marks 
<br>a turning point towards<br> more rhythmic songs."
madonna_years %>% 
  ggplot(aes(loudness,tempo, label = year)) + 
  geom_curve(aes(xend =c(tail(loudness, n = -1), NA),yend =c(tail(tempo, n = -1), NA)), 
             curvature = .15, size = 2, color = "grey") + 
  geom_point(size = 20, pch = 21, fill = "black", color = "white") + 
  geom_text(color = "white")   + 
  annotate(geom = "richtext", x = -6, y = 99, label = annotation_1992,vjust = .5, fill = NA, label.size = 0) +
  annotate(geom = "richtext", x = -14, y = 116, label = annotation_1996, vjust = 0, fill = NA, label.size = 0) +
  annotate(geom = "text", x = -14, y = 101, label = annotation_1997, vjust = 1) +
  annotate(geom = "text", x = -9.95, y = 128, label = annotation_middle) +
  annotate(geom = "richtext", x = -4.75, y = 144.5, label = annotation_2012, hjust = 1, fill = NA, label.size = 0) +
  annotation_custom(g_pic, xmin=-15, xmax=-12, ymin=130, ymax=155)+
  labs(title = "Rythmic Evolution of Madonna",
       caption = "dB: decibels<br>BPM : Beats per minute<br>
       Data from ***Data.World*** by way of Sean Miller, Billboard.com and Spotify.<br>
       Tidytuesday Week-38 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**.")  + 
  scale_x_continuous(
    name = "Loudness",
    breaks = seq(-15,-6, by = 3), 
    labels = c("More Louder (-15dB)", "-9dB", "-6dB", "(-3dB) Less Louder")
  ) +
  scale_y_continuous(
    name = "Tempo", 
    breaks = seq(110,150, by = 10),
    labels = function(x) paste0(x, "BPM")
  )+ 
  theme_minimal() + 
  coord_cartesian(clip ="off") + 
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(size = .25, color = "black"),
    axis.ticks.length = unit(.25, "cm"),
    axis.title = element_text(size = rel(1.25)),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(hjust = .95),
    axis.title.y = element_text(hjust = 0.1),
    plot.caption = element_markdown(margin = margin(t = 15)), 
    plot.margin = margin(t = 25, r = 10, b = 20, l = 15)
  )



2012 

# Saving ------------------------------------------------------------------
