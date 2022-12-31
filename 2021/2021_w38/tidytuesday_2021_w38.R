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

madonna_years <- madonna %>% 
  group_by(year) %>% 
  summarise(
    across(danceability: spotify_track_popularity, mean)
  ) 


# Graphic -----------------------------------------------------------------
img <- png::readPNG(here::here("2021_w38/madonna.png"))
g_pic <- rasterGrob(img, interpolate = T)
annotation_1992  <-  "<span style='color : black;'>***'This Used To Be My Playground', 'Erotica' and <br>'Deeper And Deeper'***</span> were released.<br>
She can't help falling in love, so <br>
She felt deeper and deeper the further She went."
annotation_1996 <-  "<span style='color : black;'>***'Love Don't Live Here Anymore'***</span> <br>enters in Billboard Magazine <br>on 04/20/1996 
at the 91th place."
annotation_1997 <- " In 1997, She didn't want \n Argentina to cry for her."
annotation_middle <- "Queen of Pop kept doing what \nshe is the best at, Pop Music."
annotation_2012 <- "<span style='color : black;'>***'Give Me All Your Luvin'***</span> featuring<br> **Nicki Minaj** & **M.I.A.** in 2012 marks 
<br>a turning point towards<br> more rhythmic songs."
madonna_years %>% 
  ggplot(aes(loudness,tempo, label = year)) + 
  geom_curve(aes(xend =c(tail(loudness, n = -1), NA),yend =c(tail(tempo, n = -1), NA)), 
             curvature = .15, size = 2, color = "grey") + 
  geom_point(size = 20, pch = 21, fill = "#CB2027", color = "white") + 
  geom_text(color = "white", family = "Lato Black")   + 
  annotate(geom = "richtext", x = -6.65, y = 99, label = annotation_1992,size = 4, color = "grey30",
           vjust = .5, fill = NA, label.size = 0, label.color = NA, family = "Mercury", fontface = "bold.italic") +
  annotate(geom = "richtext", x = -14, y = 116, label = annotation_1996, size = 4,color = "grey30",
           vjust = 0, fill = NA, label.size = 0, label.color = NA,family = "Mercury", fontface = "bold.italic") +
  annotate(geom = "text", x = -14, y = 101, label = annotation_1997, vjust = 1,size = 4,color = "grey30",
           family = "Mercury", fontface = "bold.italic") +
  annotate(geom = "text", x = -9.95, y = 128, label = annotation_middle, size = 4,color = "grey30",
           family = "Mercury", fontface = "bold.italic") +
  annotate(geom = "richtext", x = -4.75, y = 144.5, label = annotation_2012, size = 4,hjust = 1, color = "grey30",
           fill = NA, label.size = 0, label.color = NA, family = "Mercury", fontface = "bold.italic") +
  annotation_custom(g_pic, xmin=-15, xmax=-12, ymin=130, ymax=155)+
  # Axis titles
  annotate(geom = "text", x = -2.9, y = -Inf, label = "Loudness", hjust = "inward", vjust = "inward", size = 5.5, color = "grey15", family = "Lato Black") + 
  annotate(geom = "text", x = -Inf, y = 100, label = "Tempo",angle = 90, hjust = "inward", vjust = "inward", size = 5.5, color = "grey15", family = "Lato Black") + 
  labs(title = "Rythmic Evolution of Madonna",
       caption = "**dB**: decibels<br>**BPM**: Beats Per Minute<br>
       Data from ***Data.World*** by way of Sean Miller, Billboard.com and Spotify.<br>
       Tidytuesday Week-38 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**.")  + 
  scale_x_continuous(
    breaks = seq(-15,-6, by = 3), 
    labels = c("<-- More Loud (-15dB)", "-9dB", "-6dB", "(-3dB) Less Loud -->")
  ) +
  scale_y_continuous(
    breaks = seq(110,150, by = 10),
    labels = function(x) paste0(x, "BPM"),
    expand = expansion(add = c(5,0))
  )+ 
  theme_minimal() + 
  coord_cartesian(clip ="off") + 
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(size = .25, color = "black"),
    axis.ticks.length = unit(.35, "cm"),
    axis.title = element_blank(),
    axis.text = element_markdown(color = "black", family = "Lato", size = rel(1.05)),
    plot.title = element_text(size = rel(3), hjust = .5,family = "Mercury", face = "bold", margin = margin(b = 25)),
    plot.caption = element_markdown(size = rel(1.1),family = "Lato Semibold", color = "black", margin = margin(t = 20)), 
    plot.margin = margin(t = 25, r = 10, b = 20, l = 15),
    plot.background = element_rect(fill = "grey97", color = NA)
  )

# Saving ------------------------------------------------------------------
path <-  here::here("2021_w38/tidytuesday_2021_w38")

ggsave(glue::glue("{path}.pdf"), width = 14.5, height = 10.5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)
