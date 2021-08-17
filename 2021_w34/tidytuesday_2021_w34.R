# Load libraries ----------------------------------------------------------
library(tidyverse)
library(treemapify) # For geom_treemap and friends
library(paletteer) # For color palette 
library(ggtext) # For customize text (used in this script with element_markdown)
library(ragg) # For the device for save the plot

# Data Reading and Wrangling ----------------------------------------------
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')
characters <- computer %>% 
  count(char, sort= T)

characters <- characters %>% 
  mutate(char = factor(char))

# Graphic -----------------------------------------------------------------
# Interpolate color palette to create new color palettes
extended_palette <- colorRampPalette(paletteer_d("rcartocolor::Prism",12)) 

characters %>%
  ggplot(aes(fill = char,area = n, label = char)) + 
  geom_treemap(color = "black", size = 1) + 
  geom_treemap_text(family = "Lato Black",fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  labs(title = "Which characters interacted the more with VUIs\n in the Star Trek Saga?",
    caption = "Data from ***SpeechInteraction.com*** and shared by Sara Stoudt.<br>
       Tidytuesday Week-34 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**.") + 
  scale_fill_manual(values = extended_palette(nrow(characters))) + 
  theme(text =element_text(family = "Lato"),
        plot.title = element_text(family = "Lato Black",size = rel(2.5), hjust = .5, margin = margin(t = 10,b = 10)),
        plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
        legend.position = "none",
        panel.spacing  = margin(15, unit = "mm")
  )

# Saving ------------------------------------------------------------------
ggsave("2021_w34/tidytuesday_2021_w34_twitter.png", width = 12, height = 12, device = agg_png, dpi = 160)
ggsave("2021_w34/tidytuesday_2021_w34.png", width = 12, height = 12, device = agg_png, dpi = 640)
