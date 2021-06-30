
# Load libraries ----------------------------------------------------------
library(tidyverse) 
library(waffle)
library(MESS) # round_percent 
library(paletteer)
library(lubridate)
library(ggtext)
library(cowplot)
library(gggrid)
Sys.setlocale(locale="en_US.UTF-8")


# Retrieve data & Wrangling -----------------------------------------------
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

# Top animals rescued
top_animals <- animal_rescues %>% 
  count(animal_group_parent, sort = T) %>% 
  head(8) %>% 
  pull(animal_group_parent)

# Compute proportions of properties animals are rescued from 
rescued_animals_properties <- animal_rescues %>% 
  mutate(animal_group_parent = fct_other(animal_group_parent, keep = top_animals, other_level = "Others"),
         property_category = fct_infreq(property_category)
  ) %>% 
  count(animal_group_parent, property_category, sort = T) %>% 
  group_by(animal_group_parent) %>% 
  mutate(prop = round_percent(prop.table(n))) %>% 
  arrange(animal_group_parent,property_category)

# Monthly proportions of properties animals are rescued from 
monthly_proportions <- animal_rescues %>% 
  filter(property_category != "Boat") %>% 
  mutate( property_category = fct_infreq(property_category),
          date_time_of_call = dmy_hm(date_time_of_call),
          month = fct_inorder(month(date_time_of_call, label = T, abbr = T))) %>% 
  filter(year(date_time_of_call) < 2021) %>% 
  count(month, property_category) %>% 
  group_by(property_category) %>% 
  mutate(monthly_prop = round_percent(prop.table(n))) %>% 
  arrange(property_category)

# Graphics ----------------------------------------------------------------
common_theme <- theme_minimal(base_family = "Lato Semibold")+  
  theme(panel.grid = element_blank(),
        plot.subtitle = element_text(size = rel(1.7), face = "italic", hjust = .5, margin = margin(b = 15)),
        strip.text = element_text(size = rel(0.75))
  )

# Waffle Plot 
plot <- rescued_animals_properties %>% 
  mutate(animal_group_parent = str_wrap(str_to_upper(animal_group_parent), width = 20)) %>% 
  ggplot(aes(fill = property_category, values = prop )) + 
  geom_waffle(color = "white", size=.15, n_rows = 10, flip = T) + 
  labs(subtitle = "Where are animals rescued from ?",
       fill = NULL) +
  coord_equal(expand = F) +
  facet_wrap(vars(animal_group_parent)) + 
  grid_panel(
    grob = function(data, coords) {
      if (data$PANEL[1] == 3) {
        gList(
          textGrob(
            label = "Deer in Dwelling ?\n Are you serious?",
            x = unit( .5, "npc"),
            y = unit(0.28, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "white",
              fontfamily = "Lato Semibold",
              fontsize = 7.5
            )
          )
        )
      }
      else {
        nullGrob()
      }
    },
  )+
  scale_fill_manual(
    values = c("Dwelling" = "#2A363BFF",
               "Outdoor" = "#019875FF",
               "Non Residential" = "#FECEA8FF",
               "Outdoor Structure" = "#FF847CFF",
               "Road Vehicle" = "#E84A5FFF",
               "Other Residential" = "#96281BFF"
    )
  ) + 
  common_theme +
  theme(
    panel.border = element_rect(colour = "black",size = 1.25, fill = "transparent"),
    plot.margin = margin(t = 10,b= 10),
    axis.text = element_blank(),
    legend.position = "top",
    legend.title = element_text(margin = margin(b = 10)),
    legend.spacing.x = unit(10,"pt"),
    legend.spacing.y = unit(5,"pt"),
  )

# Bar plot
monthly_plot <- monthly_proportions %>% 
  mutate(property_category = str_to_upper(property_category)) %>% 
  ggplot() + 
  geom_col(aes(month, monthly_prop, fill = property_category)) +
  geom_hline(data = tibble(y =  seq(5,15, 5)),aes(yintercept = y), color = "white", linetype = "dashed") + 
  labs(
    subtitle = "Monthly distribution of calls for animal Rescues",
    x = NULL,
    y = NULL,
    caption= "Data from London.gov by way of Data is Plural and Georgios Karamanis.
      Tidytuesday Week-27 2021 - @issa_madjid"
  ) + 
  scale_fill_manual(
    values = c(
      "DWELLING" = "#2A363BFF",
      "OUTDOOR" = "#019875FF",
      "NON RESIDENTIAL" = "#FECEA8FF",
      "OUTDOOR STRUCTURE" = "#FF847CFF",
      "ROAD VEHICLE" = "#E84A5FFF",
      "OTHER RESIDENTIAL" = "#96281BFF"
      
    ),
    guide = "none"    
  ) + 
  scale_y_continuous(
    breaks = c(5,10,15),
    labels = function(x) {scales::percent(x,scale = 1,accuracy = 1)}
  ) + 
  coord_cartesian(expand = F) + 
  facet_wrap(vars(property_category), scales = "free_x", ncol = 2 ) + 
  common_theme +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = rel(0.95),face = "italic"),
    axis.text.y = element_text(size = rel(0.85)),
    axis.ticks.x = element_line(size = 0.25),
    axis.ticks.length.x = unit(0.20,"cm"),
    axis.line.x = element_line(size = .75),
    plot.caption = element_text(color = "grey35", size = rel(0.8), margin = margin(t = 10,b = 5)),
    plot.margin = margin(t = 10,r = 5)
  )

# Title
title <- ggdraw() + 
  draw_label(
    "Animal Rescues by The London Fire brigade",
    fontface = 'bold',
    size = 25,
    fontfamily = "Lato Black",
    x = 0,
    hjust = 0
  ) + 
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(15, 0, 15,  250)
  )
# 2 plots compoistion 
composition <- plot_grid(plot, monthly_plot, nrow = 1, rel_widths = c(0.85, 1))
# Title + 2 plots
composition <- plot_grid(
  title, composition,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.05, 1)
)

# Graphic saving ----------------------------------------------------------
png(here::here("2021_w27/tidytuesday_2021_w27.png"),width = 14, height = 7.5,res = 320, units = "in",type = "cairo")
composition
dev.off()

# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 27.
# The plot is  a composition of facets plots.
# The first one is about where each family of animal is generally rescued from.
# The second one is about the monthly distribution of call for animal rescues for each property type.
# Data comes from  London.gov by way of Data is Plural and Georgios Karamanis.
