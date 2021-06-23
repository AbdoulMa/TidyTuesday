library(tidyverse)
library(ggtext)
library(ragg)
library(gggrid)


# Read data & Wrangling ---------------------------------------------------
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

# Compute
nationwide <- parks %>% 
  select(ends_with("points"), total_pct) %>% 
  summarise(across(everything(), mean, na.rm = T)) %>% 
  mutate(across(everything(), ~round(., digits = 0), na.rm = T),
         city = "United States \nof America")



# Graphic -----------------------------------------------------------------
plot <-  parks %>% 
  filter(year == 2020 , rank <= 48) %>% 
  add_row(nationwide) %>% 
  mutate(
    city = fct_relevel(city,"United States \nof America")
  ) %>% 
  ggplot() + 
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1, fill = total_pct, 
                color = ifelse(city == "United States \nof America", "black","white"),
                size = ifelse(city == "United States \nof America", 1.3,0.25),
  )) +
  geom_text(aes(label = glue::glue("{city}"), color = ifelse(total_pct >= 80, "white", "black"),
                y = ifelse(city == "United States \nof America", 0.6,0.75)
  ), x = 0, hjust = 0.5, 
  size = rel(3.2), family = "Lato Black",
  lineheight = 0.95) + 
  geom_richtext(aes(label = glue::glue("👣 {pct_near_park_points}% &bull; 💵 {spend_per_resident_points}% &bull; 🏀 {basketball_points}% <br>
                                    🐕 {dogpark_points}% &bull; 🎠 {playground_points}% &bull; 👴 {rec_sr_points}% <br>
                                    🚽 {restroom_points}% &bull; 🏄 {splashground_points}% &bull; 🎯 {amenities_points}%"),
                    x = 0, y = -0.1, color= ifelse(total_pct >= 80, "white", "black")), hjust = 0.5, #FIXME adapt color 
                size = rel(2.4), family = "Lato Medium",
                fontface = "bold",
                lineheight = 1.45,
                fill = NA, label.color = NA) + 
  coord_fixed(expand = F) + 
  scale_fill_stepsn( name = "ParkScore index",
                     colours = c("#EDF8B1FF", "#7FCDBBFF","#1D91C0FF","#253494FF"),
                     labels = c("&#8592; Less Good","","","Better &#8594;"),
                     guide = guide_legend(
                       title.position = "top",
                       label.position = "bottom",
                       keywidth = unit(1,"cm"),
                       keyheight  = unit(.25,"cm"),
                       title.hjust = .5,
                       title.vjust = 0,
                       label.vjust = 3,
                       nrow = 1,
                     ),
  ) +
  scale_color_identity() +
  scale_size_identity() +
  grid_panel(
    grob = function(data, coords) {
      if (data$PANEL[1] == 1) {
        gList(
          textGrob(
            label = "National average values",
            x = unit( .5, "npc"),
            y = unit(0.15, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
              fontfamily = "Lato Medium",
              fontsize = 8
            )
          )
        )
      }
      else if (data$PANEL[1] == 5) {
        gList(
          textGrob(
            label = "has the best dog friendly\n park in the country.",
            x = unit( .5, "npc"),
            y = unit(0.8, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "white",
              fontfamily = "Lato Medium",
              fontsize = 8
            )
          )
        )
      }
      else if (data$PANEL[1] == 20) {
        gList(
          textGrob(
            label = "Surf City is clearly\n not splashgrounds city.",
            x = unit( .5, "npc"),
            y = unit(0.8, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
              fontfamily = "Lato Medium",
              fontsize = 7.5
            )
          )
        )
      }
      else if (data$PANEL[1] == 27) {
        gList(
          textGrob(
            label = "Not a place for Grandpa !!",
            x = unit( .5, "npc"),
            y = unit(0.75, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
              fontfamily = "Lato Medium",
              fontsize = 8
            )
          )
        )
      }
      else if (data$PANEL[1] == 29) {
        gList(
          textGrob(
            label = "has the best public parks\n in the country.",
            x = unit( .5, "npc"),
            y = unit(0.8, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "white",
              fontfamily = "Lato Medium",
              fontsize = 8
            )
          )
        )
      }
      else if (data$PANEL[1] == 31) {
        gList(
          textGrob(
            label = "too isn't Grandpa\n friendly.",
            x = unit( .5, "npc"),
            y = unit(0.80, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
              fontfamily = "Lato Medium",
              fontsize = 8
            )
          )
        )
      }
      
      
      else {
        nullGrob()
      }
    },
  ) + 
  labs(
    title = str_to_upper("TOP 35  of the best cities with public parks\n in United States of America (2020)."),
    caption= "Data from ***The Trust for Public Land***.<br>
      Tidytuesday Week-26 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**",  
  ) +  
  facet_wrap(vars(city), ncol = 7) + 
  theme_void(base_family = "Lato") +
  theme(
    plot.title = element_text(family = "Lato Black",size = rel(1.5), hjust = .5, margin = margin(t = 20,b = 25)),
    plot.subtitle = element_text(family = "Lato, Semibold",size = rel(1.3), face = "italic", hjust = .5),
    panel.spacing = unit(0,"points"),
    strip.text = element_blank(),
    legend.title=element_text(size=12, family = "Lato Semibold"),
    legend.margin=margin(t = 15),
    legend.position = "top",
    legend.spacing.x = unit(0, 'cm'),
    legend.text = element_markdown(face = "bold"),
    legend.box.margin=margin(-30,0,15,0),
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_markdown(color = "grey35", size = rel(0.8), margin = margin(t = 10,b = 10)),
    plot.margin = margin(0.075, unit = "cm"),
  ) 


# Save with anti-aliasing processing  -------------------------------------
png(here::here("2021_w26/tidytuesday_2021_w26.png"),width = 11, height = 12,res = 360, units = "in",type = "cairo")
plot
dev.off()

# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 26.
# The plot is  a facet of informations of TOP 35  of the best cities with public parks\n in USA in 2020.
# Data comes from The Trust for Public Land with the computing of differents scores according to several criteria. 
