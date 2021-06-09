
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gggrid) # https://github.com/pmur002/gggrid remotes::install_github("pmur002/gggrid")
library(packcircles)
library(ggtext)
library(hrbragg) # 
library(cowplot)
# Read & Data Wrangling ---------------------------------------------------
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')


fishing %>% 
  distinct(lake)
annual_production <- fishing %>% 
  filter(str_detect(region, "Canada", negate = T), str_detect(region, "Total", negate = T) ) %>%  
  filter(!is.na(values)) %>% 
  mutate(
    species = case_when(
      species %in% c("Cisco and Chub","Cisco and Chub", "Cisco and Chubs",  "Cisco and chubs") ~ "Cisco",
      species == "Walleye and Blue Pike" ~ "Blue Pike",
      str_detect(species,"salmon") | str_detect(species,"Salmon")    ~ "Salmon",
      str_detect(species,"Lake Trout")     ~ "Lake Trout",
      str_detect(species,"Channel Catfish")     ~ "Channel catfish",
      TRUE ~ species
      )
  ) %>% 
  group_by(species, year) %>% 
  summarise(prod_amount = sum(values, na.rm = T)) %>% 
  ungroup()


top_12 <- annual_production %>% 
  mutate(species = fct_lump_n(species , 19, w = prod_amount))  %>% 
  count(species, year, wt = prod_amount)
  
top_12
 
 
top_12 %>%
  ggplot(aes(year, n)) + 
  geom_line() + 
  facet_wrap(vars(species), scales = "free_y")


# Proportions de poissons collectés  par lac par décénnie circlelayout
lakes_records <- fishing %>% 
  filter(str_detect(region, "Total", negate = T) ) %>%  
  filter(!is.na(values)) %>%  
  count(year, lake, wt = values) %>% 
  mutate(
    decade = year %/% 10,
    decade = decade *10
  ) %>% 
  count(decade, lake, wt = n) %>% 
  group_by(decade) %>% 
  mutate(
    proportion = n / sum(n),
    proportion = round(proportion*100)
  ) %>% 
  ungroup()

decade_summary <- lakes_records %>% 
  group_by(decade) %>% 
  count(wt = proportion) %>% 
  ungroup()

(decades_rep <- rep(lakes_records$decade, lakes_records$proportion))
(lakes_rep <- rep(lakes_records$lake, lakes_records$proportion))

lakes_points <- decade_summary %>% pmap_df(
  .f = ~circleProgressiveLayout(rep(0.5, ..2))
) %>% 
  mutate(decade = decades_rep,
         lake = lakes_rep)

my_gpar <- gpar(
  col = "black",
  fontfamily = "Lato",
  fontsize = 7
)

(plot <-  lakes_points%>% 
  ggplot(aes(x, y, fill = lake)) + 
  geom_point(
    size = 3.5,
    pch = 21
    
  ) +
   grid_panel(
     grob = function(data, coords) {
       if (data$PANEL[1] == 1) {
         .x <- min(coords$x) + 0.085
         .y <- min(coords$y) + 0.085
         gList(
           segmentsGrob(
             .x, .y,
             unit(.x/2, "npc"),
             unit(.y/2, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           textGrob(
             label = "Each  circle represents\n1% of fish collected.",
             x = unit(.x/2.5, "npc"),
             y = unit(.y/2.5, "npc"),
             just = c("left", "top"),
             gp = my_gpar
           )
         )
       }
       else if (data$PANEL[1] == 3) {
         gList(
           textGrob(
             label = "In 1880s, approximatively 171,913 (71%)\n thousand pounds of fish\n were collected from Herie.",
             x = unit(.5, "npc"),
             y = unit(1., "npc"),
             just = c("left", "top"),
             gp = my_gpar
           )
         )
       }
       else if (data$PANEL[1] == 6) {
         gList(
           textGrob(
             label = "The Great Lakes Storm of 1913\n had severly impacted the Lake Huron fishing\n ecosystem and production.",
             x = unit(.3, "npc"),
             y = unit(1., "npc"),
             just = c("right", "top"),
             gp = my_gpar
           )
         )
       }
      
     else if (data$PANEL[1] == 11) {
         gList(
           textGrob(
             label = "Dr. Howard Tanner, the  fisheries\nchief  for the state of Michigan\nre-introduced the salmon in 1966.",
             x = unit(.2, "npc"),
             y = unit(1., "npc"),
             just = c("right", "top"),
             gp = my_gpar
           )
         )
       }
       else if (data$PANEL[1] == 13) {
         gList(
           textGrob(
             label = "The extinction of Blue Pike, approximatively in 1983\n impacted Lake Erie fishing industry but relatively little\n compared to the proportion of fish collected.  ",
             x = unit(.6, "npc"),
             y = unit(1., "npc"),
             just = c("left", "top"),
             gp = my_gpar
           )
         )
       } else {
         nullGrob()
       }
     }
   )+ 
     
    
    labs(
      title =  str_to_upper("Proportions of fish collected by lake for each decade"),
      x = NULL,
      y = NULL, 
      fill = "Lakes"
    ) + 
   scale_x_continuous(
     limits = c(-5.5,5.5)
   ) + 
   scale_y_continuous(
     limits = c(-5.5,5.5)
   ) + 
   scale_fill_manual(
     values = c("#E69F00","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
   )+ 
   guides(fill=guide_legend(
     label.position = 'bottom', 
     title.position = 'top', 
     keywidth=.45,
     keyheight=.15,
     default.unit="inch", 
     title.hjust = .5,
     title.vjust = 0,
     label.vjust = 3,
     nrow = 1)) + 
  coord_equal( clip = "off") + 
  facet_wrap(~decade) + 
   theme_minimal(base_family = "Lato") +
   theme (
     panel.spacing  = unit( 1, unit = "cm"),
   legend.spacing.x = unit(0, 'cm'),
 legend.title=element_text(family = "Lato Black",size = rel(1.3)),
 legend.text = element_text(size = rel(0.8)),
 legend.margin=margin(-10,0,-1,0),
 legend.position = 'bottom',
 legend.box.margin=margin(15,0,15,0),
 strip.text = element_text(family = "Lato Black", size = rel(1.1)),
 plot.title  = element_text(family = "Lato Black", size = rel(1.3), hjust = .5, margin = margin(t = 10, b= 10)),
 strip.placement = "outside",
 axis.text = element_blank(),
 panel.grid = element_blank()
)
)  

plot <- plot %>% 
  ggdraw()

ggsave(here::here("2021_w24/tidytuesday_2021_w24.png"),width = 10.5, height = 12,dpi = 320,type = "cairo")
# https://ohiohistorycentral.org/w/Blue_Pike Blue Pike extinction 

 # https://www.r-bloggers.com/2018/12/bubble-packed-chart-with-r-using-packcircles-package/
 