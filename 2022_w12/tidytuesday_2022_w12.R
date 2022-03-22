
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(babynames)
library(ggtext)
library(glue)
library(gggrid)

# Data Wrangling ----------------------------------------------------------
popular_unisex_names <- c("Jessie","Marion","Jackie","Alva","Ollie",
                          "Jody","Cleo","Kerry","Frankie","Guadalupe",
                          "Carey","Tommie","Angel","Hollis","Sammie",
                          "Jamie","Kris","Robbie","Tracy","Merrill",
                          "Noel","Rene","Johnnie","Ariel","Jan",
                          "Devon","Cruz","Michel","Gale","Robin",
                          "Dorian","Casey","Dana","Kim","Shannon"
)


babynames_df <- babynames %>% 
  select(-prop) %>%
  filter(name %in% popular_unisex_names) %>% 
  filter(year > 1925)

unisex_babynames <- babynames_df %>% 
  pivot_wider(names_from = sex, values_from = n) %>% 
  drop_na(`F`,`M`) %>% 
  mutate(
    prop_f = `F`/(`F`+`M`), # Proportion of girls
    prop_m = `M`/(`F`+`M`) # Proportion of boys 
  )


# Compute Proportions distance Means for order names 
(most_unisex_props_means <-  unisex_babynames %>% 
    mutate(prop_dist = abs(prop_f-prop_m)) %>% 
    group_by(name) %>% 
    summarise(prop_dist_mean = mean(prop_dist)) %>% 
    mutate(rank = rank(prop_dist_mean)) %>% 
    arrange(rank))

# Order names by proortion distance means 
unisex_babynames <- unisex_babynames %>% 
  left_join(most_unisex_props_means, by = "name") %>% 
  mutate(
    name_ranked = glue("{rank}. {name}"),
    name_ranked = fct_reorder(name_ranked,rank)
  )

# Determine most unisex year for each name 
most_unisex_year <- unisex_babynames  %>% 
  mutate(prop_dist = abs(prop_f-prop_m)) %>% 
  group_by(name_ranked) %>% 
  slice_min(prop_dist,n=1, with_ties = F) %>% 
  ungroup() 

# Function for normalize between range ( I use that inside grobs for panels annotations )
range_between <-  function(x,range){ (x-min(range)) / (max(range)-min(range)) }

# Jackie Robinson introduction to MLB 
j_rob_in_mlb <- unisex_babynames %>% 
  filter(year == 1947, name == 'Jackie')
  
# Graphic -----------------------------------------------------------------
(plot <- unisex_babynames %>%   
   ggplot(aes(year)) + 
   geom_ribbon(aes(ymin = 0, ymax = prop_f), fill = "#E3837D")+
   geom_ribbon(aes(ymin = prop_f, ymax = 1), fill = "#96CBFE")+
   geom_line(aes(y=prop_f), size = 0.35) +
   
   facet_wrap(~name_ranked, ncol = 7, scales ='free_x')+
   scale_x_continuous(
     name = NULL,
     breaks = c(1940,1960,1980,2000),
     labels = c(1940,"'60","'80",2000)
   )+
   scale_y_continuous(
     name = NULL, 
     breaks = c(0,0.5,1),
     labels = scales::percent
   )+
   grid_panel(
     grob = function(data, coords) {
       if (data$PANEL[1] == 1) {
         .x <- range_between(select(filter(most_unisex_year, rank == 1), year), range(unisex_babynames$year))
         .y <- range_between(select(filter(most_unisex_year, rank == 1), prop_f)*100, c(0,100))
         gList(
           segmentsGrob(
             .x, .y,
             unit(.x - 0.125, "npc"),
             unit(.y, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           segmentsGrob(
             .x-0.125, .y,
             unit(.x - 0.125, "npc"),
             unit(.y -0.080, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           textGrob(
             label = "Most\n unisex year",
             x = unit(.x- 0.035, "npc"),
             y = unit(.y -.075,  "npc"),
             just = c("center", "top"),
             gp = gpar(
               col = "black",
               fontfamily = "Lato",
               lineheight = .75,
               fontsize = 10
             )
           ),
           textGrob(
             label = "GIRLS",
             x = unit(0.75, "npc"),
             y = unit(0.25,  "npc"),
             just = c("center", "center"),
             gp = gpar(
               col = "white",
               fontfamily = "I Sans Semibold Pro",
               fontsize = 12
             )
           ),
           textGrob(
             label = "BOYS",
             x = unit(0.75, "npc"),
             y = unit(0.75,  "npc"),
             just = c("center", "center"),
             gp = gpar(
               col = "white",
               fontfamily = "I Sans Semibold Pro",
               fontsize = 12
             )
           )
           
         )
       }
       else if (data$PANEL[1] == 3) {
         .x <- range_between(2000, range(unisex_babynames$year))
         .y <- range_between(60, c(0,100))
         gList(
           segmentsGrob(
             .x, .y,
             unit(.x, "npc"),
             unit(.y/2, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           textGrob(
             label = "Marion Jones wins\n gold in Olympics",
             x = unit(.x, "npc"),
             y = unit(.y/2, "npc"),
             just = c("right", "top"),
             gp = gpar(
               col = "black",
               fontfamily = "Lato",
               lineheight = .75,
               fontsize = 10
             )
           )
         )
       } 
       else if(data$PANEL[1] == 4) {
         .x <- range_between(1947, range(unisex_babynames$year)) 
         .y <- range_between(pull(j_rob_in_mlb,prop_f)*100, c(0,100))
         gList(
           segmentsGrob(
             .x, .y,
             unit(.x, "npc"),
             unit(.y+0.25, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           
           textGrob(
             label = "Jackie Robinson\n to Major League",
             x = unit(.x+0.1, "npc"),
             y = unit(.y + 0.25, "npc"),
             just = c("center", "bottom"),
             gp = gpar(
               col = "black",
               fontfamily = "Lato",
               lineheight = .75,
               fontsize = 10
             )
           )
         )
       }
       else if(data$PANEL[1] == 21) {
         gList(
           textGrob(
             label = "James Hunter Cartwright\n appears on Bonanza",
             x = range_between(1971, range(unisex_babynames$year)),
             y = range_between(pull(filter(unisex_babynames, year == 1971, rank== 21),prop_f)*100, c(0,100))- 0.075,
             just = c("center", "top"),
             gp = gpar(
               col = "black",
               fontfamily = "Lato",
               lineheight = .75,
               fontsize = 10
             )
           )
         )
       }
       else if(data$PANEL[1] == 16) {
         .x <- range_between(1989, range(unisex_babynames$year)) 
         .y <- range_between(pull(filter(unisex_babynames, year == 1989, rank== 16),prop_f)*100, c(0,100))
         gList(
           segmentsGrob(
             .x, .y,
             unit(.x-0.125, "npc"),
             unit(.y, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           segmentsGrob(
             .x-0.125, .y,
             unit(.x-0.125, "npc"),
             unit(.y-0.075, "npc"),
             gp = gpar(
               col = "black",
               lwd = 0.5
             )
           ),
           textGrob(
             label = "The little Mermaid\n sways Ariel towards girls",
             x = .x-0.125,
             y = .y-0.075,
             just = c("center", "top"),
             gp = gpar(
               col = "black",
               fontfamily = "Lato",
               lineheight = .75,
               fontsize = 9
             )
           )
         )
       }
       else {
         nullGrob()
       }
     }
   )+
   geom_point(data = most_unisex_year, aes(year, prop_f), pch = 21, fill = "white", size = 2.5)+
   labs(
     title = "The Most Unisex Names in US History",
     caption = "Data:**{babynames}** R Packages<br> **Tidytuesday** Week-12 2022<br>**Abdoul ISSA BIDA** inspired by **Nathan YAU**."
   ) +   
   coord_cartesian(expand = F) +
   theme_minimal(base_family = "Lato") + 
   theme(
     plot.title = element_text(family = "NY Extra Bold", size = rel(3), hjust =.5, margin = margin(b = .5,unit = "cm")),
     plot.caption = element_markdown(size = rel(1.25)),
     axis.line.x = element_line(),
     axis.ticks.y = element_blank() ,
     axis.ticks.x = element_line(),
     axis.ticks.length.x = unit(0.125,"cm"),
     axis.text = element_text(color = "black"),
     strip.text = element_text(family = "I Sans Bold Pro", size = rel(1.5))
   )
)


cowplot::ggdraw(plot) + 
  theme(plot.background = element_rect(fill="white", color = NA),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 5))

# Saving ------------------------------------------------------------------
path <- here::here("2022_w12", "tidytuesday_2022_w12")
ggsave(filename = glue::glue("{path}.png"), width = 12, height = 9, device = ragg::agg_png, dpi=300)

