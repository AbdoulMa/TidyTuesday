# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggnewscale)
library(gggrid)

# Read data & Wrangling ---------------------------------------------------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')
states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")


col_hot <- "#B8001F"
col_hot_low <- "#FFBEC9"
col_cold <- "#0067A3"
col_cold_low <- "#BAE6FF"
drought %>% 
  filter(drought_lvl == "D3")

drought %>% 
  filter(state_abb == "CA") %>% 
  group_by(map_date) %>% 
  slice_max(order_by = area_pct,n = 1, with_ties = F) 

monthly_drought <- drought %>% 
  mutate(map_date = lubridate::ymd(map_date),
         drought_lvl = fct_other(drought_lvl, keep = "None", other_level = "Drought")) %>% 
  # filter(state_abb %in% c("DC","NV","AL")) %>%
  group_by(state_abb,map_date) %>% 
  slice_max(order_by = area_pct,n = 1, with_ties = F) %>% 
  ungroup() %>% 
  group_by(state_abb,year = lubridate::year(map_date),month =  lubridate::month(map_date), drought_lvl) %>% 
  count() %>% 
  pivot_wider(names_from = drought_lvl, values_from = n,values_fill = 0) %>% 
  mutate(Total = None + Drought) %>% 
  filter(between(year, 2002,2020))


source("2021_w30/theme_tk.R")
theme_set( theme_tk() + 
            theme(rect = element_blank(),
                  panel.border = element_blank(),
                  strip.background = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title=element_blank(),
                  axis.text = element_blank(),
                  legend.direction = "horizontal", 
                  legend.box = "horizontal",
                  legend.spacing = unit(1, 'cm'),
                  plot.margin = unit(c(5,5,5,5), "mm"),
                  plot.title = element_text(size = 60,
                                            family="Lato",
                                            color = "black"),
                  plot.subtitle = element_text(size = 40,
                                               family="Lato",
                                               color = "black"),
                  strip.text = element_text(face = "bold", size = 20,
                                            color = "black"),
                  plot.caption = element_text(size = 30,
                                              color = "black"),
                  legend.text = element_text(size = 15,
                                             color = "black"),
                  legend.title = element_text(size = 30, color = "black")))


monthly_drought %>% 
  left_join(states, by = c("state_abb" = "Abbreviation")) %>% 
  ggplot() +
  geom_tile(aes(x = month, y = year, fill = None, alpha = None / Total), 
            col = "grey95", size = .01) + 
  scale_fill_gradient2(name = "Less Drought-Weeks", 
                       #trans="sqrt",
                       low="#1a9641", high=col_cold, mid="#abd9e9",
                       midpoint = max(monthly_drought$None)/2,
                       breaks = c(min(monthly_drought$None),
                                  max(monthly_drought$None)),
                       limits = c(min(monthly_drought$None), 
                                  max(monthly_drought$None))
  ) + 
  new_scale_fill() +
  geom_tile(aes(x = month, y = year, fill = Drought, alpha = Drought / Total),
            col = "grey95", size = .05) +
  scale_fill_gradient2(name = "More Drought-Weeks",
                       #trans="sqrt",
                       low="#1a9641", high=col_hot, mid = "#fee08b",
                       midpoint = max(monthly_drought$Drought)/2,
                       breaks = c(min(monthly_drought$Drought),
                                  max(monthly_drought$Drought)),
                       limits = c(min(monthly_drought$Drought),
                                  max(monthly_drought$Drought))
  ) +
  # expand_limits(y = min(monthly_drought$year) - length(unique(monthly_drought$year))) +
  scale_alpha(guide = 'none')  + 
  facet_wrap(vars(State)) + 
  coord_cartesian(expand = F, clip = "off") + 
  # coord_polar() + 
  grid_panel(
    grob = function(data, coords) {
      if (data$PANEL[1] == 1) {
        gList(
          textGrob(
            label = "Deer in Dwelling ?\n Are you serious?",
            x = unit( .5, "npc"),
            y = unit(0.28, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
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
  )
  
