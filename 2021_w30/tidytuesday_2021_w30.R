# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggnewscale)
library(gggrid)
library(ggtext)

# Read data & Wrangling ---------------------------------------------------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')
states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR")


monthly_drought <- drought %>% 
  mutate(map_date = lubridate::ymd(map_date),
         drought_lvl = fct_other(drought_lvl, keep = "None", other_level = "Drought")) %>% # Put all drought lvls together
  group_by(state_abb,map_date) %>% 
  slice_max(order_by = area_pct,n = 1, with_ties = F) %>% 
  ungroup() %>% 
  group_by(state_abb,year = lubridate::year(map_date),month =  lubridate::month(map_date), drought_lvl) %>% 
  count() %>% 
  pivot_wider(names_from = drought_lvl, values_from = n,values_fill = 0) %>% 
  mutate(Total = None + Drought) %>% 
  filter(between(year, 2002,2020))


# Graphic -----------------------------------------------------------------
# Parameters for grid_panel
my_gpar <- gpar(
  col = "black",
  fontfamily = "Lato Semibold",
  fontface = "bold.italic",
  fontsize = 9.5
)

# PLot
(plot <- monthly_drought %>% 
  left_join(states, by = c("state_abb" = "Abbreviation")) %>%  
  ggplot(aes(x = month, y = year)) +
  geom_tile(aes(fill = None, alpha = None / Total), 
            color = "black", size = .01) + 
  scale_fill_gradient2(name = "Less Drought-Weeks", 
                       low="#1a9641", high= "#0067A3", mid="#abd9e9",
                       midpoint = max(monthly_drought$None)/2,
                       breaks = c(min(monthly_drought$None),
                                  max(monthly_drought$None)),
                       limits = c(min(monthly_drought$None), 
                                  max(monthly_drought$None))
  ) + 
  new_scale_fill() +
  geom_tile(aes(fill = Drought, alpha = Drought / Total),
            color = "black", size = .05) +
  labs(
    title = str_to_upper("Droughts in USA \n 2002 -2020"),
    subtitle = "Number of weeks per month each state is affected",
    caption = "Data from ***U.S. Drought Monitor***.<br>
      Tidytuesday Week-30 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid** inspired by Taras Kaduk." 
  ) + 
  scale_fill_gradient2(name = "More Drought-Weeks",
                       low="#1a9641", high="#B8001F", mid = "#fee08b",
                       midpoint = max(monthly_drought$Drought)/2,
                       breaks = c(min(monthly_drought$Drought),
                                  max(monthly_drought$Drought)),
                       limits = c(min(monthly_drought$Drought),
                                  max(monthly_drought$Drought))
  ) +
  scale_x_continuous(
    breaks = seq(1,12, by = 3), # Year months by 3 months step
    labels = function(x) {str_to_upper(month.abb[x])} # Print the abbrecviations
  ) +
  scale_y_continuous(
    labels = function(x) {str_c("'",str_sub(x, 3, 4))}
  ) +
  scale_alpha(guide = 'none') + 
  facet_wrap(vars(State)) + 
  coord_cartesian(expand = F, clip = "off") + 
  grid_panel(
    grob = function(data, coords) {
      if (data$PANEL[1] == 2) {
        gList(
          textGrob(
            label = "Alaska is the most untouched \n state in the country over\n the last two decades.",
            x = unit( .5, "npc"),
            y = unit(0.75, "npc"),
            just = c("center", "top"),
            gp = my_gpar
          )
        )
      }
      else if (data$PANEL[1] == 5) {
        gList(
          textGrob(
            label = "Things have gotten worse \nsince 2010 in Salifornia.",
            x = unit( .5, "npc"),
            y = unit(0.65, "npc"),
            just = c("center", "top"),
            gp =my_gpar
          )
        )
      }
      else if (data$PANEL[1] == 12) {
        gList(
          textGrob(
            label = "Drought in Hawaii ?",
            x = unit( .5, "npc"),
            y = unit(0.65, "npc"),
            just = c("center", "top"),
            gp =my_gpar
          )
        )
      }
      else if (data$PANEL[1] == 52) {
        gList(
          textGrob(
            label = "Things get better in Wyoming \nsince 2010.",
            x = unit( .5, "npc"),
            y = unit(0.65, "npc"),
            just = c("center", "top"),
            gp =my_gpar
          )
        )
      }
      else {
        nullGrob()
      }
    },
  ) +
  theme_minimal(base_family = "Lato") + 
  theme(rect = element_blank(),
        panel.border = element_rect(color = "black", size = 0.4, fill =NA),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text = element_text(color = "black"),
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.spacing = unit(1, 'cm'),
        plot.margin = unit(c(5,5,5,5), "mm"),
        plot.title = element_text(size = 25,
                                  family="Lato Black",
                                   hjust = .5),
        plot.subtitle = element_text(size = 20,
                                     color = "grey25", family = "Lato Semibold",
                                     face = "italic",
                                     hjust = .5, margin = margin(t = 15, b = 15)),
        strip.text = element_text(face = "bold", size = 12,
                                  color = "black"),
        plot.caption = element_markdown(color = "grey15", size = rel(1), margin = margin(t = 10,b = 10)),
        legend.text = element_text(size = 7.5,
                                   color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        line = element_line(colour = "Dark"),
        text = element_text(colour = "#2A2E45"),
        axis.ticks = element_blank(),
        legend.position="bottom"
        )
)


# Saving ------------------------------------------------------------------
png(here::here("2021_w30/tidytuesday_2021_w30.png"), width = 15, height = 14, units = "in",res = 320, type = "cairo")
plot
dev.off()

# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 30.
# The plot is  a facet of  monthly number of drought weeks in each state between 2002 & 2020.
# Data comes from US Drought Monitor.