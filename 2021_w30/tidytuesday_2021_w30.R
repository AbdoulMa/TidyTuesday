# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(geofacet)

# Read data & Wrangling ---------------------------------------------------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')
states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR")

# Add Puerto-Rico to US facet grid 
us_state_grid1_with_pr <- us_state_grid1 %>% 
  add_row(row = 7, col = 10, code = "PR", name = "Puerto Rico")

monthly_drought <- drought %>% 
  mutate(map_date = lubridate::ymd(map_date),
         drought_lvl = fct_other(drought_lvl, keep = "None", other_level = "Drought")) %>% # Put all drought lvls together
  group_by(state_abb,map_date) %>% 
  slice_max(order_by = area_pct,n = 1, with_ties = F) %>% 
  ungroup() %>% 
  group_by(state_abb,year = lubridate::year(map_date),month =  lubridate::month(map_date), drought_lvl) %>% 
  count() %>% 
  pivot_wider(names_from = drought_lvl, values_from = n,values_fill = 0) %>% 
  select(-None) %>% 
  filter(between(year, 2002,2020)) 


# Graphic -----------------------------------------------------------------
(plot <- monthly_drought %>% 
    left_join(states, by = c("state_abb" = "Abbreviation")) %>%  
    ggplot(aes(x = month, y = year)) +
    geom_tile(aes(fill = Drought), 
              color = "black", size = .1) +
    labs(
      title = str_to_upper("Droughts in USA \n 2002 -2020"),
      subtitle = "Number of weeks per month each state is affected",
      caption = "Data from ***U.S. Drought Monitor***.<br>
      Tidytuesday Week-30 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**." 
    ) + 
    scale_x_continuous(
      breaks = seq(1,12, by = 3), # Year months by 3 months step
      labels = function(x) {str_to_upper(month.abb[x])} # Print the abbrecviations
    ) +
    scale_y_continuous(
      labels = function(x) {str_c("'",str_sub(x, 3, 4))}
    ) +
    scale_alpha(guide = 'none') + 
    scale_fill_distiller(name = "Monthly Drought-Weeks",
                         palette = "Spectral", direction = -1) + 
    facet_geo(vars(State), grid = us_state_grid1_with_pr) +
    coord_cartesian(expand = F, clip = "off") +  
    theme_minimal(base_family = "Lato") + 
    theme(rect = element_blank(),
          panel.border = element_rect(color = "black", size = 0.4, fill =NA),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.title=element_blank(),
          axis.text = element_text(color = "black"),
          legend.direction = "horizontal", 
          legend.box = "horizontal",
          panel.spacing.x = unit(1,"mm"),
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
png(here::here("2021_w30/tidytuesday_2021_w30.png"), width = 21, height = 15, units = "in",res = 300, type = "cairo")
plot
dev.off()

# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 30.
# The plot is  a facet of  monthly number of drought weeks in each state between 2002 & 2020.
# Data comes from US Drought Monitor.