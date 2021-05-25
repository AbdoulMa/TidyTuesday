# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(ggbeeswarm)
library(prismatic)
# Read Data & Wrangling ---------------------------------------------------
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

drivers_df <- drivers %>% 
  select(player, year, nation)

five_years_records <- records %>% 
  left_join(drivers_df) %>% 
  filter(!is.na(nation)) %>% 
  mutate(year = year(date),
         five_y_span = year %/% 5, 
         five_y_span = five_y_span *5,
         five_y_span = fct_inorder(as.character(five_y_span)),
         five_y_span = fct_relabel(five_y_span , ~ paste0(.x, "-", as.integer(.x) + 5) )
         ) %>% 
  group_by(five_y_span, nation) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(five_y_span, n ) 

# Distinct countries 
countries_colors <- read_csv("countries_colors.csv")
countries_colors
five_years_records <- five_years_records %>% 
  left_join(countries_colors, by = c("nation" = "name"))

five_years_records <- five_years_records %>% 
  mutate(
    alternate_color = case_when( nation ==  "Australia" ~ "003E42",
                                 TRUE ~ alternate_color)
  )
five_years_records

title <- '<img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 />  <span style = "color:#C5000BFF; " >M</span><span style = "color:#004586FF; " >A</span><span style = "color:#FFD320FF; " >R</span><span style = "color:#579D1CFF; " >I</span><span style = "color:#FF420EFF; " >0</span> <span>KART</span> <img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 />'
separators_df <- tibble(
  x = seq(1.5, 5.5, by =1), 
  xend = seq(1.5, 5.5, by =1),  
  y = 25, 
  yend = 10000 
)

separators_df
five_years_records  %>% 
  ggplot(aes(five_y_span, n)) + 

  geom_point( aes(fill = paste0("#",color), color = after_scale(clr_darken(fill, 0.1))),size = 12, pch = 21, stroke = 1,
             # position =position_beeswarm(priority ="ascending", dodge.width = 1.5)
             position = position_jitter(width = 0.2, height = 0.6, seed= 1)
             # position = position_quasirandom(width = .5, dodge.width = .5)
             ) + 
 
  geom_text(aes(label = abbreviation,color = paste0("#",alternate_color)), family = "IBM Plex Sans", size=3,fontface = "bold",
            position = position_jitter(width = 0.2, height = 0.6, seed= 1)) +
  geom_segment(data= separators_df, aes (x = x,xend= xend, y = y  , yend = yend), linetype = "dotted") + 
  labs(
    title = title
  ) + 
  scale_y_log10()  +
  scale_fill_identity() + 
  scale_color_identity() + 
  theme_minimal() + 
  theme(
    plot.title = element_markdown( family = "Lato Black", size = 40, hjust = .5)
  )
  

