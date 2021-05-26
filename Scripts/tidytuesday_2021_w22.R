# https://www.gamercoach.com/coach-jeu-video/coach-mario-kart
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(ggbeeswarm)
library(prismatic)
library(glue)
library(cowplot)
library(ggrepel)
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
  summarise(
    n = n(), 
    n_dist_players = n_distinct(player)
  ) %>% 
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

(spans_summarize <- five_years_records %>% 
  group_by(five_y_span) %>% 
  summarise(
    n = sum(n, na.rm = T),
    n_dist_players = sum(n_dist_players, na.rm =T)
    ) %>% 
    ungroup() %>% 
    rename("nb_records" = "n") %>% 
    mutate(
      id = row_number(),
      rank = rank(-nb_records),
      fancy_label = case_when(id == 1 ~  glue('<b style = "font-size : 18px;">#{rank}</b> <br> <span style = "font-size : 13px;"><b><i>{nb_records}</i></b> records established by <br> <b>{n_dist_players}</b> distincts players.</span>'),
                              TRUE ~  glue('<b style = "font-size : 18px;">#{rank}</b> <br> <span style = "font-size : 13px;"><b><i>{nb_records}</b></i> records  by <br> <b>{n_dist_players}</b>  players.</span>')
                              )
    )
) 
  
five_years_records

title <- '<img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 /> <span style = "color:#C5000BFF; " >  M</span><span style = "color:#004586FF; " >A</span><span style = "color:#FFD320FF; " >R</span><span style = "color:#579D1CFF; " >I</span><span style = "color:#FF420EFF; " >0</span> <span>KART</span> <img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 />'
separators_df <- tibble(
  x = seq(1.5, 5.5, by =1), 
  xend = seq(1.5, 5.5, by =1),  
  y = 25, 
  yend = 15000 
)

(plot <- five_years_records  %>% 
  ggplot(aes(five_y_span, n)) + 

  # geom_point( aes(fill = paste0("#",color), color = after_scale(clr_darken(fill, 0.1))),size = 10, pch = 21, stroke = 1,
  #            # position =position_beeswarm(priority ="descending", groupOnX = T,dodge.width = .5,cex = 1.1)
  #            position = position_jitter(width = 0.25, height = 0., seed= 234),
  #            # position = position_quasirandom(width = .4, dodge.width = .5),
  #            alpha = .9
  #            ) + 
  # 
  # geom_text(aes(label = abbreviation,color = paste0("#",alternate_color)), family = "Lato Black", size=2.5,fontface = "bold",
  #           position = position_jitter(width = 0.25, height = 0., seed= 234)
  #           # position = position_quasirandom(width = .4, dodge.width = .5)
  #           ) +
    
    geom_label_repel(aes(label = nation, fill = paste0("#",color),  color = paste0("#",alternate_color)), direction = "y", 
                     fontface = "bold", family = "Lato Black", label.padding =0.35, label.r = 0.25, label.size = 0.5) +
  geom_segment(data= separators_df, aes (x = x,xend= xend, y = y  , yend = yend), linetype = "dotted") + 
  geom_richtext(data = spans_summarize, aes(x = five_y_span,label = fancy_label, y = 25000),  fill = NA, label.color = NA, family = "Inconsolata") +
  annotate(geom = "richtext", x = 0, y = 15000, label = "NUMBER OF RECORDS." ,size = 4,hjust = 0.4, fill =NA,label.color = NA,family = "Inconsolata") +
  labs(
    # title = title,
    x = NULL,
    y = NULL,
    alt = "This figure shows the distribution of mario kart records extablished by 5 five years span since 1995"
  ) +
  scale_y_log10()  +
  scale_fill_identity() + 
  scale_color_identity() + 
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(
    plot.title = element_markdown( family = "Lato Black", size = 40, hjust = .5, margin = margin(b = 0.5, unit = "cm")),
    axis.text = element_text(family =  "Inconsolata", color = "black"), 
    axis.text.x = element_text(size = rel(1.3),face = "bold"), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(size = 0.25),
    axis.ticks.length.y = unit(5,"pt"),
    plot.margin = margin(t = 20, b = 15,l = 20),
    plot.background = element_rect(fill = "#f9fbfc")
  )
)  
# (plot <- ggdraw(plot) + 
#   theme(plot.background = element_rect(fill="#f9fbfc", color = NA))
# )  

# ggsave("Outputs/mario_v1.png", plot,width = 12, height = 12*6/9,type = "cairo")

