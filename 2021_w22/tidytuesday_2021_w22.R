# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(lubridate)
library(ggbeeswarm)
library(prismatic)
library(glue)
library(cowplot)
library(ggrepel)
library(patchwork)
# Read Data & Wrangling ---------------------------------------------------
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Select distinct drivers
drivers_df <- drivers %>% 
  distinct(player, nation) 
    
# Annual nb of records set 
year_records <- records %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup()

# 5-y period  nb of records set by countries
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

# Read countries colors Data 
countries_colors <- read_csv(here::here("2021_w22/countries_colors.csv"))

five_years_records <- five_years_records %>% 
  left_join(countries_colors, by = c("nation" = "name"))

five_years_records <- five_years_records %>% 
  mutate(
    alternate_color = case_when( nation ==  "Australia" ~ "003E42",
                                 TRUE ~ alternate_color)
  ) 

# Summarize of each 5 y period
periods_summary <- five_years_records %>% 
  group_by(five_y_span) %>% 
  summarise(
    n = sum(n, na.rm = T),
    n_dist_players = sum(n_dist_players, na.rm =T) # Nb of distinct players
  ) %>% 
  ungroup() %>% 
  rename("nb_records" = "n") %>% 
  mutate(
    id = row_number(),
    rank = rank(-nb_records),
    fancy_label = case_when(id == 1 ~  glue('<b style = "font-size : 18px;">#{rank}</b> <br> <span style = "font-size : 13px;"><b><i>{nb_records}</i></b> records set by <br> <b>{n_dist_players}</b> distincts players.</span>'),
                            TRUE ~  glue('<b style = "font-size : 18px;">#{rank}</b> <br> <span style = "font-size : 13px;"><b><i>{nb_records}</b></i> records  by <br> <b>{n_dist_players}</b>  players.</span>')
    )
  )

# Fancy title
title <- '<img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 /> <span style = "color:#C5000BFF; " >  M</span><span style = "color:#004586FF; " >A</span><span style = "color:#FFD320FF; " >R</span><span style = "color:#579D1CFF; " >I</span><span style = "color:#FF420EFF; " >0</span> <span>KART</span> <img src = "https://www.gamercoach.com/static/uploads/1569319353514.png" width = 50 />'
separators_df <- tibble(
  x = seq(1.5, 5.5, by =1), 
  xend = seq(1.5, 5.5, by =1),  
  y = 0, 
  yend = 1000 
)


# Plot --------------------------------------------------------------------
# Define a common theme
common_theme <- theme_minimal(base_family = "Inconsolata")+ 
  theme(
    panel.grid = element_blank(),
    plot.caption = element_markdown(family = "Lato Black",size = rel(.75)),
    plot.subtitle = element_markdown( family = "Lato Black", size = 20, hjust = 0, margin = margin(b = 0.5, unit = "cm")),
  )

# Record holders' countries distribution Plot
(plot <- five_years_records  %>% 
  ggplot(aes(five_y_span, n)) + 
  geom_label_repel(aes(label = nation, fill = paste0("#",color),  color = paste0("#",alternate_color)), direction = "y", 
                   fontface = "bold", family = "Lato Black", label.padding =0.35, label.r = 0.25, label.size = 0.5) +
  geom_segment(data= separators_df, aes (x = x,xend= xend, y = y  , yend = yend), linetype = "dashed") + 
  geom_richtext(data = periods_summary, aes(x = five_y_span,label = fancy_label, y = 1000),  fill = NA, label.color = NA, family = "Inconsolata") +
  annotate(geom = "richtext", x = 0, y = 650, label = "NUMBER OF RECORDS." ,size = 4,hjust = 0.4, fill =NA,label.color = NA,family = "Inconsolata") +
  labs(
    title = title,
    subtitle = "Distribution of  record holders' countries by 5-year period",
    x = NULL,
    y = NULL,
    alt = "This figure shows the distribution of mario kart records set by 5 years period since 1995"
  ) +
  scale_y_log10(
    breaks = c(0, 1, 10, 100)
  )  +
  scale_fill_identity() + 
  scale_color_identity() + 
  coord_cartesian(clip = "off") + 
  common_theme + 
  theme(
    plot.title = element_markdown( family = "Lato Black", size = 40, hjust = .5, margin = margin(b = 0.5, unit = "cm")),
    axis.text = element_text(family =  "Inconsolata", color = "black"), 
    axis.text.x = element_text(size = rel(1.3),face = "bold"), 
    axis.ticks.y = element_line(size = 0.25),
    axis.ticks.length.y = unit(5,"pt"),
    plot.margin = margin(t = 20, b = 15,l = 20)
    
  )
)

#  Yearly evolution of nb of records established 
yearly_plot <- year_records %>% 
  ggplot(aes(year, n)) +
  geom_line(color = "grey80", size = 1.5) + 
  geom_segment(aes(x = year, xend = year , y = 0, yend = n), color = "grey50", linetype = "dashed") +
  geom_point(size = 4, fill = "#0072B2", color = "white", pch = 21, stroke = 1) + 
  annotate(geom = "text", x = 2020, y = 215, hjust = .8, label = "In 2020, we note a surge \nafter years of stability.\n May be the pandemic effect ?😕", family = "Lato Black",size = 3.5) +
  labs( 
    subtitle = "Annually number of records set",
    caption = "Data from  Mario Kart World Records.<br/>  Image Credit : Gamercoach.<br/>
      Tidytuesday Week-22 2021 | <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**",
    alt = "Mario Kart Annualy number of records set"
  ) + 
  scale_y_continuous(
    name = NULL,
    limits = c(0,400),
    breaks = seq(0,400,100),
    expand = expansion (add = c(0,0.1))
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = c(1972, seq(2000, 2020, 5)),
    expand = expansion(mult= c(0.01,0.0))
  ) +
  coord_cartesian(clip = "off") +
  common_theme + 
  theme(
    axis.line.x = element_line(size = .35),
    axis.ticks.y = element_line(size = 0.25),
    axis.ticks.x = element_line(size = 0.75),
    axis.text = element_text(size = rel(1.1), color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.ticks.length = unit(0.20,"cm"),
    plot.margin = margin(r = 20, b = 15, l = 20)
  )

# Combine Plot
plot / yearly_plot  + 
  plot_layout(ncol = 1, heights  = c(1.5,1)) & 
  theme(
    plot.background = element_rect(fill="#f9fbfc", color = NA)    
  )

# Save the Final Graphic
ggsave(here::here("2021_w22/tidytuesday_2021_w22.png"),width = 13, height = 13*8/9,dpi = 320,type = "cairo")



