# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggstream)
library(ggtext)

# Data reading and Wrangling ----------------------------------------------
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
olympics <- olympics %>% 
  mutate(team = case_when(
    team == "Soviet Union" ~ "Russia",
    team == "East Germany" ~ "Germany",
    team == "West Germany" ~ "Germany",
    team == "United States-1" ~ "United States",
    team == "China-1" ~ "China",
    team == "Australasia" ~ "Australia",
    TRUE ~ team
  ))

# Summer Olympic Games
summer_og <- olympics %>% 
  filter(season == "Summer")

# Determine the 15 best federations
top_federations <- summer_og %>% 
  filter(medal == "Gold") %>%
  count(team, sort = T) %>% 
  head(15) %>% 
  pull(team)



# Retrieve olympic cities and years
summer_og_editions <- summer_og %>% 
  distinct(year, city) %>% 
  mutate(edition = glue::glue("{city} \n {year}")) %>% 
  filter(!(year == 1906 & city == "Athina"),!(year == 1956 & city == "Stockholm")) %>% 
    arrange(year) %>% 
    rowid_to_column() 

# Compute each federation gold medals number per edition
gold_medalists <- summer_og %>% 
  filter( medal == "Gold") %>% 
  mutate(
    team = fct_other(team,keep = top_federations, other_level = "Other countries")) %>% 
  count(team,  year) %>% 
  arrange(year) 

# Graphic -----------------------------------------------------------------
subtitle <- "Some nations are predominants about the the olympic gold medals collected.\nOver the years, they have increased their hegemonies even though \nsecond-class nations are increasingly able to produce Olympic gold medalists."
og_segments <-  tibble (x = c(seq(1896,1912 , by = 4),seq(1920,1936 , by = 4),seq(1948,2016 , by = 4)), y = 620)
gold_medalists %>% 
  ggplot() +
  geom_segment(data = og_segments , aes(x =x  , xend =x, y = 0, yend = -y), size = 0.5, color = "grey15", linetype = "dashed")+ 
  geom_text( data = summer_og_editions, aes(x = year, y =-650, label = str_to_upper(edition)), 
             size = 5.5,lineheight = .90, angle = 45, family = "Inconsolata", fontface = "bold") +
  geom_stream(aes(year, n, fill = team),color = "black", size = .25) +
  geom_stream_label(aes(x=year,y = n ,fill = team, label = str_to_upper(team)), family ="Inconsolata", size = 6.5, fontface = "bold") + 
  annotate(geom= "text", x = 1900, y = 330, label = "GOLD MEDALS", family = "Inconsolata",  size = 5.5, fontface = "bold", hjust = 1.1) +
  annotate(geom= "text", x = 1916, y = -250, label = "WORLD WAR I\n(1914-1918)", family = "Inconsolata", fontface = "bold", size = 6.5) +
  annotate(geom= "text", x = 1942, y = -250, label = "WORLD WAR II\n(1939-1945)",family = "Inconsolata", fontface = "bold", size = 6.5) +
  annotate(geom = "richtext", x = 1930, y = 500, label = '<img src="2021_w31/olympic_flag.png" width ="200"/>', fill = NA, label.color = NA) + 
  annotate(geom = "text", x = 1930, y = 350, label = "SUMMER OLYMPIC GAMES",   family = "Lato Black", size = 14.5, lineheight = 2.5) + 
  annotate(geom = "text", x = 1930, y = 270, label = subtitle, family = "Lato Semibold", fontface = "italic", lineheight = 0.95, size = 6, color = "grey5") +
  coord_cartesian(clip = "off") + 
  scale_fill_manual(
    values =c("United States" = "#0C5BB0FF", 
              "China" = "#EE0011FF",
              "Russia" = "#149BEDFF",
              "Germany" = "#15983DFF",
              "Great Britain" = "#16A08CFF",
              "Australia" = "#FEC10BFF",
              "Netherlands" = "#FA6B09FF",
              "Other countries" = "#9A703EFF",
              "Hungary" = "#A1C720FF",
              "France" = "#EC579AFF",
              "Sweden" = "#FBBB68FF",
              "Romania" = "#EEC229FF",
              "Japan" = "#F51E02FF",
              "Italy" = "#05B102FF",
              "Denmark" = "#6351A0FF",
              "Cuba" = "#026CCBFF"
          ),
    na.value = "grey90"
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    name = NULL,
    expand = expansion(mult = 0.025), 
  )  +
  scale_y_continuous(
    name = NULL,
    labels = abs
  ) + 
  labs(
    caption = "Data from ***Kaggle***.<br>
      Tidytuesday Week-31 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) + 
  theme_minimal(base_family = "Lato") + 
  theme(
    panel.grid = element_blank(),
    axis.text.x =  element_blank(),
    axis.text.y =  element_text(size = 15, color = "black", family = "Inconsolata", face = "bold"),
    axis.ticks.y  = element_line(color ="black", size = .25),
    axis.ticks.length.y = unit(.25, "cm"),
    plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 10,b = 10))
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w31/tidytuesday_2021_w31")
ggsave(glue::glue("{path}.pdf"), width = 18, height = 12, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 600)
