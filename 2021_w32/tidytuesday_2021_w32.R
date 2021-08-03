# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(gganimate)
# Data Reading and Wrangling ----------------------------------------------
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')
athletes <- athletes %>% 
  mutate(abb = case_when(abb == "URS" ~ "RUS",
                         abb == "FRG" ~ "GER",
                         TRUE ~ abb))

# Get Paralympic Games Editions from last week Dataset
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
games_editions <- olympics %>% 
  filter(season == "Summer") %>% 
  distinct(year, city) %>% 
  mutate(edition = glue::glue('{city} <br> <span style = "color: grey35;">{year}</span>')) %>% 
  filter(between(year, 1980,2016)) %>% 
  arrange(year)

# Retrieve best federations 
top_federations <- athletes %>% 
  filter(medal == "Gold") %>% 
  count(abb, sort = T) %>% 
  head(20)

# Pull federations
top_federations_list <- top_federations %>% 
  pull(abb)

# Establish countries ranking per edition
medals_records <- athletes %>% 
  filter(medal == "Gold", abb %in% top_federations_list) %>% 
  distinct(abb, year,event , .keep_all = T) %>% # (One gold medal for collective sport)
  count(abb, year,  sort = T)  %>% 
  complete(abb, year, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(rk = rank(-n, ties.method = "first")) %>%
  ungroup(year) %>%
  arrange(year)

# Graphic -----------------------------------------------------------------
highlight_federations <- c("USA","CHN","AUS")
plot <- medals_records %>% 
  ggplot() + 
  geom_line(data = medals_records %>%
              group_by(abb) %>%
              do({
                tibble(year = seq(min(.$year), max(.$year),length.out=100),
                       rk = pracma::pchip(.$year, .$rk, year))
              }),
            aes(year, rk, group = abb, alpha = ifelse(abb %in% highlight_federations, 1, .25),
                color = ifelse(abb %in% highlight_federations, abb, NA),
                size = ifelse(abb %in% highlight_federations, 2.5, 1.5))) +
  geom_point(aes(year, rk, color = ifelse(abb %in% highlight_federations, abb, NA), 
                 stroke = ifelse(abb %in% highlight_federations, 1.75, .75),
                 size =  ifelse(abb %in% highlight_federations, 11.5, 8.5),
                 group = seq_along(year)
  ),
  shape = 21, fill = "white") + 
  geom_text(aes(year, rk, label = abb, group = seq_along(year),
                size =  ifelse(abb %in% highlight_federations, 3.75, 2.75),
                color = ifelse(abb %in% highlight_federations, abb, NA)), 
            family = "Lato Black",
            fontface = "bold") + 
  geom_richtext(data = games_editions, aes(x = year, y = 22, group = seq_along(year), label = edition),
                fill = NA, label.color = NA, size = 6,  family = "Lato Black", lineheight = .9) + 
  annotate(geom= "text", x = 1980, y = 0, label = "# RANK", family = "Inconsolata",  size = 5.5, fontface = "bold", hjust = 1.6) +
  labs(title = '<img src="2021_w32/paralympic_logo.png" width ="100"/><br>PARALYMPIC GAMES<br><span style = "color: grey25;"> GOLD MEDALS RACE</span></br>',
       caption = "Data from ***International Paralympic Committee***.<br>
      <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**.") + 
  scale_y_reverse(
    breaks = 1:20
  ) +
  scale_alpha_identity() + 
  scale_color_manual(
    values = c(
      "USA" = "#001c5a",
      "CHN" = "#df1b12",
      "AUS" = "#FFCD00"
    ),
    na.value = "black",
    guide ="none"
  ) +
  scale_size_identity() + 
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Lato") + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y =  element_text(size = 15, color = "black", family = "Inconsolata", face = "bold"),
    axis.title = element_blank(),
    plot.title = element_markdown(color = "black", family = "Lato Black", size = rel(2), margin = margin(t = 5,b = 5), hjust = .5),
    plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
    plot.margin = margin(t = 10,l = 15,r = 15)
  ) 

plot 
# Saving ------------------------------------------------------------------
path <- here::here("2021_w32/tidytuesday_2021_w32")
ggsave(glue::glue("{path}.pdf"),plot = plot, width = 18, height = 12, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 480)


# Animation ---------------------------------------------------------------
animation <- plot + 
  transition_reveal(year)

animate(animation, 10*5, fps = 5 , duration = 30, width = 1200, height = 950, end_pause = 10, 
        renderer = gifski_renderer(glue::glue("{path}.gif")))
