# Loading libraries -------------------------------------------------------
library(tidyverse)
library(ggtext)
library(paletteer)
library(patchwork)
library(ragg)

# Read Data & Wrangling ---------------------------------------------------
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
wikimedia <- "https://upload.wikimedia.org/wikipedia/commons/thumb/"
scoobydoo_series <- scoobydoo %>% 
  mutate(imdb = parse_number(imdb),
         # Get tv logos on wikimedia
         network_logo = case_when(
           network == "Cartoon Network" ~ paste0(wikimedia,"1/1c/Cartoon_Network_logo.svg/800px-Cartoon_Network_logo.svg.png"),
           network == "The CW" ~ paste0(wikimedia,"4/4c/The_CW.svg/800px-The_CW.svg.png"),
           network== "The WB" ~ paste0(wikimedia,"0/02/Logo_of_The_WB.svg/248px-Logo_of_The_WB.svg.png"),
           network == "ABC" ~ paste0(wikimedia,"5/54/American_Broadcasting_Company_Logo.svg/512px-American_Broadcasting_Company_Logo.svg.png"),
           network == "Boomerang" ~ paste0(wikimedia,"3/35/Boomerang_2014_logo.svg/300px-Boomerang_2014_logo.svg.png"),
           network == "CBS" ~ paste0(wikimedia,"e/ee/CBS_logo_(2020).svg/799px-CBS_logo_(2020).svg.png"),
           TRUE ~ ""
          ),
         national_tv = case_when(
           network %in% c("ABC", "The CW","CBS") ~ TRUE,
           TRUE ~ FALSE),
         network_logo = glue::glue("<img src='{network_logo}' width='30'/>")) %>%  
  filter(format == "TV Series", !season %in% c("Movie","Special")) # Just keepTV Series
  
# Compute each network average imdb mean 
scoobydoo_nw_imdb <- scoobydoo_series %>% # Networks imdb average
  group_by(network_logo, national_tv) %>% 
  summarise(
    nb_ep = n(),
    imdb = mean(imdb, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(network_logo = fct_reorder(network_logo, imdb)) 

# Define plots common theme
common_theme <- theme_minimal(base_family = "Lato") + 
  theme(
    plot.title = element_text(family = "Lato Black", color = "black", size = rel(3.5), hjust = 0.5, margin = margin(t = 15,b = 20)),
    plot.subtitle = element_text(family = "Lato Semibold", color = "grey40", size = rel(2), face = "italic", margin = margin(t = 15,b = 20)),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_markdown(),
    axis.text.x = element_text(family = "Inconsolata", face = "bold", size = rel(1.25), colour = "black" ),
    legend.title = element_blank(),
    legend.text = element_text(family = "Lato", face = "bold"),
    plot.margin = margin(l = 10, r = 10),
    plot.caption = element_markdown(color = "grey15", size = rel(1), margin = margin(t = 10,b = 10)),
    legend.position = "top"
  )

average_imdb <- mean(scoobydoo_nw_imdb$imdb) # Overall Average
light_grey <- rgb(201,201,201,80,maxColorValue=255)
bright_grey <- rgb(201,201,201,120,maxColorValue=255)

# IMDB Score Plot
(p1 <-  scoobydoo_nw_imdb %>% 
  ggplot() + 
  geom_col(aes(x = imdb, y = network_logo))+ 
  annotate(geom = "rect", xmin = 0, xmax = 2, ymin = -0.0675, ymax = 7, fill = light_grey) +
  annotate(geom = "rect", xmin = 2, xmax = 4, ymin = -0.0675, ymax = 7, fill = bright_grey) +
  annotate(geom = "rect", xmin = 4, xmax = 6, ymin = -0.0675, ymax = 7, fill = light_grey) +
  annotate(geom = "rect", xmin = 6, xmax = 8, ymin = -0.0675, ymax = 7, fill = bright_grey) +
  annotate(geom = "rect", xmin = 8, xmax = 10, ymin = -0.0675, ymax = 7, fill = light_grey) +
   geom_richtext(aes(x = imdb, y = network_logo, label = glue::glue("{nb_ep} EPS<br>{round(imdb,2)}")) , hjust = 0., family = "Inconsolata", fontface = "bold", fill= NA, label.color = NA)+ 
  geom_col( data =  filter(scoobydoo_nw_imdb,  national_tv),aes(x= imdb, y = network_logo), fill = "#1d457f") +
  annotate(geom = "segment", x = average_imdb, xend = average_imdb, y = -.125, yend = 7, col = "skyblue3", size = .65) +
  annotate(geom = "segment", x = average_imdb, xend = average_imdb, y = -.125, yend = 0,  size = 1.3) +
  annotate(geom = "segment", x = average_imdb, xend = average_imdb, y = 7, yend = 7.125,  size = 1.3) +
  annotate(geom = "richtext", x = average_imdb, y = 7.125, label = glue::glue("AVERAGE : <b>{round(average_imdb,2)} /10</b>"), hjust = 0,family = "Inconsolata",  fill =NA, label.color = NA) + 
   annotate("text", x = 4.5, y = 5, label = "NATIONAL TV", family = "Inconsolata", size = rel(5.5), color = "white",fontface = "bold") + 
   labs(title = "Scooby Doo TV Series broadcasting on US TV", 
        subtitle = "What average scores viewers give to episodes according to the broadcaster?") + 
  scale_x_continuous(
    limits = c(0,10),
    breaks= seq(0,10, by = 2),
    expand = expansion(add = 0)
  ) +
  scale_y_discrete(
    expand =expansion(mult = 0)
  ) +
   coord_cartesian(clip = "off") + 
   common_theme
) 

# Pull common motives
common_motives <- scoobydoo_series %>% 
  count(motive, sort = T) %>% 
  pull(motive)

# Leveling TV by common motives
scoobydoo_epi_motives <- scoobydoo_series %>%  
  mutate( network = fct_relevel(network, c("The WB","Cartoon Network","Boomerang","CBS","ABC","The CW")),                                  
    motive = fct_other(motive, keep = common_motives[1:4], other_level = "Other motives")) 
  
# Pulling TV logos
labels <- scoobydoo_epi_motives %>% 
  arrange(desc(network)) %>% 
  pull(network_logo) %>% 
  unique()

# Compute common motive by network 
scoobydoo_epi_motives <- scoobydoo_epi_motives %>% 
  count(network, network_logo,  motive) %>% 
  mutate(
    motive = fct_reorder(motive, n)) %>% 
  group_by(network) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup()

# Motives Plot
(p2 <- scoobydoo_epi_motives %>% 
 ggplot(aes(x = n, y = fct_rev(network), fill = motive)) + 
 geom_bar(color = "white", size = 0.125,stat = "identity", position = position_fill(reverse = -1 )) + 
 geom_text(data = filter (scoobydoo_epi_motives, motive == "Competition"),aes(label =  glue::glue("{round(prop,3)*100}%")),
           position = position_fill(), hjust = 1.5,  family = "Inconsolata", 
           size = rel(4),color = "white", fontface = "bold")  +
  scale_x_continuous(
    expand = expansion(mult = 0), 
    breaks= seq(0,1, by = .2),
    labels = scales::percent
 ) + 
  scale_y_discrete(
    labels = labels
  ) +
  scale_fill_paletteer_d(
    palette = "PNWColors::Bay",
    labels = str_to_upper
  ) + 
  labs(subtitle = "What are the crimes motives ?",
       caption = "Data from ***plummye*** recommended by ***Sara Stoudt***.<br>
      Tidytuesday Week-29 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**") +
  coord_cartesian(clip = "off") + 
  common_theme
)

# Combine plot
plot <- p1 / p2  +
  plot_layout(heights = c(1.35,1))


# Saving ------------------------------------------------------------------
ggsave(here::here("2021_w29/tidytuesday_2021_w29.png"), width = 13, height = 12, device = agg_png(), units = "in", dpi = 300)

# ALT TEXT
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 29.
# This week data is about Scooby Doo Episodes and comes from plummye's Kaggle and is recommended by Sara Stoudt.
# The plot is a composition of two. A first one about average imdb viewers scores according to broadcasters.
# The second one is about how do mobiles crimes vary according to broadcasters.
