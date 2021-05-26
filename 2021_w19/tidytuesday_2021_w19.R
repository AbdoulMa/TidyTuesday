library(tidytuesdayR)
library(tidyverse)
library(ggtext)

# Inspiration Nathan Yau :  https://flowingdata.com/2019/03/26/bump-chart-r/ 


# Load Data & Wrangling ---------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 19)
water_data <- pluck(tuesdata,"water")
data_wrangled <- water_data %>% 
  filter(!is.na(install_year), !is.na(country_name)) %>% 
  mutate(install_decade = install_year %/% 10, 
         install_decade = install_decade * 10) %>%  
  filter(between( install_decade,  1970,2020)) %>% 
  group_by(country_name, install_decade)  %>% 
  summarize(nb_install = n()) %>% 
  pivot_wider(names_from = install_decade, values_from = nb_install) %>% 
  filter(across(everything(), ~!is.na(.))) %>% # Just keep those with data available
  pivot_longer(
    cols = -country_name,
    names_to = "decade",
    values_to = "nb_install"
  ) %>% 
  group_by(decade) %>% 
  mutate(country_name = str_to_upper(country_name),
         rank = rank(-nb_install)) %>%  
  arrange(country_name,decade)

# For detach from the facets  
data_wrangled_bis <- data_wrangled %>% 
  mutate(country = country_name) %>% 
  select(everything(), -country_name)

# Plot --------------------------------------------------------------------
decades <- data_wrangled %>% 
  filter(country_name ==str_to_upper("Ethiopia")) %>% 
  pull(decade) 
ranks <- data_wrangled %>% 
  filter(country_name ==str_to_upper("Ethiopia")) %>% 
  pull(rank) 
decades_legend <-  tibble(
  country_name = str_to_upper("Ethiopia"),
  decades = c(first(decades) ,last(decades)),
  ranks = c(first(ranks),last(ranks)),
  vjusts = -0.1
) %>% 
  mutate(
    decades_labels = str_c(str_sub(decades,-2,-1),"s"),
    labels = glue::glue("<b>#{ranks}</b> in <br>{decades_labels}")
  )

 data_wrangled %>% 
  ggplot() + 
  geom_line(data = data_wrangled_bis,aes(x = decade, y = rank , group = country), color = "grey75",size = .7) + 
  geom_line(aes( x = decade, y = rank , group = country_name), size = 1.1, color = "#0b53c1") +
  geom_point(aes(x = decade, y = rank), shape = 21, fill = "white", color = "#0b53c1", stroke = 1.3,  size = 2.8) +
  geom_richtext(data = decades_legend, aes(x = decades, y = ranks, label = labels, vjust = vjusts), label.color = NA,fill = NA, family="Lato", fontface = "italic", size =3 ) +
   labs(
     title = "Ranking some countries by the number<br>of water sources installations by decade",
     caption = "Data from Water Point Data Exchange.<br>
      <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**"
   ) + 
  scale_y_reverse(
    name = NULL
  ) + 
   scale_x_discrete(
     name = NULL, 
     labels = function(x) {paste0(str_sub(x,-2,-1),"s")}
   ) + 
   coord_cartesian(clip = "off") + 
  facet_wrap(vars(country_name)) + 
  theme_minimal(base_family = "Inconsolata") +
   theme(
     plot.background = element_rect(fill = "#f9fbfc"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     axis.text.y = element_blank(),
     axis.text =  element_text(color = "black"),
     plot.title = element_markdown(face = "bold", hjust = 0.5, size = rel(1.8), margin = margin(t=10, b = 15)),
     plot.subtitle = element_markdown(size = rel(0.9)),
     plot.caption = element_markdown(family = "Source Sans Pro",size = rel(.8), margin = margin(t = 10)),
     strip.text = element_text( face = "bold", size = rel(1.1)),
     plot.margin = margin(t= 15, r = 10,b = 20, l = 10)
   )
  

# Save image --------------------------------------------------------------
ggsave(here::here("Outputs","tidytuesday_2021_w19.png"),height = 8, width = 15)

 