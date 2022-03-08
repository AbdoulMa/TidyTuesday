
# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------

erasmus <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv") %>% 
  mutate(
    receiving_country_code = case_when(receiving_country_code == 'EL' ~ 'GR',
                                       receiving_country_code == 'UK' ~ 'GB',
                                       TRUE ~ receiving_country_code)
  )

erasmus %>% 
  count(participant_gender)
countries_codes <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

women_receiving_countries <- erasmus %>% 
  filter(participant_gender == "Female") %>% 
  group_by(receiving_country_code) %>% 
  summarise(
    nb_participants = n()
  ) %>% 
  ungroup() %>% 
  left_join(countries_codes, by = c('receiving_country_code' = 'Alpha-2 code')) %>% 
  mutate(
    Country = ifelse(str_detect(Country,"Macedonia"), "Macedonia", Country)
  )

country_max_letters <- max(str_length(women_receiving_countries$Country))
women_receiving_countries <- women_receiving_countries %>% 
  rowwise() %>%
  mutate(
    Country = paste0(Country, str_c(rep(".",3+country_max_letters - str_length(Country)), collapse = ""))
  ) %>% 
  ungroup() %>% 
  mutate(
    Country = str_to_upper(Country),
    Country = fct_reorder(Country, nb_participants)
  )


# Alternative 
visited_cities <- erasmus %>% 
    filter(participant_gender =="Female") %>% 
    mutate(
      receiving_city = ifelse(str_detect(receiving_city,"tochowa"),"Częstochowa", receiving_city),
      receiving_city = str_to_title(receiving_city), 
      receiving_city = paste0(receiving_city, ",", receiving_country_code)
    ) %>% 
    count(receiving_city) %>% 
    slice_max(order_by = n, n = 25) 

cities_max_letters <- max(str_length(visited_cities$receiving_city))

visited_cities <- visited_cities %>% 
    rowwise() %>% 
    mutate(
      receiving_city = paste0(receiving_city, str_c(rep(".", 2 + cities_max_letters - str_length(receiving_city)), collapse = ""))
    ) %>% 
    ungroup() %>% 
    mutate(receiving_city = fct_reorder(receiving_city,n)) 


# Title : Where women go for their ERASMUS Exchan
# A quick  plot for this week tidytuesday about . 
# I visualized where the women go for their exchange program. 
 
# Graphic -----------------------------------------------------------------
common_theme <- function() {
  theme_minimal() +
    theme(
      axis.ticks.length = unit(0,"pt"), 
      axis.text.y = element_text(family = "Inconsolata Black", color ="#FFFFFF", size = rel(1.5)),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )
}

(main_plot <- women_receiving_countries %>% 
   ggplot(aes(x = nb_participants, y = Country)) + 
   geom_col(fill = "#f24a4a") + 
   geom_text(aes(label = format(nb_participants, big.mark = ","),
                 hjust = ifelse(str_detect(Country,str_to_upper("Liechtenstein")), -.125, 1.125)),
             family = "DecimaMonoPro",
             size = 5, 
             color = "#FFFFFF") +
   coord_cartesian(expand = F, clip = "off") + 
   common_theme()
)

(visited_cities_plot <- visited_cities %>% 
    ggplot(aes(x = n, y = receiving_city)) +
    geom_col(fill = "#0085ca") + 
    geom_text(aes(label = format(n, big.mark = ",")),
              color = "#FFFFFF",
              family = "DecimaMonoPro",
              size = 5, 
              hjust = 1.125) + 
    labs(
      title = "TOP HOSTING CITIES"
    ) + 
    coord_cartesian(expand = F, clip = "off") + 
    common_theme() + 
    theme(
      plot.title = element_text(family = "Go Bold", color = "#FFFFFF", size = rel(2), margin = margin(b = .5, unit ="cm"))
    )
)

# Plots combine
cowplot::ggdraw(main_plot) + 
  cowplot::draw_plot(visited_cities_plot, x = .25, y = -.175, scale = .6) + 
  labs(
    title = "Where do women go for their ERASMUS exchanges?", 
    subtitle = "Between 2014 and 2020, 88452 women\ntook advantage of the Erasmus exchange throughout Europe.", 
    caption = "Data from Data.Europa<br>
Tidytuesday Week-10 2022 &bull;Abdoul ISSA BIDA &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) + 
  theme(
    plot.background = element_rect(fill = "#181818", color = NA),
    plot.margin = margin(t = .5, r = 2, b = .5, l = 1, unit = "cm"),
    plot.title = element_text(color = "#FFFFFF", family = "Go Black", size = rel(2), margin = margin(t = .5, b = .5, unit = "cm")),
    plot.subtitle = element_text(color = "#FFFFFF", family = "NY Bold Italic", size = rel(1.75),  margin = margin(b = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(size = rel(1.05), family = "Go Medium", colour = "#FFFFFF", margin = margin(b = .5, unit = "cm"))
  )


# Saving ------------------------------------------------------------------
path <- here::here("2022_w10", "tidytuesday_2022_w10")
ggsave(filename = glue::glue("{path}.png"), width = 13.5, height = 13, dpi = 300, device = ragg::agg_png)


