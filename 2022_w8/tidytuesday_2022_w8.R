
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(ggforce)

# Data Wrangling ----------------------------------------------------------
freedom <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv")

# Selection some countries
countries_selection =  c("Canada", "United States of America", "Democratic People's Republic of Korea", "Republic of Korea",
                         "China", "Australia", "India", "Iran (Islamic Republic of)", "Benin", "Saudi Arabia","Israel","Japan",
                         "Syrian Arab Republic","Kenya", "Russian Federation","Mexico","Ghana","Germany","New Zealand", "Turkey")

# Select randomly other 30 countries
others_countries <- freedom %>% 
  distinct(country) %>% 
  filter(!country %in% countries_selection) %>% 
  sample_n(size = 30) %>% 
  pull()

freedom_index_evolution <- freedom %>% 
  filter(country %in% c(countries_selection, others_countries)) %>% 
  mutate(
    total_index = CL + PR,
    country = str_to_upper(country),
    country = fct_recode(
      country, 
      "TANZANIA" = "UNITED REPUBLIC OF TANZANIA", 
      "IRAN" = "IRAN (ISLAMIC REPUBLIC OF)", 
      "RUSSIA" = "RUSSIAN FEDERATION", 
      "SYRIA" = "SYRIAN ARAB REPUBLIC", 
      "SOUTH KOREA" = "REPUBLIC OF KOREA",
      "NORTH KOREA" = "DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA",
      "USA" = "UNITED STATES OF AMERICA"),
    # Reorder country by index
    country = fct_reorder(country, total_index,.fun = mean),
    CL_evolution = c(NA,-diff(CL)),
    PR_evolution = c(NA,-diff(PR))
  ) %>% 
  mutate(
    CL_evolution_sign = case_when(
      CL_evolution > 0 ~ "↑",
      CL_evolution < 0 ~ "↓",
      TRUE ~ "="
    ),
    PR_evolution_sign = case_when(
      PR_evolution > 0 ~ "↑",
      PR_evolution < 0 ~ "↓",
      TRUE ~ "="
    )
  )


# Graphic -----------------------------------------------------------------
main_plot <- freedom_index_evolution %>%
   ggplot() + 
   geom_circle(aes( x0= 1, y0 = 1, r = .5, fill = 8-CL), size = .125) +
   geom_circle(aes( x0= 2, y0 = 1, r = .5, fill = 8-PR), size = .125) +
   geom_text(aes(x = 1, y = 1, label = CL_evolution_sign), color = "#111111", size = 3.5) + 
   geom_text(aes(x = 2, y = 1, label = PR_evolution_sign), color = "#111111", size = 3.5) + 
   facet_grid(rows = vars(country), cols =vars(year), switch = "y") + 
   coord_equal() + 
   colorspace::scale_fill_continuous_diverging(name = "Freedom Index", palette = "Red-Green", mid = 4,
                                               guide = guide_colourbar(
                                                 barwidth =unit(6, "cm"),
                                                 barheight = unit(.75, "cm"),
                                                 title.position = "top",
                                                 title.hjust = .5,
                                                 title.theme = element_text(family = "I Sans Semibold Pro", face = "bold", size = 18),
                                                 label.theme = element_text(family = "I Sans Semibold Pro", size = 15)
                                               ),
                                               breaks = c(2,6),
                                               labels = c("← Bad","Good →")
   ) +
   theme_minimal() + 
   theme(
     text = element_text(family = "I Sans Medium Pro"),
     strip.placement = "outside", 
     strip.text.y.left = element_text(family ="I Sans Semibold Pro", face = "bold", color = "#111111", size = 12, angle = 0, hjust = 1),
     strip.text.x =  element_text(family ="I Sans Semibold Pro", face = "bold", color = "#111111", size = 12),
     panel.spacing.x = unit(.25, "cm"),
     axis.text = element_blank(),
     axis.title = element_blank(),
     panel.grid = element_blank(),
     legend.direction = "horizontal", 
     legend.position = "top"
   )


legend_plot <- ggplot(data = NULL) + 
    geom_circle(aes(x0= c(0,1), y0 = 1,r = .5), size = .25) +
    annotate(geom = "segment", x = 0:1, y = 1.5, xend = 0:1, yend = 1.75) +
    annotate(geom = "segment", x = 0:1, y = 1.75, xend = c(-.5,1.5), yend = 1.75) +
    annotate(geom = "point", x = 0:1, y = 1.5, shape = 21, size = 2.5, fill = "white") + 
    annotate(geom = "text", x = c(-.55, 1.55), y = 1.75 , hjust = c(1,0), 
             size = 3.5,
             color = "#111111",
             label = c("Civil\nLiberties","Political\nRights"),
             family = "I Sans Medium Pro") + 
    coord_equal(clip = "off") + 
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )


evolution_legend_plot <- ggplot(data = NULL) + 
    geom_circle(aes(x0 = 0, y0 = 1:3, r= .45), size = .25) + 
    geom_text(aes(x = 0, y = 1:3, label = c("↑","↓", "="))) + 
    geom_text(aes(x = .75, y = 1:3, label = str_to_upper(c("improved","deteriorated", "stabilized"))), family = "I Sans Medium Pro",  hjust = 0, size = 2.5) + 
    annotate(geom = "text", x = -.75, y = 2, label = str_to_upper("evolution compared \nto the previous year"),  family = "I Sans Semibold Pro", angle = 90, vjust = 0, size = 2.5) + 
    coord_equal(clip = "off") + 
    theme_void()


caption <- "Data  from Freedom House and the United Nations by way of Arthur Cheib.\n Tidytuesday Week-8 2022 · Abdoul ISSA BIDA"
cowplot::ggdraw(main_plot) + 
  cowplot::draw_plot(legend_plot, x = .35, y = .425, scale = .075) +
  cowplot::draw_plot(evolution_legend_plot, x = -.25, y = .445, scale = .075) + 
  labs(
    title =  "State of freedoms in the World",
    caption =  caption
  ) +
  theme(
    text = element_text(family = "I Sans Medium Pro", color = "#111111", face= "bold"),
    plot.title = element_text(family = "NY Bold", size = rel(4), hjust = .5),
    plot.caption = element_text(size = rel(1.5), hjust = .5),
    plot.background = element_rect(fill = "#c3c2c1", color = NA),
    plot.margin = margin(t =0.75, r= 1 , b =.5, l=.5, unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2022_w8", "tidytuesday_2022_w8")
ggsave(filename = glue::glue("{path}.png"), width = 15, height = 18, device = ragg::agg_png, dpi = 300)
ggsave(filename = glue::glue("{path}.jpg"), width = 15, height = 18, device = ragg::agg_jpeg)
