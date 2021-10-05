# Load libraries ----------------------------------------------------------
library(tidyverse)
library(geofacet)
library(ggtext)
library(biscale)
library(cowplot) 
# Data Reading and Wrangling ----------------------------------------------
nurses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')
states <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR")

# Data for states abbr
states_populations <- read_csv("2021_w41/states_populations.csv") %>% 
  select(State, Pop)

# Add Puerto-Rico
us_state_grid1_with_pr <- us_state_grid1 %>% 
  add_row(row = 7, col = 10, code = "PR", name = "Puerto Rico")


nurses_2020 <- nurses %>% 
  filter(Year == 2020) %>% 
  select(State, Total_nurses = `Total Employed RN`, hourly_wage = `Hourly Wage Avg`) %>% 
  left_join(states_populations, by = "State") %>%
  drop_na() %>% 
  mutate(nurse_pop_ration = (Total_nurses/ Pop)* 1000) %>% 
  # Join States for the abbreviations
  left_join(states) %>% 
  bi_class( x = nurse_pop_ration, y = hourly_wage, style = "quantile", dim = 3)

# Graphic -----------------------------------------------------------------
# Palette
custom_pal <- bi_pal_manual(val_1_1 = "#A50026", val_1_2 = "#D73027", val_1_3 = "#F46D43",
                            val_2_1 = "#FEE08B", val_2_2 = "#FFFFBF",val_2_3 = "#D9EF8B",
                            val_3_1 = "#66BD63", val_3_2 = "#1A9850",val_3_3 = "#006837")


(legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Higher ratio of Nurses per hab",
                    ylab = "Higher Hourly Wage",
                    size = 8) + 
  bi_theme(base_family = "Gotham Black") + 
  theme(
        rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 6.5,
                                    color = "#66BD63"),
        axis.title.y = element_text(size = 6.5,
                                    color = "#F46D43"))
)

# Facet Grid
(map <- nurses_2020 %>% 
  ggplot() + 
  geom_rect(aes(fill = bi_class), xmin = -1, ymin = -1, xmax = 1, ymax = 1, color = "white") + 
  geom_richtext(aes(label = glue::glue("<span style='font-size: 25px;font-family: \"Gotham Black\";'>{Abbreviation}</span><br><br><span style='font-size: 20px; font-family:\"Mercury Display\";'>${hourly_wage}</span>"),color = ifelse(str_detect(bi_class,"1-1|1-2|3-3"), "white","black")),
                hjust = 0.5,
                size = 3,
                x = 0, y = 0.05,
                fontface = "bold",
                fill = NA, label.colour = NA, show.legend = F) + 
  coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
    scale_color_identity() + 
  bi_scale_fill(pal=custom_pal, dim =3, guide = "none") + 
  facet_geo(vars(State), grid = us_state_grid1_with_pr) + 
  labs(
    title = str_to_upper("nursing staff"),
    subtitle = "A bivariate map showing number of practicing nurses  per 1,000 inhabitants\n and hourly wage in each state.",
    caption = "Data from Data.World.\n Tidytuesday Week-41 2021 · Abdoul ISSA BIDA."
  ) + 
  theme_minimal() + 
  theme(
    panel.spacing = unit(-5,"points"),
    strip.text = element_blank(),
    plot.title = element_text(size = rel(3.5),hjust = .5, margin = margin(t = 10, b = 15), family = "Gotham Black",  face = "bold"),
    plot.subtitle = element_text(size = rel(1.45),hjust = .5, margin = margin(b = 25), family = "Mercury Display"),
    plot.caption = element_text(color = "black", size = rel(.95), family = "Gotham Medium", margin = margin(t = 15, b = 10, r = 10), hjust = 1),
    plot.margin = margin(t = 20, r = 10, b =10, l = 10), 
    plot.background = element_rect(fill = "white", color = NA)
  )
)

(finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.8, 0.2, 0.2)
)

# Saving ------------------------------------------------------------------
path <-  here::here("2021_w41/tidytuesday_2021_w41")
ggsave(glue::glue("{path}.pdf"), width = 15, height = 9, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}_twitter.png"),
                      format = "png", dpi = 320)
