
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggforce)
library(cowplot)
# Data Wrangling ----------------------------------------------------------
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

ratings %>% 
filter(average > 7.5) %>% view()
ratings %>% 
  mutate(average_fct = cut(average, breaks = c(0, 4.5, 5.25, 6, 6.75, 7.5, 10), labels = str_c("Level ", 1:6))) %>% 
  count(average_fct) %>% 
  mutate(n = MESS::round_percent(prop.table(n))*10) %>% 
  pmap_dfr(~tibble(average_fct = rep(.x, .y))) %>% 
  mutate(
    row_num = row_number()-1, 
    x = row_num %% 20, 
    y = row_num %/% 20
  ) -> ratings_circles

# Graphic -----------------------------------------------------------------
(plot <-ratings_circles %>% 
 ggplot() + 
  geom_circle(aes(x0= x, y0 =y, r = .5, fill = average_fct)) + 
  coord_equal(expand = F, clip = "off") + 
  scale_fill_manual(
    values = c(
      "Level 1" = "#00e07b",
      "Level 2" = "#ff0052", 
      "Level 3" = "#ffc750", 
      "Level 4" = "#00d0e0",
      "Level 5" = "#ff009d",
      "Level 6" = "#611e86"
    ),
    guide= "none"
  ) + 
  annotate(geom = "text", x = -1, y = 0.5, label = "<4.5, 2.72%", size = 7.5, hjust = 1) + 
  annotate(geom = "text", x = -1, y = 3, label = "[4.5-5.25], 7.3%", size = 7.5, hjust = 1) + 
  annotate(geom = "text", x = -1, y = 10, label = "[5.25-6], 21%", size = 7.5, hjust = 1) + 
  annotate(geom = "text", x = -1, y = 24, label = "[6-6.75], 32.9%",size = 7.5, hjust = 1) + 
  annotate(geom = "text", x = -1, y = 38, label = "[6.75-7.5], 24.7%", size = 7.5, hjust = 1) + 
  annotate(geom = "text", x = -1, y = 46, label = "> 7.5, 11.4%", size = 7.5, hjust = 1) + 
  theme_minimal() 
)

ggdraw(plot) + 
theme(
    plot.background = element_rect(fill = "#2d2d2e", color = NA), 
    panel.grid = element_blank()
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w4", "tidytuesday_2022_w4")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

