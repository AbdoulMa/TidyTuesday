
# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Data Wrangling ----------------------------------------------------------
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')
fifteens <- fifteens %>% 
  mutate(year = lubridate::year(date), 
         .after = "date")

fifteens <- fifteens %>% 
  group_by(year) %>% 
  mutate( intercept = coef(lm(score_1 ~ score_2))[1]
  )  



fifteens %>% 
  filter(year > 1992) %>% 
  ggplot(aes(score_1, score_2)) + 
  geom_point() + 
  ggtext::geom_richtext(aes(x = 0, y = Inf, label = glue::glue("<span style='color:pink;'>{round(intercept,2)}</span> Correlation")), stat = "unique", hjust = 0, inherit.aes = F) +
  geom_smooth(method = "lm") + 
  scale_y_continuous(
    expand = expansion(mult = c(0, .2))
  ) +
  facet_wrap(vars(year), nrow = 6, scales = "free") +  
  coord_cartesian( clip = "off") + 
  theme_minimal(base_family = "UEFA Supercup") + 
  theme(
    strip.text = element_text(hjust = 0, margin = margin(b = .5, unit = "cm"))
  )
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w21", "tidytuesday_2022_w21")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

