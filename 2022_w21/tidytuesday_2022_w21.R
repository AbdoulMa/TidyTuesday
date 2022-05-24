
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

cowplot::set_null_device("pdf")
cowplot::set_null_device("png")
cowplot::set_null_device("cairo")
cowplot::set_null_device("agg")
# Data Wrangling ----------------------------------------------------------
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')
fifteens <- fifteens %>% 
  mutate(year = lubridate::year(date), 
         .after = "date")

fifteens <- fifteens %>% 
  group_by(year) %>% 
  mutate( intercept = coef(lm(score_1 ~ score_2))[1]
  ) %>% 
  ungroup()


caption <- "Data from ScrumQueens by way of Jacquie Tran.<br>
Tidytuesday Week-21 2022 &bull;Abdoul ISSA BIDA &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."

# points color with competions (world cup, six nation, tri nation )
fifteens %>% 
  filter(year > 1992) %>% 
  ggplot(aes(score_1, score_2)) + 
  geom_smooth(method = "lm", color = "pink", size = 1.5) + 
  geom_point() + 
  geom_richtext(aes(x = -Inf, y = Inf, label = glue::glue("<span style='color:pink;font-weight:15pt; font-family:\"Gotham Black\"'>**{round(intercept,2)}**</span> <span style='font-family: Inconsolata;'>CORRELATION</span>")), stat = "unique", hjust = 0, inherit.aes = F) +
  labs(
    caption = caption
  ) + 
  scale_y_continuous(
    expand = expansion(mult = c(0, .2))
  ) +
  facet_wrap(vars(year), nrow = 5, scales = "free") +  
  coord_cartesian( clip = "off") + 
  theme_minimal(base_family = "Gotham Bold") + 
  theme(
    strip.text = element_text(hjust = 0, size= rel(1.25), face = "bold", margin = margin(b = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(size= 7.5)
    )
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w21", "tidytuesday_2022_w21")
ggsave(filename = glue::glue("{path}.pdf"), width = 15, height = 14.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

