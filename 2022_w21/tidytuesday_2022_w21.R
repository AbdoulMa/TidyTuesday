
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

majors_tournaments <- c("World Cup", "5 Nations", "6 Nations", "Laurie O'Reilly Memorial Trophy", "Nations Cup")

# Compute games scores correlation per year
fifteens <- fifteens %>% 
  mutate(
    tournament = fct_other(tournament, keep = majors_tournaments),
    year = lubridate::year(date), 
    .after = "date") %>% 
  group_by(year) %>% 
  mutate( intercept = coef(lm(score_1 ~ score_2))[1]
  ) %>% 
  ungroup()

fifteens %>% 
  filter(tournament == "World Cup", year == 1998) %>% 
  arrange(desc(score_2))
# Graphic -----------------------------------------------------------------
caption <- "Data from ScrumQueens by way of Jacquie Tran.<br>
Tidytuesday Week-21 2022<br>
Abdoul ISSA BIDA &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."

fifteens %>% 
  filter(year > 1992) %>% 
  ggplot(aes(score_1, score_2)) + 
  geom_point(aes(fill = tournament), size = 3,  stroke = .125, shape = 21, color = "white") + 
  geom_smooth(method = "lm", color = "#111111", fill = "#111111", size = 1.125, level = .9) +
  geom_richtext(inherit.aes = F, aes(x = -Inf,  label = glue::glue("<span style='color:#f53e60;font-weight:15pt; font-family:\"Gotham Black\"'>**{round(intercept,2)}**</span> <span style='font-family: Inconsolata;'>CORRELATION</span>")), y = 148, stat = "unique", hjust = 0,
                label.padding = unit(0, "cm"), label.size = unit(0, "cm"), label.color = NA, fill = NA) +
  labs(
    x = "HOME TEAM SCORE", 
    y = "AWAY TEAM SCORE",
    caption = caption
  ) + 
  scale_x_continuous(
    limits = c(0,max(fifteens$score_1)),
    breaks = seq(0, max(fifteens$score_1), by = 20)
  ) + 
  scale_y_continuous(
    limits = c(0,max(fifteens$score_2)),
    breaks = seq(0,max(fifteens$score_2), by = 20)
  ) +
  scale_fill_manual(
    values = c("World Cup" = "#7DC462",
               "5 Nations" = "#0D95D0", 
               "6 Nations" = "#0D95D0", 
               "Laurie O'Reilly Memorial Trophy" = "#774fA0", 
               "Nations Cup" = "#EFB743",
               "Other" = "#7C0091")
  ) + 
  facet_wrap(vars(year), nrow = 5) +  
  coord_cartesian(expand = F, clip = "off") + 
  theme_minimal() + 
  theme(
    strip.text = element_text(hjust = 0, size= rel(1.75), family = "Gotham Bold", margin = margin(b = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(size= 10, family = "Gotham Book"), 
    axis.title = element_text(color = "#111111", size = rel(1.5), family = "Inconsolata"),
    axis.text = element_text(color = "#111111", size = rel(1.125), family = "Inconsolata"),
    panel.grid = element_line(color = "grey45"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(.25, "cm"),
    axis.ticks = element_line(), 
    axis.ticks.length = unit(.1, "cm"), 
    legend.position = "none"
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w21", "tidytuesday_2022_w21")
ggsave(filename = glue::glue("{path}.pdf"), width = 12.5, height = 10.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}_polished.png"),
  dpi = 640
)

