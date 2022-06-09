
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(treemapify)

# Data Wrangling ----------------------------------------------------------
# Read data
pride_aggregates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv")
# Exclude the total
pride_aggregates <- pride_aggregates %>%
  filter(Company != "Grand Total")

# Define a color for each company
company_colors <- tribble(
  ~"Company", ~"color",
  "Aetna", "#743293",
  "Amazon", "#F79F00",
  "AT&T", "#00a8e0",
  "Bank of America", "#00005a",
  "Budweiser", "#CC122C",
  "Capital One", "#004977",
  "Chevron", "#2491ca",
  "Citigroup", "#ee1c25",
  "Comcast", "#fdb913",
  "ConocoPhillips", "#040404",
  "Cox Enterprises", "#3c5774",
  "Enterprise", "#34b56a",
  "FedEx", "#32008d",
  "General Motors", "#0a70cd",
  "Gilead", "#C6093B",
  "Jack Daniel's (Brown-Forman)", "#080808",
  "Johnson & Johnson", "#d30000",
  "JPMorgan Chase", "#006ebb",
  "Microsoft", "#F35325",
  "NBC", "#cc004c",
  "Outfront", "#771397",
  "PNC", "#f48024",
  "State Farm", "#ed1d24",
  "Sysco", "#0081c6",
  "Target", "#cc0000",
  "Toyota", "#e40521",
  "Truist", "#140037",
  "Trulieve", "#bed330",
  "Walgreens", "#e62725",
  "Walmart", "#1d75cf"
)

# Graphic -----------------------------------------------------------------
title <- 'Contributions from companies to anti-<span style="color:#e40303;">L</span><span style="color:#ff8c00;">G</span><span style="color:#ffed00;">B</span><span style="color:#008026;">T</span><span style="color:#004dff;">Q</span><span style="color:#750787;">+</span> politicians'
subtitle <- "Data for Progress revelates how corporations presenting themselves as LGBTQ+ allies, are giving
to state politicians behind some of the most bigoted and harmful policies in over a decade.
The companies that support these politicians must be held accountable."
caption <- "&#42;value indicates the total contributed to anti-LGBTQ politicians.<br> Data from **Data For Progress** &bull; Tidytuesday Week-23 2022 &bull;  Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."

pride_aggregates %>%
  left_join(company_colors) %>%
  mutate(
    Company = str_wrap(Company, 14)
  ) %>%
  ggplot(aes(fill = color, color = after_scale(prismatic::best_contrast(fill, y = c("#111111", "#FFFFFF"))), area = `Total Contributed`, label = scales::comma(round(`Total Contributed`)), subgroup = Company)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#111111", size = 3) +
  geom_treemap_subgroup_text(place = "topleft", family = "Gotham Black", grow = T, min.size = 0) +
  geom_treemap_text(place = "bottomright", family = "Gotham Condensed", alpha = .5, grow = T, reflow = T) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(c(1, 1, 1, 1), unit = "cm"),
    plot.title = ggtext::element_markdown(hjust = .5, family = "Lato Black", size = rel(2.75), margin = margin(b = .5, unit = "cm")),
    plot.subtitle = element_text(hjust = .5, family = "NY Bold Italic", size = rel(1.75), margin = margin(b = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(hjust = .5, family = "Lato", size = rel(1.5), margin = margin(t = .5, unit = "cm"))
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w23", "tidytuesday_2022_w23")
ggsave(filename = glue::glue("{path}.png"), width = 12.5, height = 12.5, dpi = 640, device = ragg::agg_png)
