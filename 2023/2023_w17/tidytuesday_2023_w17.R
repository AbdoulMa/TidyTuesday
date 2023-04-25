
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(treemapify)

# Data Wrangling ----------------------------------------------------------
winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')


winners_by_nationality <- winners |> 
  mutate(
    fancy_winner = paste0(Athlete, "·","(",Year,")")
  ) |> 
  summarize(
    winners = str_flatten(fancy_winner, collapse = "\n"),
    n = n()
    ,.by = c(Category, Nationality)) |> 
  arrange(Category, desc(n))

unique(winners_by_nationality[winners_by_nationality$Category %in% c("Men", "Women"),c("Nationality")])
# Graphic -----------------------------------------------------------------
winners_by_nationality |>
  mutate(fancy_category = case_match(Category, 
                                "Men" ~ '<span style="color:#003B66;">Men</span>', 
                                .default = '<span style="color:#CE1126;">Women</span>'
                                ),
         fancy_category = fct_inorder(fancy_category)
         ) |>
  filter(Category %in% c("Men", "Women")) |> 
  ggplot(aes(area = n, fill = Category, subgroup = winners, label = Nationality)) + 
  geom_treemap() +
  geom_treemap_text(family = "Google Sans", color = "white", fontface = "bold") +
  geom_treemap_subgroup_text(color = "white", place = "bottomleft", family = "Google Sans", grow = F, reflow = T, min.size = 0, size = 11) +
  labs(
    title = "LONDON Marathon",
    subtitle = "Winners by nationality",
    caption =  "Tidytuesday Week-17 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      Data from **{LondonMarathon}** by **Nicola Rennie**"
  ) +
  facet_wrap(vars(fancy_category)) + 
  scale_fill_manual(
    values = c(
      "Men" = "#003B66",
      "Women" = "#CE1126"
    ),
    guide = "none"
  ) + 
  coord_cartesian() + 
  theme(
    text = element_text(color = "#111111",family = "Google Sans"),
    panel.spacing.x = unit(1, "cm"), 
    plot.title = element_text(size = rel(3.5), face = "bold"),
    plot.subtitle = element_text(size = rel(1.75), family = "MercurySSm-Bold",  margin = margin(b = .25, unit = "cm")),
    plot.caption = element_markdown(size = rel(1.125),hjust = .5, margin = margin(t = .75, unit = "cm")),
    strip.text = element_markdown(family = "Google Sans", face = "bold", size = 35, hjust = 0), 
    strip.background = element_blank(),
    plot.margin = margin(c(.75, 1, .25, 1), unit = "cm")
  )


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w17", "tidytuesday_2023_w17")
ggsave(filename = glue::glue("{path}.pdf"), width = 14.5, height = 10, device = cairo_pdf)

# Additional annotations with Illustrator 
pdftools::pdf_convert(
  pdf = glue::glue("{path}_polished.pdf"),
  filenames = glue::glue("{path}_polished.png"),
  dpi = 300
)

