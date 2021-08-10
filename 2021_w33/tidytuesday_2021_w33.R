# Load libraries ----------------------------------------------------------
library(tidyverse)
library(waffle)
library(ggtext)
library(gggrid)
# Data Reading and Wrangling ----------------------------------------------
investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')

main_investments <- investment %>%
  filter(!str_detect(meta_cat, "infrastructure")) %>% 
  mutate(meta_cat = str_wrap(str_to_upper(meta_cat), width = 20)) %>% 
  count(meta_cat, wt = gross_inv, sort = T)

main_categories <- main_investments %>% 
  pull(meta_cat)

main_categories
categories_investments <- main_investments  %>% 
  mutate(prop = MESS::round_percent(prop.table(n)),
         remaining_prop = 100-prop) %>% 
  pivot_longer(cols = c("prop", "remaining_prop"), names_to = "proportions", values_to = "prop")
categories_investments 

# Graphic -----------------------------------------------------------------
categories_investments %>% 
  mutate(meta_cat = fct_relevel(meta_cat, main_categories))%>% 
  ggplot(aes(fill = proportions, values = prop)) +
  geom_waffle(color = "white", size = .15, n_rows = 10, flip = T) + 
  scale_fill_manual(
    values = c(
      "prop" = "#009E73",
      "remaining_prop" = "grey90"),
    guide = "none"
  ) + 
  labs(
    title = "For every $100 invested, \nhow much is in key categories?",
    caption = "Data from ***Bureau of Economic Analysis***.<br>
       Tidytuesday Week-33 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) + 
  coord_equal(expand = F) + 
  facet_wrap(vars(meta_cat)) + 
  grid_panel(
    grob = function(data, coords) {
      if (data$PANEL[1] == 1) {
        gList(
          segmentsGrob(
            0.05, 0.3,
            unit( 0.05, "npc"),
            unit(0.4, "npc"),
            gp = gpar(
              col = "black",
              lwd = 0.5
            )
          ),
          segmentsGrob(
            0.05, 0.4,
            unit( 0.15, "npc"),
            unit(0.4, "npc"),
            gp = gpar(
              col = "black",
              lwd = 0.5
            )
          ),
          textGrob(
            label = "Each green square represents\n an investment of $1.",
            x = unit( .5, "npc"),
            y = unit(0.5, "npc"),
            just = c("center", "top"),
            gp = gpar(
              col = "black",
              fontfamily = "Lato Semibold",
              fontface = "italic",
              fontsize = 9.5
            )
          )
        )
      }
      else {
        nullGrob()
      }
    },
  ) + 
  theme_minimal(base_family = "Lato Semibold") + 
  theme(
    plot.title = element_text(family = "Lato Black",size = rel(2.5), hjust = .5, margin = margin(b = 10)),
    panel.border = element_rect(color = "black", size = 1.1, fill = NA),
    axis.text = element_blank(),
    strip.text = element_text(size= rel(1.1)),
    plot.caption = element_markdown(color = "black", size = rel(1.2), margin = margin(t = 20,b = 10)),
    plot.margin = margin(5, unit = "mm")
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w33/tidytuesday_2021_w33")
ggsave(glue::glue("{path}.pdf"), width = 9, height = 12, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 480)


