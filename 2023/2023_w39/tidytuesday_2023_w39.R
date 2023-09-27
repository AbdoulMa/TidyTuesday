
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
rk_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv")

rk_df <- rk_df |> 
  mutate(
    ep = paste0(ifelse(Episode == 1, "Ep/", ""), Episode),
    season = paste0("Season ", Season),
    ep = fct_inorder(ep)
         ) |> 
  select(season, ep, count = F_count_RK)

max(rk_df$count)


# Graphic -----------------------------------------------------------------
(plot <- rk_df |> 
  rowwise() |> 
  mutate(
    fancy_label = paste0(rep("F*CK", count), collapse = "\n")
  ) |> 
  ggplot() + 
  geom_text(aes(label = fancy_label, color = count), x= 0, y = 0, fontface = "bold", vjust = 0, size = 3.75, lineheight = 1) + 
  labs(title = "Roy Kent F*cking Counts", 
       caption = "") + 
  facet_grid(season ~ ep, switch = "y", scales = "free_y") +
  scale_x_continuous(
    expand = expansion(mult = c(1.5, .35))
  ) + 
  viridis::scale_color_viridis(option = "viridis",discrete = FALSE, direction = -1, 
                               guide= "none") + 
  # scale_y_continuous(limits = c(0, 25)) + 
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(
    text = element_text(color = "#FFFFFF"),
    panel.spacing.y = unit(1.5, "cm"),
    plot.title =element_text(size = rel(2.75), face = "bold", hjust = 0.5, margin = margin(b = 1, unit = "cm")),
    strip.text = element_text(color = "#FFFFFF", size = rel(1.125)),
    strip.text.y.left = element_text(angle = 0, face = "bold", vjust = 0.0325, size = rel(1.375)),
    plot.background = element_rect(fill = "#010101", colour = NA)
  )
)

roy_image <-here::here("2023/2023_w39/roy_coach.png")
cowplot::ggdraw(plot) + 
  cowplot::draw_image(roy_image, x = 0.05, y = 1, hjust = 0, vjust = 1, width = 0.2, height = 0.3) +
  cowplot::draw_label(x = 0.15, y = 0.775, vjust = 1, label = "\"We are in a shit fucking mood\nbecause we never fucking win\n and it sucks fucking shit.\"", 
                      fontface = "italic", size = 12.5, color = "#FFFFFF")  
  
# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w39", "tidytuesday_2023_w39")
ggsave(filename = glue::glue("{path}.png"), width = 11.5, height = 13.5, device = ragg::agg_png)

