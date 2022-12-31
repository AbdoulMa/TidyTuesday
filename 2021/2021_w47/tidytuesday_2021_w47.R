# Load libraries ----------------------------------------------------------
library(tidyverse)
library(cowplot)
# Data Reading and Wrangling ----------------------------------------------
# Not Classic Tidytuesday
df <- tibble(
  year = c("1860", "1870", "1878", "1884", "1888", "1891", "1897"), 
  number = c(7, 10351, 72655, 110150, 120533, 156836, 180565)
)

# Graphic -----------------------------------------------------------------

caption <- "Replication W. E. B. Du Bois Data Portraits \n#BlkTIDES #BlkNData · Abdoul ISSA BIDA"
(plot <- df %>% 
  ggplot(aes(x = year)) +
  geom_bar(aes( y = -number), stat = "identity", width = 0.5, fill = "#2B055A") + 
  geom_text(aes(y = -(number + 5500), label = scales::comma(number)), family = "Gotham Bold", size = 5) + 
  scale_x_discrete(position = "top") + 
  scale_y_continuous(
    expand = expansion(mult = c(.05,0.0125))
  ) + 
  labs(title = "NEGRO CHILDREN ENROLLED\n IN THE PUBLIC SCHOOLS.") +
    
  theme(
    text = element_text(color = "#111111"),
    axis.text = element_text(family = "Gotham Bold", size = rel(1.5)),
    plot.title = element_text(family = "Gotham Bold",hjust = .5, size = rel(3), margin = margin(b = 25)),
    axis.text.y = element_blank(),
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    
    panel.background = element_rect(fill = "#F3F5F7", color = NA),
    plot.background = element_rect(fill = "#F3F5F7", color = NA),
    plot.margin = margin(t = 35)
  )
)

ggdraw(plot) + 
  draw_label(x = .075, y = .075, label = caption, hjust = 0, fontfamily= "Gotham Bold" ) + 
  theme(
    plot.background = element_rect(fill = "#F3F5F7", color = NA), 
    plot.margin = margin(t = 15)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w47", "tidytuesday_2021_w47")
ggsave(filename = glue::glue("{path}.pdf"), width = 12, height = 12.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)
