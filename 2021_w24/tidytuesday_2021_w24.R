
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(gggrid) # remotes::install_github("pmur002/gggrid")
library(packcircles)
library(ggtext)
library(cowplot)

# Read & Data Wrangling ---------------------------------------------------
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

#  Compute the proportion by lake for each decade
lakes_records <- fishing %>% 
  filter(str_detect(region, "Total", negate = T) ) %>%  
  filter(!is.na(values)) %>%  
  count(year, lake, wt = values) %>% 
  mutate(
    decade = year %/% 10,
    decade = paste0(decade *10,"s")
  ) %>% 
  count(decade, lake, wt = n) %>% 
  group_by(decade) %>% 
  mutate(
    proportion = n / sum(n),
    proportion = round(proportion*100)
  ) %>% 
  ungroup()

# Build a circles df for packing layout 
decade_summary <- lakes_records %>% 
  group_by(decade) %>% 
  count(wt = proportion) %>% 
  ungroup()

# Define points coordinates and radius 
lakes_points <- decade_summary %>% pmap_df(
  .f = ~circleProgressiveLayout(rep(0.5, ..2))
) %>% 
  mutate(decade = rep(lakes_records$decade, lakes_records$proportion),# appropriate decade for each circle
         lake = rep(lakes_records$lake, lakes_records$proportion) # appropriate lake for each circle
  )


# Graphic -----------------------------------------------------------------
# Graphical parameter settings for grids annotations
my_gpar <- gpar(
  col = "black",
  fontfamily = "Lato Semibold",
  fontsize = 7
)

# Plot
(plot <-  lakes_points%>% 
    ggplot(aes(x, y, fill = lake)) + 
    geom_point(
      size = 4,
      pch = 21
      
    ) +
    grid_panel(
      grob = function(data, coords) {
        if (data$PANEL[1] == 1) {
          .x <- min(coords$x) + 0.085
          .y <- min(coords$y) + 0.085
          gList(
            segmentsGrob(
              .x, .y,
              unit(.x/2, "npc"),
              unit(.y/2, "npc"),
              gp = gpar(
                col = "black",
                lwd = 0.5
              )
            ),
            textGrob(
              label = "Each  circle represents\n1% of fish collected.",
              x = unit(.x/2.5, "npc"),
              y = unit(.y/2.5, "npc"),
              just = c("left", "top"),
              gp = my_gpar
            )
          )
        }
        else if (data$PANEL[1] == 3) {
          gList(
            textGrob(
              label = "In 1880s, approximatively 171,913 \n thousand pounds (71%) of fish\n were caught from Herie Lake.",
              x = unit(.75, "npc"),
              y = unit(1., "npc"),
              just = c("left", "top"),
              gp = my_gpar
            )
          )
        }
        else if (data$PANEL[1] == 6) {
          gList(
            textGrob(
              label = "The Great Lakes Storm of 1913\n had severly impacted the Lake Huron fishing\n ecosystem and production.",
              x = unit(.3, "npc"),
              y = unit(1., "npc"),
              just = c("right", "top"),
              gp = my_gpar
            )
          )
        }
        
        else if (data$PANEL[1] == 11) {
          gList(
            textGrob(
              label = "Dr. Howard Tanner, the  fisheries\nchief  for the state of Michigan\nre-introduced the salmon in 1966.",
              x = unit(.3, "npc"),
              y = unit(1., "npc"),
              just = c("right", "top"),
              gp = my_gpar
            )
          )
        }
        else if (data$PANEL[1] == 13) {
          gList(
            textGrob(
              label = "The extinction of Blue Pike, approximatively in 1983\n impacted Lake Erie fishing industry but relatively little\n compared to the proportion of fish collected.",
              x = unit(.5, "npc"),
              y = unit(0., "npc"),
              just = c("left", "top"),
              gp = my_gpar
            )
          )
        } else {
          nullGrob()
        }
      }
    )+ 
    labs(
      title =  str_to_upper("Proportions of fish caught by lake through decades"),
      fill = "Lakes",
      caption= "Data from *Great Lakes Database.*<br>
      Tidytuesday Week-24 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**",  
      x = NULL,
      y = NULL 
    ) + 
    scale_x_continuous(
      limits = c(-5,5)
    ) + 
    scale_y_continuous(
      limits = c(-5,5)
    ) + 
    scale_fill_manual(
      values = c("#E69F00","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7") # colorblind_pal() 
    )+ 
    guides(fill=guide_legend(
      label.position = 'bottom', 
      title.position = 'top', 
      keywidth=.55,
      keyheight=.15,
      default.unit="inch", 
      title.hjust = .5,
      title.vjust = 0,
      label.vjust = 3,
      nrow = 1)) + 
    coord_equal( clip = "off") + 
    facet_wrap(~decade) + 
    theme_minimal(base_family = "Lato") +
    theme (
      panel.spacing  = unit( 1.1, unit = "cm"),
      legend.spacing.x = unit(0, 'cm'),
      legend.title=element_text(family = "Lato Black",size = rel(1.3)),
      legend.text = element_text(size = rel(0.8)),
      legend.margin=margin(-10,0,-1,0),
      legend.position = 'bottom',
      legend.box.margin=margin(15,0,15,0),
      strip.text = element_text(family = "Lato Black", size = rel(1.1)),
      plot.title  = element_text(family = "Lato Black", size = rel(1.3), hjust = .5, margin = margin(t = 10, b= 10)),
      plot.caption = element_markdown(color = "grey15", size = rel(0.8)),
      strip.placement = "outside",
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
)  

# Saving ------------------------------------------------------------------
path <- here::here("2021_w24/tidytuesday_2021_w24")
ggsave(glue::glue("{path}.pdf"), width = 10, height = 13, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 600)

# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 24.
# The plot is  a facet of proportions of fish catch by lake for each decade since 1860s.
# Each circle represents 1% of catch and each color represents a specific lake.
# Some annotations explain the variations observed due to many reasons.
# Data comes from Great Lakes Database which reports the commercial fish catch data from 1867 to 2015.