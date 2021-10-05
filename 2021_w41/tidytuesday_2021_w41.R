# Load libraries ----------------------------------------------------------
library(tidyverse)
library(geofacet)
library(ggtext)
library(biscale)
library(cowplot) 
library(gggrid)
# Data Reading and Wrangling ----------------------------------------------
nurses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')
states <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR")

states_populations <- read_csv("2021_w41/states_populations.csv") %>% 
  select(State, Pop)

us_state_grid1_with_pr <- us_state_grid1 %>% 
  add_row(row = 7, col = 10, code = "PR", name = "Puerto Rico")

glimpse(us_state_grid1_with_pr)

glimpse(nurses)

nurses_2020 <- nurses %>% 
  filter(Year == 2020) %>% 
  select(State, Total_nurses = `Total Employed RN`, hourly_wage = `Hourly Wage Avg`) %>% 
  left_join(states_populations, by = "State") %>%
  drop_na() %>% 
  mutate(nurse_pop_ration = (Total_nurses/ Pop)* 1000) %>% 
  # Join States for the abbreviations
  left_join(states) %>% 
  bi_class( x = nurse_pop_ration, y = hourly_wage, style = "quantile", dim = 3)

# Graphic -----------------------------------------------------------------
custom_pal <- bi_pal_manual(val_1_1 = "#A50026", val_1_2 = "#D73027", val_1_3 = "#F46D43",
                            val_2_1 = "#FEE08B", val_2_2 = "#FFFFBF",val_2_3 = "#D9EF8B",
                            val_3_1 = "#66BD63", val_3_2 = "#1A9850",val_3_3 = "#006837")

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Higher % Nurse/hab",
                    ylab = "Higher Hourly Wage",
                    size = 8)
(map <- nurses_2020 %>% 
  # filter(State == "California") %>% 
  ggplot() + 
  geom_rect(aes(fill = bi_class), xmin = -1, ymin = -1, xmax = 1, ymax = 1, color = "white") + 
  geom_richtext(aes(label = glue::glue("{Abbreviation}<br><br>${hourly_wage}"),color = ifelse(str_detect(bi_class,"1-1|1-2|3-3"), "white","black")),
                hjust = 0.5,
                size = 3,
                x = 0, y = 0.05,
                fill = NA, label.colour = NA, show.legend = F) + 
  coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
    scale_color_identity() + 
  bi_scale_fill(pal=custom_pal, dim =3, guide = "none") + 
  facet_geo(vars(State), grid = us_state_grid1_with_pr) + 
    grid_panel(
      grob = function(data, coords) {
        if (data$PANEL[1] == 76) {
          gList(
            segmentsGrob(
              1, 0.5,
              unit( 0.05, "npc"),
              unit(0.4, "npc"),
              gp = gpar(
                col = "black",
                lwd = 0.5
              )
            ),
            segmentsGrob(
              1.5, 0.5,
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
  theme_minimal() + 
  theme()
)

(finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.8, 0.2, 0.2)
)

paletteer::paletteer_d("RColorBrewer::RdYlGn") %>% 
  scales::show_col()
# Saving ------------------------------------------------------------------
