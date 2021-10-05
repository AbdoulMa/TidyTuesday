# Load libraries ----------------------------------------------------------
library(tidyverse)
library(geofacet)
library(ggtext)
library(biscale)
library(cowplot) 
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

(legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Higher % Nurse/ hab",
                    ylab = "Higher Hourly Wage",
                    size = 8) + 
  bi_theme(base_family = "Lato Black") + 
  theme(
        rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 8.5,
                                    color = "#66BD63"),
        axis.title.y = element_text(size = 8.5,
                                    color = "#F46D43"))
)

(map <- nurses_2020 %>% 
  # filter(State == "California") %>% 
  ggplot() + 
  geom_rect(aes(fill = bi_class), xmin = -1, ymin = -1, xmax = 1, ymax = 1, color = "white") + 
  geom_richtext(aes(label = glue::glue("<span style='font-size: 15px;font-family: \"Lato Black\";'>{Abbreviation}</span><br><br>${hourly_wage}"),color = ifelse(str_detect(bi_class,"1-1|1-2|3-3"), "white","black")),
                hjust = 0.5,
                size = 3,
                x = 0, y = 0.05,
                fill = NA, label.colour = NA, show.legend = F) + 
  coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
    scale_color_identity() + 
  bi_scale_fill(pal=custom_pal, dim =3, guide = "none") + 
  facet_geo(vars(State), grid = us_state_grid1_with_pr) + 
  labs(
    title = "Nursing personnel",
    subtitle = "A bivariate map showing nurses density and hourly wage in each states",
    caption = "Data from ***Data.World*** <br>Tidytuesday Week-41 2021 &bull;<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
  ) + 
  theme_minimal() + 
  theme(
    panel.spacing = unit(-5,"points"),
    strip.text = element_blank(),
    plot.title = element_markdown(hjust = .5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = .5, margin = margin(b = 25)),
    plot.caption = element_markdown(color = "black", size = rel(.95), family = "Lato", margin = margin(t = 15,b = 5), hjust = 1)
  )
)

(finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.7, 0.2, 0.2)
)

label_pr <- ggplot() + 
  annotate("text", x = 0, y = 0, label = "In Puerto Rico, we have 1 registred \n nurse for 1000 habitants") + 
  coord_cartesian(clip = "off")

label_pr
map +
  inset_element(legend, 0.1, 0.8, 0.3, 1) + 
  inset_element(label_pr, 0.7, 0.0, 1,0.1 )

# Saving ------------------------------------------------------------------
