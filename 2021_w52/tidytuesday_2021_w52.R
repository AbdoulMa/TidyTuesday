# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

cowplot::set_null_device("pdf")
cowplot::set_null_device("png")
cowplot::set_null_device("cairo")
cowplot::set_null_device("agg")

# Data Reading and Wrangling ----------------------------------------------
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

# Nutritions Average Summarize
nutritions_summary <- starbucks %>% 
   mutate(
      across(.cols = c(trans_fat_g, fiber_g), ~parse_number(.x), .names = "{.col}"),
      across(.cols =  calories:caffeine_mg, ~ (.x * 100) / serv_size_m_l, .names = "{.col}_per_100_ml")) %>% 
   select(product_name:serv_size_m_l, ends_with("per_100_ml")) %>%
   group_by(product_name) %>% 
   summarise(across(.cols = ends_with("per_100_ml"), mean, .names = "{.col}_avg"))


# Graphic -----------------------------------------------------------------
plot <- nutritions_summary %>% 
   mutate(
      fancy_product_name = str_replace_all(str_wrap(product_name, 25),'\n','<br>'), 
      fancy_title = glue::glue("{str_to_title(fancy_product_name)} <br> {round(total_fat_g_per_100_ml_avg)}g Fats · {round(total_carbs_g_per_100_ml_avg)}g Carbs · {round(fiber_g_per_100_ml_avg)}g Fibers")
   ) %>% 
   filter(cholesterol_mg_per_100_ml_avg + sodium_mg_per_100_ml_avg + caffeine_mg_per_100_ml_avg != 0) %>% 
   pivot_longer(
      cols = -c(product_name, fancy_product_name, fancy_title), 
      names_to = "nutriment", 
      values_to = "nutriment_val"
   ) %>% 
   filter(str_detect(nutriment,"cholesterol_mg_per_100_ml_avg|sodium_mg_per_100_ml_avg|caffeine_mg_per_100_ml_avg")) %>% 
   mutate(nutriment =fct_relevel(nutriment, c("caffeine_mg_per_100_ml_avg","sodium_mg_per_100_ml_avg","cholesterol_mg_per_100_ml_avg"))) %>%
   arrange(nutriment) %>% 
   ggplot() + 
   annotate(geom = "rect", xmin = .5, xmax = 1.5 , ymin = 0, ymax = 1, size = 2.5, fill = NA,color = "white") + 
   geom_col(aes(x = 1, y = nutriment_val, fill = nutriment), width = 1, position = "fill") +
   annotate(geom = "segment", x = 1.5, xend = 2.05 , y = .25, yend = .25, size = 4.5, color = "white") + 
   facet_wrap(vars(fancy_title), ncol = 9) + 
   scale_x_continuous(
      expand = expansion(mult = 0)) +
   scale_fill_manual(
      values = c("caffeine_mg_per_100_ml_avg" = "#03161D",
                 "sodium_mg_per_100_ml_avg" = "#FDAC07",
                 "cholesterol_mg_per_100_ml_avg" = "#EF6101")
   ) + 
   coord_polar(theta = "y") + 
   theme_minimal() + 
   theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.spacing.x = unit(.25, "cm"),
      panel.spacing.y = unit(.05, "cm"),
      strip.text = element_markdown(family = "Verlag", face = "bold", size = 4.05, color = "white")
   )

# Extra Informations Plotting
cowplot::ggdraw(plot) +
   labs(title = "Starbucks", 
        subtitle = "Micro-nutritional information* (<span style='color:#03161D;'>Caffeine</span>, <span style='color:#EF6101;'>Cholesterol</span> and <span style='color:#FDAC07;'>Sodium</span>) of drinks") + 
   # Caption with draw_label
   cowplot::draw_label(x = .9, y = .075, label = "*Average value for 100 ml of beverage \nData from Official Starbucks Nutritional dataset\n with @StarTrek_Lt Contribution.\n Tidytuesday Week-52 2021 · Abdoul ISSA BIDA.",
                       fontfamily = "Verlag", fontface = "bold", color = "white", hjust = 1, vjust = 1, size = 8) + 
   theme(
      text = element_text(color = "white"),
      plot.title = element_text(family = "Verlag", size = rel(5), face = "bold", margin = margin(t = .25, b = .25, unit = 'cm')),
      plot.subtitle = element_markdown(family = "Mercury Display", size = rel(1.15), face = "bold", margin = margin(t = .125, b = .125, unit = "cm")),
      plot.background = element_rect(fill = "#00704A", color = NA),
      plot.margin = margin(t = .25,r=0, b = .25, l=0, unit = "cm")
   )


# Saving ------------------------------------------------------------------
path <- here::here("2021_w52", "tidytuesday_2021_w52")
ggsave(filename = glue::glue("{path}.png"), width = 9, height = 12, device = ragg::agg_png, dpi = 320)
