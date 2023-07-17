
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
detectors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv")

detectors_acc_df <- detectors |> 
  filter(!is.na(native)) |>
  summarise(
    accuracy = sum(kind == .pred_class) / n(),
    .by = c(detector, native)
  ) |> 
  arrange(detector)
  
# Graphic -----------------------------------------------------------------

best_no_native_detecors <- detectors_acc_df |>  
  filter(native == "No") |>  
  arrange(desc(accuracy)) |> 
  pull(detector)

title <- "**Me**: Hey TidyTuesdayGPT, can you visualize me detectors accuracies 
according to  wether essay was written by a native English writer or not ?"
 
subtitle <- "**TIdytuesdayGPT**: For sure, here is a pretty quick visualization of detectors accuracies bias according
writers english fluency."
detectors_acc_df |>   
  mutate(
    detector = fct_relevel(detector, best_no_native_detecors)
  ) |>   
  ggplot() + 
  geom_col(aes(native, accuracy, fill = native), position = "dodge")  + 
  labs(
    title = title, 
    subtitle =subtitle
  ) + 
  facet_wrap(vars(detector), nrow = 1) + 
  theme_minimal() + 
  theme(
    panel.spacing.x = unit(1, "cm"), 
    plot.title = element_markdown(hjust = 0.5, margin = margin(t = 0.5,b = 0.5, unit = "cm")),
    plot.subtitle = element_markdown(hjust = 0.5, margin= margin(b = 0.75, unit = "cm"))
  )


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w29", "tidytuesday_2023_w29")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, dpi = 300, device = ragg::agg_png)
