# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)

# Data Wrangling ----------------------------------------------------------
chips <- readr::read_csv(here::here("2022_w34/chip_dataset.csv"))

daily_releases <- chips |> 
  mutate(
    release_date = `Release Date`,
    release_date = readr::parse_date(release_date, format = "%Y-%m-%d"),
    .before = 1L
  ) |> 
  count(release_date, Vendor, Type, wt = `Transistors (million)`, sort = T, name = "Total") |> 
  mutate(Vendor = fct_reorder(Vendor, Total, .fun = sum, .desc = T)) |>
  filter(Total > 0)

# Stylize text for fancy summary
stylize_vendor_text <- \(Vendor) {glue::glue("<span style='font-family: \"UEFA Supercup\"; font-weight:bold; font-size: 45px;'>{Vendor}</span>")}
stylize_summary_text <- \(transistors_total, Category_prop_GPU, Category_prop_CPU) {glue::glue("<span style='font-family: \"UEFA Supercup\";'><span style='font-weight:bold;font-size: 35px;'>{scales::label_comma(suffix = 'M')(transistors_total)}</span> <br> {Category_prop_GPU} {Category_prop_CPU}</span>")}

releases_summary <-  daily_releases |> 
  count(Vendor, Type, wt = Total, name = "Category_total") |> 
  group_by(Vendor) |> 
  mutate(
    Category_prop = prop.table(Category_total)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = Type, 
    values_from = c("Category_total", "Category_prop")
  ) |> 
  rowwise() |> 
  mutate(
    transistors_total = sum(Category_total_GPU,Category_total_GPU, na.rm = T), 
    Category_prop_GPU = ifelse(!is.na(Category_total_GPU), paste0(scales::percent(Category_prop_GPU),' of **GPU** <br>'), ''),
    Category_prop_CPU = ifelse(!is.na(Category_total_CPU), paste0(scales::percent(Category_prop_CPU),' of **CPU**'), ''),
    fancy_summary = glue::glue("{stylize_vendor_text(Vendor)}<br><br>{stylize_summary_text(transistors_total, Category_prop_GPU, Category_prop_CPU)}")
  )

# Graphic -----------------------------------------------------------------

# Define colors to vendors
main_vendors <- levels(daily_releases$Vendor)
vendors_colors <- setNames (c("#01d88b", "#ffc935", "#1e61c9", "#f36442", "#24bfff"), main_vendors)

# Releases dates range
dates_range <- range(daily_releases$release_date, na.rm = T)
start_date <- format(dates_range[1], "%m.%d.%Y" )
end_date <- format(dates_range[2], "%m.%d.%Y" )

subtitle <- "The number of transitions in a dense integrated circuit (IC)\n doubles approximately every two years, improving
   performance on both CPUs and GPUs."

# First plot: Scatterplot 
(daily_releases_plot <- daily_releases |> 
   ggplot(aes(release_date, Total)) + 
   geom_point(aes( shape = Type, fill = Vendor), color = "white", size = 2.5) + 
   geom_smooth(aes(color = Vendor), se = F) + 
   labs(
     subtitle = subtitle,
     x = "Release date",
     y = "Nb of releases\n in millions"
   ) + 
   facet_wrap(vars(Vendor), nrow = 1) + 
   scale_fill_manual(
     values = vendors_colors
   ) + 
   scale_color_manual(
     values = vendors_colors
   ) + 
   scale_shape_manual(
     values = c(
       "CPU" = 21,
       "GPU" = 22
     )
   ) +
   scale_y_continuous(
     trans = scales::log10_trans(),
     labels = scales::label_comma()
   ) + 
   coord_cartesian(clip = "off") + 
   theme_minimal() +
   theme(
     legend.position = "none",
     panel.grid.minor  = element_blank(),
     panel.grid.major = element_line(size = .2, color = "#111111", linetype = "dashed"),
     panel.background = element_rect(fill = "#FFFFFF", color = NA), 
     panel.spacing.x = unit(.5, "cm"),
     plot.subtitle = element_text(margin = margin(t= .75, b = .75, unit = "cm")),
     strip.text = element_text(size = rel(3), family = "UEFA Supercup", face = "bold", margin = margin(b = .75, unit ="cm")),
     axis.text = element_text(size = rel(1.125), family = "Inconsolata"),
     axis.title = element_text(size = rel(1.5), family = "UEFA Supercup"),
     axis.title.y = element_text(angle = 0, vjust = 1.1, margin = margin(r = -50, unit = "pt")),
     axis.ticks = element_line(size = unit(.25, "pt")),
     axis.ticks.length.x  =  unit(3, "pt"),
     axis.ticks.length.y  =  unit(5, "pt"),
     axis.line.x = element_line(size = unit(.25, "pt"))
   )
)

# Second plot: Releases Summary


overall_realeases <- scales::label_comma(suffix = " Millions")(sum(releases_summary$transistors_total))
(summary_plot <- releases_summary |> 
    ggplot() + 
    geom_rect(
      aes(fill = Vendor), 
      xmin = -1, ymin = -1, xmax = 1, ymax = 1, color = "white"
    ) + 
    ggtext::geom_richtext(aes(label = fancy_summary),
                          hjust = 0.5,
                          size = 5,
                          color = "white",
                          x = 0, y = 0.05,
                          lineheight = 1.5,
                          fontface = "bold",
                          fill = NA, label.colour = NA, show.legend = F) + 
    labs(
      subtitle = glue::glue("{overall_realeases} of chips have been released between {start_date} and {end_date} \n by the major manufacturers")
    ) + 
    coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
    scale_fill_manual(values = vendors_colors,
                      guide = "none") +
    theme_minimal() + 
    theme(
      legend.position = "none",
      panel.spacing.x = unit(-5,"points"),
      plot.subtitle = element_text( margin = margin(t= .5, unit = "cm")),
      strip.text = element_blank()) +
    facet_wrap(vars(Vendor), nrow = 1)
)

# Plot 
caption <- "Data from **CHIP Dataset** <br> Tidytuesday Week-34 2022 &bull;  Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**."
daily_releases_plot / summary_plot + 
  plot_annotation(
    title = "Chips Manufacturing",
    caption = caption
  ) &
  theme(
    plot.title = element_text(size = rel(3.5), family = "UEFA Supercup", face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 25, color = "#111111", family =  "NY Bold Italic", hjust = .5),
    plot.caption = ggtext::element_markdown(family = "UEFA Supercup", size = rel(1.5), hjust = .5),
    plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm")
  )

# Additional annotations with Illustrator
# Saving ------------------------------------------------------------------
path <- here::here("2022_w34", "tidytuesday_2022_w34")
height <- 12
ggsave(filename = glue::glue("{path}.png"), width = height*4/3, height = 12, device = ragg::agg_png, dpi = 300)

