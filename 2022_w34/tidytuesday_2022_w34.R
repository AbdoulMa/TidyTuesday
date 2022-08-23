
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)


# Data Wrangling ----------------------------------------------------------
chips <- readr::read_csv("/home/abdoul-ma/Téléchargements/chip_dataset.csv")



# Graphic -----------------------------------------------------------------
glimpse(chips)

daily_releases <- chips |> 
  mutate(
    release_date = `Release Date`,
    release_date = readr::parse_date(release_date, format = "%Y-%m-%d"),
    .before = 1L
  ) |> 
  count(release_date, Vendor, Type, wt = `Transistors (million)`, sort = T, name = "Total") |> 
  mutate(Vendor = fct_reorder(Vendor, Total, .fun = sum, .desc = T)) |>
  filter(Total > 0)

dates_range <- range(daily_releases$release_date, na.rm = T)
start_date <- format(dates_range[1], "%m.%d.%Y" )
end_date <- format(dates_range[2], "%m.%d.%Y" )
  
daily_releases |> 
  ggplot(aes(release_date, Total)) + 
  geom_point(aes( shape = Type, fill = Vendor), color = "white", size = 2.5) + 
  geom_smooth(aes(color = Vendor), se = F) + 
  facet_wrap(vars(Vendor), nrow = 1) + 
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
    panel.grid.minor  = element_blank(),
    panel.grid.major = element_line(size = .2, color = "#111111", linetype = "dashed"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.spacing.x = unit(.5, "cm"),
    strip.text = element_text(size = rel(2.5), margin = margin(b = .5, unit ="cm")),
    axis.text = element_text(color = "#111111", size = rel(1.125)),
    axis.title.y = element_text(angle = 0, vjust = .5),
    axis.ticks = element_line(size = unit(.25, "pt")),
    axis.ticks.length.x  =  unit(3, "pt"),
    axis.ticks.length.y  =  unit(5, "pt"),
    axis.line.x = element_line(size = unit(.25, "pt"))
  )


# Second facet plot 
stylize_vendor_text <- \(Vendor) {glue::glue("<span style='font-weight:bold;font-size: 45px;'>{Vendor}</span>")}

stylize_summary_text <- \(transistors_total, Category_prop_GPU, Category_prop_CPU) {glue::glue("<span style='font-weight:bold;font-size: 35px;'>{scales::label_comma(suffix = 'M')(transistors_total)}</span> <br> {Category_prop_GPU} {Category_prop_CPU}</span>")}

daily_releases_summary <-  daily_releases |> 
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

overall_realeases <- scales::label_comma(suffix = " Millions")(sum(daily_releases_summary$transistors_total))
daily_releases_summary |> 
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
    title = glue::glue("{overall_realeases} of chips have been released between {start_date} and {end_date} \n by the major manufacturers")
  ) + 
  coord_fixed(xlim =c(-1,1), ylim = c(-1,1)) + 
  # scale_fill_manual(values = type_fillings, 
                    # guide = "none") + 
  theme_minimal() + 
  theme(panel.spacing.x = unit(-5,"points"),
        plot.title = element_text(size = 25, color = "black", hjust = .5),
        strip.text = element_blank()) +
  facet_wrap(vars(Vendor), nrow = 1)


# Saving ------------------------------------------------------------------
path <- here::here("2022_w34", "tidytuesday_2022_w34")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

