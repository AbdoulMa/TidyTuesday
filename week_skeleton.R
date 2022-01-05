
# Load Libraries ----------------------------------------------------------


# Data Wrangling ----------------------------------------------------------


# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("{{year}}_w{{week}}", "tidytuesday_{{year}}_w{{week}}")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

