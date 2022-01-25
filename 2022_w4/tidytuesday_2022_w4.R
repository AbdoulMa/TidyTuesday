
# Load Libraries ----------------------------------------------------------


# Data Wrangling ----------------------------------------------------------


# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2022_w4", "tidytuesday_2022_w4")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

