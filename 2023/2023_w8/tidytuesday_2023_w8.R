
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(furrr)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv") 
# Paintings downloads
paintings_dir <- here::here("2023", "2023_w8", "paintings")
if(!fs::dir_exists(paintings_dir)) { 
  fs::dir_create(paintings_dir)
  # Download paintings
  plan(multisession, workers = 4) 
  future_walk(df$img_src[1:400], \(img_src) { 
    download.file(url=img_src, destfile = here::here(paintings_dir, basename(img_src)))
  })
}

# Painting Colors Sorting with external code & Results in csv
# Feel free to reach me if you want to know how I do it 
images_positions <- read_csv(here::here("2023","2023_w8", "painting_positions.csv"))

# Rename pics with their indices in sorting data
images_positions |> 
  pwalk(\(images, indices) { 
    fs::file_move(here::here(paintings_dir, images), here::here(paintings_dir, paste0(indices, '.png') ))
    })

system(command = glue::glue("montage -mode Concatenate -tile 20x20 $(ls {paintings_dir}/*.png | sort -V)  {here::here('2023','2023_w8')}/montage.png"))
# Additional annotations with Illustrator
# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w8", "tidytuesday_2023_w8_polished")

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 300
)

