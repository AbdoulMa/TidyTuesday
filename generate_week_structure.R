# Inspired by (Georgios Karamanis) https://github.com/gkaramanis/tidytuesday/blob/master/newweek.R
library(purrr)
library(here)
library(fs)
library(lubridate)

generate_tidytuesday_structure <- function(week, year) {
  directory <- paste(year,paste0("w",week),sep = "_")
  script_file <- paste("tidytuesday", paste0(directory,".R"), sep = "_")
  image_file <- paste("tidytuesday", paste0(directory,".png"), sep = "_")
  if (!dir_exists(here(directory))) {
    dir_create(here(directory))
    
    # Create Script file & write  skeleton 
    file_create(here(directory, script_file))
    script_text <- paste0(
      '# Load libraries ----------------------------------------------------------\n',
      'library(tidyverse)',
      '\n\n',
      '# Data Reading and Wrangling ----------------------------------------------\n',
      '\n\n',
      '# Graphic -----------------------------------------------------------------\n',
      '\n\n',
      '# Saving ------------------------------------------------------------------'
    )
    write(as.character(script_text), file(here(directory, script_file)))
    
    # Create README file & write  skeleton 
    file_create(here(directory, "README.md"))
    write(as.character(paste0("![](",image_file,")")), file(here(directory, "README.md")))
    
    # Open Script 
    file.edit(here(directory, script_file))
  }
}

generate_tidytuesday_structure(32,2021)

