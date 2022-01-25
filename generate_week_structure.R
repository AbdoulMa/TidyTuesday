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
    
    # Create week script file  
    file_create(here(directory, script_file))
  
    # Generate Week script file from skeleton  
    week_skeleton <- whisker::whisker.render(readr::read_lines("week_skeleton.R"))
    readr::write_lines(week_skeleton, here(directory, script_file))
    
    # Create README file & write  skeleton 
    file_create(here(directory, "README.md"))
    readr::write_lines(paste0("![](",image_file,")"), here(directory, "README.md"))
    
    # Open Script 
    file.edit(here(directory, script_file))
  }
}

generate_tidytuesday_structure(04,2022)

