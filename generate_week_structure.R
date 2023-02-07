# Inspired by (Georgios Karamanis) https://github.com/gkaramanis/tidytuesday/blob/master/newweek.R
library(purrr)
library(here)
library(fs)
library(lubridate)

generate_tidytuesday_structure <- function(week, year) {
  directory <- paste(year, paste0("w", week), sep = "_")
  script_file <- paste("tidytuesday", paste0(directory, ".R"), sep = "_")
  image_file <- paste("tidytuesday", paste0(directory, ".png"), sep = "_")
  # Create year dirextory if not exists
  if (!dir_exists(as.character(year))) {
    dir_create(as.character(year))
  }

  # Files paths
  week_directory_path <- here(year, directory)
  week_script_path <- here(week_directory_path, script_file)
  week_readme_path <- here(week_directory_path, "README.md")

  if (!dir_exists(week_directory_path)) {
    dir_create(week_directory_path)

    # Create week script file
    file_create(week_script_path)

    # Generate Week script file from skeleton
    week_skeleton <- whisker::whisker.render(readr::read_lines("week_skeleton.R"))
    readr::write_lines(week_skeleton, week_script_path)

    # Create README file & write skeleton
    file_create(week_readme_path)
    readr::write_lines(paste0("![](", image_file, ")"), week_readme_path)

    # Open Script
    file.edit(week_script_path)
  }
}

generate_tidytuesday_structure(6, 2023)
