
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(glue)
library(gt)

# Read & Data  Wrangling --------------------------------------------------
summary_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
challenges_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/challenges.csv')
castaways_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

# States Table
states <- tibble(
  state = state.name, 
  state_abb = state.abb
)

# Castaways  
castaways_df <- castaways_df %>%
  arrange(season) %>% 
  distinct(season_name,season, full_name, castaway, age,.keep_all = T)

# Candidates Challenges Wins Table
goats_df <- challenges_df %>% 
  group_by(season_name, winners, challenge_type) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(challenge_type) %>% 
  pivot_wider(names_from = "challenge_type", values_from = "n") %>% 
  arrange(-immunity, -reward)

# Retrieve Season Year
summary_df <- summary_df %>% 
  mutate(year = year(premiered)) %>% 
  arrange(year)

# Concatenation Function
concatenate <- function(words) {
  words <- as.character(words)
  words <- unique(words)
  if (length(words) > 1) {
    str_c(c(str_c(head(words,length(words)-1), collapse = '<span style="font-size:10px;color:grey">, </span>'), last(words)), collapse = ' <span style = "font-size:10px;color:grey;">and</span> ')
  }
  else {
    words
  }
}

goats_final_df <- goats_df %>% 
  left_join(castaways_df, by = c("season_name", "winners" = "castaway")) %>% 
  left_join(states) %>% 
  left_join(select(summary_df, season_name,year)) %>% 
  mutate(city = glue('{city} <span style= "font-size:8px;font-style:italic;">· {state_abb}</span>'),
         season_name = glue('<span style="font-size:14px"><span style="font-weight:bold;">{season_name}</span> <span style= "font-size:9px;font-style:italic;">· {year}</span></span>'))  %>% 
  select(season_name:personality_type, -state) %>%
  filter(!is.na(full_name)) %>%
  mutate(season_name = str_remove(season_name, "Survivor: ")) %>%
  group_by(full_name,winners) %>%
  arrange(age) %>% 
  summarise(
    across(immunity:reward, sum, na.rm =T, .names = "{.col}_wins"),
    across(c(season_name,age:personality_type), list(collapse = ~concatenate(.x)), .names = "{.col}s"),
  ) %>%
  ungroup() %>%
  arrange(-immunity_wins, -reward_wins) %>% 
  mutate(
    rk = paste0("#",row_number())
  )



# Table -------------------------------------------------------------------
pkg_logo <- "https://camo.githubusercontent.com/535fee5e6b44ba2757ffb9ff97c73c358f94d6619dfb5bfb67ad171614841007/687474703a2f2f6772616469656e7464657363656e64696e672e636f6d2f77702d636f6e74656e742f75706c6f6164732f323032302f31312f6865782d746f7263682e706e67"
fire_emo <- "https://images.emojiterra.com/google/android-11/512px/1f525.png"
reward_emo <- "https://emojigraph.org/media/microsoft/trophy_1f3c6.png"  

# Define a bio from several columns
combine_bio <- function(full_name, aka, person_type, cities, ages) {
  glue::glue('<div style = "color:black;"><span style= "font-size:14px;line-height:15px;"><span style= "font-weight: bold; ">{full_name}</span> <span style= "font-size:9px;color:grey">aka</span> <span style = "font-size:11px;font-weight: 350;font-style:italic;">{aka}</span></span> <br/>
  <span style= "font-size:9px; line-height:.9;"><span style="font-size:8px;color:grey">personality type </span>{person_type}</span><br>
  <span style= "font-size:9px; line-height:.9;"><span style="font-size:8px;color:grey">participated at </span>{ages}<span style = "font-size:10px;color:grey;"> yo</span></span><br>
  <span style= "font-size:9px; line-height:.9;"><span style="font-size:8px;color:grey">lived in </span>{cities}</span><br>
  </div>')
}

n_rows <- 20
goats_tab <- goats_final_df %>% 
  head(n_rows) %>% 
  mutate(
    combo = combine_bio(full_name, winners, personality_types,citys, ages), 
    combo = map(combo, gt::html),
    season_names = map(season_names, gt::html)
  ) %>% 
  select(rk,combo, season_names,everything(), -full_name, -winners, -citys, -ages, -personality_types) %>% 
  gt() %>% 
  cols_label(
    rk = "",
    combo = "",
    season_names = "PARTICIPATIONS",
    immunity_wins = "IMMUNITY",
    reward_wins = "REWARD"
  ) %>% 
  data_color(
    columns = c(immunity_wins,reward_wins),
    colors = scales::col_numeric(
      palette = rev(rcartocolor::carto_pal(n = 7, name = "ag_GrnYl")),
      domain = NULL
    )
  ) %>% 
  text_transform(
    locations =  cells_body(c(immunity_wins)),
    fn = function(x){
      paste(x,glue('<img width = 15, style = "vertical-align: middle;" src = "{fire_emo}"/>'))
    }
  ) %>%
  text_transform(
    locations =  cells_body(c(reward_wins)),
    fn = function(x){
      paste(x,glue('<img width = 15, style = "vertical-align: middle;" src = "{reward_emo}"/>'))
    }
  ) %>%
  tab_spanner(
    label = "WINS",
    columns = c(immunity_wins, reward_wins)
  ) %>% 
  cols_align(
    align = "left",
    columns = c(combo)
  ) %>% 
  cols_width(
    rk ~ px(50),
    combo ~ px(300),
    immunity_wins ~ px(75),
    reward_wins ~ px(75)
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        font = "Inconsolata",
        align = "center",
        weight = "700"
      ) 
    ), 
    locations = list(
      cells_body(c("rk","immunity_wins","reward_wins"))
      
    )
  ) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", color = "transparent", weight = px(1)),
    locations = cells_body(
      rows = everything()
    )
  ) %>%
  tab_style(
    style =  list(cell_text(
      font = "Inconsolata", 
      weight = "bold",
      align = "center"
    )
    ),
    locations = list(cells_column_spanners(spanners = "WINS"),cells_column_labels(columns = c("season_names","immunity_wins","reward_wins")))
  ) %>% 
  tab_style(
    style =  cell_text(
      size = "x-small",
    ),
    locations = cells_column_labels(columns = c("immunity_wins","reward_wins"))
  ) %>% 
  tab_header(
    title = md(glue('<img src="{pkg_logo}" width = 55> ')),
    subtitle = "SURVIVOR GREATESTS OF ALL TIME"
  ) %>% 
  # Source note
  tab_source_note(
    source_note = md("**Table: Abdoul ISSA BIDA inspired by Cédric Scherer & Thomas Mock   &bull; Data:  *survivorR* R package**<span style='font-size:7pt;color:grey;'> Images Credits : EmojiTerra & Emojigraph </span>")
  ) %>% 
  # Subtitle
  tab_style(
    style = cell_text(
      font = "Lato Black",
      weight = "bold",
      size = px(25)
    ),
    locations = cells_title(groups = c("subtitle"))
  ) %>%
  # Last row bottom
  tab_style(
    style = cell_borders(sides = "bottom", color = "transparent", weight = px(3)),
    locations = cells_body(
      rows = n_rows
    )
  ) %>% 
  opt_table_font(font = list("Lato Light","Lato Black","Lato")) %>%
  opt_row_striping() %>% 
  tab_options(
    heading.align = "center",
    heading.border.bottom.width = px(3),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.width = px(3),
    table.border.top.color =  "transparent",
    table.border.bottom.color =  "transparent",
    table.border.bottom.width =  px(3),
    table.background.color = "#f9fbfc",
    table_body.hlines.color = "#ededed",
    data_row.padding = px(4),
    row.striping.background_color = "#f2f2f2",
    table.width = px(950)
  )

# Save the table
gtsave(goats_tab,here::here("2021_w23/tidytuesday_2021_w23.png"), zoom = 1)
