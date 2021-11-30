# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
# Data Reading and Wrangling ----------------------------------------------
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

countries_flags <- tribble(
  ~country, ~flag_path, 
  "England", "https://a.espncdn.com/i/teamlogos/cricket/500/1.png",
  "India", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/cricket/500/6.png",
  "New Zealand","https://a1.espncdn.com/combiner/i?img=/i/teamlogos/cricket/500/5.png",
  "Pakistan", "https://a.espncdn.com/i/teamlogos/cricket/500/7.png",
  "South Africa", "https://a.espncdn.com/i/teamlogos/cricket/500/3.png",
  "Sri Lanka", "https://a.espncdn.com/i/teamlogos/cricket/500/8.png",
  "West Indies", "https://a.espncdn.com/i/teamlogos/cricket/500/4.png",
  "Zimbabwe", "https://a.espncdn.com/i/teamlogos/countries/500/zim.png",
  "Australia", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/cricket/500/2.png",
  "Bangladesh",  "https://a.espncdn.com/i/teamlogos/cricket/500/25.png",
  "Kenya", "https://a.espncdn.com/i/teamlogos/countries/500/ken.png",
  "Netherlands", "https://a.espncdn.com/i/teamlogos/countries/500/ned.png"
)

teams_scorings <- matches %>% 
  pivot_longer(c(team1, team2), names_to = "Teams_names", values_to = "Teams") %>%
  mutate(
    score = case_when(Teams_names == "team1" ~ score_team1,
                      TRUE ~ score_team2
    ), 
    year = lubridate::year(parse_date(match_date, format = "%b %d, %Y"))
  )  %>% 
  select(Teams, score, year) %>% 
  group_by(Teams, year) %>% 
  summarise(mean_score = mean(score)) %>% 
  ungroup() 

top_12 <- teams_scorings %>% 
  count(Teams) %>% 
  slice_max(order_by = n, n = 12) %>% 
  pull(Teams)

scorings_rankings <- teams_scorings %>% 
  drop_na() %>% 
  filter(Teams %in% top_12) %>% 
  complete(Teams, year, fill = list(mean_score = 0)) %>% 
  group_by(year) %>% 
  mutate(
    rk = rank(desc(mean_score),ties.method = "first")
  ) %>% 
  ungroup() %>% 
  left_join(countries_flags, by = c("Teams" = "country")) %>% 
  arrange(Teams)  %>% 
  mutate(
    fancy_strip = glue::glue("<img src='{flag_path}' width='25' height='25'><br>{Teams}"),
    fancy_strip = fct_inorder(fancy_strip)
  )

scorings_rankings_alt <- scorings_rankings %>% 
  mutate(Teams_alt = fancy_strip) %>% 
  select(-fancy_strip)

# https://html-color.codes/grey
top_12_confrontations <- matches %>% 
  complete(team1, team2) %>% 
  mutate(is_played = !is.na(match_date)) %>% 
  rowwise() %>% 
  mutate(pair = sort(c(team1, team2)) %>% paste0(collapse = ",")) %>% 
  count(pair, wt = is_played, sort = T) %>% 
  separate(pair, into = c("Team1","Team2"), sep =",") %>% 
  filter(if_all(.cols =c(Team1, Team2),  ~ . %in% top_12))



# Graphic -----------------------------------------------------------------
ggplot() + 
  geom_line(data = scorings_rankings_alt,
            aes(year, rk, group = Teams_alt), size = 1.5, color = "grey65") + 
  geom_point(data = scorings_rankings_alt, aes(year, rk), fill = "white" , color = "grey65", shape = 21,size = 2, stroke = 1) + 
  geom_line(data = scorings_rankings, aes(year, rk, group = fancy_strip), size = 2.5, color = "#B10DC9") + 
  geom_point(data = scorings_rankings, aes(year, rk), size = 4, stroke = 1.75,shape = 21, color = "#B10DC9", fill = "white") + 
  scale_y_reverse(
    breaks = 1:12
  ) +
  scale_x_continuous(
    breaks = 1996:2005,
    position = "top"
  ) + 
  facet_wrap(vars(fancy_strip), strip.position = "bottom") + 
  coord_cartesian() + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#dcdcdc", color = NA),
    panel.spacing.x = unit(1, "cm"),
    strip.text = element_markdown()
  )

top_12_confrontations %>% 
  ggplot(aes(fct_rev(Team1), Team2, fill = n)) + 
  geom_tile(height = 1, width = 1, color = "white") +
  # TODO , color = after_scale(prismat) After scale
  geom_text(data = filter(top_12_confrontations, Team1 != Team2), 
            aes(label = n,
                color = after_scale(prismatic::best_contrast(fill)))) + 
  scale_x_discrete(position = "top") + 
  colorspace::scale_fill_continuous_sequential(
    palette = "RdPu",
    name = "Nb Matches",
    breaks = seq(0,60, by = 10),
    guide = guide_coloursteps(
      barwidth = unit(5, "cm"),
      title.position = "top",
      title.hjust = .5
    )
  ) +
  coord_equal() + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 0),
    legend.direction = "horizontal",
    legend.position = c(.35, .15),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank()
  )

# Saving ------------------------------------------------------------------
