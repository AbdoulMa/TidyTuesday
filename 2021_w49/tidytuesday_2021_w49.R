# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)
library(patchwork)

# Data Reading and Wrangling ----------------------------------------------
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# Countries Flags Links
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

# Compute Teams Average Score for each year 
teams_scorings <- matches %>% 
  pivot_longer(c(team1, team2), names_to = "Teams_names", values_to = "Teams") %>%
  mutate(
    score = case_when(Teams_names == "team1" ~ score_team1,
                      TRUE ~ score_team2), 
    year = lubridate::year(parse_date(match_date, format = "%b %d, %Y"))
  )  %>% 
  select(Teams, score, year) %>% 
  group_by(Teams, year) %>% 
  summarise(mean_score = mean(score)) %>% 
  ungroup() 

# Retrieve most common teams
top_12 <- teams_scorings %>% 
  count(Teams) %>% 
  slice_max(order_by = n, n = 12) %>% 
  pull(Teams)

# Establish yearly ranking 
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
    fancy_strip = glue::glue("<img src='{flag_path}' width='45' height='45'><br>{str_to_upper(Teams)}"),
    fancy_strip = fct_inorder(fancy_strip)
  )

scorings_rankings_alt <- scorings_rankings %>% 
  mutate(Teams_alt = fancy_strip) %>% 
  select(-fancy_strip)


top_12_confrontations <- matches %>% 
  complete(team1, team2) %>% 
  mutate(is_played = !is.na(match_date)) %>% 
  rowwise() %>% 
  mutate(pair = sort(c(team1, team2)) %>% paste0(collapse = ",")) %>% 
  count(pair, wt = is_played, sort = T) %>% 
  separate(pair, into = c("Team1","Team2"), sep =",") %>% 
  filter(if_all(.cols =c(Team1, Team2),  ~ . %in% top_12))

# Graphic -----------------------------------------------------------------
# Yearly Rankings Plot 
(scoring_rk_plot <- ggplot() + 
   geom_line(data = scorings_rankings_alt,
             aes(year, rk, group = Teams_alt), size = 1.5, color = "grey65") + 
   geom_point(data = scorings_rankings_alt, aes(year, rk), fill = "white" , color = "grey65", shape = 21,size = 2, stroke = 1) + 
   geom_line(data = scorings_rankings, aes(year, rk, group = fancy_strip), size = 3.5, color = "#B10DC9") + 
   geom_point(data = scorings_rankings, aes(year, rk), size = 5, stroke = 2,shape = 21, color = "#B10DC9", fill = "white") + 
   labs(
     x = NULL,
     title = "International Cricket Council",
     subtitle = "<b>Ranking of the average scores of the best nations<br> between 1996 and 2005</b>"
   ) +  
   scale_y_reverse(
     name = NULL,
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
     panel.spacing.x = unit(0.5, "cm"),
     strip.text = element_markdown(size = rel(2), color = "white", family =  "Verlag", face = "bold"),
     strip.background = element_rect(fill = "#111111", color = NA),
     axis.text = element_text(size = rel(1.15),color = "#111111", family =  "Verlag")
   )
)

# Confrontations Heatmap
(confrontations_plot <- top_12_confrontations %>% 
    ggplot(aes(fct_rev(str_to_upper(Team1)), str_to_upper(Team2), fill = n)) + 
    geom_tile(height = 1, width = 1, color = "white") +
    # TODO , color = after_scale(prismat) After scale
    geom_text(data = filter(top_12_confrontations, Team1 != Team2), 
              aes(label = n,
                  color = after_scale(prismatic::best_contrast(fill))),
              size = 10, family = "Gotham Bold") + 
    labs(
      x = NULL,
      y = NULL, 
      
      subtitle = "<b>Most common confrontations</b><br>
      <i>
      <span>The India–Pakistan cricket rivalry took place 58 times over the period.</span><br>
      <span>**35** wins for Team **Pakistan**, **23** for Team **India**.</span>
      </i>"
    ) + 
    scale_x_discrete(position = "top") + 
    colorspace::scale_fill_continuous_sequential(
      palette = "RdPu",
      name = "nb of matches",
      breaks = seq(0,60, by = 10),
      guide = guide_coloursteps(
        barwidth = unit(15, "cm"),
        barheight = unit(1.5, "cm"),
        title.position = "top",
        title.hjust = .5
      )
    ) +
    coord_equal() + 
    theme_minimal() + 
    theme(
      axis.text = element_text(size = rel(2.25), family = "Verlag", face = "bold", color = "#111111"),
      axis.text.x = element_text(angle = 60, hjust = 0),
      legend.direction = "horizontal",
      legend.position = c(.35, .15),
      legend.title = element_text(size = rel(4.5), family = "Verlag"),
      legend.text = element_text(size = rel(3.5),  family = "Verlag"),
      legend.background = element_rect(fill = "#dcdcdc", color = NA),
      panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_line(color = "grey25", linetype = "dotted")
    )
)

# Combine Plots
(combine_plot <- (scoring_rk_plot / confrontations_plot) +
    plot_layout(
      ncol = 1,
      heights = c(.8,1)
    ) + 
    plot_annotation(
      caption = "Data from  from ESPN Cricinfo by way of Hassanasir.\n Tidytuesday Week-49 2021 · Abdoul ISSA BIDA."
    ) &
    theme(
      text = element_text(family = "Gotham Book", color = "#111111"),
      plot.title = element_markdown(size = rel(7.5), family = "Gotham Bold" , hjust = .5, margin = margin(t = 10, b = 5)),
      plot.subtitle = element_markdown(size = rel(4.5), family = "Mercury", margin = margin(t = 15, b = 15), hjust = .5, lineheight = 1.25),
      plot.background = element_rect(fill = "#dddddd", color = NA),    
      plot.margin = margin(t= 1, r = 1, b = 1, unit = "cm"),
      plot.caption = element_text(size = rel(2.5), hjust = .5, margin = margin(b = .5, unit = "cm"))
    ) 
) 

# Saving ------------------------------------------------------------------
path <- here::here("2021_w49", "tidytuesday_2021_w49")
ggsave(filename = glue::glue("{path}.pdf"), width = 27.5, height = 35.5, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 640
)
