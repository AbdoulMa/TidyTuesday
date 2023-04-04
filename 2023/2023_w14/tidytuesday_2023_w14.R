
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv")
teams_off <- df |> 
  select(matches("Team"), FTHG, FTAG) |> 
  pivot_longer(
    cols = ends_with("Team"), 
    names_to = "statut",
    values_to = "team", 
    names_pattern = "(.*)Team"
  ) |> 
  mutate(
    team_goal = ifelse(statut == "Home", FTHG, FTAG)
  ) |> 
  select(-starts_with("FT")) |> 
  count(team, wt = team_goal, sort = T, name = "gs")

teams_off <- teams_off |> 
  mutate(
    team = fct_reorder(team, desc(gs))
  ) 

teams_off <- teams_off |> 
  mutate(
    row_num = row_number(),
    fancy_title = glue::glue("{team}")
  )
# |> 
#   ggplot() + 
#   geom_col(aes(team, gs))

rect_mid <- function(width, x, xend, n ) {
  seq(x + (width/2), xend - (width/2), length.out = n)
} 

n <- nrow(teams_off)
l1 <- n 
rw1 <- l1 / n
l2 <- l1 - 1
rw2 <- l2 / n
x1 <- 0 
x2 <- x1 + 1/2 
y1 <- 0 
y2 <- y1 + ( 1 / (2 * tan(pi/3)))

#  TODO add height column for values
(coords_df <- tibble(
  rw1, 
  x1 = x1,
  x1_end =  x1 + l1, 
  rw2, 
  x2 =  x2,
  x2_end =  x2 + l2, 
  y1,
  y2,
  n, 
) |> 
    mutate(
      pt1_x = pmap(list(width= rw1, x = x1, xend = x1_end, n), rect_mid),
      pt2_x = pmap(list(width= rw2, x = x2, xend = x2_end, n), rect_mid)
    ) |> 
    unnest_longer(
      c(pt1_x, pt2_x)
    ) |> 
    mutate(
      group = as.factor(row_number()), 
      height = teams_off$gs /8.5
    )
)

margin <- 20

final_coords_df <- coords_df |> 
  mutate(
    ptx_1_borders = map2(pt1_x, rw1, \(x,y) {c(x - y/2 + y/margin, x + y/2 - y/margin)}), 
    ptx_2_borders = map2(pt2_x, rw2, \(x,y) {c(x + y/2 - y/margin, x - y/2 + y/margin)})
  ) |> 
  unnest_longer(c(ptx_1_borders, ptx_2_borders)) |> 
  pivot_longer(
    names_to = "pt_name",
    cols = c("ptx_1_borders", "ptx_2_borders"),
    values_to = "ptx"
  ) |> 
  mutate(
    pty = ifelse(pt_name == "ptx_1_borders", y1, y2)
  ) |>
  arrange(group, pt_name)


coords_df <- coords_df |> 
  mutate(
    row_num = row_number()
  ) |> 
  left_join(teams_off)


coords_df |> 
  ggplot() + 
  # geom_point(aes(pt1_x, y1)) + 
  # geom_point(aes(pt2_x, y2)) + 
  geom_rect(aes(xmin = pt1_x - rw1 / 2 + rw1 /margin, ymin = y1-1, xmax = pt1_x + rw1 / 2 - rw1 /margin, ymax = y1, fill = group))+ 
  geom_rect(aes(xmin = pt2_x - rw2 / 2 + rw2 /margin, ymin = y2, xmax = pt2_x + rw2 / 2 - rw2 /margin, ymax = y2 + height, fill = group))+ 
  geom_polygon(data = final_coords_df, aes(x = ptx, y = pty, group = group, fill = group), alpha = .65) +
  geom_richtext(aes(x = pt1_x, y = 0), label = "<img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/382.png' width='30'/>", vjust = 0) + 
  # textbox
  geom_textbox(aes(x = pt1_x  , y = -1, label = fancy_title), width = .035) + 
  # geom_segment(aes(x1, y1, xend = x1_end, yend = y1), stat = "unique") + 
  # geom_segment(aes(x2, y2, xend = x2_end, yend = y2), stat = "unique") + 
  # scale_y_continuous(limits = c(-1, 11)) + 
  coord_equal()

ggtext::geom_textbox()
# Graphic -----------------------------------------------------------------


# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w14", "tidytuesday_2023_w14")
ggsave(filename = glue::glue("{path}.pdf"), width = 9, height = 9, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 320
)

