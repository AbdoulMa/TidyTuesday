
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggtext)

# Data Wrangling ----------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv")
clubs <- read_csv(here::here("2023/2023_w14/clubs.csv")) # Dataset with club main color 

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
  ) |> 
  left_join(clubs, by = c("team" = "club_name"))
  
teams_off
teams_off <- teams_off |> 
  mutate(
    row_num = row_number(),
    fancy_title = glue::glue("{team}")
  )

# Compute polygons coords
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

coords_df <- coords_df |> 
  mutate(
    row_num = row_number()
  ) |> 
  left_join(teams_off) |> 
  mutate(
    logo = glue::glue("<img src='{logo_link}' width='35'/>")
  )

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

# Graphic -----------------------------------------------------------------
gs_note_y <- coords_df$y2[1] +coords_df$height[1] + .125
coords_df |> 
  ggplot(aes(fill = hex_code)) + 
  # geom_point(aes(pt1_x, y1)) + 
  # geom_point(aes(pt2_x, y2)) + 
  geom_rect(aes(xmin = pt1_x - rw1 / 2 + rw1 /margin, ymin = y1-1, xmax = pt1_x + rw1 / 2 - rw1 /margin, ymax = y1))+ 
  geom_rect(aes(xmin = pt2_x - rw2 / 2 + rw2 /margin, ymin = y2, xmax = pt2_x + rw2 / 2 - rw2 /margin, ymax = y2 + height))+ 
  geom_polygon(data = final_coords_df, aes(x = ptx, y = pty, group = group), alpha = .45) +
  geom_richtext(aes(x = pt1_x, y = 0, label = logo), 
                fill = NA,
                label.color = NA,
                vjust = 0) + 
  geom_text(aes(x = pt2_x  , y = y2 + 1, label = fancy_title, 
                color = after_scale(prismatic::best_contrast(fill))), 
            angle = 90, family = "Ve Black", 
            size = 6,
            hjust = 0,
            ) + 
  geom_text(
      aes(x = pt2_x, y = y2 + height, label = gs),
      color = "white",
      family = "DecimaMonoPro",
      size = 5,
      vjust = -.5
  ) + 
  annotate(GeomSegment, x = -.25, xend = 0.45, y = gs_note_y, yend = gs_note_y, color = "white", linetype = "dashed") +
  annotate(GeomSegment, x = -.25, xend = -.25, y = gs_note_y, yend = gs_note_y - .5, color = "white", linetype = "dashed") +
  annotate(GeomText, x = -.25, y = gs_note_y - 1, label = "Goals\n scored", family = "DecimaMonoPro", color = "white", size =  5) +
  annotate(GeomText, x = 20, y = gs_note_y, label = "Premier League\nAttacks Standing", 
           color = "white", family = "Go Bold",
           size = 12.5,hjust = 1, vjust = 1) +
  labs(
    caption = "Tidytuesday Week-14 2023<br> Abdoul ISSA BIDA <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**<br>
      Data from **Evan Gower**"
  ) + 
  coord_equal(expand = F, clip = "off") + 
  scale_fill_identity() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_markdown(family = "Go Book", color = "white", size = rel(1.125)),
    plot.background = element_rect(fill = "#111111", color = NA),
    plot.margin = margin(c(1, .5, .25, 1), unit = "cm"),
  )

# Saving ------------------------------------------------------------------
path <- here::here("2023", "2023_w14", "tidytuesday_2023_w14")
ggsave(filename = glue::glue("{path}.png"), width = 12, height = 9, device = ragg::agg_png, dpi = 144)
