# Packages loading --------------------------------------------------------
library(patchwork)
library(tidyverse)
library(ggsvg)
library(rvest)
library(ggsvg)
library(prismatic)

# Data scraped from ESPN & FBREF
names_matching_df <- read_csv("Soccer/names_matching.csv")
leagues_teams <- read_csv("Soccer/leagues_teams.csv")

# Ball svg
ball_svg <- '
<svg viewBox="0 0 194 194">
	<circle fill="#000000" cx="97" cy="97" r="97" />
	<path fill="#ffffff" d="m 94,9.2 a 88,88 0 0 0 -55,21.8 l 27,0 28,-14.4 0,-7.4 z m 6,0 0,7.4 28,14.4 27,0 a 88,88 0 0 0 -55,-21.8 z m -67.2,27.8 a 88,88 0 0 0 -20,34.2 l 16,27.6 23,-3.6 21,-36.2 -8.4,-22 -31.6,0 z m 96.8,0 -8.4,22 21,36.2 23,3.6 15.8,-27.4 a 88,88 0 0 0 -19.8,-34.4 l -31.6,0 z m -50,26 -20.2,35.2 17.8,30.8 39.6,0 17.8,-30.8 -20.2,-35.2 -34.8,0 z m -68.8,16.6 a 88,88 0 0 0 -1.8,17.4 88,88 0 0 0 10.4,41.4 l 7.4,-4.4 -1.4,-29 -14.6,-25.4 z m 172.4,0.2 -14.6,25.2 -1.4,29 7.4,4.4 a 88,88 0 0 0 10.4,-41.4 88,88 0 0 0 -1.8,-17.2 z m -106,57.2 -15.4,19 L 77.2,182.6 a 88,88 0 0 0 19.8,2.4 88,88 0 0 0 19.8,-2.4 l 15.4,-26.6 -15.4,-19 -39.6,0 z m -47.8,2.6 -7,4 A 88,88 0 0 0 68.8,180.4 l -14,-24.6 -25.4,-16.2 z m 135.2,0 -25.4,16.2 -14,24.4 a 88,88 0 0 0 46.4,-36.6 l -7,-4 z"/>
</svg>
'

# Match scraping ----------------------------------------------------------
game_plot <- function(game_id, home_color = NULL,
                      away_color = NULL, match_week = NULL) {
  match_page <- glue::glue(
    "https://fbref.com/en/matchs/{game_id}"
  ) |> 
  read_html()

  teams_shots <- match_page |>
    html_elements("div[data-controls=\"#switcher_shots\"] div a.sr_preset")

  home_team_abbr <- teams_shots[2] |> html_text2()
  away_team_abbr <- teams_shots[3] |> html_text2()

  teams_scoreboxes <- match_page |>
    html_element(".scorebox")

  match_meta <- match_page |>
    html_element(".scorebox_meta")
  match_date <- match_meta |>
    html_element(".venuetime") |>
    html_attr("data-venue-date")

  match_time <- match_meta |>
    html_element(".venuetime") |>
    html_attr("data-venue-time")

  match_extra_meta <- match_meta |>
    html_elements("div")

  match_day_info <- match_extra_meta |>
    pluck(2) |>
    html_text2()

  match_league <- str_extract(match_day_info, "(.*)\\s\\(.*\\)", 1)
  match_week <- match_week %||% str_extract(match_day_info, "(?:.*)\\s\\(Matchweek (\\d+)\\)", 1)

  is_venue_block <- match_extra_meta |>
    pluck(5) |>
    html_text2() |>
    str_detect("Venue")

  (match_stadium <- match_extra_meta |>
    # TODO: Fix
    pluck(ifelse(is_venue_block, 5, 6)) |> # 5 or 6 according to page
    html_elements("small") |>
    pluck(2) |>
    html_text2()
  )

  teams_logos <- teams_scoreboxes |>
    html_elements(".teamlogo") |>
    html_attr("src")

  teams_infos <- teams_scoreboxes |>
    html_elements("div > div > strong")

  home_team <- teams_infos[1] |> html_text2()
  away_team <- teams_infos[4] |> html_text2() # TODO: 3 /4

  # teams_logos
  teams_infos <- names_matching_df |>
    filter(fbref_short_name %in% c(home_team, away_team)) |>
    select(fbref_short_name, short_display_name = espn_short_name) |>
    left_join(leagues_teams)

  view(teams_infos)

  home_team_logo <- teams_infos$logo_link[teams_infos$fbref_short_name == home_team]
  away_team_logo <- teams_infos$logo_link[teams_infos$fbref_short_name == away_team]

  home_team_color <- teams_infos$color[teams_infos$fbref_short_name == home_team]
  away_team_color <- teams_infos$color[teams_infos$fbref_short_name == away_team]

  home_team_alt_color <- teams_infos$alternate_color[teams_infos$fbref_short_name == home_team]
  away_team_alt_color <- teams_infos$alternate_color[teams_infos$fbref_short_name == away_team]

  # Manual or automation colors selection

  home_team_color <- home_color %||% best_contrast("#FFFFFF", paste0("#", c(home_team_color, home_team_alt_color)))
  away_team_color <- away_color %||% best_contrast("#FFFFFF", paste0("#", c(away_team_color, away_team_alt_color)))

  team_scores <- match_page |>
    html_elements("div.scores > .score")

  home_score <- team_scores[1] |> html_text2()
  away_score <- team_scores[2] |> html_text2()

  match_data <- match_page |>
    html_element("#shots_all") |>
    html_table() |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names()

  found_sep <- FALSE
  for (index in seq_along(match_data$minute)) {
    # Half definition
    if (match_data$minute[index] == "") {
      found_sep <- TRUE
    }
    match_data$half[index] <- ifelse(!found_sep, "First", "Second")
  }


  match_data <- match_data |>
    filter(minute != "") |>
    separate_wider_delim(
      minute,
      delim = "+",
      names = c("minute", "extra"),
      too_few =  "align_start"
    ) |>
    select(1:5, outcome, half) |>
    mutate(across(.cols = c("minute", "extra", "x_g"), as.double, .names = "{.col}")) |>
    rowwise() |>
    mutate(
      minute = sum(minute, extra, na.rm = TRUE)
    ) |>
    select(-extra)



  # half time plot ----
  half_time_plot <- function(is_fh) {
    first_half <- match_data |>
      filter(half == ifelse(is_fh, "First", "Second"))
    starting_minute <- ifelse(is_fh, 0, 45)

    first_half <- first_half |>
      complete(
        minute = starting_minute:max(first_half$minute),
        # TODO: Be careful
        # nesting(
        squad = unique(match_data$squad) # ,
        # home_away = c("h","a")),
        # fill = list(x_g = 0)
      ) |>
      arrange(minute) |>
      mutate(
        minute_wa = ifelse(sum(is.na(outcome)) != 2, TRUE, FALSE), # minute with action
        .by = minute
      ) |>
      mutate(
        x_g = ifelse(minute_wa & is.na(x_g), 0, x_g),
        x_g = ifelse(minute == starting_minute, 0.0, x_g)
      ) |>
      mutate(
        home_away = ifelse(squad == home_team_abbr, "h", "a")
      ) |>
      arrange(minute, home_away) |>
      mutate(
        row_num = row_number(),
        .before = minute
      )


    rows_wa <- which(!is.na(first_half$x_g)) # rows with actions ids

    # https://stackoverflow.com/questions/40119425/how-to-find-points-by-linear-interpolation
    for (index in seq_along(first_half$x_g)) {
      if (is.na(first_half$x_g[index])) {
        na_row_num_inf <- max(rows_wa[which(rows_wa < index)])
        na_row_num_sup <- min(rows_wa[which(rows_wa > index)])


        if (sum(first_half$x_g[c(na_row_num_inf - 1, na_row_num_sup)]) == 0) { # away == 0
          x <- first_half$minute[c(na_row_num_inf, na_row_num_sup + 1)]
          y <- first_half$x_g[c(na_row_num_inf, na_row_num_sup + 1)]
          y_inter <- approx(x, y, xout = first_half$minute[index])
          first_half$x_g[index] <- ifelse(first_half$home_away[index] == "a", 0.0, y_inter$y)
        } else { # away != 0.0
          x <- first_half$minute[c(na_row_num_inf - 1, na_row_num_sup)]
          y <- first_half$x_g[c(na_row_num_inf - 1, na_row_num_sup)]
          y_inter <- approx(x, y, xout = first_half$minute[index])
          first_half$x_g[index] <- ifelse(first_half$home_away[index] == "h", 0.0, y_inter$y)
        }
      }
    }
    first_half <- first_half |>
      mutate(
        x_g = sum(x_g),
        .by = c(minute, squad)
      )

    goal_lines_df <- first_half |>
      filter(outcome == "Goal") |>
      mutate(
        player = str_wrap(player, width = 9),
        goal_line_st = ifelse(home_away == "h", x_g + 0.0015, -0.065 - x_g - 0.0015),
        goal_line_end = ifelse(home_away == "h", x_g + 0.0015 + 0.065, -0.065 - x_g - 0.0015 - 0.065),
      )

    minutes_range <- range(first_half$minute) + 1

    max_x_g <- max(match_data$x_g)

    # Plot --------------------------------------------------------------------
    ggplot() +
      geom_rect(data = filter(first_half, home_away == "h"), aes(xmin = minute, xmax = minute + 0.95, ymin = 0, ymax = x_g, fill = home_away)) +
      geom_rect(data = filter(first_half, home_away == "a"), aes(xmin = minute, xmax = minute + 0.95, ymin = -0.065, ymax = -0.065 - x_g, fill = home_away)) +
      geom_text(data = tibble(x = starting_minute + seq(0, 45, by = 15)), aes(x = x + 1.5, y = -0.065 / 2, label = paste0(x, ifelse(x - starting_minute == 45, "+", ""))), family = "Decima Mono", size = 2.5) +
      geom_segment(data = goal_lines_df, aes(x = minute + 0.5, xend = minute + 0.5, y = goal_line_st, yend = goal_line_end)) +
      geom_point_svg(
        data = goal_lines_df, aes(x = minute + 0.5, y = goal_line_end),
        svg = ball_svg,
        size = 3.5
      ) +
      ggrepel::geom_text_repel(
        data = goal_lines_df, aes(
          x = minute + 0.5,
          y = ifelse(home_away == "h", goal_line_end + 0.025, goal_line_end - 0.025),
          label = paste0(minute, "' - ", player, " · ", x_g, "xG "),

          # hjust = ifelse(home_away == "h", 1, 0),
          vjust = ifelse(home_away == "h", -0.5, 0),
        ), bg.color = "#FFFFFF",
        color = "#111111",
        bg.r = 0.15, size = 3, hjust = 0.5, family = "UEFA Supercup", fontface = "bold", min.segment.length = Inf
      ) +
      annotate(geom = "segment", x = minutes_range[1], xend = minutes_range[2], y = 0, yend = 0, linewidth = 0.5) +
      annotate(geom = "segment", x = minutes_range[1], xend = minutes_range[2], y = -0.065, yend = -0.065, linewidth = 0.5) +
      annotate(geom = "rect", xmin = minutes_range[1], xmax = minutes_range[2], ymin = 0, ymax = -0.065, fill = "#000000", alpha = 0.025) +
      guides(
        fill = "none"
      ) +
      scale_fill_manual(
        values = c(
          "h" = home_team_color,
          "a" = away_team_color
        )
      ) +
      scale_x_continuous(limits = c(starting_minute, max(first_half$minute) + 2), expand = expansion(mult = 0.01)) +
      scale_y_continuous(limits = c(-0.25 - 0.025 - max_x_g, 0.25 + max_x_g), expand = expansion(add = 0))
  }

  max_chars <- max(nchar(home_team), nchar(away_team))
  home_spaces <- paste0(rep("<span> </span>", max_chars - nchar(home_team)), collapse = "")
  away_spaces <- paste0(rep("<span> </span>", max_chars - nchar(away_team)), collapse = "")
  home_team <- paste0(home_spaces, home_team)
  away_team <- paste0(away_team, away_spaces)
  subtitle <- glue::glue("{match_league}/W{match_week} <br/>{match_date} - {match_time}<br/>{match_stadium}")
  (final_plot <- half_time_plot(TRUE) +
    half_time_plot(FALSE) +
    plot_annotation(
      title = glue::glue("**{toupper(home_team)} <span style=\"font-size: 45pt;color: {home_team_color};\">|</span> <img src='{home_team_logo}' width='35'/> {home_score} - {away_score} <img src='{away_team_logo}' width='35'/><span style=\"font-size: 45pt; color: {away_team_color};\">|</span> {toupper(away_team)}**<br/><br/><span style=\"font-size: 8pt;\">{subtitle}</span>"),
      caption = "Data from **Football Reference**<br/>**Match Momentum: 2023/24**<br/>Abdoul ISSA BIDA  <span style='font-family: \"Font Awesome 6 Brands\"'>&#xe61b;</span>**@issa_madjid** <span style='font-family: \"Font Awesome 6 Brands\"'>&#xf09b;</span>**github.com/abdoulma**."
    ) &
    theme_minimal() +
      theme(
        plot.title = ggtext::element_markdown(family = "UEFA Supercup", size = rel(1.85), lineheight = 0.75, hjust = 0.5, margin = margin(b = -0.5, unit = "cm")),
        plot.caption = ggtext::element_markdown(family = "UEFA Supercup", hjust = 0.5, size = rel(0.75), margin = margin(t = 0, b = 0.25, unit = "cm")),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#F5F7F9", color = NA),
        plot.margin = margin(c(0.5, 0.5, 0.25, 0.5), unit = "cm"),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
  )

  cowplot::ggdraw(final_plot) +
    cowplot::draw_label(x = 0.5, y = 0.4125, label = "HT", size = 20, fontfamily = "Decima Mono", fontface = "bold")
}

# Example Granada - Barca
game_plot(game_id =  "e2ce9278", home_color =NULL, away_color = "#ECBC0B")

ggsave("Soccer/granada_barca.png", width = 9.5, height  = 6.75, device = ragg::agg_png, dpi =  240)

