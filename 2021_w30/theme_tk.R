options(warn=-1)
library(tibble)
library(extrafont)
library(scales)
loadfonts()

theme_foundation <- function(base_size=12, base_family="") {
  thm <- theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA),
              legend.background = element_rect(colour = NA),
              line = element_line(colour = "black"),
              rect = element_rect(fill = "white", colour = "black"),
              text = element_text(colour = "black"))
}

colors <- tibble::tribble(
  ~name, ~value,
  "Dark", "#2A2E45",
  "Light", "#F3F6F7",
  "Medium", "#D1D2DB",
  "Blue", "#3F88C5",
  "Yellow", "#EAC435",
  "Red", "#DF2935"
)


theme_tk <- function(base_size = 12, base_family = "Lato") {
  colors <- deframe(colors)
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(
      line = element_line(colour = "Dark"),
      rect = element_rect(fill = colors["Light"],
                          linetype = 0, colour = NA),
      text = element_text(colour = colors["Dark"]),
      axis.title = element_blank(),
      axis.text = element_text(family="Roboto Mono"),
      axis.ticks = element_blank(),
      # axis.line = element_blank(),
      legend.background = element_rect(),
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.grid = element_line(colour = NULL),
      panel.grid.major =
        element_line(colour = colors["Medium"]),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect(),
      plot.title = element_text(hjust = 0, size = rel(1.5),
                                family="Lato"),
      plot.caption = element_text(family="Lato"),
      legend.title = element_text(family="Lato"),
      legend.text = element_text(family="Lato"),
      legend.position="bottom"
    ))
}


theme_set(theme_tk())

tk_pal <- function() {
  colors <- deframe(colors)
  values <- unname(colors[c("Blue", "Yellow", "Red")])
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

scale_colour_tk <- function(...) {
  discrete_scale("colour", "tk", tk_pal(), ...)
}

scale_color_tk <- scale_colour_tk

scale_fill_tk <- function(...) {
  discrete_scale("fill", "tk", tk_pal(), ...)
}
