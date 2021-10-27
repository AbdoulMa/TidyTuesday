# Load libraries ----------------------------------------------------------
library(tidyverse)
library(countrycode)
library(ggtext)
library(patchwork)

# TODO I will comment the code on 10/27/2021 
# Data Reading and Wrangling ----------------------------------------------
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


# Compute count ries nb of ultra runners by gender 
ultra_representations <- ultra_rankings %>% 
  distinct(runner,.keep_all = T) %>% 
  filter(!is.na(gender)) %>% 
  count(nationality, gender) %>% 
  pivot_wider(names_from = gender, values_from = n ) %>% 
  drop_na() %>% 
  mutate(sum = M + W) %>% 
  slice_max(order_by = sum, n= 15) %>% 
  mutate(across(c(M, W), .fns = list(prop = ~ .x /sum), .names = "{.fn}_{.col}"))


#  Retrieve country full name from  countrycode library 
ultra_representations <- ultra_representations %>% 
  mutate(country_name = countrycode(nationality, origin = 'iso3c', destination = 'country.name'), 
         country_name = case_when(nationality == "GER" ~ "Germany", 
                                  str_detect(country_name, "Hong\\sKong") ~ "Hong Kong", 
                                  TRUE ~ country_name)
  )


# Compute Km running time for finishers by gender 
ultra_rankings_speed <- ultra_rankings %>% 
  left_join(race) %>% 
  filter(!is.na(gender),!is.na(time_in_seconds), !is.na(distance), distance != 0) %>% 
  mutate(mile_sp = time_in_seconds / distance )

# Graphic -----------------------------------------------------------------

# Define background color
bg_color <- "#F7F7F7"

summary <- "USA, France and United Kingdom have the highest proportions of world's ultra runners. But, when we take the number of runners from the country compared to its population France is largely at top.
For parity between women and men, serious progress must be made for accessibility for women."
# Wrap the summary
summary <- str_wrap(summary, 50) %>% str_replace_all("\n","<br>")

# Representations Plot
(countries_rep_plot  <- ultra_representations %>% 
    ggplot() + 
    geom_col(aes(x = sum, y = fct_reorder(nationality, sum)), fill = alpha("#FFA900", .55)) + 
    geom_text(aes(x = 500, y = fct_reorder(nationality, sum), label = country_name),
              hjust = 0,
              family = "Gotham Black",
              size = 5.5) + 
    labs( 
      x = NULL,
      y = NULL,
      subtitle = "TOP 15 Countries with the most ultra Trail Runners") + 
    annotate(geom = "richtext", x = 12000, y = 6, label = summary, 
             fill = bg_color, label.color = NA,
             size = 4.5,
             family = "Mercury", 
             fontface = "bold",
             color  = "#111111"
    ) + 
    coord_cartesian() + 
    scale_x_continuous(
      expand = expansion(add = 0),
      position = "top", 
      breaks = 5000*1:4,
      labels =  scales::comma_format(decimal.mark = ".",big.mark = ",")
    ) + 
    theme_minimal() + 
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(1.15),color = "#111111"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(size = .75)
    )
)

# Gender Distribution Plot
(countries_gender_plot <- ultra_representations %>% 
    mutate(nationality = fct_reorder(nationality, prop_W)) %>% 
    pivot_longer(cols = c(prop_M, prop_W), names_to = "prop", values_to = "gender_proportion") %>% 
    mutate(prop = fct_relevel(prop, c("prop_W", "prop_M"))) %>% 
    ggplot(aes(x = gender_proportion, fill = prop, y = nationality)) + 
    geom_col(show.legend = F) + 
    geom_text(aes(x = ifelse(prop == "prop_M", 0.01, 0.99),label = glue::glue("{round(gender_proportion, 2)*100}%"), group =prop, hjust = ifelse(prop == "prop_M", 0, 1)), 
              size = 4.5,
              family = "Gotham Black",
              color = "white") + 
    labs(subtitle = 'For 100 runners from a country,<br> how many are <span style="color:#52006A;">**Men**</span> or <span style="color:#CD113B;">**Women**</span>?') + 
    scale_x_continuous(
      breaks = .25*1:3,
      labels = ~ paste0(. *100, "%"),
      position = "top"
    ) + 
    scale_fill_manual(
      values = c(prop_M = "#52006A", prop_W = "#CD113B")
    ) +
    coord_cartesian(expand = F) + 
    theme_minimal() + 
    theme(
      axis.title = element_blank(),
      axis.text = element_text(face = "bold",size = rel(1.15)),
      axis.text.y = element_text(color = "#111111"),
      axis.text.x = element_text(color = "grey25")
    )
) 

men_summary  <- "<span>MEN</span><br> 96,502 finishers<br> 12,63mins/Km on average."
women_summary  <- "<span>WOMEN</span><br> 16,993 finishers<br> 12,16mins/Km on average."

# Finishers Km running time Distibution Plot 
(finisher_plot <- ultra_rankings_speed %>% 
    ggplot() +
    geom_histogram(aes(x = mile_sp,fill = gender), binwidth = 60,color = "white", alpha = .85, show.legend = F) + 
    scale_x_time(
      name = "Kilometer time (each bar is one minute)",
      labels = scales::time_format("%M mins"), 
      breaks = seq(5,30, by = 5)*60
    ) + 
    annotate(geom = "richtext", x = 1350, y = 14000, 
             color = "#52006A",
             size = 6.5,
             family = "Mercury",
             fontface = "bold",
             fill = bg_color, label.color = NA, label = men_summary) + 
    annotate(geom = "richtext", x = 1350, y = 10000, 
             color = "#CD113B",
             size = 6.5,
             family = "Mercury",
             fontface = "bold",
             fill = bg_color, label.color = NA, label = women_summary) + 
    labs(
      y = NULL,
      subtitle = "Distribution of kilometer pace by gender"
    ) + 
    coord_cartesian(expand = F) + 
    scale_fill_manual(
      values =c(M = "#52006A", W = "#CD113B")
    ) + 
    scale_y_continuous(
      # labels = scales::comma_format(decimal.mark = ".",big.mark = ","),
      breaks = seq(2.5, 12.5, 2.5)*1000,
      labels = function(x) {
        format_x <- scales::comma(x, decimal.mark = ".",big.mark = ",")
        ifelse(x == 12500, paste0(format_x,"\n finishers"), format_x)
        }
    ) + 
    theme_minimal() + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(size =.5),
      axis.text = element_text(color = "#111111", size =rel(1.15)),
      axis.ticks = element_line(size =.5), 
      axis.ticks.length = unit(.25, "cm")
    )
) 

# Combine the 3 plots
(countries_rep_plot + countries_gender_plot ) / finisher_plot  + 
  plot_annotation(title = str_to_upper("- Ultra Trail Running -"), 
                  caption = "Data from from Benjamin Nowak by way of ITRA.\n Tidytuesday Week-44 2021 · Abdoul ISSA BIDA.") & 
  theme(
    text = element_text(family = "Gotham Bold", color = "#111111"),
    plot.background = element_rect(fill = bg_color, color = NA),
    plot.title = element_text(size = rel(2.5),hjust = .5, margin = margin(t = 15, b = 15), family = "Mercury", color = "black",  face = "bold"),
    plot.subtitle = element_markdown(hjust = .5, color = "grey5", size = rel(1.55),margin = margin(t = 15, b = 10)), 
    plot.caption = element_text( size = rel(.95), family = "Gotham Medium", margin = margin(t = 15, b = 10, r = 25)),
    plot.margin = margin(t = 15,r = 20, b = 15, l = 20)
  )

# Saving ------------------------------------------------------------------
path <- here::here("2021_w44", "tidytuesday_2021_w44")
ggsave(filename = glue::glue("{path}.pdf"), width = 15, height = 16, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  dpi = 640
)
