
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
# Data Wrangling ----------------------------------------------------------

cran <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv")

cran <- cran %>% 
  distinct(package, .keep_all = T) %>% 
  mutate(datetime = readr::parse_datetime(date, format = "%Y-%m-%d %H:%M:%S UTC")) %>% 
  drop_na(datetime) %>% 
  arrange(datetime) %>% 
  mutate(
    date = lubridate::date(datetime),
    year = lubridate::year(date),
    year_month = zoo::as.yearmon(date)) %>% 
  filter(year_month >= zoo::as.yearmon(as.Date("2010-01-01")), year_month < zoo::as.yearmon(as.Date("2021-01-01"))) 


nrow(cran) %>% 
  count(date, sort = T)
cran_steps <- cran %>% 
  count(year_month, year)

monthly_avg <- mean(cran_steps$n)
x_limits <- c(head(cran_steps$year_month,1),tail(cran_steps$year_month,1))
y_limits <- c(head(cran_steps$n,1),tail(cran_steps$n,1))

cran_areaStep <- bind_rows(
  old = cran_steps, 
  new = mutate(cran_steps, n = lag(n)),
  .id = "source") %>% 
  arrange(year_month, source)

# https://gist.github.com/Teebusch/db0ab76d31fd31a13ccf93afa7d77df5
# cran_steps <- cran_steps %>% 
#   mutate(n = ifelse(year_month %in% zoo::as.yearmon(c("Jan 2010","Dec 2020")), 0, n))




# Graphic -----------------------------------------------------------------
(decade_plot <- ggplot() + 
   geom_hline(yintercept = monthly_avg, linetype = "dashed", size = .5) +
   geom_step(data = cran_steps, aes(x = year_month, y = n)) +
   geom_ribbon(data = cran_areaStep, aes(x = year_month, ymin = 0, ymax = n),
               alpha = .75
   ) +
   annotate(geom = "text", x = 2010 , y = monthly_avg + 7.5, label = glue::glue("Monthly average: {round(monthly_avg, 0)} packages"), 
            color = "#111111", size = 3.5, hjust = 0) +
   annotate(geom = "segment", x = x_limits, xend = x_limits, y = 0, yend = y_limits) + 
   annotate(geom = "label", x = 2017, y = 240, hjust= 0, 
            label = "Between 2010 and 2020,\n 14,514 new packages\n were released on CRAN.",
            label.r = unit(0,'cm'),
            label.padding = unit(.5,'cm'),
            size = 5) + 
   labs(title = "Evolution of CRAN packages releases", 
        subtitle = "Monthly Releases") +
   scale_x_continuous(
     breaks = 2010:2020
   ) + 
   coord_cartesian(expand = F, clip = "off") + 
   theme_minimal() + 
   theme(
     axis.ticks.length.x = unit(.125,"cm"),
     axis.ticks.x = element_line(),
     axis.title = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_blank()
   )
)

(yearly_plot <- cran %>% 
    count(date, year) %>% 
    filter(n >= 2) %>%
    mutate(year_day = lubridate::yday(date), 
           height = case_when(n < 5 ~ 1, 
                              n < 10 ~ 2.5,
                              n < 25 ~ 5,
                              n < 50 ~ 15,
                              TRUE ~ 20)) %>% 
    ggplot(aes(x = year_day, y = year)) +
    geom_rect(aes(xmin = year_day-height/2, xmax = year_day+height/2, ymin = year, ymax = year +height/20,
    ),size = .125, color = "black",  alpha = .75) + 
    annotate(geom = "text", x = lubridate::yday("2011-09-19"), y = 2011 + .5, label = "The first version of {scales} 0.1.0\n (one of my favorite pakages).", lineheight = .75 ) + 
    annotate(geom = "segment", x = lubridate::yday("2011-09-19"), xend = lubridate::yday("2011-09-19"), y = 2011 + .25, yend =  2011 + .1) + 
    annotate(geom = "text", x = lubridate::yday("2012-10-29"), y = 2012 + .65, label = "192 packages were released\n on 29th, October 2021.", lineheight = .75) + 
    annotate(geom = "text", x = lubridate::yday("2014-01-29"), y = 2014 + .5, label = "{dplyr} 0.1.1\n by Hadley Wickham.", lineheight = .75 ) + 
    annotate(geom = "segment", x = lubridate::yday("2011-01-29"), xend = lubridate::yday("2011-01-29"), y = 2014 + .3, yend =  2014 + .1) + 
    annotate(geom = "text", x = lubridate::yday("2016-09-09"), y = 2016 + .65, label = "{tidyverse} realesed, \n and the wizard starts.", lineheight = .75) + 
    annotate(geom = "segment", x = lubridate::yday("2016-09-09"), xend = lubridate::yday("2016-09-09"), y = 2016 + .4, yend =  2016 + .2) + 
    annotate(geom = "text", x = lubridate::yday("2017-04-17"), y = 2017 + .65, label = "{glue} by Jennifer Bryan,\n one of the most downloaded package.", lineheight = .75) + 
    annotate(geom = "segment", x = lubridate::yday("2017-04-17"), xend = lubridate::yday("2017-04-17"), y = 2017 + .4, yend =  2017 + .2) + 
    annotate(geom = "text", x = lubridate::yday("2018-01-18"), y = 2018 + .65, label = "{fs} by Gábor Csárdi,\n for file system operations.", lineheight = .75) + 
    annotate(geom = "segment", x = lubridate::yday("2018-01-18"), xend = lubridate::yday("2018-01-18"), y = 2018 + .4, yend =  2018 + .2) +
    labs(
      subtitle = "Daily Releases",
      caption = "Higher square means more releases<br>
      Data from **Robert Flight**<br>
Tidytuesday Week-10 2022 &bull;Abdoul ISSA BIDA"
    ) + 
    scale_x_continuous(
      breaks = lubridate::yday(as.Date(paste0(1:12,'-1'), format = "%m-%d")),
      labels = str_to_upper(month.name),
      expand = expansion(mult = 0)
    ) + 
    scale_y_continuous(
      breaks = 2010:2020, 
      expand = expansion(mult = c(.05,0))
    ) + 
    coord_fixed(ratio = 20, clip = "off") + 
    theme_minimal() + 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linetype = "dotted", color = "#111111", size = .25), 
      axis.title = element_blank()
    )
)

decade_plot / yearly_plot + 
  plot_layout(heights = c(.6, 1)) & 
  theme(
    plot.title = element_text(size = rel(3)),
    plot.subtitle = element_text(hjust = 0, size = rel(1.75), margin = margin(t = .5, unit = "cm")),
    plot.caption = ggtext::element_markdown(size = rel(1.5))
  )

# Saving ------------------------------------------------------------------
path <- here::here("2022_w11", "tidytuesday_2022_w11")
ggsave(filename = glue::glue("{path}.png"), width = 12, height = 15, device = ragg::agg_png, dpi = 300)

