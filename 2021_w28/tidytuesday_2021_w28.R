library(tidyverse)
library(sf)
library(ggtext)


# Read Data & Wrangling ---------------------------------------------------
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

independences_adjusted <- holidays %>% 
  separate_rows(independence_from, sep = " and ") %>% 
  mutate(country = case_when(country == "Central African Republic
" ~ "Central African Rep.",
country == "Congo, Republic of the" ~ "Congo (Brazzaville)",
country == "Congo, Democratic Republic of the" ~ "Congo (Kinshasa)",
country == "Congo, Democratic Republic of the" ~ "Congo (Kinshasa)",
country == "Gambia, The" ~ "Gambia",
country == "Equatorial Guinea" ~ "Eq. Guinea",
country == "Central African Republic" ~ "Central African Rep.",
country == "Guinea-Bissau" ~ "Guinea Bissau",
country == "South Sudan" ~ "S. Sudan",
country == "Eswatini" ~ "Swaziland",
country == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
TRUE ~ country),
independence_from = case_when(
  country == "Cameroon" ~ "United Kingdom",
  independence_from == "Egypt and the United Kingdom" ~ "United Kingdom",
  independence_from == "the United Kingdom" ~ "United Kingdom",
  independence_from == "South Africa" ~ "United Kingdom",
  is.na(independence_from) ~ "Independents countries",
  TRUE ~ independence_from)
  )

# Read world Map and corvert it to SF
sf_world <- 
  st_as_sf(rworldmap::getMap(resolution = "low")) %>%
  st_transform(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")  %>% 
  dplyr::select(ISO_A3, NAME,continent)

# Filter and keep only african countries
african_countries <- sf_world %>% 
  filter(continent == "Africa")

# Join with the holidays dataset
african_countries_sf <- african_countries%>% 
  inner_join(independences_adjusted, by =c("NAME" = "country")) %>% 
  mutate(independence_from =  case_when(NAME == "W. Sahara" ~ "France",
                                        NAME == "Somaliland" ~ "United Kingdom",
                                        TRUE ~ independence_from),
         independence_from = fct_relevel(str_to_upper(independence_from), str_to_upper("Independents countries"), after = 0)
  ) 


# Graphic -----------------------------------------------------------------
subtitle <- "Despite having experienced brief periods of dependency, <b style = 'color: #2A363BFF;'>Egypt</b> and <b  style = 'color: #2A363BFF'>Ethiopia</b> are among the rare countries of the continent never to have known a total dependence vis-à-vis foreign powers.<br>
The <b style = 'color: #FF0000FF;'>American Colonization Society (ACS)</b> formed in 1817 to send free African-Americans to Africa as an alternative to emancipation in the United States, gave his independency to Liberia in 1847.<br>
<b style = 'color: #92D050FF;'>France</b> and the <b style = 'color: #7030A0FF'>United Kingdom</b> were the main colonizers. Some countries obtained their independence after armed struggles, this is notably the case of Algeria, whose independence was acquired <br>
following an armed struggle between the National Liberation Front and France.<br>
Other countries have built it through negotiation. This is particularly the case with Nigeria, which obtained it in 1960. However, most countries obtained it following grants from settlers.<br>
We can cite as an example the Republic of Benin (formerly Kingdom of Danhomè) which, like some of these neighbors of West Africa obtained it from the grant by France."
(plot <- african_countries_sf %>% 
  ggplot() + 
  geom_sf(data = african_countries, color = "black") +  
  geom_sf(aes(fill = independence_from), color = "black") + 
  facet_wrap(vars(independence_from)) + 
  labs(
    title = str_to_upper("How did the powers subdivide Africa during the colonial period ?"),
    subtitle = subtitle,
  caption = "Data from ***Wikipedia*** by the way of  ***Isabella Velasquez***.<br>
      Tidytuesday Week-28 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**" 
  ) + 
    scale_fill_manual(
      name = NULL, 
      values = c("#2A363BFF", as.character(paletteer::paletteer_d(palette = "Redmonder::qMSOStd"))), # "#2A363BFF" for independent countries 
      guide = "none"
    ) + 
  theme_minimal(base_family = "Lato Medium") + 
    theme(
      
      axis.text = element_blank(), 
      strip.text = element_text(face = "bold", size = rel(.9)),
      plot.title = element_text(color = "black",  family = "Lato Black", size = rel(1.5), hjust = .5, margin = margin(t = 15,b = 15)),
      plot.subtitle = element_markdown(color = "grey25", family = "Lato", face = "bold.italic", size = rel(.85), lineheight = 1.15, linewidth = 80), 
      plot.caption = element_markdown(color = "grey15", size = rel(0.8), margin = margin(t = 10,b = 10)),
      plot.margin = margin(r = 10, l = 0)
    )
)

# Saving 
png(here::here("2021_w28/tidytuesday_w28.png"), width = 12, height = 11, units = "in",res = 320, type = "cairo")
plot
dev.off()


# ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 28.
# The plot is  a map facet of  colonial authorities on african continent.
# Data comes from Wikipedia by the way of Isabella Velasquez.