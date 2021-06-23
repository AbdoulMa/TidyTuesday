library(tidyverse)
library(ggtext)


# Read Data & Wrangling ---------------------------------------------------
tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

# Summary of users with the most interactions
tweets_summary <- tweets %>% 
  group_by(username) %>% 
  summarise(across(retweet_count:quote_count, sum, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(
    total = retweet_count +  like_count + quote_count
  ) %>% 
  arrange(-total) %>% 
  head(8) %>% 
  mutate(
    colors = c("#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000") # Okabe-Ito Codes
  )


# Plot --------------------------------------------------------------------
# Plot function 
spiro_dubois <- function(interactions_df) {
  temp <- tweets_summary %>% 
    head(8) %>% 
    mutate(
      colors = c("#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000") # Okabe-Ito Codes
    )
  
  
  nb_lines  <- nrow(temp)
  decalage <- 3*nb_lines+2
  (temp <- temp %>% mutate(y_zero = (decalage+nb_lines) - row_number()))
  temp$name_and_odd_length <- nchar(temp$total) + nchar(temp$username)
  (max_length <- max(temp$name_and_odd_length))
  (temp <- temp %>% group_by(username) %>% 
      mutate(separator = paste(rep("_",3+(max_length-name_and_odd_length)),collapse=""),
             label = paste(username, separator, total)
      )
  )
  pente <- (min(temp$y_zero)-nb_lines) / max(temp$total)
  temp <- temp %>% mutate(y_final = y_zero - total*pente)
  positions <- temp %>% mutate(x_1=0, x_2=total, x_3=total, x_4=0,
                               y_1=y_zero, y_2=y_final, y_3=y_final+1, y_4=y_zero+1) %>%
    select(username,x_1:y_4) %>%
    pivot_longer(cols=-username,
                 names_sep="_",
                 names_to=c("coord","rang")) %>%
    pivot_wider(names_from = "coord", values_from=value)
  positions <- positions %>% left_join(temp %>% select(username,label,colors),by="username")
  n_recouvrement <- 2
  recouvrement <- max(temp$total)/n_recouvrement
  
  position_caption <- max(temp$y_zero)
  p <- ggplot(positions, aes(x = x, y = y, group=username)) +
    geom_polygon(aes(group=username, fill=I(colors)),color="black") +
    scale_y_continuous(expand=expansion(add=c(11,-5))) +
    scale_x_continuous(expand=expansion(add=c(0,-recouvrement))) +
    coord_polar() +
    geom_text(data = . %>% filter(rang==1),
              aes(label = paste(label,"   ",sep="")),
              adj=1, nudge_y=.5, nudge_x = -0000, color="black",size=3,family="Ubuntu Mono", fontface = "bold") +
    theme_void() +
    theme(legend.position="none",
          text = element_text(family="Lato Medium"),
          plot.title = element_text( family = "Lato Black", size = 18, margin =  margin(t = 10,b = 15), face = "bold", hjust = 0.5),
          plot.subtitle = element_text( family = "Lato Medium", face = "italic", hjust = 0.5),
          plot.caption = element_markdown(hjust = .95, size = 9.5, margin = margin(b = 10)),
          plot.background = element_rect(fill="#FFFFFF", color="#FFFFFF00"),  # fill="#e9d9c9aa", color="#FFFFFF00", fill = "#F0EFF1", colour = "#F0EFF1"
          plot.margin  = margin(c(10,0,0,0), unit = "mm")) + 
    labs(
      title = "Twitter Users with the most interactions\n during the 2021 #DuBoisChallenge",
      caption = "Data from *Anthony Starks, Allen Hillery & Sekou Tyler.*<br>
      Tidytuesday Week-25 2021 &bull; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span>**@issa_madjid**"
    )
  
  p
}

# Build graphic
spiro_dubois(tweets_summary)

# Save  graphic 
ggsave(here::here("2021_w25/tidytuesday_2021_w25.png"),width = 8, height = 8,dpi = 300, device = "png")

#  ALT TEXT 
# This graphic is  Abdoul ISSA BIDA submission for the  Tidytuesday Challenge for 2021 Week 25.
# The plot is  a reporduction of a famous WEB Dubois about “Assessed Values of Household and Kitchen Furniture Owned by Georgia Negroes”, 1900.
# Data comes from Anthony Starks, Allen Hillery Sekou Tyler and is an  aggregation of tweets from 2021 with #DuBoisChallenge.