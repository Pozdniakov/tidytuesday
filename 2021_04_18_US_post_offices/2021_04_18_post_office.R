library(tidyverse)
library(gganimate)  

postal <- readr::read_csv("https://raw.githubusercontent.com/cblevins/us-post-offices/main/us-post-offices-random-coords.csv", 
                          guess_max = 100000)

skimr::skim(postal)

postal %>%
  janitor::get_dupes(ID)

postal %>%
  group_by(is.na(Discontinued)) %>%
  summarise(mean(Established, na.rm = TRUE))

postal %>%
  mutate(Discontinued = if_else(nchar(Discontinued) > 4, as.numeric(str_sub(Discontinued, 1, 4)), Discontinued)) %>%
  mutate(Established = if_else(nchar(Established) == 3, as.numeric(str_c(Established, "0")), Established)) %>%
  mutate(Discontinued = if_else(is.na(Discontinued), 2002, Discontinued)) %>%
  count(Discontinued > 2010)

postal_clean <- postal %>%
  mutate(Discontinued = if_else(nchar(Discontinued) > 4, as.numeric(str_sub(Discontinued, 1, 4)), Discontinued)) %>%
  mutate(Established = if_else(nchar(Established) == 3, as.numeric(str_c(Established, "0")), Established)) %>%
  mutate(Discontinued = if_else(is.na(Discontinued), 2002, Discontinued)) %>%
  filter(Discontinued > 1500 & Discontinued < 2010) %>%
  filter(Established > 1500 & Established < 2010) %>%
  mutate(Location = if_else(RandomCoordsFlag, "Approximate", "Exact"))

states <- map_data("state")

set1_cols <- RColorBrewer::brewer.pal(4, "Set1")[c(4, 2, 3, 1)]


# We use this theme for ggplot2 that was used by Timo Grossenbacher for 

postal_clean %>%
  filter(!State %in% c("AK", "HI")) %>%
  select(Established, Discontinued, Longitude, Latitude, Location) %>%
  #skimr::skim()
  #slice_min(order_by = Established, n = 1000) %>%
  #slice_sample(prop = 0.2) %>%
  mutate(Established_decade = Established %/% 100 * 100) %>%
  ggplot() +
  geom_polygon(data = states, aes(x = long, 
                                  y = lat, 
                                  group = group), 
               fill = "white", colour = "grey45") +
  geom_point(aes(x = Longitude, 
                 y = Latitude, 
                 colour = Established_decade), alpha =  0.1, size = 0.25) +
  #scale_colour_viridis_c(direction = -1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Established post offices in US by decade",
       caption = "Воскресный СкRинкаст 18.04.2021: US Post Offices \n  https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md") +
  theme_void()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                     margin = margin(b = -0.1, 
                                                     t = -0.1, 
                                                     l = 2, 
                                                     unit = "cm"), 
                                     debug = FALSE),
        plot.margin = unit(c(.5,.5,.2,.5), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank(),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184")
  ) +
  scale_colour_gradientn(colours = set1_cols)



gganim <- postal_clean %>%
  filter(!State %in% c("AK", "HI")) %>%
  select(Established, Discontinued, Longitude, Latitude, Location) %>%
  #skimr::skim()
  #slice_min(order_by = Established, n = 1000) %>%
  slice_sample(prop = 0.02) %>%
  mutate(Established_decade = Established %/% 100 * 100) %>%
  ggplot() +
  geom_polygon(data = states, aes(x = long, 
                                  y = lat, 
                                  group = group), 
               fill = "white", colour = "grey45") +
  geom_point(aes(x = Longitude, 
                 y = Latitude, 
                 colour = Established_decade), alpha =  0.6, size = 1.3) +
  transition_time(as.integer(Established)) + 
  #scale_colour_viridis_c(direction = -1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Established post offices in US by year: {frame_time}",
       caption = "Воскресный СкRинкаст 18.04.2021: US Post Offices \n  https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md") +
  theme_void()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                     margin = margin(b = -0.1, 
                                                     t = -0.1, 
                                                     l = 2, 
                                                     unit = "cm"), 
                                     debug = FALSE),
        plot.margin = unit(c(.5,.5,.2,.5), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank(),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184")
  ) +
  scale_colour_gradientn(colours = set1_cols)

gganim

postal_long <- postal_clean %>%
  filter(!State %in% c("AK", "HI")) %>%
  select(ID, Established, Discontinued, Longitude, Latitude, Location) %>%
  group_by(ID) %>%
  summarise(Longitude, Latitude, Location, Established, Year = Established:Discontinued) %>%
  ungroup()

postal_long %>%
  #filter(ID %in% sample(.$ID, size = 10000)) %>%
  mutate(Established_decade = Established %/% 100 * 100) %>%
  ggplot() +
  geom_polygon(data = states, aes(x = long, 
                                  y = lat, 
                                  group = group), 
               fill = "white", colour = "grey45") +
  geom_point(aes(x = Longitude, 
                 y = Latitude, 
                 colour = Established), alpha = 0.125, size = 0.6) +
  transition_time(as.integer(Year)) + 
  #scale_colour_viridis_c(direction = -1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Established post offices in US by year: {frame_time}",
       caption = "Воскресный СкRинкаст 18.04.2021: US Post Offices \n  https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md") +
  theme_void()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                     margin = margin(b = -0.1, 
                                                     t = -0.1, 
                                                     l = 2, 
                                                     unit = "cm"), 
                                     debug = FALSE),
        plot.margin = unit(c(.5,.5,.2,.5), "cm"),
        panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_blank(),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184")
  ) +
  scale_colour_gradientn(colours = set1_cols)

anim_save("post_office_animation.gif")

postal_clean %>%
  count(Discontinued > 1970)