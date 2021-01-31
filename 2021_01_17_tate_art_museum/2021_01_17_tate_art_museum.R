library(tidyverse)
library(imager)

tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork

artwork %>%
  count(medium, sort = TRUE)

artwork %>% 
  slice(1) %>%
  pull(thumbnailUrl)

skimr::skim(artwork)

artwork %>%
  filter(medium == "Oil paint on canvas") %>%
  drop_na(thumbnailUrl) %>%
  slice_sample(n = 5) %>%
  pull(url)

boys <- "https://www.tate.org.uk/art/images/work/T/T11/T11922_10.jpg"
clusters <- imager::load.image(boys) %>%
  as.data.frame(wide = "c") %>%
  select(starts_with("c")) %>%
  kmeans(centers = 40)

rgb(red = clusters$centers[,1],
    green = clusters$centers[,2],
    blue = clusters$centers[,3],
    alpha = 1) %>%
  plot(x = 1:40, y = 1:40, col = ., cex = 7, pch = 16)

get_dominant_hex_from_url <- function(url, colours_n = 8, ...) {
imager::load.image(url) %>%
  as.data.frame(wide = "c") %>%
  select(starts_with("c")) %>%
  kmeans(centers = colours_n, ...) %>%
  {rgb(red = .$centers[,1],
      green = .$centers[,2],
      blue = .$centers[,3],
      alpha = 1)}
}

get_dominant_hex_from_url("https://www.tate.org.uk/art/images/work/N/N01/N01566_10.jpg") %>%
  plot(x = 1:8, y = 1:8, col = ., cex = 7, pch = 16)

system.time({ 
  artwork %>%
    filter(medium == "Oil paint on canvas") %>%
    drop_na(thumbnailUrl) %>%
    slice_sample(n = 25) %>%
    mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_url, NA)))
})
beepr::beep()
art50 <- artwork %>%
  filter(medium == "Oil paint on canvas") %>%
  drop_na(thumbnailUrl) %>%
  slice_sample(n = 50) %>%
  mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_url, NA)))


art50 %>%
  unnest() %>%
  select(year, id, dom_colours)

artwork %>%
  filter(medium == "Oil paint on canvas") %>%
  drop_na(thumbnailUrl) %>%
  pull(year) %>%
  hist()

art70_00 <- artwork %>%
  filter(medium == "Oil paint on canvas") %>%
  drop_na(thumbnailUrl) %>%
  filter(year >= 1970 & year <= 2000) %>%
  mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_url, NA)))
beepr::beep()

library(plotwidgets)

hue_from_hex <- function(hex) plotwidgets::col2hsl(hex)[1,1] %>% unname()



art70_00 %>%
  unnest() %>%
  select(year, id, dom_colours) %>%
  mutate(hue = map_dbl(dom_colours, hue_from_hex),
         dom_colours_bright = scales::col2hcl(dom_colours, c = 100, l = 50)) %>%
  arrange(year, hue) %>%
  group_by(year) %>%
  mutate(ncol = row_number()) %>%
  ungroup() %>% 
  group_by(dom_colours) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(hue) %>%
  mutate(n = row_number(),
         dom_colours = factor(n, labels = dom_colours)) %>%
  ggplot(aes(x = year, fill = dom_colours)) +
  geom_bar(position = "fill") +
  scale_fill_identity() +
  coord_flip() +
  scale_x_reverse(breaks = 1970:2000, 
                     labels = as.character(1970:2000) %>% 
                    substr(3,4) %>% 
                    str_c("'", .),
                  minor_breaks = NULL) +
  labs(title = "Evolution of oil paints dominant colours",
       subtitle = "Tate Art Museum, 1970 - 2000 years",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Art Collections dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
## Post scriptum: all paintings and 16 dominant colours for a painting

art_full <- artwork %>%
  filter(medium == "Oil paint on canvas") %>%
  drop_na(thumbnailUrl) %>%
  mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_url, NA), 16))
beepr::beep()

# art_full %>%
#   unnest() %>%
#   write_csv("2021_01_17_tate_art_museum/dominant_colours_full.csv")

art_full_unnested <- read_csv("2021_01_17_tate_art_museum/dominant_colours_full.csv")


dom_colours_by_year <- art_full_unnested %>%
  select(year, id, dom_colours) %>%
  mutate(hue = map_dbl(dom_colours, hue_from_hex)) %>%
  arrange(year, hue) %>%
  group_by(year) %>%
  mutate(ncol = row_number()) %>%
  ungroup() %>% 
  group_by(dom_colours) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(hue) %>%
  mutate(n = row_number(),
         dom_colours = factor(n, labels = dom_colours))

poster <- dom_colours_by_year %>%
  filter(year > 1900 & year <= 2000) %>%
  ggplot(aes(x = year, fill = dom_colours)) +
  geom_bar(position = "fill") +
  scale_fill_identity() +
  coord_flip() +
  scale_x_reverse(minor_breaks = NULL) +
  labs(title = "Evolution of oil paints dominant colours",
       subtitle = "Tate Art Museum, 1901 - 2000 years",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Art Collections dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
