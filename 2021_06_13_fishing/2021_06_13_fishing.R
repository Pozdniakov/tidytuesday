library(tidyverse)
library(ggtext)

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

fishing

hist(fishing$year)

fishing %>%
  count(lake)

fishing %>%
  count(region, sort = TRUE) %>% View()

fishing %>%
  filter(region == "U.S. Total") %>%
  count(species, sort = TRUE)

fish_top <- fishing %>%
  filter(region == "U.S. Total") %>%
  group_by(species) %>%
  filter(sum(values, na.rm = TRUE) > 500000) %>%
  ungroup()
  # summarise(sum = sum(values, na.rm = TRUE)) %>%
  # arrange(desc(sum))

species_label_dict <- tibble(
  species = unique(fish_top$species),
  img = here::here(
    "2021_06_13_fishing",
    "data",
    "fish_images",
    c(
      "Cisco.jpg",
      "laketrout.jpg",
      "Lake_whitefish1.jpg",
      "yellowPerch.jpg",
      "Creek_chub_(Semotilus_atromaculatus).jpg",
      "alewife.jpg"
    )
  )
) %>%
  mutate(species_label = glue::glue('<img src={img} height="40"> <span style="font-size:26pt"> <sup><b> {species} </b></sup> </span>')) %>%
  select(!img)


fish_top %>%
  group_by(year, species) %>%
  summarise(amounts = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(species_label_dict) %>%
  ggplot(aes(x = year, y = amounts)) +
  geom_line() +
  labs(title = "Fish production in U.S. (Great Lakes) by years",
       x = "Year",
       y = "Production amounts (thousand pounds)",
       caption = "Воскресный СкRинкаст 13.06.2021: Commercial Fishing \n  https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-08/readme.md") +
  facet_wrap(~species_label) +
  hrbrthemes::theme_ipsum_rc(plot_title_size = 28) +
  theme(
    strip.text = element_textbox())

ggsave("fish_production.png", width = 12.3, height = 9)
