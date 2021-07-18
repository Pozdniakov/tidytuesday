library(tidyverse)
library(echarts4r)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv',
                             na = c("NA", " ", "NULL"))
View(scoobydoo)

scoobydoo %>%
  count(monster_real)

scoobydoo

skimr::skim(scoobydoo)

scoobydoo %>%
  count(series_name, season)

monster_cum <- scoobydoo %>%
  arrange(date_aired, index) %>%
  select(index, series_name, season, title, date_aired, starts_with("caught_")) %>%
  pivot_longer(cols = starts_with("caught_"), names_to = "character", values_to = "caught_monster") %>%
  #drop_na(caught_monster) %>%
  mutate(caught_monster = replace_na(caught_monster, FALSE)) %>%
  group_by(character) %>%
  mutate(monster_caught_cum = cumsum(caught_monster)) %>%
  ungroup() %>%
  mutate(character = character %>% str_remove("caught_") %>% str_to_title() %>% str_replace("Not", "Not Caught"))

# monster_cum %>%
#   ggplot(aes(x = date_aired, y = monster_caught_cum, colour = character)) +
#   geom_line()

# monster_cum %>%
#   group_by(character) %>%
#   mutate(spikes = c(0, diff(monster_caught_cum)) < 0) %>%
#   filter(spikes)



monster_cum %>%
  #filter(series_name == "Scooby Doo, Where Are You!") %>%
  group_by(date_aired) %>%
  e_charts(character, timeline = TRUE) %>%
  e_bar(monster_caught_cum, 
        itemStyle = list(color = "#e78b9a"),
        label = list(show = TRUE,
                     color = "#000000",
                     fontWeight = 'bold',
                     fontSize = 14,
                     position = "right",
                     #backgroundColor = "#e78b9a",
                     formatter = '{@[1]}: {@[0]}')) %>%
  e_timeline_opts(loop = FALSE, playInterval = 50, autoPlay = TRUE) %>%
  e_flip_coords() %>%
  e_title(text = "Caught monsters by Scooby Doo characters",
          subtext = "Воскресный СкRинкаст 18.07.2021: Scooby Doo data",
          sublink = "https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md")
# 
# monster_cum %>%
#   select(!caught_monster) %>%
#   mutate(character2 = character) %>%
#   pivot_wider(names_from = character, values_from = monster_caught_cum) %>% View()
#   #filter(series_name == "Scooby Doo, Where Are You!") %>%
#   group_by(date_aired) %>%
#   e_charts(character2, timeline = TRUE) %>%
#   e_bar(Scooby, stack = "grp") %>%
#   e_bar(Fred, stack = "grp") %>%
#   e_bar(Shaggy, stack = "grp") %>%
#   e_bar(Velma, stack = "grp") %>%
#   e_bar(Daphnie, stack = "grp") %>%
#   e_bar(Other, stack = "grp") %>%
#   e_bar(`Not Caught`, stack = "grp") %>%
#   e_timeline_opts(loop = FALSE, playInterval = 50, autoPlay = TRUE) %>%
#   e_flip_coords()
