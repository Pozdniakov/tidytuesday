# Packages ----------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggforce)
library(vapoRwave)
library(extrafont)

loadfonts(quiet = TRUE)


# Import data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics

#plastics %>% View()


# Explore data ------------------------------------------------------------

skimr::skim(plastics)

plastics %>%
  count(country, sort = TRUE)

plastics %>%
  count(parent_company, sort = TRUE)

#null and Unbranded!


# Preprocessing -----------------------------------------------------------

plastics_long <- plastics %>%
  filter(year == 2020) %>%
  select(!c(year, grand_total:volunteers)) %>%
  pivot_longer(cols = empty:pvc,
               names_to = "plastic_type",
               values_to = "count") %>%
  mutate(parent_company = str_to_lower(parent_company),
         parent_company = case_when(
           parent_company == "null" ~ "unbranded",
           parent_company == "nestle" ~ "Nestlé",
           TRUE ~ parent_company
         ) %>% str_to_title(),
         parent_company = ifelse(parent_company == "Pepsico", 
                                 "PepsiCo",
                                 parent_company)) %>%
  mutate(continent = countrycode::countrycode(country, origin = "country.name", destination = "continent"),
         .before = everything()) %>%
  mutate(plastic_type = str_to_upper(plastic_type))


# Plotting Sankey diagram -------------------------------------------------


plastics_sankey_all_gg <- plastics_long %>%
  mutate(country = fct_lump_n(country, 6),
         parent_company = fct_lump_n(parent_company, 5) %>% 
           fct_collapse(`Other/Unbranded` = c("Other", "Unbranded"))) %>%
  group_by(continent, country, parent_company, plastic_type) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  gather_set_data(x = 1:4) %>% 
  ggplot(aes(x = x, id = id, split = y, value = count)) +
  geom_parallel_sets(aes(fill = continent), alpha = 0.6, show.legend = FALSE,
                     axis.width = 0.5, sep = 0.0001) +
  geom_parallel_sets_axes(axis.width = 0.5, sep = 0.0001, fill = "#34645A") +
  geom_parallel_sets_labels(colour = "white", angle = 0, sep = 0.0001,
                            family = "Verdana", size = 3) +
  vapoRwave::scale_fill_jazzCup() +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 0.01)) +
  labs(title = "Top polluters according to Break Free From Plastic",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Plastic Pollution dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md")

ggplot2::ggsave("2021_01_31_plastic/sankey_all.png", 
                scale = 1.2, 
                width = 8,
                height = 6)


# Plotting Sankey Diagram 2 -----------------------------------------------

plastics_sankey_top_gg <- plastics_long %>%
  mutate(country = fct_lump_n(country, 10),
         parent_company = fct_lump_n(parent_company, 9) %>% 
           fct_collapse(`Other/Unbranded` = c("Other", "Unbranded"))) %>%
  group_by(continent, country, parent_company, plastic_type) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(country != "Other") %>%
  filter(parent_company != "Other/Unbranded") %>%
  gather_set_data(x = 1:4) %>% 
  ggplot(aes(x = x, id = id, split = y, value = count)) +
  geom_parallel_sets(aes(fill = parent_company), alpha = 0.6, show.legend = FALSE,
                     axis.width = 0.5, sep = 0.0001) +
  geom_parallel_sets_axes(axis.width = 0.5, sep = 0.0001, fill = "#6B6566") +
  geom_parallel_sets_labels(colour = "white", angle = 0, sep = 0.0001,
                            family = "Verdana", size = 3) +
  #vapoRwave::scale_fill_vapoRwave() +
  ggthemes::scale_fill_gdocs() +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 0.01)) +
  labs(title = "Top polluters according to Break Free From Plastic",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Plastic Pollution dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md")

ggplot2::ggsave("2021_01_31_plastic/sankey_top.png", 
                scale = 1.2, 
                width = 8,
                height = 6)

