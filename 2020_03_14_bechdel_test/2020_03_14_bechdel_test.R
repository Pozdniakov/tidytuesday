library(tidyverse)
library(wesanderson)

tuesdata <- tidytuesdayR::tt_load('2021-03-09')

tuesdata

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')



movies %>%
  count(test)

movies %>%
  count(clean_test)

movies %>%
  count(binary)

skimr::skim(movies)

movies %>%
  arrange(desc(budget_2013)) %>%
  select(title, clean_test, year, budget_2013, budget)

cut_years <- function(x, bin) x %/% bin * bin

#geom_area() leads to issues on 70s:
# movies %>%
#   ggplot(aes(x = cut_years(year, 7), fill = clean_test)) +
#   geom_area(stat = "count", position = "fill")


movies_long <- movies %>%
  tidyr::separate_rows(country, sep = ", ") %>% 
  tidyr::separate_rows(genre, sep = ", ") %>% 
  filter(!is.na(country)) %>%
  filter(!is.na(genre))

movies_long %>%
  janitor::tabyl(country, genre)

breaks_years <- function(year) glue::glue("{substr(year, 3,4)}'s")

movies_long %>%
  mutate(country = fct_lump(country, 1)) %>%
  add_count(country) %>%
  rename(country_n = n) %>%
  mutate(country = glue::glue("{country}, N={country_n}") %>% fct_rev()) %>%
  distinct() %>%
  mutate(genre = if_else(genre %in% c("Music", "Musical"), "Music/Musical", genre)) %>%
  mutate(genre = fct_lump_min(genre, 400) %>% fct_rev()) %>%
  distinct() %>%
  add_count(genre) %>%
  rename(genre_n = n) %>%
  mutate(genre = glue::glue("{genre}, N={genre_n}")) %>%
  mutate(genre = forcats::fct_infreq(genre)) %>%
  mutate(clean_test = forcats::fct_relevel(clean_test, "nowomen", "notalk", "men", "dubious", "ok")) %>%
  ggplot(aes(x = cut_years(year, 5), fill = clean_test)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = rev(wes_palette("Zissou1", n = 5)),
                    labels = c("\nLess than 2 women\n",
                               "\nWomen do not talk\nto each other\n",
                               "\nWomen talk only\nabout men\n",
                               "\nDubious\n",
                               "\nWomen talk to each\nother not about man\n")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = breaks_years) +
  facet_grid(country ~ genre) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "", fill = "Bechdel test", 
       title = "Ratio of films that pass the Bechdel test \ndiffers across genres, country and time (but not that much)",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Bechdel Test dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md") 
  
ggsave("bechdel_test_genres_time.png", scale = 2.4)

zissou1_cols <- rev(wes_palette("Zissou1", n = 5))

movies %>% 
  tidyr::separate_rows(director, sep = ", ") %>% 
  filter(!is.na(director)) %>%
  group_by(director) %>%
  summarise(bias_rating = mean(binary == "PASS", na.rm = TRUE),
            n = n()) %>%
  mutate(director = glue::glue("{director} : {n}"),
         director = fct_reorder(director, bias_rating)) %>%
  slice_max(order_by = n, n = 25) %>%
  ggplot() +
  geom_col(aes(x = director, y = bias_rating, fill = bias_rating)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient2(low = zissou1_cols[1], high = zissou1_cols[5], mid = zissou1_cols[3],
                       midpoint = 0.5) +
  coord_flip() +
  theme_bw() + 
  labs(y = "Percentage of films that pass the Bechdel test", x = "",
       fill = "",
       title = "The Wachowskis are the only top film directors \nwith 100% of films passed the Bechdel test",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Bechdel Test dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-09/readme.md")
  
ggsave("bechdel_testtop_directors.png", scale = 2.4)
