library(tidyverse)
library(hrbrthemes)
library(patchwork)
tuesdata <- tidytuesdayR::tt_load('2021-02-09')

lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

lifetime_earn
student_debt
retirement
home_owner
race_wealth
income_time
income_limits
income_aggregate
income_distribution
income_mean

income_limits %>%
  count(income_quintile)

income_limits %>%
  count(race)

min(income_limits$year)
range(income_limits$year)

income_limits_2019 <- income_limits %>%
  filter(year == 2019) %>%
  filter(race == "All Races") %>%
  filter(dollar_type == "2019 Dollars") %>%
  mutate(income_percentile = case_when(
    income_quintile == "Lowest" ~ "Top 80%",
    income_quintile == "Second" ~ "Top 60%",
    income_quintile == "Third" ~ "Top 40%",
    income_quintile == "Fourth" ~ "Top 20%",
    TRUE ~ "Top 5%"
  ))

income_plot_one <- income_limits %>%
  filter(race == "All Races") %>%
  filter(dollar_type == "2019 Dollars") %>%
  pivot_wider(names_from = "income_quintile", values_from = "income_dollars") %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = Second, ymax = Third), alpha = 0.3, fill = "#23d0fc")+
  geom_ribbon(aes(x = year, ymin = Lowest, ymax = Fourth), alpha = 0.3, fill = "#23d0fc")+
  geom_line(aes(x = year, y = `Top 5%`)) +
  ggrepel::geom_text_repel(data = income_limits_2019, aes(x = year, 
                                           y = income_dollars, 
                                           label = income_percentile),
            hjust = 2,
            segment.linetype = 3,
            colour = 'white') +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(labels = scales::dollar, 
                     limits = c(0, NA), 
                     breaks = seq(0, 1000000, 50000)) +
  labs(y = "Familial income, in 2019 $", 
       title = "Income inequality increases over time",
       subtitle = "Changes for population income for all races") +
  theme_ft_rc()

income_plot_many <- income_limits %>%
  filter(race != "All Races") %>%
  filter(dollar_type == "2019 Dollars") %>%
  pivot_wider(names_from = "income_quintile", values_from = "income_dollars") %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = Second, ymax = Third, fill = race), alpha = 0.3)+
  geom_ribbon(aes(x = year, ymin = Lowest, ymax = Fourth, fill = race), alpha = 0.3)+
  geom_line(aes(x = year, y = `Top 5%`, colour = race)) +
  scale_y_continuous(labels = scales::dollar, 
                     limits = c(0, NA), 
                     breaks = seq(0, 1000000, 100000)) +
  labs(y = "", subtitle = "Changes for population income for different races") +
  facet_grid(. ~ race) +
  guides(colour = FALSE, fill = FALSE) +
  theme_ft_rc(plot_title_margin = 0,
              plot_title_size = 0)

combined_plot <- income_plot_one /
  income_plot_many +
  plot_layout(heights = c(3,1))


ggsave("combined_races_wealth.png", combined_plot, scale = 1.15)
