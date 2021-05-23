library(tidyverse)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey %>% View()

survey %>%
  summarise(range(timestamp))

survey %>%
  count(industry, sort = TRUE)

survey %>%
  count(job_title)

survey %>%
  count(currency)

survey %>% 
  filter(currency == "Other") %>% View()

au_nz_countries <- survey %>% 
  filter(currency == "AUD/NZD") %>% 
  count(country) %>% pull(country)

str_detect(tolower(au_nz_countries), "australi")
str_detect(tolower(au_nz_countries), "(new zealand)|(nz)")

other_currencies <- survey %>% 
  filter(currency == "Other") %>% 
  count(currency_other) %>% pull(currency_other)
str_extract(tolower(other_currencies), "(^[a-z]{3}\\s)|(^[a-z]{3}$)")

survey_clean <- survey %>%
  mutate(currency = case_when(
    str_detect(tolower(currency), "australi") ~ "AUD",
    str_detect(tolower(currency), "(new zealand)|(nz)") ~ "NZD",
    TRUE ~ currency
  )) %>%
  mutate(currency = if_else(currency == "Other",
                            currency_other %>% tolower() %>% str_extract("(^[a-z]{3}\\s)|(^[a-z]{3}$)") %>% toupper() %>% str_trim(),
                            currency))

library(quantmod)
?getQuote
?getFX

rub_usd <- getFX("RUB/USD", from = Sys.Date() - 1, auto.assign = FALSE)
str(rub_usd)
as.vector(rub_usd)

exchange_to_usd <- function(x) str_c(x, "/USD") %>% getFX(from = Sys.Date() - 1, auto.assign = FALSE) %>% as.vector()

exchange_to_usd("RUB")

currencies_dict <- survey_clean %>%
  count(currency) %>%
  mutate(coef_to_usd = map_dbl(currency, possibly(exchange_to_usd, otherwise = NA_real_))) %>%
  select(!n)

survey_prepared <- survey_clean %>%
  left_join(currencies_dict, by = "currency") %>%
  mutate(annual_salary_usd = round(annual_salary * coef_to_usd), .after = annual_salary) %>%
  mutate(annual_salary_usd = if_else(annual_salary_usd < 10000000 & annual_salary_usd >= 2000, 
                 annual_salary_usd,
                 NA_real_)) %>%
  mutate(job_title_length = nchar(job_title), .after = job_title) %>%
  mutate(highest_level_of_education_completed = factor(highest_level_of_education_completed, 
                levels = c("High School", 
                           "Some college",
                           "College degree",
                           "Master's degree",
                           "Professional degree (MD, JD, etc.)",
                           "PhD")))
  

survey_prepared %>%
  skimr::skim(starts_with("annual"))

hist(survey_prepared$annual_salary_usd, breaks = 100)
hist(log(survey_prepared$annual_salary_usd))

cor.test(survey_prepared$job_title_length, survey_prepared$annual_salary_usd)
lm(log(annual_salary_usd) ~ log(job_title_length), data = survey_prepared) %>%
  summary()
glm(annual_salary_usd ~ log(job_title_length), data = survey_prepared, family = gaussian(link = "log")) %>%
  summary()
glm(annual_salary_usd ~ log(job_title_length), data = survey_prepared, family = Gamma(link = "log")) %>%
  summary()

survey_prepared %>%
  count(how_old_are_you)

survey %>%
  count(highest_level_of_education_completed)

ggplot(survey_prepared %>% drop_na(highest_level_of_education_completed), aes(x = job_title_length, y = annual_salary_usd)) +
  geom_point(data = survey_prepared %>% select(!highest_level_of_education_completed), alpha = 0.04, colour = "grey10") +
  geom_point(aes(colour = highest_level_of_education_completed), alpha = 0.25) +
  facet_wrap(~highest_level_of_education_completed) +
  scale_x_log10(labels = scales::label_number(accuracy = 1)) +
  scale_y_log10(labels = scales::label_dollar()) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Senior Assistant Directors of Brand Marketing Communications Management",
    subtitle = "Does length of a job title matter?",
       x = "Number of characters in a job title",
       y = "Salary (USD)",
       caption = "Воскресный СкRинкаст 24.05.2021: Ask a Manager Survey \n  https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-18/readme.md") +
  guides(colour = FALSE) +
  theme_linedraw() 

ggsave("ask_a_manager_survey.png", scale = 2.3)




