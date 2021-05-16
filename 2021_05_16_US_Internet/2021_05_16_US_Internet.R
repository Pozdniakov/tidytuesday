
# Import packages ---------------------------------------------------------

library(tidyverse)
library(patchwork)

# Download data -----------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

us_elections_2020 <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv") %>%
  mutate(biden_dominance = -diff/total_votes)


# Explore data ------------------------------------------------------------

tuesdata$broadband %>% 
  pull(`COUNTY ID`) %>%
  n_distinct()

tuesdata$broadband_zip %>% 
  pull(`COUNTY ID`) %>%
  n_distinct()

# tuesdata$broadband %>%
#   rename_with(tolower) %>%
#   rename_with(make.names)


# Preprocess data ---------------------------------------------------------

broadband_clean <- tuesdata$broadband %>%
  janitor::clean_names() %>%
  mutate(fips = str_pad(county_id, width = 5, pad = 0)) %>%
  mutate(across(starts_with("broadband"), parse_number, na = "-"))

broadband_clean %>%
  skimr::skim()


# Add SF mulipolygons for US Counties -------------------------------------


#remotes::install_github("hrbrmstr/albersusa")

us_counties <- albersusa::counties_sf()

# us_counties
# class(us_counties)
# us_counties %>% str()
# 
# multipol <- us_counties %>% 
#   pull(geometry) 
# 
# str(multipol)
# multipol[[1]][[1]][[1]]

us_counties_clean <- us_counties %>%
  mutate(fips = as.character(fips))


# Combine internet usage and elections data with SF -----------------------

# us_counties_clean %>%
#   anti_join(broadband_clean)
# 
# broadband_clean %>%
#   anti_join(us_counties_clean)

us_counties_broadband <- us_counties_clean %>%
  left_join(broadband_clean) %>%
  left_join(us_elections_2020, by = c("fips" = "county_fips"))


# Drawing maps -------------------------------------------------------------


usage_map_gg <- ggplot(us_counties_broadband) +
  geom_sf(aes(fill = broadband_usage), colour = "black", size = 0.2) +
  scale_fill_viridis_c(option = "A", 
                       labels = scales::label_percent(),
                       limits = c(0, 1)) +
  labs(x = "", 
       y= "", 
       fill = "Internet availability",
       title = "Internet users in US with access to Internet with 25 Mbps/3 Mbps speed (2019)") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  hrbrthemes::theme_modern_rc()

access_map_gg <- ggplot(us_counties_broadband) +
  geom_sf(aes(fill = broadband_availability_per_fcc), colour = "black", size = 0.2) +
  scale_fill_viridis_c(option = "A", 
                       labels = scales::label_percent(),
                       limits = c(0, 1)) +
  labs(x = "", 
       y= "", 
       fill = "Internet usage",
       title = "US population with access to Internet with 25 Mbps/3 Mbps speed (2017)") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  hrbrthemes::theme_modern_rc()

# us_counties_broadband %>%
#   summarise(across(starts_with("broad"), range, na.rm = TRUE))

biden_map_gg <- us_counties_broadband %>%
  ggplot() +
  geom_sf(aes(fill = biden_dominance), colour = "black", size = 0.2) +
  scale_fill_gradient2(low = "#E9141D", high = "#0015BC",
                      labels = scales::label_percent(),
                      limits = c(-1, 1)) +
  labs(x = "",
       y = "",
       fill = "Difference in votes for Biden",
       title = "Biden dominance for Presidental Elections 2020") +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  hrbrthemes::theme_modern_rc()


# Drawing scatterplots ----------------------------------------------------

access_usage_scatter_gg <- ggplot(us_counties_broadband, aes(x = broadband_availability_per_fcc, y = broadband_usage)) +
  geom_jitter(alpha = 0.25, colour = viridis::magma(n = 7)[5]) +
  geom_smooth(colour = "white", fill = "white", alpha = 0.25, size = 0.7) +
  labs(x = "Percents of US population with access to Internet with 25 Mbps/3 Mbps speed (2017)", 
       y = "Internet users in US with access to Internet with 25 Mbps/3 Mbps speed (2019)",
       title = "Internet Access (2017) vs Internet Usage (2019)") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  hrbrthemes::theme_modern_rc() +
  coord_fixed()

access_biden_scatter_gg <- ggplot(us_counties_broadband, aes(x = broadband_availability_per_fcc, y = per_dem)) +
  geom_jitter(alpha = 0.25, colour = viridis::magma(n = 7)[6]) +
  geom_smooth(colour = "white", fill = "white", alpha = 0.25, size = 0.7) +
  labs(x = "Percents of US population with access to Internet with 25 Mbps/3 Mbps speed (2017)", 
       y = "Percents of votes for Biden (2020)",
       title = "Internet Access (2017) vs Votes for Biden (2020)") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  hrbrthemes::theme_modern_rc() +
  coord_fixed()

usage_biden_scatter_gg <- ggplot(us_counties_broadband, aes(x = broadband_usage, y = per_dem)) +
  geom_jitter(alpha = 0.25, colour = viridis::magma(n = 7)[7]) +
  geom_smooth(colour = "white", fill = "white", alpha = 0.25, size = 0.7) +
  labs(x = "Internet users in US with access to Internet with 25 Mbps/3 Mbps speed (2019)", 
       y = "Percents of votes for Biden (2020)",
       title = "Internet Usage (2019) vs Votes for Biden (2020)") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  hrbrthemes::theme_modern_rc() +
  coord_fixed()

# #doesn't work:
# us_counties_broadband %>%
#   pivot_longer(cols = starts_with("broad"),
#                names_to = "metric", values_to = "percent") %>%
#   ggplot() +
#   geom_sf(aes(fill = percent), colour = "black", size = 0.2) +
#   scale_fill_viridis_b(option = "A",
#                        labels = scales::label_percent()) +
#   labs(x = "",
#        y= "",
#        title = "Internet users in US with access to Internet with at least 25 Mbps/3 Mbps speed") +
#   coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
#   hrbrthemes::theme_modern_rc() +
#   facet_grid(~metric)

full_fig <- access_map_gg + usage_map_gg + biden_map_gg +
  access_usage_scatter_gg + access_biden_scatter_gg + usage_biden_scatter_gg +
  plot_layout(nrow = 3, byrow = FALSE, widths = c(1.2, 1)) &
  theme(legend.position = c(1, 0.2),
        plot.title = element_text(hjust = 0.5))

full_fig 

ggsave("full_fig.png", scale = 2.4, width = 9, height = 9)


