library(tidyverse)
#install.packages("gt")
library(gt)

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')
parks %>% View()

skimr::skim(parks)

parks %>%
  filter(year == 2020) %>%
  skimr::skim()

original_names <- colnames(parks) %>%
  str_subset("_points") %>%
  head(-1)

new_names <- c("Size", "Density", "Access", "Prices", "Basketball", 
               "Dogs", "Playgrounds", "Seniors", "Restrooms", "Swimming",
               "Amenities")

emojis <- c("&#127758;", "&#128506;", "&#127939;", "&#128181;", "&#127936;", "&#128054;", "&#129342;", "&#129491;", "&#128701;", "&#127946;", "&#127905;")

column_labels <- as.list(glue::glue('<p><font size="+3">{emojis}</font></p> {new_names}'))
names(column_labels) <- original_names
column_labels

parks_gt <- parks %>%
  filter(year == 2020) %>%
  select(rank, city, ends_with("_points"), total_pct) %>%
  select(!total_points) %>%
  mutate(across(ends_with("_points"), function(x) x/max(x, na.rm = TRUE) * 100)) %>%
  # rename_with(str_remove, ends_with("_points"), "_points") %>%
  # rename_with(str_replace_all, everything(), "_", " ") %>%
  # rename_with(str_to_title, everything()) %>%
  gt() %>%
  fmt_percent(ends_with("_points"), decimals = 0, scale_values = FALSE) %>%
  data_color(ends_with("_points"), 
             colors = scales::col_numeric(palette = "plasma", domain = c(0, 100)),
             alpha = 0.75) %>%
  cols_align("center", columns = ends_with("_points")) %>%
  cols_label(total_pct = "Total (% of max)",
             city = "City",
             rank = "Rank") %>%
  cols_label(.list = map(column_labels, html)) %>%
  tab_header(title = html('
&#127803; &#127774; &#128696; &#127794; &#9968; &#9968; &#9968; &#9978; &#128696; &#127774; &#127803; <br> Park Access in US by cities <br> &#127803; &#127774; &#128696; &#127794; &#9968; &#9968; &#9968; &#9978; &#128696; &#127774; &#127803;   ')) %>%
  tab_spanner(label = html('Measurement Scales'), columns = ends_with("_points")) %>%
  tab_options(column_labels.font.size = "smaller",
              table.font.size = "smaller",
              data_row.padding = px(3)) %>%
  tab_source_note(source_note = html("Воскресный СкRинкаст 27.06.2021: Park Access in US <br> 
                                     https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-22/readme.md")) %>%
  tab_style(style = list(cell_fill(color = "#FEF4D2"), 
                         cell_text(align = "left", 
                                   #style = "italic", 
                                   stretch = "expanded",
                                   size = "large")),
    locations = cells_column_spanners('Measurement Scales'))  %>%
  tab_style(style = list(cell_text(align = "right", 
                                   style = "italic", 
                                   size = "x-small",
                                   stretch = "ultra-condensed")),
    locations = cells_source_notes()) %>%
  tab_style(style = list(cell_borders(sides = c("left", "right"), 
                                      color = "#B2B2B2", 
                                      weight = px(3))),
    locations = cells_body(columns = c(city, total_pct))) %>%
  tab_style(style = list(cell_text(size = "x-large")),
    locations = cells_title(groups = "title")) 

parks_gt %>%
  gtsave("park_access_us.html")
