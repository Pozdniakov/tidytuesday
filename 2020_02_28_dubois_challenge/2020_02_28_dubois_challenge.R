#colours (extracted from the original pictures):

red = "#d00232"
yellow = "#fdc200"
blue = "#3f5499"
light_bage = "#cebca6"
dark_bage = "#9e7f5d"
paper = "#ebe1d2"

# The font family I used:

font_fam = "B52-ULCW00-ULC"


# Packages ----------------------------------------------------------------

library(tidyverse)
library(ragg) #for fonts
library(patchwork) #to combine the second plot and legend


# 1 - Line Chart: "Comparative increase..."  ------------------------------



georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

georgia_pop %>%
  pivot_longer(cols = !Year, names_to = "Race", values_to = "Percent") %>%
  ggplot(aes(x = Year, y = Percent, linetype = Race)) + 
  geom_line() +
  coord_flip() +
  scale_x_continuous(breaks = georgia_pop$Year, minor_breaks = NULL, expand = c(0, 0)) +
  scale_y_reverse(breaks = seq(0, 100, by = 5), minor_breaks = NULL, expand = c(0, 0)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "", y = "Percents", title = "COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.")  +
  theme(text = element_text(family = font_fam),
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = paper),
        panel.background = element_rect(fill = paper, colour = "black"),
        panel.grid = element_line(colour = "red", size = 0.05),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill = paper),
        legend.background = element_rect(fill = paper),
        legend.text = element_text(margin = margin(r = 2, unit='cm')),
        legend.spacing.x = unit(1.5, "cm"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 7)
        )

ggsave("georgia_pop_plot.png", scale = 2)

# 2 - Pie chart - "Occupation of negroes and whites in Georgia." ----------




occupation %>%
  mutate(Percentage = if_else(Group == "Whites" & Occupation == "Manufacturing and Mechanical Industries",
                              13.5, #mistake in the data! Thank to Nastya who noticed that =)
                              Percentage)) %>%
  bind_rows(tribble(~Group, ~Occupation, ~Percentage,
                    "Negroes", NA, 40,
                    "Whites", NA, 40)) %>%
  arrange(Group, Occupation) %>%
  mutate(end_rect = cumsum(Percentage),
         start_rect = end_rect - Percentage) 

occupation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')

main_pie <- occupation %>%
  mutate(Percentage = if_else(Group == "Whites" & Occupation == "Manufacturing and Mechanical Industries",
                 13.5,
                 Percentage)) %>%
  bind_rows(tribble(~Group, ~Occupation, ~Percentage,
                    "Negroes", NA, 40,
                    "Whites", NA, 40)) %>%
  mutate(Occupation = fct_relevel(Occupation, "Professions", after = 5)) %>%
  arrange(Group, Occupation) %>%
  mutate(end_rect = cumsum(Percentage),
         start_rect = end_rect - Percentage,
         Percentage_text = if_else(is.na(Occupation), 
                                   NA_character_, 
                                   str_c(Percentage, "%"))) %>%
  ggplot() +
  geom_rect(aes(xmin = start_rect, 
                xmax = end_rect, 
                ymin = 0, 
                ymax = 1, 
                fill = Occupation)) +
  geom_text(aes(x = (start_rect + end_rect)/2, y = 0.85, label = Percentage_text), 
            family = font_fam, 
            size = 2) +
  geom_text(data = tribble(~Race, ~x, ~y,
                           "NEGROES.", 50, 1.05,
                           "WHITES.", 190, 1.05),
            aes(x = x, 
                y = y, 
                label = Race),
            size = 3,
            family = font_fam) +
  coord_polar(start = -50/280 * 2 * pi) +
  scale_fill_manual(values = c(red, yellow, blue, light_bage, dark_bage))+
  guides(fill = FALSE) +
  labs(x = "", y = "", title = "OCCUPATION OF NEGROES AND WHITES IN GEORGIA.") +
  theme_void() +
  theme(text = element_text(family = font_fam),
        plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = paper, colour = NA),
        panel.background = element_rect(fill = paper, colour = NA),
        panel.grid = element_blank(),
        legend.position = c(0.5, -0.2),
        legend.title = element_blank(),
        legend.key = element_rect(fill = paper, colour = NA),
        legend.background = element_rect(fill = paper, colour = NA),
        axis.ticks = element_blank(),
        axis.text = element_blank()
  ) 

leg_margin <- 17
legend_over <- occupation %>%
  distinct(Occupation) %>%
  mutate(x = c(c(100, 100, 200, 200, 200)),
         y = seq(0.35, 0.65, length.out = 5)[c(2,4,1,3,5)],
         x_text = x + c(leg_margin, leg_margin, -leg_margin, -leg_margin , -leg_margin),
         y_text = y,
         labels = str_c(str_wrap(Occupation, 25), ".")) %>%
  ggplot() +
  geom_point(aes(x = x, y = y, colour = Occupation), size = 5) +
  geom_text(aes(x = x_text, y = y_text, label = labels), size = 2, family = font_fam) +
  scale_colour_manual(values = c(red, yellow, blue, light_bage, dark_bage)) +
  scale_x_continuous(expand = c(1, 1)) +
  scale_y_continuous(expand = c(0.5, 0.5)) +
  theme_void() +
  guides(colour = FALSE) 

legend_over

main_pie +
  
  inset_element(
    legend_over,
    left = -0.6,
    bottom = 0.2,
    right = 1.6,
    top = 0.8
  )

ggsave("occupation_plot.png", scale = 2)
  
