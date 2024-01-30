# Annual CO2 emissions
# Source: Our World in Data
# URL: https://ourworldindata.org/co2-emissions

library(tidyverse) ; library(ggtext)

df <- read_csv("data/annual-co2-emissions-per-country.csv") %>% 
  filter(Entity == "World") %>% 
  select(Year, Value = `Annual CO₂ emissions`) %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         Value = Value/1000000000) # Billion tonnes CO2

ggplot(df, aes(x = Year, y = Value, group = 1)) +
  geom_area(fill = "#dddddd") +
  geom_line(linewidth = 1, colour = "#000000") +
  scale_x_date(expand = expansion(),
               breaks = seq.Date(as.Date("1750-01-01"), as.Date("2022-01-01"), by = "50 years"),
               date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), 
                     limits = c(0, 43)) +
  labs(x = NULL, y = NULL,
       title = "CO<sub>2</sub> emissions from fossil fuel burning",
       subtitle = "<span style = 'color:#757575;'>Annual global CO<sub>2</sub> emissions from fossil fuels and industry, 1750-2022</span>",
       caption = "Source: Our World in Data") +
  theme_minimal(base_size = 12) +
  theme(plot.margin = unit(c(1.5,2,1.5,1.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12)) + 
  annotate("text", x = as.Date("1755-01-01"), y = 43, label = "Billion tonnes", size = 3) +
  geom_point(data = filter(df, Year == last(Year)),
             aes(x = Year, y = Value), 
             colour = "#000000", size = 2) +
  geom_text(data = filter(df, Year == last(Year)),
            aes(label = paste0(format(Year, "%Y"), "\n", round(Value, 1), "bn t")),
            hjust = -0.3, size = 3, fontface = "bold") +
  coord_cartesian(clip = "off")

ggsave("plot.png", scale = 1, dpi = 300) 
