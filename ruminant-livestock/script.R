# Ruminant livestock population
# Source: Food and Agriculture Organization Corporate Statistical Database (FAOSTAT)
# URL: https://www.fao.org/faostat/en/#data/QCL

library(tidyverse) ; library(lubridate) ; library(scales); library(ggtext)

df <- read_csv("data/FAOSTAT_data_en_2-6-2024.csv") %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         Value = Value/1000000000)

ggplot(df, aes(x = Year, y = Value)) +
  geom_line(linewidth = 1.5, colour = "#CC79A7") +
  geom_point(data = filter(df, Year == last(Year)),
             aes(x = Year, y = Value), 
             shape = 21, fill = "#CC79A7", colour = "#FFFFFF", size = 3) +
  geom_text(data = filter(df, Year == last(Year)),
            aes(label = paste0(format(Year, "%Y"), "\n", round(Value, 1))),
            hjust = -0.3, size = 3, fontface = "bold") +
  annotate("text", as.Date("1960-01-01"), y = 4.4, 
           label = "Billions", size = 3, hjust = 0.4) +
  scale_x_date(expand = expansion(),
               breaks = seq.Date(as.Date("1960-01-01"), as.Date("2022-01-01"), by = "10 years"),
               date_labels = "%Y") +
  scale_y_continuous(expand = expansion(), limits = c(0, 4.5)) +
  labs(x = NULL, y = NULL,
       title = "Cows, sheep, goats and buffaloes",
       subtitle = "<span style = 'color:#757575;'>Global ruminant livestock population, 1960-2022</span>",
       caption = "Source: FAOSTAT") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(1.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12)) + 
  coord_cartesian(clip = "off")

ggsave("plot.jpeg", scale = 1, dpi = 300) 
