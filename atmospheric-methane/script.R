# Atmospheric methane
# Source: NOAA
# URL: https://gml.noaa.gov/ccgg/trends_ch4/

library(tidyverse) ; library(lubridate) ; library(scales) ; library(ggtext)

df <- read_csv("https://gml.noaa.gov/webdata/ccgg/trends/ch4/ch4_annmean_gl.csv",
               skip = 43) %>% 
  select(Year = year, Value = mean) %>% 
  mutate(Year = ymd(Year, truncated = 2L))

ggplot(df, aes(x = Year, y = Value, group = 1)) +
  geom_line(linewidth = 1.5, colour = "#d95f0e") +
  geom_point(data = filter(df, Year == last(Year)),
             aes(x = Year, y = Value), 
             shape = 21, fill = "#d95f0e", colour = "#FFFFFF", size = 3) +
  geom_text(data = filter(df, Year == last(Year)),
            aes(label = paste0(format(Year, "%Y"), "\n", format(round(Value, 0), big.mark = ",", scientific = FALSE))),
            hjust = -0.3, size = 3, fontface = "bold") +
  annotate("text", x = as.Date("1984-01-01"), y = 2020, label = "Parts per billion", size = 3, hjust = 0.6) +
  scale_x_date(expand = expansion(),
               breaks = seq.Date(as.Date("1984-01-01"), as.Date("2022-01-01"), by = "5 years"),
               date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), 
                     limits = c(1600, 2030),
                     labels = comma) +
  labs(x = NULL, y = NULL,
       title = "Atmospheric methane (CH<sub>4</sub>)",
       subtitle = "<span style = 'color:#757575;'>Global average atmospheric abundance of methane, 1984-2022</span>",
       caption = "Source: NOAA") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(c(0.5,1.5,0.5,0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12)) + 
  coord_cartesian(clip = "off")

ggsave("plot.jpeg", scale = 1, dpi = 300) 
