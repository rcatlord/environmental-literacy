# Atmospheric CO₂ concentrations
# Source: NOAA
# URL: https://gml.noaa.gov/ccgg/trends/data.html

library(tidyverse) ; library(ggtext)

df <- read_csv("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.csv",
               skip = 40) %>% 
  filter(year >= 1960) %>% 
  mutate(date = as.Date(date_decimal(`decimal date`, tz = "UTC"))) %>% 
  select(date, average)

ggplot(df, aes(x = date, y = average, group = 1)) +
  geom_ribbon(aes(ymin = 280, ymax = average), fill = "#dddddd") +
  geom_line(linewidth = 0.5, colour = "#000000") +
  scale_x_date(expand = c(0.005, 0.005),
               breaks = seq.Date(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
               date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), 
                     limits = c(280, 450)) +
  labs(x = NULL, y = NULL,
       title = "Atmospheric CO<sub>2</sub> concentration",
       subtitle = "<span style = 'color:#757575;'>Monthly average, Mauna Loa Observatory</span>",
       tag = "Chart baseline is 280ppm - the preindustrial average",
       caption = "Source: NOAA") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(c(0.5,3,1,0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 1),
        plot.tag = element_text(size = 8),
        plot.tag.position = c(0.27, 0.02)) +
  annotate("text", x = as.Date("1960-07-01"), y = 450, label = "Parts per million", size = 3) +
  geom_text(data = filter(df, date == last(date)),
            aes(label = paste0(format(date, "%b %Y"), "\n", round(average, 0), "ppm")),
            hjust = -0.1, size = 3, fontface = "bold") +
  coord_cartesian(clip = "off")

ggsave("plot.png", scale = 1, dpi = 300) 
