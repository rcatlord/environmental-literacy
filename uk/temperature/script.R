# UK temperatures
# Source: Met Office
# URL: https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/haduk-grid

library(tidyverse) ; library(RColorBrewer)

df <- read_csv("data.csv") %>% 
  mutate(Year = ymd(Year, truncated = 2L))

ggplot(df, aes(x = Year, y = 1, fill = `Temperature Annomaly (°C)`)) +
  geom_tile() +
  scale_x_date(expand = expansion(), 
               breaks = c(as.Date(ymd("1884-01-01")),
                               as.Date(ymd("1950-01-01")),
                               as.Date(ymd("2000-01-01")),
                               as.Date(ymd("2022-01-01"))), 
               date_labels = "%Y") +
  scale_y_continuous(expand = expansion()) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu"))) +
  labs(title = "UK annual temperatures are rising",
       subtitle = "<span style = 'color:#757575;'>Annual UK temperature (°C) relative to 1981-2000</span>",
       caption = "Source: Met Office") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5,"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0, margin = margin(t = 15)),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        axis.text.y = element_blank(),
        legend.position = "none")
        
ggsave("plot.jpeg", dpi = 300, scale = 1)
