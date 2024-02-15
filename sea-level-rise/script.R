# Rising sea levels
# Source: NOAA
# URL: https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/LSA_SLR_timeseries_global.php

library(tidyverse) ; library(lubridate) ; library(ggtext)

df <- read_csv("https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_txj1j2_90.csv", skip = 5) %>% 
  mutate(Date = as.Date(date_decimal(year, tz = "UTC"))) %>% 
  select(-year) %>% 
  pivot_longer(-Date, names_to = "System", values_to = "Value") %>% 
  mutate(System = fct_relevel(System, "TOPEX/Poseidon", "Jason-1", "Jason-2", "Jason-3"))

ggplot(na.omit(df), aes(x = Date, y = Value, group = System, colour = System)) +
  geom_hline(yintercept = 0, linewidth = 1, colour = "#000000") +
  geom_line(key_glyph = "timeseries") +
  scale_colour_manual(values = c("TOPEX/Poseidon" = "#bdd7e7",
                                 "Jason-1" = "#6baed6",
                                 "Jason-2" = "#3182bd",
                                 "Jason-3" = "#08519c")) +
  scale_x_date(expand = c(0.005, 0.005),
               breaks = seq.Date(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 years"),
               date_labels = "%Y") + 
  scale_y_continuous(expand = c(0.005, 0.005), 
                     limits = c(-50, 115),
                     position = "right") +
  labs(x = NULL, y = "mm",
       title = "Rising sea levels",
       subtitle = "<span style = 'color: #757575;'>Change in global mean sea level from 1 January 2000</sub>",
       caption = "Source: NOAA",
       colour = "Monitoring system") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12),
        axis.title.y.right = element_text(size = 8, angle = 0, vjust = 1, margin = margin(l = -15)),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_text(size = 10, face = "bold", colour = "#757575"),
        legend.text = element_text(size = 10, colour = "#757575"))

ggsave("plot.jpeg", scale = 1, dpi = 300) 
