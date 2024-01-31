# Global Surface Air Temperature Anomaly
# Source: Copernicus
# URL: https://climate.copernicus.eu/global-climate-highlights-2023

library(tidyverse) ; library(shadowtext) ; library(ggtext) ; library(ggrepel)

df <- read_csv("https://climate.copernicus.eu/sites/default/files/custom-uploads/Global%20Climate%20Highlights%202023/fig3_GCH2023_PR_daily_global_temperature_increase_above_preindustrial_2023.csv",
         skip = 11) %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE),
         day_month = format(as.Date(date), "%d-%m"),
         day_month = as.Date(paste0(day_month,"-2000"), format = "%d-%m-%Y")) %>%
  mutate(day_month = case_when(is.na(day_month) ~ as.Date(as.character("2000-02-29"), format = "%d-%m-%Y"), TRUE ~ day_month)) %>% 
  select(day_month, year, ano_pi)

ggplot(df, aes(day_month, ano_pi, group = year)) +
  geom_line(colour = ifelse(df$year == 2023, "red", "#dddddd")) +
  geom_hline(yintercept = 0, linewidth = 0.8, linetype = 2, colour = "#333333") +
  geom_hline(yintercept = 1.5, linewidth = 0.8, linetype = 2, colour = "#333333") +
  geom_shadowtext(aes(x = as.Date("2000-01-6"), y = 0.1),
                  label = "Pre-industrial average", check_overlap = TRUE, hjust = 0,
                  bg.colour = "#FFFFFF", colour = "#212121", size = 4) +
  geom_shadowtext(aes(x = as.Date("2000-01-6"), y = 1.6),
                  label = "1.5 degrees", check_overlap = TRUE, hjust = 0,
                  bg.colour = "#FFFFFF", colour = "#212121", size = 4) +
  geom_shadowtext(x = as.Date("2000-07-15"), y = 0.85, 
                  label = "Each grey line\nrepresents a year\nbetween 1940 and 2022",
                  check_overlap = TRUE, hjust = 0,
                  bg.colour = "#FFFFFF", colour = "#212121", size = 4) +
  geom_curve(aes(x = as.Date("2000-07-10"), xend = as.Date("2000-06-01"), y = 0.85, yend = 0.95),
             colour = "#000000", curvature = -0.3, 
             arrow = arrow(ends = "last", length = unit(0.03, "npc"))) +
  scale_x_date(expand = expansion(),  date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = NULL,
       title = "<span style = 'color: red;'>2023</span> was the hottest year on record",
       subtitle = "<span style = 'color: #757575;'>Increase in global-average temperatures (°C) above 1850-1900 levels</span>",
       caption = "Source: Copernicus Climate Change Service") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5,4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        axis.text.x = element_text(hjust = -0.5, vjust = 0.5), 
        axis.ticks.x = element_line(colour = "#212121", linewidth = 1),
        axis.ticks.length = unit(0.2, "cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0, margin = margin(t = 20)))

ggsave("plot.png", scale = 1, dpi = 300) 
