# Emissions gap, December 2023
# Source: Climate Action Tracker
# URL: https://climateactiontracker.org/global/cat-emissions-gaps/

library(tidyverse) ; library(lubridate) ;  library(ggtext)

df <- read_csv("data/data.csv") %>% 
  mutate(year = ymd(year, truncated = 2L))

ggplot(data = df) +
  geom_ribbon(aes(x = year, ymin = current_policy_low, ymax = current_policy_high), fill = "#9AC3E0") +
  geom_line(aes(year, target_2030), colour = "#795BA1", linewidth = 1.5) +
  geom_line(aes(x = year, y = pledges_high), colour = "#96BCD4", linewidth = 1.5) +
  geom_line(aes(x = year, y = optimistic), colour = "#86ADB1", linewidth = 1.5) +
  geom_ribbon(aes(x = year, ymin = paris_low, ymax = paris_high), fill = "#DAE5B8") +
  geom_line(aes(year, paris_median), colour = "#92BA7B", linewidth = 1.5, linetype = 2) +
  geom_line(aes(year, historical), colour = "#000000", linewidth = 1.5) +
  geom_hline(yintercept = 0, linewidth = 0.8, colour = "#333333") +
  annotate("text", x = as.Date("1995-01-01"), y = 64, label = expression("Gt" * CO[2] * "e/year"), size = 3) +
  annotate("text", x = as.Date("2100-01-01"), y = 55, 
           label = "Warming projected\nby 2100", 
           colour = "#000000",  hjust = 0) + 
  annotate("text", x = as.Date("1992-01-6"), y = 48, 
           label = "Historical", 
           colour = "#000000", fontface = "bold", hjust = 0) + 
  annotate(geom = "richtext", x = as.Date("2100-01-01"), y = 38, 
           label = "<span style='color: #9AC3E0;'>**Policies & action**</span><br>+2.5 - 2.9°C", 
           fill = NA, label.color = NA, hjust = 0) +
  annotate(geom = "richtext", x = as.Date("2100-01-01"), y = 27, 
           label = "<span style='color: #795BA1;'>**2030 targets only**</span><br>+2.5°C", 
           fill = NA, label.color = NA, hjust = 0) +
  annotate(geom = "richtext", x = as.Date("2100-01-01"), y = 14, 
           label = "<span style='color: #96BCD4;'>**Pledges & targets**</span><br>+2.1°C", 
           fill = NA, label.color = NA, hjust = 0) +
  annotate(geom = "richtext", x = as.Date("2100-01-01"), y = 5.5, 
           label = "<span style='color: #86ADB1;'>**Optimistic scenario**</span><br>+1.8°C", 
           fill = NA, label.color = NA, hjust = 0) +
  annotate(geom = "richtext", x = as.Date("2100-01-01"), y = -6, 
           label = "<span style='color: #92BA7B;'>**1.5°C consistent**</span>", 
           fill = NA, label.color = NA, hjust = 0) +
  geom_point(aes(x = as.Date("2030-01-01"), y = 50), size = 4, shape = 21,
             colour = "#000000", fill = "#96BCD4") +
  geom_point(aes(x = as.Date("2030-01-01"), y = 27), size = 4, shape = 21,
             colour = "#000000", fill = "#92BA7B") +
  geom_segment(x = as.Date("2030-01-01"), xend = as.Date("2030-01-01"), y = 29, yend = 48,
               colour = "#EB694B", linewidth = 1.5,
               arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
  geom_richtext(x = as.Date("2022-01-01"), y = 33, 
                label = "<span style='color: #EB694B;'>**2030<br>target gap**</span><br>19-22 GtCO<sub>2</sub>e",
                fill = "#FAD9CA", hjust = 1) +
  scale_x_date(expand = expansion(),  date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(), breaks = seq(-20,60,10), limits = c(-20, 65)) +
  labs(x = NULL, y = NULL,
       title = "Greenhouse gas emission projections",
       subtitle = "<span style = 'color:#757575;'>December 2023</span>",
       caption = "Source: Climate Analytics and NewClimate Institute") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(c(0.5,4,0.5,0.5), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5), 
        axis.ticks.x = element_line(colour = "#212121", linewidth = 1),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12))  + 
  coord_cartesian(clip = "off")

ggsave("plot.jpeg", dpi = 300, scale = 1)
