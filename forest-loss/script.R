# Global forest loss
# Source: Global Forest Watch
# URL: https://www.globalforestwatch.org/dashboards/global/

library(tidyverse) ; library(lubridate) ; library(ggtext)

df <- read_csv("data/treecover_loss_in_primary_forests_2001_tropics_only__ha.csv") %>% 
  group_by(umd_tree_cover_loss__year) %>% 
  summarise(Value = sum(umd_tree_cover_loss__ha)) %>% 
  rename(Year = umd_tree_cover_loss__year) %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         Value = Value / 1000000) # Million hectares

ggplot(df, aes(x = Year, y = Value)) +
  geom_col(fill = "#3A5F0B") +
  scale_x_date(expand = expansion(),
               breaks = seq.Date(as.Date("2001-01-01"), as.Date("2022-01-01"), by = "5 years"),
               date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), 
                     limits = c(0, 6.5),
                     position = "right") +
  labs(x = NULL, y = "Million hectares",
       title = "Global forest loss",
       subtitle = "<span style = 'color:#757575;'>Annual primary forest loss, 2001-2022</span>",
       caption = "Source: Global Forest Watch") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12),
        axis.title.y.right = element_text(size = 8, angle = 0, vjust = 1, margin = margin(l = -60)))

ggsave("plot.jpeg", scale = 1, dpi = 300) 
