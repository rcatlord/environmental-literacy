# New tree planting
# Source: Forest Research
# URL: https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/

library(tidyverse) ; library(httr) ; library(readODS) ; library(ggtext)

tmp <- tempfile(fileext = ".ods")
GET(url = "https://cdn.forestresearch.gov.uk/2023/06/nprs-timeseries-15jun23.ods", write_disk(tmp))
df <- read_ods(tmp, sheet = "Table_C9", skip = 4) %>% 
  select(year = 1, value = 6) %>% 
  mutate(year = ymd(year, truncated = 2L))

ggplot(df, aes(y = value, x = year)) + 
  geom_col(fill = "#C5D59B") +
  geom_segment(x = as.Date("2025-01-01"), xend = as.Date("2050-01-01"), 
               y = 30, yend = 30,
               colour = "#333333", linewidth = 1) +
  annotate(geom = "richtext", x = as.Date("2025-01-01"), y = 27, 
           label = "<span style='color: #795BA1;'>**Government ambition**</span><br>+30,000 hectares per year", 
           fill = NA, label.color = NA, hjust = 0) +
  scale_x_date(expand = expansion(), date_breaks = "10 years", date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), limits = c(0, 45), position = "right") +
  labs(x = NULL, y = "Thousand hectares",
       title = "Tree planting rates are well below target",
       subtitle = "<span style = 'color:#757575;'>New tree planting, UK, 1970-2023</span>",
       caption = "Source: Forest Research",
       fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12),
        axis.title.y.right = element_text(size = 8, angle = 0, vjust = 1, margin = margin(l = -60)),
        legend.position = "top") +
  expand_limits(x = c(as.Date("1970-01-01"), as.Date("2050-01-01")))

ggsave("plot.jpeg", dpi = 300, scale = 1)
