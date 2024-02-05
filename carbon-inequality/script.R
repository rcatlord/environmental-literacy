# Carbon Inequality
# Source: World Inequality Database
# URL: https://wid.world/

library(tidyverse) ; library(lubridate) ; library(ggtext) ; library(ggh4x)

df <- read_csv("data.csv") %>% 
  mutate(Country = fct_recode(Country, "USA" = "United States", "UK" = "United Kingdom"),
         Country = fct_relevel(Country, "World","USA","UK","China","Brazil","India","Nigeria"),
         Percentile = fct_relevel(Percentile, "Top 1%","Top 10%","Middle 40%","Bottom 50%","Full population"))

ggplot(df, aes(Value, fct_rev(Country))) +
  geom_point(aes(colour = Percentile), size = 7) +
  geom_text(aes(label = ifelse(Percentile == "Bottom 50%" & Country != "USA", NA, round(Value, 0))), 
            fontface = "bold", color = "#FFFFFF", size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("Bottom 50%" = "#305697",
                                 "Middle 40%" = "#A7C88F",
                                 "Top 10%" = "#C20E1A",
                                 "Top 1%" = "#D5793C")) +
  scale_x_continuous(expand = expansion()) +
  labs(x = NULL, y = NULL,
       title = "Carbon inequality between and within countries",
       subtitle = "<span style = 'color:#757575;'>Per capita emissions by income group, tCO<sub>2</sub>e/capita, 2019</span>",
       caption = "Source: Chancel et al. (2022)",
       colour = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(1.5, 4), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 10)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12),
        axis.title.x = element_markdown(size = 10, hjust = 0),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0, margin = margin(r = 10)),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = c(0.84,-0.004),
        legend.direction = "horizontal",
        legend.justification = "right") +
  guides(colour = guide_legend(reverse = TRUE, override.aes = list(size = 3))) +
  facet_grid(. ~ Group, scales = "free_x", space = "free_x") +
  force_panelsizes(cols = c(0.15, 1)) + 
  coord_cartesian(clip = "off")

ggsave("plot.jpeg", scale = 1, dpi = 300)
