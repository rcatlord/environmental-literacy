# Cumulative CO₂ emissions, 1750-2022
# Source: Our World in Data
# URL: https://ourworldindata.org/co2-and-greenhouse-gas-emissions

library(tidyverse) ; library(ggtext)

df <- read_csv("data/cumulative-co-emissions.csv") %>% 
  filter(Year == 2022, !is.na(Code), Entity != "World") %>% 
  rename(Value = `Cumulative CO₂ emissions`) %>% 
  mutate(Value = round(Value/1000000000,0)) %>% # GtCO₂
  arrange(desc(Value)) %>% 
  slice(1:10) %>% 
  mutate(label = case_when(Entity == "United States" ~ paste(" ", Value, "GtCO<sub>2</sub> "), TRUE ~ as.character(Value)),
         place = if_else(row_number() == 1, 0, 1))

ggplot(df, aes(Value, fct_reorder(Entity, Value))) +
  geom_col(fill = ifelse(df$Entity == "United States", "#c51b8a", "gray70"), 
           show.legend = FALSE) +
  geom_richtext(aes(label = label, hjust = place), 
                color = "#000000", fontface = "bold", 
                fill = NA, label.color = NA) +
  scale_x_continuous(expand = expansion()) +
  labs(x = NULL, y = NULL,
       title = "The USA has contributed the most emissions",
       subtitle = "<span style = 'color:#757575;'>Cumulative CO<sub>2</sub> emissions, 1750-2022</span>",
       caption = "Source: Our World in Data") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(c(0.5,3,0.5,0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0)) +
  coord_cartesian(clip = "off")

ggsave("plot.png", scale = 1, dpi = 300) 
