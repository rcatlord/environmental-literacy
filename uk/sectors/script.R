# GHG emissions by sector, UK, 2022
# Source: DESNZ
# URL: https://www.gov.uk/government/statistics/final-uk-greenhouse-gas-emissions-national-statistics-1990-to-2022

library(tidyverse) ; library(treemapify) ; library(ggtext)

df <- tribble(
  ~Sector, ~Value,
  "Domestic transport", 28,
  "Buildings and product uses", 20,
  "Industry", 14,
  "Electricity supply", 14,
  "Agriculture", 12,
  "Fuel supply", 8,
  "Waste", 5
)

ggplot(df, aes(area = Value, fill = Sector, 
               label = paste0(Sector,"\n", Value, "%"))) + 
  geom_treemap(layout = "squarified", colour = "#FFFFFF") +
  geom_treemap_text(place = "topleft", colour = "#FFFFFF", fontface = "bold", size = 12, reflow = T) +
  scale_fill_manual(values = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")) +
  labs(x = NULL, y = NULL,
       title = "Domestic transport responsible for over a quarter of emissions",
       subtitle = "<span style = 'color:#757575;'>Territorial UK greenhouse gas emissions by sector, 2022</span>",
       caption = "Source: DESNZ") +
  theme(plot.margin = unit(rep(0.5,4), "cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10), size = 12),
        legend.position = "none")

ggsave("plot.jpeg", dpi = 300, scale = 1)
