# Climate anxiety
# Source: Hickman et al, 2021
# URL: https://doi.org/10.1016/S2542-5196(21)00278-3

library(tidyverse) ; library(ggtext)

df <- read_csv("data/data.csv") %>% 
  mutate(Country = fct_relevel(Country, "Philippines","India","Portugal","Brazil","Australia","Nigeria","UK","USA","France","Finland"),
         Response = fct_relevel(Response, c("Extremely", "Very", "Moderately", "A little", "Not worried")),
         Background = 100)
  
ggplot(df, aes(x = fct_rev(Country), y = Value)) +
  geom_col(aes(x = fct_rev(Country), y = Background), fill = "#E5E6E7", width = 0.9) +
  geom_col(aes(fill = Response), width = 0.9) +
  geom_text(aes(label = paste0(Value, "%"), hjust = ifelse(Value > 30, 1.15, -0.1)), 
            colour = ifelse(df$Value > 30, "#FFFFFF", "#000000"), fontface = "bold") +
  scale_fill_manual(values = c("#EF2F22","#F38160","#FFDC01","#CCDFB7","#B198C7")) +
  facet_wrap(~Response, nrow = 1) +
  coord_flip() +
  labs(title = "'I am worried that climate change threatens people and the planet'",
       subtitle = "<span style = 'color:#757575;'>Proportion of respondents, 18 May - 7 June 2021</span>",
       tag = "Based on responses from 10,000 participants ages 16–26",
       caption = "Source: Hickman et al. (2021)") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Open Sans"),
        plot.margin = unit(rep(0.5,4),"cm"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_markdown(size = 14, margin = margin(b = 15)),
        plot.tag = element_text(size = 10),
        plot.tag.position = c(0.25, 0.08),
        plot.caption = element_text(size = 12, colour = "grey60", margin = margin(t = 35)),
        strip.text.x = element_text(size = 12, face = "bold", hjust = 0, vjust = -0.15))

ggsave("plot.jpeg", dpi = 300, scale = 1)
