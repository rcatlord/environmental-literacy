# Emissions by sector
# Source: DESNZ
# URL: https://www.gov.uk/government/statistics/final-uk-greenhouse-gas-emissions-national-statistics-1990-to-2022

library(tidyverse) ; library(httr) ; library(readxl) ; library(ggrepel) ; library(ggtext)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/media/65c0d17663a23d0013c821ea/final-greenhouse-gas-emissions-tables-2022.xlsx", write_disk(tmp))
df <- read_xlsx(tmp, sheet = "1.2", skip = 6) %>% 
  filter(`TES Category` == "Total") %>% 
  mutate(Sector = str_remove_all(`TES Sector`, " total")) %>% 
  select(-c(1,2,3)) %>% 
  pivot_longer(-Sector, names_to = "Year", values_to = "Value") %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         place = if_else(Sector == "Industry", 0, 1))

ggplot(df, aes(x = Year, y = Value)) +
  geom_line(aes(color = Sector), linewidth = 1) +
  geom_text(data = filter(df, Year == max(Year)),
            aes(label = Sector, colour = Sector, vjust = place),
            hjust = 0, nudge_y = 3, fontface = "bold") +
  scale_x_date(expand = expansion(), date_breaks = "5 years", date_labels = "%Y") + 
  scale_y_continuous(expand = expansion()) +
  scale_colour_manual(values = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")) +
  labs(x = NULL, y = NULL,
       title = "Transport and agriculture have changed little",
       subtitle = "<span style = 'color:#757575;'>Territorial UK greenhouse gas emissions by sector, 1990-2022</span>",
       caption = "Source: DESNZ") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(0.5,5.5,0.5,0.5,"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0, margin = margin(t = 15)),
        legend.position = "none") +
  coord_cartesian(clip = "off") 

ggsave("plot.jpeg", dpi = 300, scale = 1)
