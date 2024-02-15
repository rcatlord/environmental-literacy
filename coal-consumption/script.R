# Coal consumption
# Source: Energy Institute
# URL: https://www.energyinst.org/statistical-review

library(tidyverse) ; library(httr) ; library(readxl) ; library(geomtextpath) ; library(ggtext)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.energyinst.org/__data/assets/excel_doc/0007/1055545/EI-stats-review-all-data.xlsx", write_disk(tmp))

coal <- read_xlsx(tmp, sheet = "Coal Consumption - EJ", range = "A3:BG111") %>% 
  slice(108) %>% 
  pivot_longer(-Exajoules, names_to = "Year", values_to = "Coal") %>% 
  select(-1)

oil <- read_xlsx(tmp, sheet = "Oil Consumption - EJ", range = "A3:BG111") %>% 
  slice(108) %>% 
  pivot_longer(-Exajoules, names_to = "Year", values_to = "Oil") %>% 
  select(-1)

gas <- read_xlsx(tmp, sheet = "Gas Consumption - EJ", range = "A3:BG111") %>% 
  slice(108) %>% 
  pivot_longer(-Exajoules, names_to = "Year", values_to = "Gas") %>% 
  select(-1)

solar <- read_xlsx(tmp, sheet = "Solar Consumption - EJ", range = "A3:BG111") %>% 
  slice(108) %>% 
  pivot_longer(-`Exajoules (input-equivalent)`, names_to = "Year", values_to = "Solar") %>% 
  select(-1)

wind <- read_xlsx(tmp, sheet = "Wind Consumption - EJ", range = "A3:BG111") %>% 
  slice(108) %>% 
  pivot_longer(-`Exajoules (input-equivalent)`, names_to = "Year", values_to = "Wind") %>% 
  select(-1)

df <- left_join(coal, oil, by = "Year") %>% 
  left_join(gas, by = "Year") %>% 
  left_join(solar, by = "Year") %>% 
  left_join(wind, by = "Year") %>% 
  mutate(`Solar and wind` = Solar + Wind,
         Year = ymd(Year, truncated = 2L)) %>% 
  select(-c(Solar, Wind)) %>% 
  pivot_longer(-Year, names_to = "Source", values_to = "Value") 

ggplot(df, aes(Year, Value, group = Source, colour = Source)) +
  geom_line(linewidth = 1.5) +
  geom_textline(aes(x = Year, y = Value, group = Source, colour = Source, label = Source),
                vjust = -0.5, hjust = 0.5) +
  annotate("text", x = as.Date("1965-01-01"), y = 210, label = "Exajoules", size = 3, hjust = 0.7) +
  scale_x_date(expand = expansion(), date_breaks = "5 years", date_labels = "%Y") + 
  scale_colour_manual(values = c("Oil" = "#000000", "Coal" = "#D55E00", "Gas" = "#E69F00", 
                                 "Solar and wind" = "#0072B2"), guide = "none") +
  scale_y_continuous(expand = expansion()) +
  labs(x = NULL, y = NULL,
       title = "Global coal consumption is near record levels",
       subtitle = "<span style = 'color:#757575;'>Energy consumption, 1965-2022</span>",
       caption = "Source: Energy Institute") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5,4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0, margin = margin(t = 15))) +
  coord_cartesian(clip = "off") 

ggsave("plot.jpeg", scale = 1, dpi = 300) 
