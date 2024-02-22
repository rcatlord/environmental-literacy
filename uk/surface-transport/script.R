# Surface transport emissions by mode, 1990 - 2022
# Source: DESNZ
# URL: https://www.gov.uk/government/statistics/final-uk-greenhouse-gas-emissions-national-statistics-1990-to-2022

library(tidyverse) ; library(httr) ; library(readxl) ; library(ggrepel) ; library(ggtext)

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/media/65c0d17663a23d0013c821ea/final-greenhouse-gas-emissions-tables-2022.xlsx", write_disk(tmp))
df <- read_xlsx(tmp, sheet = "1.2", skip = 6) %>% 
  fill(`TES Subsector`) %>% 
  filter(`TES Subsector` %in% c("Road","Railways")) %>% 
  mutate(Mode = case_when(
    `TES Category` == "Passenger cars" ~ "Cars",
    `TES Category` == "Light duty vehicles" ~ "Vans",
    `TES Category` == "HGVs" ~ "HGVs",
    `TES Category` == "Buses" ~ "Buses and coaches",
    `TES Category` %in% c("Railways - mobile combustion","Railways - stationary combustion") ~ "Rail",
    TRUE ~ "Other"
    )) %>% 
  select(!starts_with("TES")) %>% 
  pivot_longer(-Mode, names_to = "Year", values_to = "Value") %>% 
  group_by(Year, Mode) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Year = ymd(Year, truncated = 2L),
         Mode = fct_rev(fct_relevel(Mode, "Cars","HGVs","Vans","Buses and coaches","Rail","Other")))

ggplot(df, aes(Year, Value, group = Mode, fill = Mode)) +
  geom_area(linewidth = 1.5, key_glyph = "point") +
  annotate("text", x = as.Date("1990-01-01"), y = 155, label = expression("Mt" * CO[2] * "e"), size = 3, hjust = 1) +
  scale_fill_manual(values = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")) +
  scale_x_date(expand = expansion(), 
               breaks = c(as.Date(ymd("1990-01-01")),
                          as.Date(ymd("2000-01-01")),
                          as.Date(ymd("2000-01-01")),
                          as.Date(ymd("2010-01-01")),
                          as.Date(ymd("2020-01-01")),
                          as.Date(ymd("2022-01-01"))), 
               date_labels = "%Y") + 
  scale_y_continuous(expand = expansion(), breaks = seq(0,140,20)) +
  labs(x = NULL, y = NULL,
       title = "Cars emits the majority of emissions",
       subtitle = "<span style = 'color:#757575;'>Surface transport emissions, UK, 1990-2022</span>",
       caption = "Source: DESNZ",
       fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5,4), "cm"),
        panel.ontop = TRUE,
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "#000000"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0, margin = margin(t = 15)),
        legend.position = "right",
        legend.justification = "left") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 5)))

ggsave("plot.jpeg", scale = 1, dpi = 300)
