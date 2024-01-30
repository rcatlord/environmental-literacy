# Carbon dioxide (CO₂) emissions from fossil fuels and industry, 2022
# Source: Global Carbon Project
# URL: https://globalcarbonatlas.org/emissions/carbon-emissions/

library(tidyverse) ; library(countrycode) ; library(rnaturalearth) ; library(sf) ; library(classInt)

df <- read_csv("data/export_emissions.csv", skip = 1, col_types = cols(.default = "c")) %>% 
  slice(1) %>% 
  rename(Year = ...1) %>% 
  pivot_longer(-Year, names_to = "Country", values_to = "Emissions") %>% 
  mutate(Emissions = as.numeric(Emissions),
         iso_a3 = countrycode(Country, "country.name", "iso3c")) %>% 
  relocate(iso_a3, .after = Country)

sf <- ne_countries(returnclass = "sf") %>%
  select(iso_a3) %>% 
  left_join(df, by = "iso_a3")

classes <- classIntervals(sf$Emissions, n = 5, style = "jenks")

sf <- sf %>%
  mutate(class = factor(cut(Emissions, classes$brks, include.lowest = T),
                        labels = c(
                          paste0(round(classes$brks[1],1), " - ", round(classes$brks[2],1)),
                          paste0(round(classes$brks[2],1), " - ", round(classes$brks[3],1)),
                          paste0(round(classes$brks[3],1), " - ", round(classes$brks[4],1)),
                          paste0(round(classes$brks[4],1), " - ", round(classes$brks[5],1)),
                          paste0(round(classes$brks[5],1), " - ", round(classes$brks[6],1))
                          )))


ggplot() + 
  geom_sf(data = sf, aes(fill = factor(class)), alpha = 0.8, colour = "#ffffff", size = 0.4, show.legend = "point") +
  labs(title = "Oman has double the per capita emissions of China",
       subtitle = "<span style = 'color:#757575;'>Territorial CO<sub>2</sub> emissions per capita, 2022</span>",
       caption = "Source: Global Carbon Project") +
  scale_fill_viridis_d(direction = -1, na.value = "#dddddd", name = expression(bold("t" * CO[2] * "/per person"))) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = unit(rep(0.5,4), "cm"),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_markdown(margin = margin(b = 25)),
        plot.caption = element_text(size = rel(0.8), colour = "#707071", hjust = 0),
        legend.position = "top",
        legend.justification = "left",
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_text(size = rel(0.75), colour = "#414042"),
        legend.text = element_text(size = rel(0.7), colour = "#414042")) +
  guides(fill = guide_legend(nrow = 1, title.position = 'top',
                             override.aes = list(shape = 21, size = 3))) +
  coord_sf(crs = "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0")

ggsave("plot.png", scale = 1, dpi = 300) 
