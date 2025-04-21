library(tidyverse)
library(sf)
library(geojsonsf)
library(matrixStats)
library(ggpubr)


# Map of redlining in Detroit
p_redlined_areas <- mi_redlining %>%
  filter(city == "Detroit") %>%
  # Correct ordering for categories
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Industrial and Commercial")
  )) %>%
  # filter out industrial and commercial zones
  filter(category != "Industrial and Commercial") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # legend.position = "bottom"
  ) +
  labs(title = "Detroit (Redlined areas)",
       fill = "HOLC (Redlining Category)")

p_redlined_areas

# Map of redlining in Detroit (based on census tracts)
p_redlined_tracts <- mi_2018 %>%
  left_join(
    mi_redlining_tracts,
    by = c("GEOID"="geoid")
  ) %>%
  filter(!is.na(category)) %>%
  filter(city == "Detroit") %>%
  # filter(GEOID == "26081000200") %>%
  # Correct ordering for categories
  mutate(category = factor(
    category,
    levels = c("Best", "Still Desirable", "Definitely Declining", "Hazardous", "Industrial and Commercial")
  )) %>%
  # filter out industrial and commercial zones
  filter(category != "Industrial and Commercial") %>%
  ggplot() +
  geom_sf(aes(fill = category)) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_bw() + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Detroit (Redlined census tracts)",
       fill = "HOLC (Redlining Category)")


ggarrange(
  p_redlined_areas, p_redlined_tracts, 
  ncol = 2, 
  common.legend = TRUE,
  legend = "bottom")

ggsave("redlined_maps_plot.png", units = "in", width = 6.5, height = 3)


