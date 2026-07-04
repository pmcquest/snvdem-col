

# we could visualize the data by municipality ...
## Maps
library(sf)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)
library(ggplot2)
library(gganimate)
library(gifski)

col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)


colmap_data <- merge(x = td_HRDAG_ym, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data <- st_as_sf(colmap_data)

# plotting homicides over time
animated_map <- ggplot(colmap_data) +
  geom_sf(aes(fill = obs_homi, geometry = geometry), color = NA) +  # Use geometry for map
  scale_fill_viridis_c(option = "inferno", name = "Homicides") +     # Color scale for intensity
  labs(title = "Municipal Homicides Over Time", subtitle = "Year: {frame_time}",
       fill = "Homicide Count") +
  theme_minimal() +
  theme(legend.position = "right") +
  transition_time(year) +          # Set animation to transition by year
  ease_aes('linear')               # Smooth transitions

# Save the animation as a GIF or animation... (not working, maybe due to NAs)
renderer <- gifski_renderer("G:/Shared drives/snvdem/snvdem-col/data/panel/10-11_HRDAG/homicides_over_time.gif")
animate(animated_map, fps = 2, width = 800, height = 600, renderer = gifski_renderer("homicides_over_time.gif"))

renderer_mp4 <- av_renderer("homicides_over_time.mp4")
animate(animated_map, fps = 2, width = 800, height = 600, renderer = renderer_mp4)

colmap_data_1985 <- colmap_data %>%
  filter(year == 1985)

# Plot for a single year
static_map <- ggplot(colmap_data_1985) +
  geom_sf(aes(fill = obs_homi, geometry = geometry), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Homicides") +
  labs(title = "Municipal Homicides in 1985",
       fill = "Homicide Count") +
  theme_minimal() +
  theme(legend.position = "right")

# Display the static map
print(static_map)

