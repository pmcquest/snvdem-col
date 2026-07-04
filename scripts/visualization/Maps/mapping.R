# ----- Merge weighted scores w/ geometries to make maps -----

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(readr)
library(gridExtra)
library(magick)
library(gganimate)

colwtd <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted2.rds")

col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)


colmap_data_yearly <- merge(x = colwtd, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data_yearly <- st_as_sf(colmap_data_yearly)

##---- (optional) Create Yearly Maps and Store in List----
# experiment per year:
year_to_plot <- 2017 # Example: Plot for the year 2010
global_min <- min(colmap_data_yearly$sndem_mean, na.rm = TRUE)
global_max <- max(colmap_data_yearly$sndem_mean, na.rm = TRUE)


ggplot() +
  geom_sf(data = colmap_data_yearly %>% filter(year == year_to_plot),
          color = "transparent", linewidth = 0.01, aes(fill = sndem_mean)) +
  theme_void() +
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key = element_rect(fill = "white", color = "lightgray")) +
  scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(global_min, global_max)) +
  labs(title = paste("SN democracy level - Year", year_to_plot)) # Add a title

#---- Faceted for each year ---- 

# 1. Setup
map_directory <- "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/dem-years"
file_prefix <- "wf_dem_"
file_suffix <- "_map.png"
gif_filename <- file.path(map_directory, paste0(file_prefix, "animation.gif"))
grid_filename <- file.path(map_directory, "all_years_map_grid.png")
faceted_filename <- file.path(map_directory, "faceted_map.png")

# 2. Calculate Global Min and Max
global_min <- min(colmap_data_yearly$sndem_mean, na.rm = TRUE)
global_max <- max(colmap_data_yearly$sndem_mean, na.rm = TRUE)

# 3. Create Yearly Maps and Store in List
plot_list <- colmap_data_yearly %>%
  group_by(year) %>%
  group_map(~ {
    current_year <- .y$year
    year_data <- .x
    
    year_plot <- ggplot() +
      geom_sf(data = year_data, color = "transparent", linewidth = 0.01, aes(fill = sndem_mean)) +
      theme_void() +
      theme(panel.background = element_rect(color = "transparent", fill = "white"),
            plot.caption = element_text(size = 12),
            legend.background = element_rect(fill = "white", color = "black"),
            legend.key = element_rect(fill = "white", color = "lightgray")) +
      scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(global_min, global_max)) +
      labs(title = paste("SN democracy level - Year", current_year))
    
    filename <- file.path(map_directory, paste0(file_prefix, current_year, file_suffix))
    ggsave(filename = filename, plot = year_plot, height = 8, width = 10, device = "png", units = "in")
    
    print(paste("Map created and saved for year:", current_year))
    year_plot # Return the plot for the list
  })

names(plot_list) <- paste0("year_", unique(colmap_data_yearly$year))

# 4. Create Faceted Plot and save
faceted_plot <- ggplot() +
  geom_sf(data = colmap_data_yearly, color = "transparent", linewidth = 0.01, aes(fill = sndem_mean)) +
  theme_void() +
  scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(global_min, global_max)) +
  labs(fill = "SN democracy level") +
  facet_wrap(~ year, ncol = 7, nrow = 3) # Adjust ncol and nrow

ggsave(filename = faceted_filename, plot = faceted_plot, height = 10, width = 15, device = "png", units = "in")

# 5. Animated
# Create the base plot
base_plot <- ggplot() +
  geom_sf(data = colmap_data_yearly, color = "transparent", linewidth = 0.01, aes(fill = sndem_mean)) +
  theme_void() +
  scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(global_min, global_max)) +
  labs(fill = "SN democracy level")

# Create the animation
animated_map <- base_plot +
  transition_time(year) +
  labs(title = "SN Democracy Level Over Time: {frame_time}")

# Save the animation as a GIF
animation_filename <- "democracy_map_animation.gif"
animate(animated_map, filename = animation_filename, height = 600, width = 800, units = "px", res = 100, duration = 10, fps = 2) # Adjust height, width, duration, and fps as needed

cat(paste0("GIF animation saved as: ", animation_filename, "\n"))


## ---- All years (average) ----
# Calculate average scores across all years
colmap_data_avg <- colmap_data_yearly %>%
  group_by(MPIO_CDPMP) %>%
  summarize(
    avg_sndem = mean(sndem_mean, na.rm = TRUE),
    avg_emel = mean(emel_score, na.rm = TRUE),
    avg_cscw = mean(cscw_score, na.rm = TRUE)
  )

# Merge averaged data with spatial data
colmap_data_avg <- merge(x = colmap_data_avg, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data_avg <- st_as_sf(colmap_data_avg)

# Democracy scores averaged over all years
ggplot() +
  geom_sf(data = colmap_data_avg, color = "transparent", linewidth = 0.01, aes(fill = avg_MLm_dem)) +
  theme_void() +
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(fill = "SN democracy level (avg)", caption = "Average democracy scores (2000-2020)")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/wf_dem_avg_map.png",
       height = 10, width = 10, device = "png", units = "in")

# emel scores averaged over all years
ggplot() +
  geom_sf(data = colmap_data_avg, color = "transparent", linewidth = 0.01, aes(fill = avg_MLm_emel)) +
  theme_void() +
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(fill = "SN elections level (avg)", caption = "Average elections scores (2000-2020)")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/wf_emel_avg_map.png",
       height = 10, width = 10, device = "png", units = "in")

# cscw scores averaged over all years
ggplot() +
  geom_sf(data = colmap_data_avg, color = "transparent", linewidth = 0.01, aes(fill = avg_MLm_cscw)) +
  theme_void() +
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(fill = "SN civil liberties level (avg)", caption = "Average civil liberties scores (2000-2020)")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/wf_cscw_avg_map.png",
       height = 10, width = 10, device = "png", units = "in")
