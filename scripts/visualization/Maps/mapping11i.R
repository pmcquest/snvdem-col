# ----- Merge weighted scores w/ geometries to make maps -----

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(readr)
library(gridExtra)
library(magick)

colwtd <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted11i.rds")
col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)

colmap_data_yearly <- merge(x = colwtd, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data_yearly <- st_as_sf(colmap_data_yearly)


##---- Faceted for each year ---- 


# 1. Setup
map_directory <- "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/dem-years/11i"
file_prefix <- "wf_dem_"
file_suffix <- "_map.png"
gif_filename <- file.path(map_directory, paste0(file_prefix, "animation.gif"))
grid_filename <- file.path(map_directory, "all_years_map_grid.png")

# 2. Calculate Global Min and Max
global_min <- min(colmap_data_yearly$sndem_mean, na.rm = TRUE)
global_max <- max(colmap_data_yearly$sndem_mean, na_rm = TRUE)

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
      labs(fill = "SN democracy level") +
      geom_label(
        data = data.frame(x = mean(st_bbox(year_data)[c(1, 3)]), y = max(st_bbox(year_data)[c(2, 4)]), label = paste("SN-VDem Col. - Year", current_year)),
        aes(x = x, y = y, label = label),
        hjust = 0.5, vjust = 1, fill = "white", color = "black", label.size = 0, size = 4, fontface = "bold"
      )
    
    filename <- file.path(map_directory, paste0(file_prefix, current_year, file_suffix))
    ggsave(filename = filename, plot = year_plot, height = 8, width = 10, device = "png", units = "in")
    
    print(paste("Map created for year:", current_year))
    year_plot # Return the plot for the list
  })

names(plot_list) <- paste0("year_", unique(colmap_data_yearly$year))

# 4. Create GIF
map_files <- list.files(path = map_directory, pattern = paste0(file_prefix, "\\d{4}", file_suffix), full.names = TRUE)
map_files <- sort(map_files)
gif_images <- image_read(map_files)
gif_animation <- image_animate(gif_images, fps = 1)
image_write(gif_animation, gif_filename)

# 5. Create Grid Plot
grid_plot <- grid.arrange(grobs = plot_list, ncol = 4)
ggsave(filename = grid_filename, plot = grid_plot, height = 12, width = 16, device = "png", units = "in")




## ---- All years (average) ----
# Calculate average scores across all years
colmap_data_avg <- FAcol_yearly %>%
  group_by(MPIO_CDPMP) %>%
  summarize(
    avg_MLm_dem = mean(MLm_dem, na.rm = TRUE),
    avg_MLm_emel = mean(MLm_emel, na.rm = TRUE),
    avg_MLm_cscw = mean(MLm_cscw, na.rm = TRUE)
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
