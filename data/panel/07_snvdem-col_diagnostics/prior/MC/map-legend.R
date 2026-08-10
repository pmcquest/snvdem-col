# Map with legend

# --- 0. LOAD LIBRARIES ---
library(sf)
library(tidyverse)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)

# --- 1. DATA PROCESSING ---
# Load panel data
master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")

# Data Cleaning and Normalization
master_clean <- master_df %>%
  filter(!is.na(snelect) & !is.na(sncivlib) & !is.na(sndem)) %>%
  mutate(
    snelect_norm = (snelect - min(snelect)) / (max(snelect) - min(snelect)),
    sncivlib_norm = (sncivlib - min(sncivlib)) / (max(sncivlib) - min(sncivlib)),
    sndem_norm    = (sndem - min(sndem)) / (max(sndem) - min(sndem))
  )

# Function to pad MPIO codes
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

# Load Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# Ensure ID columns are character for join
master_clean$MPIO_CDPMP <- as.character(master_clean$MPIO_CDPMP)
muni_geo$MPIO_CDPMP <- as.character(muni_geo$MPIO_CDPMP)

# Join democracy scores to spatial data
years_to_map <- c(2000, 2006, 2012, 2018, 2023)
map_data <- muni_geo %>%
  left_join(master_clean %>% filter(year %in% years_to_map), by = "MPIO_CDPMP")

# --- 2. GET CONTEXTUAL DATA ---
world <- ne_countries(scale = "medium", returnclass = "sf")
neighbors <- world %>% filter(continent == "South America" | name %in% c("Panama"))
col_departments <- ne_states(country = "Colombia", returnclass = "sf")

# --- 3. THE STYLIZED CONTEXT MAP (Adjusted Labels) ---
stylized_context_map <- ggplot() +
  # Ocean background
  geom_rect(aes(xmin = -82, xmax = -66, ymin = -6, ymax = 14), fill = "#e0f3ff") +
  # Neighbor countries (Light Green)
  geom_sf(data = neighbors, fill = "#d5e8d4", color = "grey70") + 
  # Colombia Departments (White)
  geom_sf(data = col_departments, fill = "white", color = "grey40", linewidth = 0.2) + 
  
  # City labels with pointer lines (ggrepel)
  geom_label_repel(
    data = data.frame(
      name = c("Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Bucaramanga"),
      x = c(-74.07, -75.56, -76.52, -74.78, -75.48, -73.12),
      y = c(4.71, 6.25, 3.45, 10.96, 10.40, 7.12)
    ),
    aes(x = x, y = y, label = name),
    size = 2.8,
    fontface = "bold",
    box.padding = 0.6, 
    point.padding = 0.3,
    segment.color = "grey30",
    min.segment.length = 0,
    fill = alpha("white", 0.8)
  ) +
  
  # Geographic Features and Neighbor Labels
  annotate("text", x = -78, y = 12.5, label = "Caribbean Sea", size = 3, fontface = "italic", color = "#4a90e2") +
  annotate("text", x = -80.5, y = 4, label = "Pacific Ocean", size = 3, fontface = "italic", angle = 90, color = "#4a90e2") +
  
  # ADJUSTED COORDINATES BELOW:
  annotate("text", x = -70, y = 10, label = "VENEZUELA", size = 3.2, color = "grey40", fontface = "bold") +
  annotate("text", x = -68.5, y = -3, label = "BRAZIL", size = 3.2, color = "grey40", fontface = "bold") + # Moved East and South
  annotate("text", x = -78.5, y = -1.5, label = "ECUADOR", size = 3.2, color = "grey40", fontface = "bold") + # Moved West and South
  annotate("text", x = -74.5, y = -5, label = "PERU", size = 3.2, color = "grey40", fontface = "bold") + # Nudged for balance
  annotate("text", x = -79.5, y = 8.8, label = "PANAMA", size = 2.8, color = "grey40", fontface = "bold") +
  
  coord_sf(xlim = c(-82, -66), ylim = c(-6, 14), expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(colour = "grey20", fill = NA, size = 0.5))

# --- 4. THE FACETED TIME-SERIES MAPS ---
facet_maps_list <- lapply(years_to_map, function(y) {
  ggplot(data = map_data %>% filter(year == y)) +
    # Background rectangle to simulate sea color in facets
    geom_rect(aes(xmin = -79.5, xmax = -66.5, ymin = -4.5, ymax = 12.5), fill = "#f0f8ff") +
    geom_sf(data = neighbors, fill = "#f2f9f2", color = "grey90", linewidth = 0.05) + 
    geom_sf(aes(fill = sndem_norm), color = NA) +
    scale_fill_viridis_c(
      option = "magma",
      name = "Democracy\nScore",
      na.value = "grey80",
      limits = c(0, 1),
      guide = guide_colorbar(barheight = unit(3, "cm"), ticks = FALSE)
    ) +
    coord_sf(xlim = c(-79.5, -66.5), ylim = c(-4.5, 12.5), expand = FALSE) +
    labs(title = as.character(y)) +
    theme_void() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
      legend.position = "none" # Temporarily remove to collect later
    )
})

# --- 5. ASSEMBLE WITH PATCHWORK ---
# Use area() to ensure rectangular patches and avoid the layout error
layout_stable <- c(
  area(1, 1), # Context Map
  area(1, 2), # 2000
  area(1, 3), # 2006
  area(2, 1), # 2012
  area(2, 2), # 2018
  area(2, 3)  # 2023
)

# Combine using patchwork
final_visual <- stylized_context_map + 
  facet_maps_list[[1]] + # 2000
  facet_maps_list[[2]] + # 2006
  facet_maps_list[[3]] + # 2012
  facet_maps_list[[4]] + # 2018
  facet_maps_list[[5]] + # 2023
  plot_layout(design = layout_stable, guides = 'collect') + 
  plot_annotation(
    title = "Figure 4: Spatial Evolution of Subnational Democracy in Colombia (2000-2023)",
    subtitle = "Normalized sndem Index (0 = Low, 1 = High)",
    caption = "Source: SNDEM 2024."
  ) & theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 12),
    legend.position = "right"
  )

# --- 6. EXPORT ---
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/SNDEM_map_legend4.png", final_visual, width = 14, height = 10, dpi = 300, bg = "white")
