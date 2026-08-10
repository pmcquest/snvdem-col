# Visualizations of SNVDEM index (Colombia, 2000-2023)

## SNVDEM Panel (2000-2023)----
#master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")
master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")



# Load necessary libraries
library(tidyverse)
library(ggridges)
library(viridis)

# 1. Data Cleaning and Normalization
# Dropping NAs and scaling indices from 0 to 1
master_clean <- master_df %>%
  filter(!is.na(snelect) & !is.na(sncivlib) & !is.na(sndem)) %>%
  mutate(
    snelect_norm = (snelect - min(snelect)) / (max(snelect) - min(snelect)),
    sncivlib_norm = (sncivlib - min(sncivlib)) / (max(sncivlib) - min(sncivlib)),
    sndem_norm    = (sndem - min(sndem)) / (max(sndem) - min(sndem))
  )


# Plots ----
### A. National Averages Over Time
# This shows the high-level trajectory of Colombian subnational democracy.
nat_avg_plot <- master_clean %>%
  group_by(year) %>%
  summarise(
    Electoral = mean(snelect_norm),
    CivilLib = mean(sncivlib_norm),
    Overall = mean(sndem_norm)
  ) %>%
  pivot_longer(cols = -year, names_to = "Index", values_to = "Score")

ggplot(nat_avg_plot, aes(x = year, y = Score, color = Index, linetype = Index)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("Electoral" = "#2c7bb6", "CivilLib" = "#d7191c", "Overall" = "black")) +
  labs(title = "National Mean Democracy Scores (2000-2023)",
       subtitle = "Normalized Scores (0-1)",
       y = "Mean Score", x = "Year") +
  theme_minimal()


  
  ### B. Ridge Plot (Distribution Evolution)
  # This visualizes how the distribution of municipalities shifts and changes shape over time.
ggplot(master_clean, aes(x = sndem_norm, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Score", option = "C") +
  labs(title = "Distribution of Municipality Democracy Scores (sndem)",
       subtitle = "Shift in density patterns from 2000 to 2023",
       x = "Normalized sndem Score", y = "Year") +
  theme_ridges()


  
### C. Decile Plot
# This helps identify if the 'gap' between the highest and lowest scoring municipalities is widening.
master_decile <- master_clean %>%
  group_by(year) %>%
  mutate(decile = ntile(sndem_norm, 10)) %>%
  group_by(year, decile) %>%
  summarise(mean_score = mean(sndem_norm), .groups = 'drop')

ggplot(master_decile, aes(x = year, y = mean_score, group = decile, color = as.factor(decile))) +
  geom_line(alpha = 0.7, size = 1) +
  scale_color_viridis_d(option = "D", direction = -1) +
  labs(title = "Democracy Scores by Municipality Decile",
       subtitle = "Tracking inequality between the top 10% and bottom 10%",
       y = "Mean sndem (Normalized)", x = "Year", color = "Decile") +
  theme_minimal()


  
### D. Jittered Scatter Plot
# This allows you to see individual municipality 'points' to detect outliers or clusters.
ggplot(master_clean, aes(x = year, y = sndem_norm)) +
  geom_jitter(alpha = 0.1, color = "steelblue", width = 0.2) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Distribution of Individual Municipalities on sndem",
       subtitle = "Each dot represents one municipality; red line shows local regression trend",
       y = "Normalized Score", x = "Year") +
  theme_light()


# Maps ----
library(sf)
library(viridis)
library(ggplot2)
library(dplyr)
# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
## Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))



# 1. Prepare the Data
# Ensure the ID columns (MPIO_CDPMP) are the same type (e.g., character)
master_clean$MPIO_CDPMP <- as.character(master_clean$MPIO_CDPMP)
muni_geo$MPIO_CDPMP <- as.character(muni_geo$MPIO_CDPMP)

# Join the democracy scores to the spatial data
# We filter for specific intervals to make the facet map readable
years_to_map <- c(2000, 2006, 2012, 2018, 2023)

map_data <- muni_geo %>%
  left_join(master_clean %>% filter(year %in% years_to_map), 
            by = "MPIO_CDPMP")

# 2. Create the Facet Map
ggplot(data = map_data) +
  geom_sf(aes(fill = sndem_norm), color = NA) + # 'color = NA' removes municipality borders for clarity
  scale_fill_viridis_c(
    option = "magma", 
    name = "Democracy\nScore",
    na.value = "grey80",
    limits = c(0, 1)
  ) +
  facet_wrap(~year, ncol = 3) +
  labs(
    title = "Spatial Evolution of Subnational Democracy in Colombia",
    subtitle = "Normalized sndem Index (0 = Low, 1 = High)",
    caption = "Grey areas indicate missing data"
  ) +
  theme_void() + # Clean background for maps
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )


# Save the output
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/map-faceted.png", width = 12, height = 14, dpi = 300)


## Geographic orientations ----
# How does 'Directionality' correlate with Democracy

library(reshape2)
library(fmsb)

# 1. Select the verification sample (Replace with actual MPIO names or codes in your data)
# Note: Ensure these names match your master_df exactly
sample_munis <- master_clean %>%
  filter(year == 2023) %>%
  filter(MPIO_CDPMP %in% c("44001", "91001", "99001", "52835")) %>% # Riohacha, Leticia, P.Carreño, Tumaco
  select(MPIO_CDPMP, North=avg6, South=avg7, West=avg8, East=avg9)

# 2. Reshape for Plotting
# Radar charts in R (fmsb) require a specific format:
# Row 1: Max values, Row 2: Min values, Rows 3+: Data
plot_data <- sample_munis %>% select(-MPIO_CDPMP)
plot_data <- rbind(rep(1,4) , rep(0,4) , plot_data)

# 3. Plotting
par(mfrow=c(2,2)) # 2x2 grid
for(i in 1:4){
  radarchart(plot_data[c(1,2,i+2),], 
             title=paste("Muni ID:", sample_munis$MPIO_CDPMP[i]),
             pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8)
}

#Avgs are inverted...

# 1. Invert the variables to match geographic intuition
master_corrected <- master_clean %>%
  mutate(
    true_north = max(avg6) - avg6,
    true_south = max(avg7) - avg7,
    true_west  = max(avg8) - avg8,
    true_east  = max(avg9) - avg9
  )

# 2. Map a "Geographic Bias" check
# This creates a color gradient from North to South using the corrected values
ggplot(muni_geo %>% left_join(master_corrected %>% filter(year == 2023), by = "MPIO_CDPMP")) +
  geom_sf(aes(fill = true_north), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Corrected North-ness") +
  labs(title = "Validation Map: Corrected North Variable",
       subtitle = "High values (yellow) should now be in the Caribbean coast") +
  theme_void()


# Define 'Remote' vs 'Central' based on your verified gradients
master_remote <- master_clean %>%
  mutate(
    Centrality = (avg6 + avg7 + avg8 + avg9),
    Location_Type = ifelse(Centrality > quantile(Centrality, 0.75), "Central/Core", "Peripheral/Frontier")
  )

# Ridge Plot comparing Core vs Frontier over time
ggplot(master_remote, aes(x = sndem_norm, y = as.factor(year), fill = Location_Type)) +
  geom_density_ridges(alpha = 0.6, scale = 1.5) +
  scale_fill_manual(values = c("Central/Core" = "#0072B2", "Peripheral/Frontier" = "#D55E00")) +
  labs(title = "Democracy Distributions: Core vs. Frontier",
       subtitle = "Does the democratic gap close over 2000-2023?",
       x = "Normalized sndem", y = "Year") +
  theme_ridges()

# Select specific years for a clean temporal comparison
map_years <- c(2000, 2008, 2016, 2023)

muni_geo %>%
  left_join(master_clean %>% filter(year %in% map_years), by = "MPIO_CDPMP") %>%
  ggplot() +
  geom_sf(aes(fill = sndem_norm), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Democracy Score") +
  facet_wrap(~year) +
  labs(title = "Subnational Democracy in Colombia (2000-2023)",
       subtitle = "Spatial progression across municipalities") +
  theme_void()

# Decile
master_decile_geo <- master_corrected %>%
  mutate(dist_decile = ntile(true_north + true_south + true_east + true_west, 10)) %>%
  group_by(year, dist_decile) %>%
  summarise(mean_dem = mean(sndem_norm, na.rm = TRUE), .groups = "drop")

ggplot(master_decile_geo, aes(x = year, y = mean_dem, color = as.factor(dist_decile), group = dist_decile)) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Centrality Decile\n(10 = Most Central)") +
  labs(title = "Democracy Growth by Geographic Centrality",
       y = "Mean sndem", x = "Year") +
  theme_minimal()
