
#---- Remoteness ----

##---- Criteria measured ----
# 13: Areas that are remote (difficult to reach by available transportation, for example). (0=No, 1=Yes) [v2*_13] 


##---- Data Sources ----
# Data 1: Distance to Market (CEDE--see script for df01)
# Data 2: OpenStreetMaps


# Note: M. Sisk provided a measure based on OpenStreetMaps but only for 2018

#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)


##---- 13 Remoteness ----

###---- Road density (2018) ----
df07 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/geospatial/13_Remote/roads_updated.csv")
df07 <- df07 %>%
  rename(nAllRds_13 = 3, lAllRds_13 = 4, lMjRds_13 = 5, nMjRds_13 = 6) %>%
  select(1|3:9) %>%
  mutate(year = 2018)

# Rescale to KM per KM2
df07 <- df07 %>%
  mutate(road_density_km = m_per_m2_roads * 1000)

# Summary to check the new distribution
summary(df07$road_density_km)




##----- Completeness ----


# Check for NA's 
na_counts <- colSums(is.na(df07))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))


#---- Save cleaned dataset ----
write_rds(df07, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df07_clean.rds")


# Visualizations
library(ggplot2)

# Distribution of Road Density
ggplot(df07, aes(x = road_density_km)) +
  geom_histogram(fill = "#2c7fb8", color = "white", bins = 50) +
  # Add a vertical line for the median to see the 'typical' remoteness
  geom_vline(aes(xintercept = median(road_density_km, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Statistical Variation in Road Density",
       subtitle = "Red line represents the median density; values to the left are more 'remote'",
       x = "Kilometers of Road per Square kilometer (km/km2)",
       y = "Number of Municipalities")


library(sf)
library(tidyverse)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%  
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# 1. Ensure the ID columns match (DANE codes are often characters)
df07$MPIO_CDPMP <- as.character(df07$MPIO_CDPMP)
muni_geo$MPIO_CDPMP <- as.character(muni_geo$MPIO_CDPMP)

# 2. Join the road data to the map
muni_roads_map <- muni_geo %>%
  left_join(df07, by = "MPIO_CDPMP")

# 3. Plot the Map
ggplot(data = muni_roads_map) +
  geom_sf(aes(fill = road_density_km), color = "white", size = 0.01) +
  # Using 'magma' scale: Yellow/White = High Density, Dark Purple = Remote/Low Density
  scale_fill_viridis_c(option = "magma", 
                       name = "Road Density\n(km/km2)",
                       na.value = "grey90",
                       trans = "sqrt") + # Square root transformation helps see variation in low-density areas
  theme_void() +
  labs(title = "Spatial Variation of Remoteness in Colombia",
       subtitle = "Darker regions indicate lower road density (higher remoteness)")


# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))
