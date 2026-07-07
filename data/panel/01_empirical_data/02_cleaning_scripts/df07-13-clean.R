
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
# roads_updated.csv is an external GIS output (per the header note above, a
# measure M. Sisk built from OpenStreetMaps) -- no generating script for it
# lives in this repo, so its 9 raw columns (MPIO_CDPMP, AREA, n, total.len,
# total.roads, total.major, m_per_m2_all, m_per_m2_roads, m_per_m2_major)
# can only be understood by inspecting the values themselves, which is what
# the renames below are based on.
df07 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/geospatial/13_Remote/roads_updated.csv")
df07 <- df07 %>%
  # Verified 2026-07-05 what these actually are, since the raw names are
  # ambiguous: `n` (position 3) is a road-segment count (values in the
  # hundreds/thousands); `total.len`/`total.roads`/`total.major`
  # (positions 4-6) parallel `m_per_m2_all`/`m_per_m2_roads`/
  # `m_per_m2_major` (confirmed total.X / AREA == m_per_m2_X exactly, for
  # all three X). So `total.len` = length of ALL road-like features,
  # `total.roads` = length of a narrower "roads" category (excludes
  # whatever `total.len` includes that isn't a proper road, e.g. paths/
  # tracks), and `total.major` is *supposed to* be length of major/
  # arterial roads only.
  #
  # BUG in the raw file, not this script: `total.major` is exactly
  # identical, row for row, to `n` (confirmed with identical()) -- i.e. the
  # "major roads length" column is actually just a copy of the segment
  # count, not a length at all (real major-road lengths would be in the
  # same millions-of-meters range as total.len/total.roads, not the
  # hundreds/thousands range `n` is in). That makes `m_per_m2_major`
  # (= total.major / AREA) meaningless as a density figure. The renames
  # below inherit this: `lMjRds_13` ("length major roads") is actually
  # `total.roads`, the mid-tier category, not majors; `nMjRds_13` is
  # actually the broken `total.major` column. Harmless for this script's
  # own output (road_density_km, below, is built from m_per_m2_roads, which
  # is fine) but anyone using lMjRds_13/nMjRds_13/m_per_m2_major downstream
  # would be using mislabeled/meaningless data -- flagging rather than
  # silently renaming around it, since the fix belongs upstream in
  # roads_updated.csv, which isn't generated in this repo.
  rename(nAllRds_13 = 3, lAllRds_13 = 4, lMjRds_13 = 5, nMjRds_13 = 6) %>%
  select(1|3:9) %>%
  mutate(year = 2018)

# Rescale to KM per KM2
# m_per_m2_roads is meters of road per square meter of area (m/m^2). To get
# km per km^2: divide the numerator by 1000 (m -> km) and the denominator by
# 1e6 (m^2 -> km^2), i.e. multiply by 1e6/1000 = 1000. Verified 2026-07-05
# this produces a realistic range (median ~0.91, max ~17.6 km/km^2 after the
# summary() check just below) -- confirms *1000 is the correct factor, not
# a typo for *1000000 or /1000.
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
# Face-validity check on road_density_km before saving: a right-skewed
# histogram with most municipalities clustered at low density and a long
# tail of well-connected/urban ones is the expected shape for this measure;
# the median line makes it easy to eyeball whether that holds.
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
# clean_mpio pads numeric-looking MPIO codes back to 5 digits with a leading
# zero -- needed because the shapefile's own MPIO_CDPMP column comes in as
# numeric/lost-leading-zero, the same class of code-formatting issue df01/
# df05 handle for their own Excel sources, just applied to a shapefile here.
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# 1. Ensure the ID columns match (DANE codes are often characters)
df07$MPIO_CDPMP <- as.character(df07$MPIO_CDPMP)
muni_geo$MPIO_CDPMP <- as.character(muni_geo$MPIO_CDPMP)

# 2. Join the road data to the map
muni_roads_map <- muni_geo %>%
  left_join(df07, by = "MPIO_CDPMP")

# 3. Plot the Map
# Second face-validity check, spatial rather than distributional: a
# plausible pattern here is low road density (dark/remote) concentrated in
# the Amazon/Pacific departments and high density (bright/connected) around
# Bogota and the Andean corridor -- anything else would suggest a join or
# unit problem upstream.
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


