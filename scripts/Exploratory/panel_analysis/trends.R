# ----- Analysis of trends -----

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(readr)
library(tidyr)

colwtd <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted.rds")
col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)

colmap_data_yearly <- merge(x = colwtd, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data_yearly <- st_as_sf(colmap_data_yearly)

## ---- Top 10 (positive and negative) ----
# 1. Calculate Democracy Change

# Find the first and last year
years <- unique(colmap_data_yearly$year)
first_year <- min(years)
last_year <- max(years)

# Deduplicate within each year and MPIO_CDPMP
deduplicated_data <- colmap_data_yearly %>%
  group_by(MPIO_CDPMP, year) %>%
  slice(1) %>% # Keep only the first row for each MPIO_CDPMP and year
  ungroup()

# Calculate change in democracy scores
democracy_change <- deduplicated_data %>%
  filter(year %in% c(first_year, last_year)) %>%
  group_by(MPIO_CDPMP) %>%
  reframe(
    first_dem = sndem_mean[year == first_year],
    last_dem = sndem_mean[year == last_year],
    dem_change = last_dem - first_dem
  )

# Join the change data back to the spatial data
colmap_data_change <- colmap_data_yearly %>%
  filter(year == last_year) %>%
  left_join(democracy_change, by = "MPIO_CDPMP")


# 2. Identify Key Municipalities
# Identify top positive and negative changes
top_positive <- colmap_data_change %>%
  top_n(10, dem_change)
top_negative <- colmap_data_change %>%
  top_n(-10, dem_change)

# 3. Visualize the Changes

# Table of top changes

table_positive <- top_positive %>%
  select(MPIO_CNMBR, dem_change) %>%
  arrange(desc(dem_change))

table_negative <- top_negative %>%
  select(MPIO_CNMBR, dem_change) %>%
  arrange(dem_change)

print("Top Positive Changes:")
print(table_positive)

print("Top Negative Changes:")
print(table_negative)

# Map of top changes

# Combine top positive and negative for mapping
top_changes <- rbind(top_positive, top_negative)

# Create the map
ggplot() +
  geom_sf(data = colmap_data_change, aes(fill = dem_change), color = "lightgray", linewidth = 0.1) +
  geom_sf(data = top_changes, fill = "transparent", color = "black", linewidth = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_void() +
  labs(fill = "Change in Democracy Score")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/maps/Top10-change.png",
       height = 10, width = 10, device = "png", units = "in")


##----Create table----
table_all <- top_changes %>%
  select(MPIO_CNMBR, dem_change) %>%
  arrange(dem_change)


table_all_df <- st_drop_geometry(table_all)
# Print the first two columns of the data frame
print(table_all_df[, 1:2], n = nrow(table_all_df), na.print = "NA")


# Optional: Add column names for clarity
colnames(table_all_df) <- c("Municipality", "Democracy Change")

table_all_df <- table_all_df %>%
  mutate(`Democracy Change` = format(`Democracy Change`, digits = 3, scientific = FALSE))

# Print the table with NA values as "NA"
print(table_all_df, n = nrow(table_all_df), na.print = "NA")

#Optional: Print the top 10 changes in each direction.
top_positive_changes <- top_changes %>%
  select(MPIO_CNMBR, dem_change) %>%
  arrange(desc(dem_change)) %>%
  head(10)

top_negative_changes <- top_changes %>%
  select(MPIO_CNMBR, dem_change) %>%
  arrange(dem_change) %>%
  head(10)

print("Top 10 Positive Changes:")
print(top_positive_changes)

print("Top 10 Negative Changes:")
print(top_negative_changes)


##---- Correlations with variables ----

# 1. Load the time-series data
df_final <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/df_final.rds")

# Join the democracy change to df_final
df_final_change <- df_final %>%
  left_join(democracy_change, by = "MPIO_CDPMP")

# 2. Time-Lagged Correlations

# Select variables for correlation analysis
correlation_vars <- df_final_change %>%
  select(-1:-6)

# Calculate lagged correlations
lagged_correlations <- lapply(names(correlation_vars), function(var_name) {
  cor_result <- cor(df_final_change$dem_change, correlation_vars[[var_name]], use = "complete.obs")
  data.frame(variable = var_name, correlation = cor_result)
}) %>%
  bind_rows()

# 3. Visualize Correlations

# Visualize correlations using a bar plot
ggplot(lagged_correlations, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation with Democracy Change", x = "Variable", y = "Correlation")



