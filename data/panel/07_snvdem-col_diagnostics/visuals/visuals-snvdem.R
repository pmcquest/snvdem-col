# Visualizations of SNVDEM index (Colombia, 2000-2023)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)

## SNVDEM Panel (2000-2023)----
master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_final.rds")


# Visualizations ----

## Decile plots ----
decile_plot_raw <- master_df %>%
  group_by(year) %>%
  summarise(
    Top_10 = mean(sndem[sndem >= quantile(sndem, 0.9)], na.rm = TRUE),
    Bottom_10 = mean(sndem[sndem <= quantile(sndem, 0.1)], na.rm = TRUE),
    National_Avg = mean(sndem, na.rm = TRUE)
  ) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Raw sndem data: Top 10% vs Bottom 10% Municipalities",
       y = "Score", x = "Year")

decile_plot_raw


# benchmarked
decile_plot_bm <- master_df %>%
  group_by(year) %>%
  summarise(
    Top_10 = mean(sndem_final[sndem_final >= quantile(sndem_final, 0.9)], na.rm = TRUE),
    Bottom_10 = mean(sndem_final[sndem_final <= quantile(sndem_final, 0.1)], na.rm = TRUE),
    National_Avg = mean(sndem_final, na.rm = TRUE)
  ) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Benchmarked sndem data: Top 10% vs Bottom 10% Municipalities",
       y = "Score", x = "Year")

decile_plot_bm

## Heatmap ----
ggplot(master_df, aes(x = year, y = MPIO_CDPMP, fill = sndem_final)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), # Hide 1,125 municipality names
        panel.grid = element_blank()) +
  labs(title = "Heatmap of Democracy scores by Municipality",
       subtitle = "A vertical 'stripe' of darker color in 2023 would confirm a ubiquitous national drop",
       fill = "Score")

## Ridge plot ----
library(ggridges)
ggplot(master_df, aes(x = sndem_final, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  theme_ridges() + 
  labs(title = "Evolution of Score Distributions",
       y = "Year", x = "SN-Democracy Score")


## Map ----
# Faceted Mapping of Democracy Scores

library(sf)
library(viridis)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
# Load Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# 1. Join master_df with the shapefile
# Ensure the join key is clean in both dataframes
master_df <- master_df %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_df <- muni_geo %>%
  left_join(master_df, by = "MPIO_CDPMP")

# 2. Reshape for Faceting
# We select the three indices and pivot them to long format
map_long <- map_df %>%
  filter(year %in% c(2005, 2016, 2023)) %>% # Milestone years to keep the plot readable
  pivot_longer(
    cols = c(EL_col_cdf, CL_col_cdf, sndem_final),
    names_to = "Index_Type",
    values_to = "Score"
  ) %>%
  mutate(Index_Type = case_when(
    Index_Type == "EL_col_cdf" ~ "Electoral Fairness (EMEL)",
    Index_Type == "CL_col_cdf" ~ "Civil Liberties (CSCW)",
    Index_Type == "sndem_final" ~ "Subnational Democracy (SNDEM)"
  ))

# 3. Create the Faceted Map Plot
facet <- ggplot(data = map_long) +
  geom_sf(aes(fill = Score), color = NA) + # color=NA removes municipality borders for clarity
  facet_grid(year ~ Index_Type) + # Rows = Years, Columns = Index Types
  scale_fill_viridis_c(
    option = "magma", 
    name = "Score",
    na.value = "grey90"
  ) +
  labs(
    title = "Evolution of Subnational Democracy in Colombia",
    subtitle = "Comparing Electoral and Civil Liberty Dimensions (2005 - 2023)",
    caption = "Source: SN-VDEM v15 (Calculated via Weighted Directional Summation)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# 4. Save the output
ggsave(facet, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/imgs/map2005-16-23_0726.png", width = 12, height = 14, dpi = 300)


