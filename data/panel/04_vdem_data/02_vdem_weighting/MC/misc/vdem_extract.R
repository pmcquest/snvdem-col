#---- Step 3: V-Dem data extract ----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2: Data reduction (calculate factor scores)
# Step 3: Subset V-Dem data (a -- this script), then calculate criteria averages (b)
# Step 4: combine V-Dem variables with Empirical data (a), then weight obs. data by coder-level analysis (b)
# Step 5: Map geolocated levels of democracy


# Install latest version of VDem (v15, as of 12/28/2025)
devtools::install_github("vdeminstitute/vdemdata", force = TRUE)
library(vdemdata)
max(vdem$year) # This should return 2025

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel")

## ----Setup ----
# load needed packages
library(tidyverse)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

#----V-Dem import and weights [pending coder-level data update] ----
# Create a data frame of version 15 dataset:
v15 <- vdem

# Filter to just Colombia since 1899:
v15_col <- filter(v15, country_name=="Colombia" & year > 1899)

# Select just the ID and subnational variables
# Select ID and Subnational variables (Criteria 0-16 relevant for Colombia)
# 0-1: Rural/Urban, 2-3: Econ, 4-5: Dist Capital, 6-7: N/S, 8-9: W/E, 
# 10-11: Unrest/Illegal, 12: Density, 13: Remoteness, 14: Indig, 15-16: Party
v15_col_sn <- v15_col %>%
  select(year, 
         v2elsnlfc_0:v2elsnlfc_16, v2elsnmrfc_0:v2elsnmrfc_16,
         v2clrgstch_0:v2clrgstch_16, v2clrgwkch_0:v2clrgwkch_16) %>%
  filter(year >= 2000 & year <= 2023)

# Clean up memory
rm(v15, v15_col)


#----Creating emel and cscw----
# The new variables (emel_* and cscw_*) measures the *relevance* of a given criteria for either elections or civil liberties, over time. This is done by taking the absolute value of the difference between Election pairs [abs(more free-less free)] and Civil liberties pairs [abs(strong-weak)]. 
## A score of 0 indicates either: (a) the criteria of interest has no relevance for elections or civil liberties at the subnational-level (all coders respond "no" for both pairs), or (b) coders are in full disagreement on whether the criteria is associated with free or unfree elections or strong or weak civil liberties; that is, the proportions cancel each other out. For example, a coder score of 1 for elections *less* free in rural areas (criteria 0) and a score of 1 for elections *more* free in rural areas (criteria 0)) would render the criteria of "rural areas" irrelevant.
## Conversely, a score of 1 indicates full agreement (e.g., coder score of 1 for elections less free in rural areas (criteria 0) and score of 0 for elections more free in rural areas (criteria 0)). These scores indicate that the criteria is totally relevant for subnational democracy components.

# Note: For cardinal directions, we create 2 additional variables (emels and cscws) that maintain the direction of coder consensus. This is because we will not be calculating the CDF of North-South or West-East variables, in order to avoid "penalizing" one or the other direction. Instead, we will be interacting these "directional consensus" ratings with the -1 to 1 scale of the N-S and W-E axes variables in the geolocated data (South is negative pole, North positive; East is negative, West positive). When we calculate the Directional Consensus (see below), we re-scale the 2-unit measure into a 1-unit measure.

# 6: North. (0=No, 1=Yes) [v2*_6] 
# 7: South. (0=No, 1=Yes) [v2*_7] 
# 8: West. (0=No, 1=Yes) [v2*_8] 
# 9: East. (0=No, 1=Yes) [v2*_9] 

## Scenario A: Experts favor the North (Weight = +0.8): 
### Northern Municipality: (+1.0 geolocation) * (+0.8 rating) = +0.8 (A democracy bonus).
### Southern Municipality: (-1.0 geolocation) * (+0.8 rating) = -0.8 (A democracy penalty).
## Scenario B: Experts favor the South (Weight = -0.8)
### Northern Municipality: (+1.0) * (-0.8) = -0.8 (The North is now penalized).
### Southern Municipality: (-1.0) * (-0.8) = +0.8 (The South now gets the bonus).
## Scenario C: Experts are neutral or geography is "Off" (Weight = 0)
### Any Municipality: Any Geolocation * (0) = 0 (No effect on the score).

col_vdem_rel <- v15_col_sn %>%
  pivot_longer(cols = -year,
               names_to = c("variable", "subset"),
               names_pattern = "(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch)_(\\d+)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    emel = abs(v2elsnmrfc - v2elsnlfc), # Relevance Weight
    cscw = abs(v2clrgstch - v2clrgwkch), # Relevance Weight
    emels = v2elsnmrfc - v2elsnlfc,      # Directional Consensus (-1 to 1)
    cscws = v2clrgstch - v2clrgwkch      # Directional Consensus (-1 to 1)
  )

## Coder-level data
v15_clcol <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/COL_combined.rds")
v15_clcol <- v15_clcol %>% filter(year >= 2000 & year <=2023) # 408 observations -- matches the main scores.

## Compare V-Dem index and coder-level data ----
col_vdem_rel <- col_vdem_rel %>% mutate(subset = as.character(subset))
# Define the ordered criteria mapping
criteria_map <- c(
  "0" = "Rural", "1" = "Urban", "2" = "Less development", "3" = "More development",
  "4" = "Inside capital", "5" = "Outside capital", "6" = "North", "7" = "South",
  "8" = "West", "9" = "East", "10" = "Civil unrest", "11" = "Illicit activity",
  "12" = "Sparse population", "13" = "Remote", "14" = "Indigenous",
  "15" = "Ruling party strong", "16" = "Ruling party weak"
)
# Convert map to a dataframe for easier joining
map_df <- enframe(criteria_map, name = "subset", value = "criteria_name")

# Join the two datasets
comparison_df <- col_vdem_rel %>%
  select(year, subset, emels, cscws) %>%
  inner_join(v15_clcol, by = c("year", "subset")) %>%
  # Add the human-readable labels back for plotting/analysis
  left_join(map_df, by = "subset") %>%
  # Reorganize for clarity
  select(year, subset, criteria_name, emels, emel_score, cscws, cscw_score)

# Calculate differences
comparison_df <- comparison_df %>%
  mutate(
    emel_diff = emels - emel_score,
    cscw_diff = cscws - cscw_score
  )

# Quick check of correlations
cor(comparison_df$emels, comparison_df$emel_score, use = "complete.obs")
cor(comparison_df$cscws, comparison_df$cscw_score, use = "complete.obs")


# Simple plot to compare EMEL scores across criteria
ggplot(comparison_df, aes(x = emel_score, y = emels)) +
  geom_point(aes(color = criteria_name)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Comparison of EMEL scores: col_vdem_rel vs v15_clcol",
       x = "Score from v15_clcol", y = "Score from col_vdem_rel")

library(corrplot)
# Select the four variables to compare
cor_data <- comparison_df %>%
  select(emels, emel_score, cscws, cscw_score) %>%
  # Rename for cleaner labels in the plot
  rename(
    "EMEL (vdem_data)" = emels,
    "EMEL (coder_level)"    = emel_score,
    "CSCW (vdem_data)" = cscws,
    "CSCW (coder_level)"    = cscw_score
  ) %>%
  drop_na() # Ensure no NAs break the correlation calculation

# Calculate the Pearson correlation matrix
M <- cor(cor_data)

# Set up a color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, 
         method = "color",       # Use colored squares
         col = col(200),         # Apply the palette
         type = "upper",         # Only show the top triangle
         order = "hclust",       # Cluster variables by similarity
         addCoef.col = "black",  # Add the correlation coefficient numbers
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Rotate text labels
         diag = FALSE            # Hide the 1.00 diagonal for clarity
)

ggplot(comparison_df, aes(x = emel_score, y = emels)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  facet_wrap(~criteria_name) +
  theme_minimal() +
  labs(
    title = "Consistency of EMEL Scores by Dimension",
    subtitle = "Comparing vdem_rel vs clcol across 17 criteria (2000-2023)",
    x = "v15_clcol Score",
    y = "col_vdem_rel Score"
  )


# Calculate correlation for each year
annual_geo_cor <- comparison_df %>%
  group_by(year) %>%
  summarise(
    emel_cor = cor(emels, emel_score, use = "complete.obs"),
    cscw_cor = cor(cscws, cscw_score, use = "complete.obs")
  ) %>%
  pivot_longer(cols = c(emel_cor, cscw_cor), 
               names_to = "measure", 
               values_to = "correlation")

# Plot the trends
ggplot(annual_geo_cor, aes(x = year, y = correlation, color = measure)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) + # Perfection line
  theme_minimal() +
  scale_color_manual(values = c("emel_cor" = "#2c7bb6", "cscw_cor" = "#d7191c"),
                     labels = c("EMEL (Elections)", "CSCW (Civil Liberties)")) +
  labs(
    title = "Annual Correlation Between vdem_rel and clcol",
    subtitle = "Tracking data consistency for Colombia (2000-2023)",
    x = "Year",
    y = "Pearson Correlation (r)",
    color = "Dimension"
  ) +
  ylim(min(annual_geo_cor$correlation) - 0.1, 1.05)

# Calculate the absolute gap between the two sources
comparison_df <- comparison_df %>%
  mutate(emel_gap = abs(emels - emel_score))

ggplot(comparison_df, aes(x = year, y = criteria_name, fill = emel_gap)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#d7191c") +
  theme_minimal() +
  labs(
    title = "Where do the datasets disagree most? (EMEL)",
    subtitle = "Heatmap of Absolute Difference between vdem_rel and clcol",
    x = "Year",
    y = "Dimension",
    fill = "Abs. Difference"
  )


# How much variation in each df?
variation_summary <- comparison_df %>%
  summarise(
    # Variation in V-Dem REL scores
    sd_emels = sd(emels, na.rm = TRUE),
    iqr_emels = IQR(emels, na.rm = TRUE),
    
    # Variation in clcol scores
    sd_emel_score = sd(emel_score, na.rm = TRUE),
    iqr_emel_score = IQR(emel_score, na.rm = TRUE),
    
    # Repeat for CSCW
    sd_cscws = sd(cscws, na.rm = TRUE),
    sd_cscw_score = sd(cscw_score, na.rm = TRUE)
  )

print(variation_summary)

library(ggridges)

# Preparing data for comparison
plot_data <- comparison_df %>%
  select(year, criteria_name, emels, emel_score) %>%
  pivot_longer(cols = c(emels, emel_score), names_to = "source", values_to = "value")

ggplot(plot_data, aes(x = value, y = criteria_name, fill = source)) +
  geom_density_ridges(alpha = 0.5, scale = 1) +
  theme_ridges() +
  scale_fill_manual(values = c("emels" = "#2c7bb6", "emel_score" = "#d7191c"),
                    labels = c("vdem_full", "coder-level")) +
  labs(title = "Comparing Distributional Variation by Dimension",
       x = "Score (-1 to 1)", y = "Dimension")

# Temporal volatility
volatility_df <- comparison_df %>%
  group_by(criteria_name) %>%
  arrange(year) %>%
  mutate(
    diff_vdem = emels - lag(emels),
    diff_clcol = emel_score - lag(emel_score)
  ) %>%
  summarise(
    volatility_vdem = sd(diff_vdem, na.rm = TRUE),
    volatility_clcol = sd(diff_clcol, na.rm = TRUE)
  )

# Plotting the volatility comparison
ggplot(volatility_df, aes(x = volatility_vdem, y = volatility_clcol, label = criteria_name)) +
  geom_point() +
  geom_text(vjust = -1, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Dataset Volatility Comparison",
       subtitle = "Above the line: clcol is more volatile | Below the line: vdem_rel is more volatile",
       x = "SD of YoY Change (vdem_rel)",
       y = "SD of YoY Change (clcol)")


# ---- Consolidate Criteria Pairs (Relevance Weights) ----
# Some criteria are natural "pairs". Keeping them separate will create unnecessary noise in our data. We decide to consolidate them by taking the mean of the relevance weights (means of 0&1, 2&3, etc.)
col_pairs <- col_vdem_rel %>%
  filter(subset %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "15", "16")) %>%
  # New "paired" criteria:
  mutate(group = case_when(
    subset %in% c("0", "1") ~ "0_1", # Rural-Urban
    subset %in% c("2", "3") ~ "2_3", # Economic development
    subset %in% c("4", "5") ~ "4_5", # Capital city proximity
    subset %in% c("6", "7") ~ "6_7", # North-South Axis (Note: for directions, average will be computed but not used in final index)
    subset %in% c("8", "9") ~ "8_9", # West-East Axis (Note: for directions, average will be computed but not used in final index)
    subset %in% c("10", "11") ~ "10_11", # "Violence" (Unrest and Illicit activity combined) (Note: We decided to combine these criteria)
    subset %in% c("15", "16") ~ "15_16" # Ruling Party support
  )) %>%
  group_by(year, group) %>%
  summarise(emel = mean(emel, na.rm = TRUE),
            cscw = mean(cscw, na.rm = TRUE), 
            .groups = "drop") %>%
  rename(row_id = group)

# Handle Non-Paired Criteria (12, 13, 14)
unchanged <- col_vdem_rel %>%
  filter(subset %in% c("12", "13", "14")) %>% # 12: Sparsely populated, 13: Remote locaion, 14: Indigenous population
  select(year, subset, emel, cscw) %>%
  rename(row_id = subset)

# Combine into primary criteria weight dataframe
cr_df <- bind_rows(col_pairs, unchanged)

## ---- Calculate Directional Consensus for Spatial Axes ----
# This is crucial for Axis_NS and Axis_WE to avoid assuming North or West is "better". 
spatial_directions <- col_vdem_rel %>%
  select(year, subset, emels, cscws) %>%
  filter(subset %in% c("6", "7", "8", "9")) %>%
  pivot_wider(names_from = subset, values_from = c(emels, cscws))
# Check pivoted scores

# Essentially, we are asking: "On average, which direction do the experts favor for this specific year?". 
spatial_directions <- spatial_directions %>%
  mutate(
    # Latitudinal Consensus: Positive favors North, Negative favors South
    ns_cons_emel = (emels_6 - emels_7) / 2, # rescale to 1-unit measure
    ns_cons_cscw = (cscws_6 - cscws_7) / 2, # rescale to 1-unit measure
    # Longitudinal Consensus: Positive favors West, Negative favors East
    we_cons_emel = (emels_8 - emels_9) / 2, # rescale to 1-unit measure
    we_cons_cscw = (cscws_8 - cscws_9) / 2 # rescale to 1-unit measure
  ) %>%
  select(year, starts_with("ns_"), starts_with("we_"))
summary(spatial_directions)
# More consensus that north has stronger electoral fairness and civil liberties than south (more positive values). Much less consensus for West-East axis (values closer to 0).


# ---- Final Merge and Export ----
# Here we join the spatial variables to the main dataframe of paired and non-paired criteria, noting the spatial variables are qualitatively different (-0.5 to 0.5 scale).
expert_ratings <- cr_df %>%
  left_join(spatial_directions, by = "year") %>%
  mutate(
    # Directional Weights: Replace absolute relevance with directional consensus
    # If positive, North/West contributes positively. If negative, South/East does.
    emel = case_when(
      row_id == "6_7" ~ ns_cons_emel,
      row_id == "8_9" ~ we_cons_emel,
      TRUE ~ emel
    ),
    cscw = case_when(
      row_id == "6_7" ~ ns_cons_cscw,
      row_id == "8_9" ~ we_cons_cscw,
      TRUE ~ cscw
    )
  ) %>%
  select(year, row_id, emel, cscw)
summary(expert_ratings)
# We see that the range for emel dips to -0.1155 - 0.9375. 
# When we exclude the spatial consensus rows, they return to 0-1 scale
expert_ratings %>%
  filter(!row_id %in% c("6_7", "8_9")) %>%
  summary()

# Save the clean expert weights for Step 4
write_rds(expert_ratings, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/vdem_col0023_axis.rds")
# Pivot weights for easier revision
cr_pivoted <- expert_weights_merged %>%
  pivot_wider(names_from = row_id, values_from = c(emel, cscw), names_sep = "")
write_rds(cr_pivoted, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/vdem_col0023_pivoted.rds")

# ---- Final Visual Check ----
## ----Faceted Line Plot ----

long_data_new <- expert_ratings %>%
  pivot_longer(cols = c(cscw, emel), names_to = "variable", values_to = "value") %>%
  rename(Criteria = row_id) %>% 
  mutate(Criteria = case_when(
    Criteria == "0_1" ~ "Rural/Urban",
    Criteria == "2_3" ~ "Econ. Dev't",
    Criteria == "4_5" ~ "Dist. Capital",
    Criteria == "6_7" ~ "Axis: North/South",
    Criteria == "8_9" ~ "Axis: West/East",
    Criteria == "10_11" ~ "Civ. Unrest/Ill. Act.",
    Criteria == "12" ~ "Pop. Density",
    Criteria == "13" ~ "Remoteness",
    Criteria == "14" ~ "Ind. Population",
    Criteria == "15_16" ~ "Ruling Party",
    TRUE ~ Criteria 
  ))

ggplot(long_data_new, aes(x = year, y = value, color = variable, group = variable)) +
  geom_line(size = 0.8) +
  # Adding a horizontal line at 0 to help see the directional poles for the axes
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  facet_wrap(~ Criteria, scales = "free_y") + # Use free_y because axes go -1 to 1
  labs(title = "Criteria Weights and Directional Consensus (2000-2023)",
       subtitle = "Spatial Axes show direction (-1 to 1); Others show absolute relevance (0 to 1)",
       x = "Year",
       y = "Weight / Consensus Value",
       color = "Component") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Axis graphics show consensus among experts that EMEL and CSCW are stronger in the North, and for the most part in the West. The dip in West/East into negative signals consensus that in 2018, the consensus is that the East had stronger EMEL (but only briefly and slightly by 0.10 points)


# "North-ness" as democratic pole in your data
long_check <- spatial_directions %>%
  pivot_longer(-year, names_to = "Dimension", values_to = "Consensus")

ggplot(long_check, aes(x = year, y = Consensus, color = Dimension)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Expert Directional Consensus (2000-2023)",
       subtitle = "Positive values = North/West preferred; Negative = South/East preferred",
       y = "Consensus Score (-1 to 1)") +
  theme_minimal()


