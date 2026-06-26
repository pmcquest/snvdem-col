#---- Step 3a: Validation ----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2: Data reduction (calculate factor scores)
# Step 3 (this script): merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/validation")

## ----Setup ----
library(dplyr)
library(ggplot2)

col0020_weighted <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-weighted.rds")
joined <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/joined-avg-weighted.rds")

# Assuming result_df and national_scores_df are your data frames
# national_scores_df has columns "year", "national_emel", "national_cscw"

# 2. Join Data and Calculate Year-Level Averages
joined_df <- joined %>%
  group_by(year) %>%
  summarize(
    avg_emel_score = mean(emel_score, na.rm = TRUE),
    avg_cscw_score = mean(cscw_score, na.rm = TRUE)
  ) %>%
  left_join(col0020_weighted, by = "year")

# Normalize national-level emel and cscw scores
joined_df <- joined_df %>%
  mutate(
    emel_norm = (emel - min(emel, na.rm = TRUE)) / (max(emel, na.rm = TRUE) - min(emel, na.rm = TRUE)),
    cscw_norm = (cscw - min(cscw, na.rm = TRUE)) / (max(cscw, na.rm = TRUE) - min(cscw, na.rm = TRUE))
  )

# 3. Statistical Comparison
# Correlation
print(cor(joined_df$avg_emel_score, joined_df$emel_norm, use = "complete.obs"))
print(cor(joined_df$avg_cscw_score, joined_df$cscw_norm, use = "complete.obs"))

# low correlation likely to to the effect of subnational-level data on the "avg_emel" and "avg_cscw" scores
