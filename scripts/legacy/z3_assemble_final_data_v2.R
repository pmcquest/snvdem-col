#---- Step 4: Weighting geolocated data with V-Dem data ----

# Step 1: Wrangle raw data, clean it, then impute missing values (Folders 01-04)
# Step 2: Calculate averages of Empirical CDF data  (Folder 05)
# Step 3: Subset V-Dem data, calculate criteria averages, apply national range (Folder 06)
# Step 4 (this script): Interact Averaged CDF data by V-Dem data (Folders 05-06)
# Step 5: Analyze geolocated levels of democracy (Folder 09)


## ----Setup ----
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(knitr)

# V-Dem Colombia ratings (cr) data
cr_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/vdem_col0023_axis.rds")
# pivot to match geo_df columns
cr_pivoted <- cr_df %>%
  pivot_wider(names_from = row_id, values_from = c(emel, cscw), names_sep = "")


# Geolocated data from Colombia
geo_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/CDF_averages.rds")
geo_df <- geo_df %>%
  rename(avg0_1 = "avg0t1", avg2_3 = "avg2t3", avg4_5 = "avg4t5", avg6_7 = "avg6t7", avg8_9 = "avg8t9", avg10_11 = "avg10t11", avg15_16 = "avg15t16")

# Geocoded data review ----
# Correlation of most concern: avg12 and avg13 (0.60)
cor_matrix <- cor(geo_df[,4:13], use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")
summary(geo_df)

# V-Dem data review ----
## Notable: Democratic "Dip" in 2023 ----
cr_long <- cr_pivoted %>%
  pivot_longer(cols = -year, names_to = "component", values_to = "score") %>%
  mutate(Category = ifelse(grepl("^emel", component), "Electoral (EMEL)", "Civil Liberties (CSCW)"))

cr_annual <- cr_long %>%
  group_by(year, Category) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")

ggplot(cr_annual, aes(x = year, y = mean_score, color = Category)) +
  geom_line(size = 1) +
  geom_point(data = filter(cr_annual, year == 2023), size = 3) + # Highlight 2023
  geom_vline(xintercept = 2023, linetype = "dashed", color = "darkgrey") +
  theme_minimal() +
  labs(
    title = "Annual Trends in Democracy Components (2000-2023)",
    subtitle = "Comparing Electoral Fairness and Civil Liberties Scores",
    x = "Year",
    y = "Average Component Score",
    caption = "Data source: cr_pivoted summary"
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 2)) +
  theme(legend.position = "bottom")

## Pre- and post-2016 Accord percentages (2015 vs. 2023)----
emel_cols <- names(cr_pivoted)[grep("^emel", names(cr_pivoted))]
cscw_cols <- names(cr_pivoted)[grep("^cscw", names(cr_pivoted))]

change_calc <- cr_pivoted %>%
  filter(year %in% c(2015, 2023)) %>%
  rowwise() %>%
  mutate(
    EMEL_Index = mean(c_across(all_of(emel_cols)), na.rm = TRUE),
    CSCW_Index = mean(c_across(all_of(cscw_cols)), na.rm = TRUE)
  ) %>%
  select(year, EMEL_Index, CSCW_Index)

results_table <- change_calc %>%
  pivot_wider(names_from = year, values_from = c(EMEL_Index, CSCW_Index)) %>%
  mutate(
    EMEL_pct_change = ((EMEL_Index_2023 - EMEL_Index_2015) / EMEL_Index_2015) * 100,
    CSCW_pct_change = ((CSCW_Index_2023 - CSCW_Index_2015) / CSCW_Index_2015) * 100
  )

# EMEL really improved after 2016 accord (+35.9%) but CSCW did not (-2.70%).
print(results_table)


## V-Dem Correlations overall---- 
# V-Dem ratings: 0.68 correlation between EMEL and CSCW
cor_matrix2 <- cor(cr_pivoted, use = "complete.obs")
corrplot(cor_matrix2, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

vdem_summary_table <- cr_pivoted %>%
  select(year, ends_with("0_1"), ends_with("2_3"), ends_with("4_5")) %>%
  arrange(year)
kable(vdem_summary_table, caption = "V-Dem Criteria Values (2000-2023)")


# ---- Join Geolocated with V-Dem data ----
# Rescale and Center the spatial axes at 0 so directional variables match the 1-unit span of CDF variables:
## Scenario A: Experts favor the North (Weight = +0.8): 
### Northern Municipality: (+1.0) * (+0.8) = +0.8 (A democracy bonus).
### Southern Municipality: (-1.0) * (+0.8) = -0.8 (A democracy penalty).
## Scenario B: Experts favor the South (Weight = -0.8)
### Northern Municipality: (+1.0) * (-0.8) = -0.8 (The North is now penalized).
### Southern Municipality: (-1.0) * (-0.8) = +0.8 (The South now gets the bonus).
## Scenario C: Experts are neutral or geography is "Off" (Weight = 0)
### Any Municipality: Any Geography * (0) = 0 (No effect on the score).

geo_prepared <- geo_df %>%
  mutate(
    # This ensures the expert weight (0-1) acts on a full 1-unit range
    avg6_7 = (avg6_7 - 0.5), 
    avg8_9 = (avg8_9 - 0.5)
  )

# Master Interaction
# We use inner_join to ensure we only calculate for years present in both datasets
full_df <- geo_prepared %>%
  inner_join(cr_pivoted, by = "year") %>%
  mutate(
    # Interacting all criteria with EMEL Weights
    across(starts_with("avg"), 
           ~ .x * get(str_replace(cur_column(), "avg", "emel")), # multiplication step: x = value of municipality (geolocated) * get(V-dem rating)
           .names = "w_emel_{str_remove(.col, 'avg')}"),
    # Interacting all criteria with CSCW Weights
    across(starts_with("avg"), 
           ~ .x * get(str_replace(cur_column(), "avg", "cscw")), 
           .names = "w_cscw_{str_remove(.col, 'avg')}")
  )


## Correlations ----
# Compute the correlation matrix to make sure there are no perfectly correlated variables, other than EMELxCSCW intersection (not quite perfect but close)
cor_matrix3 <- cor(full_df[,34:53], use = "complete.obs")
corrplot(cor_matrix3, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Distributions in violence data
hist(full_df$w_emel_10_11) # slightly right-skewed
hist(full_df$w_cscw_10_11) # more normal

# Calculate Final Indices
# Balanced Summation
master_df <- full_df %>%
  rowwise() %>%
  mutate(
    # Summing preserves the additive 'Knowledge Shock' points
    emel_index = sum(c_across(starts_with("w_emel")), na.rm = TRUE),
    cscw_index = sum(c_across(starts_with("w_cscw")), na.rm = TRUE),
    # Averaging the pillars keeps the final score on a comparable scale
    sndem_index = (emel_index + cscw_index) / 2
  ) %>%
  ungroup() %>%
  select(
    MPIO_CDPMP, year, DPTO_CCDGO, 
    starts_with("w_"), 
    emel_index, cscw_index, sndem_index
  )

summary(master_df)

#---- Write the dataframe to rds ----
write_rds(master_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")




