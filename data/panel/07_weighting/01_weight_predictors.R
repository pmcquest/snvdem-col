#---- Step 3: Weight geocoded predictors with V-Dem coder-level data ----

# Pipeline:
# Step 1: Wrangle raw data, clean it, impute missing values (Folders 01-04)
# Step 2: Calculate CDF averages for geocoded predictors (Folder 05)
# Step 3 (this script): Multiply predictors by V-Dem coder weights; normalize by weight sum
# Step 4: Benchmark against national V-Dem subnational mean and range (Folder 08_benchmark)
# Step 5: Analysis (snvdem-col/scripts/)

# Author: MC; revised by PM (Jan 11, 2026)

library(tidyverse)
library(haven)


#---- Load weights (V-Dem coder-level) ----
weights <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide.dta")
weights_col <- filter(weights, country_text_id == "COL" & year > 1999)

# Fix: cl_Less_Development is NA for years 2000-2002 and 2004 in the source data.
# Fill with the mean of available values (0.2443505) to avoid 3,375 NAs in sncivlib.
weights_col$cl_Less_development[is.na(weights_col$cl_Less_development)] <- 0.2443505

# Average civil unrest and illicit activity into a single criterion weight
weights_col <- weights_col %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity) / 2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity) / 2)

##----Visualize the variables ----
library(ggplot2)
library(patchwork) # To display plots side-by-side

# Scatter plot for 'el' variables
p1 <- ggplot(weights_col, aes(x = el_Civil_unrest, y = el_Illicit_activity)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Elections (el) Components",
       x = "Civil Unrest", y = "Illicit Activity") +
  theme_minimal()

# Scatter plot for 'cl' variables
p2 <- ggplot(weights_col, aes(x = cl_Civil_unrest, y = cl_Illicit_activity)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Civil Liberties (cl) Components",
       x = "Civil Unrest", y = "Illicit Activity") +
  theme_minimal()

# Combine plots side by side
p1 + p2

library(tidyr)

# Pivot long to plot distributions together
weights_col %>%
  select(el_Civil_unrest, el_Illicit_activity, cl_Civil_unrest, cl_Illicit_activity) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Type", "Metric"), sep = "_", extra = "merge") %>%
  
  ggplot(aes(x = Value, fill = Metric)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Type, scales = "free") +
  labs(title = "Distribution Comparison",
       x = "Value", y = "Density") +
  theme_minimal()





#---- Load geocoded predictors (CDF averages) ----
predictors <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/CDF_averages.rds")
# Expected columns: MPIO_CDPMP, year, DPTO_CCDGO,
#   avg0t1, avg2t3, avg4t5, avg6, avg7, avg8, avg9,
#   avg10t11, avg12, avg13, avg14, avg15t16
# avg6 = north, avg7 = south, avg8 = west, avg9 = east (separate directional variables)

# Split paired criteria into high/low versions at the 0.5 median split.
# This implements the asymmetric weighting function: low weight for low values,
# high weight for high values, allowing a nonlinear contribution to democracy.
predict_hilo <- predictors %>%
  mutate(avg0t1hi  = ifelse(avg0t1   > .5, avg0t1,   0),
         avg0t1lo  = ifelse(avg0t1   <= .5, avg0t1,  0),
         avg2t3hi  = ifelse(avg2t3   > .5, avg2t3,   0),
         avg2t3lo  = ifelse(avg2t3   <= .5, avg2t3,  0),
         avg4t5hi  = ifelse(avg4t5   > .5, avg4t5,   0),
         avg4t5lo  = ifelse(avg4t5   <= .5, avg4t5,  0),
         avg10t11hi = ifelse(avg10t11 > .5, avg10t11, 0),
         avg10t11lo = ifelse(avg10t11 <= .5, avg10t11, 0),
         avg15t16hi = ifelse(avg15t16 > .5, avg15t16, 0),
         avg15t16lo = ifelse(avg15t16 <= .5, avg15t16, 0))


#---- Join predictors and weights ----
Indices <- predict_hilo %>%
  left_join(weights_col, by = "year")


#---- Calculate normalized weighted indices ----
# Formula: snelect = sum(predictor_k * weight_k) / sum(weight_k)
# Each sub-index is a weighted average, so the denominator varies by year
# as expert weights change. This normalizes scores to a comparable scale.
Indices_both <- Indices %>%
  mutate(
    # --- ELECTORAL INDEX (snelect) ---
    el_num = (avg0t1hi * el_Urban) + (avg0t1lo * el_Rural) +
      (avg10t11 * wt_el_1011) + (avg12 * el_Sparse_population) +
      (avg13 * el_Remote) + (avg14 * (1 - el_Indigenous)) +
      (avg15t16hi * (1 - el_Ruling_party_strong)) + (avg15t16lo * (1 - el_Ruling_party_weak)) +
      (avg2t3hi * el_More_development) + (avg2t3lo * el_Less_development) +
      (avg4t5hi * (1 - el_Inside_capital)) + (avg4t5lo * (1 - el_Outside_capital)) +
      (avg6 * el_North) + (avg7 * el_South) + (avg8 * el_West) + (avg9 * el_East),

    el_den = el_Urban + el_Rural + wt_el_1011 + el_Sparse_population +
      el_Remote + (1 - el_Indigenous) + (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak) +
      el_More_development + el_Less_development + (1 - el_Inside_capital) + (1 - el_Outside_capital) +
      el_North + el_South + el_West + el_East,

    snelect = el_num / el_den,

    # --- CIVIL LIBERTIES INDEX (sncivlib) ---
    cl_num = (avg0t1hi * cl_Urban) + (avg0t1lo * cl_Rural) +
      (avg10t11 * wt_cl_1011) + (avg12 * cl_Sparse_population) +
      (avg13 * cl_Remote) + (avg14 * (1 - cl_Indigenous)) +
      (avg15t16hi * (1 - cl_Ruling_party_strong)) + (avg15t16lo * (1 - cl_Ruling_party_weak)) +
      (avg2t3hi * cl_More_development) + (avg2t3lo * cl_Less_development) +
      (avg4t5hi * (1 - cl_Inside_capital)) + (avg4t5lo * (1 - cl_Outside_capital)) +
      (avg6 * cl_North) + (avg7 * cl_South) + (avg8 * cl_West) + (avg9 * cl_East),

    cl_den = cl_Urban + cl_Rural + wt_cl_1011 + cl_Sparse_population +
      cl_Remote + (1 - cl_Indigenous) + (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak) +
      cl_More_development + cl_Less_development + (1 - cl_Inside_capital) + (1 - cl_Outside_capital) +
      cl_North + cl_South + cl_West + cl_East,

    sncivlib = cl_num / cl_den,

    # --- COMPOSITE INDEX ---
    sndem = 0.5 * (snelect + sncivlib)
  ) %>%
  select(-el_num, -el_den, -cl_num, -cl_den)


#---- Diagnostics ----
summary(Indices_both$snelect)
summary(Indices_both$sncivlib)
summary(Indices_both$sndem)

# Verify no remaining NAs in the indices
cat("NAs in snelect:", sum(is.na(Indices_both$snelect)), "\n")
cat("NAs in sncivlib:", sum(is.na(Indices_both$sncivlib)), "\n")

ggplot(Indices_both, aes(x = snelect, y = sncivlib)) +
  geom_point(alpha = .1) +
  theme_light() +
  labs(title = "Electoral vs. Civil Liberties Index (pre-benchmarking)")


#---- Write output ----
write_rds(Indices_both,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_weighted.rds")
