#---- Step 5: Weight geocoded predictors with V-Dem coder-level data ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation", incl.
#         "02_imputation/03_merge_imputed" for the merge + CDF-standardize sub-stage)
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5 (this script): Multiply predictors by V-Dem coder weights; normalize by weight sum
#         (Folder "05_weighting/01_weighting_geopredictors")
# Step 6: Benchmark using national V-Dem data (Folder "06_benchmark")
# Step 7: Revise final snvdem index (Folder "07_final_snvdem_data")

# Author: MC; revised by PM (Jan 11, 2026)

library(tidyverse)
library(haven)

#---- Paths ----
# 05_weighting reorganized 2026-07-03 into numbered subfolders (script+MC legacy / images),
# mirroring 04_vdem_data's structure. No "01_source_files" here -- unlike 01_empirical_data and
# 04_vdem_data, this step has no raw *external* source files of its own; both of its inputs
# (ELCLweights_wide.dta, CDF_averages.rds) are already-canonical outputs of earlier pipeline
# steps, read cross-folder like every other inter-step dependency in this pipeline.
panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
img_dir   <- file.path(panel_dir, "05_weighting", "02_images")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

#---- Load weights (V-Dem coder-level) ----
# Path fixed 2026-07-03: "coder-level/MC/" has never existed on disk. The live file was in
# 04_vdem_data/02_vdem_weighting/MC/ earlier the same day, then promoted to 04_vdem_data's own
# 03_outputs/ once 04_vdem_data was reorganized further (03_outputs holds this script's canonical
# *output*, not legacy MC material). See 04_vdem_data/weighting_bug_log_2026-07-03.md.
weights <- read_dta(file.path(panel_dir, "04_vdem_data", "03_outputs", "ELCLweights_wide.dta"))
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
# Persisted 2026-07-03: this plot previously only displayed in an interactive session and was
# silently discarded in batch/Rscript runs.
ggsave(file.path(img_dir, "civil_unrest_illicit_activity_scatter.png"), plot = p1 + p2,
       device = "png", height = 5, width = 10, units = "in", dpi = 300)

library(tidyr)

# Pivot long to plot distributions together
p3 <- weights_col %>%
  select(el_Civil_unrest, el_Illicit_activity, cl_Civil_unrest, cl_Illicit_activity) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  separate(Variable, into = c("Type", "Metric"), sep = "_", extra = "merge") %>%

  ggplot(aes(x = Value, fill = Metric)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Type, scales = "free") +
  labs(title = "Distribution Comparison",
       x = "Value", y = "Density") +
  theme_minimal()
p3
ggsave(file.path(img_dir, "el_cl_distribution_comparison.png"), plot = p3,
       device = "png", height = 5, width = 8, units = "in", dpi = 300)





#---- Load geocoded predictors (CDF averages) ----
predictors <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/03_geocoded_panel/01_clean_geocoded/CDF_averages.rds")
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
#
# Missing-predictor handling (decided 2026-07-03, see README "Open questions"): since Step 2's
# na.last = "keep" fix, real NAs flow into the 16 avgXX predictor columns for 682 of ~26,928
# municipality-years (60 municipalities, mostly small/remote departments -- San Andres 88,
# Amazonas 91, Guainia 94). A plain `+` sum blanks the WHOLE row's snelect/sncivlib whenever ANY
# one of the 16 criteria is missing, even if the other 15 are present. Investigated: none of the
# 682 rows are missing all 16 (every one is rescuable in principle), but the fraction of weight
# mass retained after dropping only the missing (predictor, weight) pairs ranges from 18% (a
# ~44-row cluster in Santa Rosalia/99624 and Cumaribo/99773 missing 12 of 16 predictors in every
# year -- the same two municipalities flagged for legacy DIVIPOLA codes in the empirical-merge
# fix; likely a residual Step 3 geocoding gap worth a separate look) to 100%, median 88%.
# weighted_avg_narm() below drops missing (predictor, weight) pairs together and renormalizes
# over what's left, but only when at least MIN_WEIGHT_FRACTION of the original weight mass
# survives -- below that, a handful of criteria isn't a reliable basis for a score, so it stays
# NA rather than resting on 3-4 of 16 criteria.
MIN_WEIGHT_FRACTION <- 0.5

# Drops a (predictor, weight) pair together whenever the predictor is NA -- NOT the same as
# replacing the predictor with 0, which would silently pull the score toward 0 without
# renormalizing the denominator to match.
weighted_avg_narm <- function(pred_mat, wt_mat, min_weight_fraction = MIN_WEIGHT_FRACTION) {
  den_mat <- wt_mat
  den_mat[is.na(pred_mat)] <- NA
  num      <- rowSums(pred_mat * wt_mat, na.rm = TRUE)
  den      <- rowSums(den_mat, na.rm = TRUE)
  full_den <- rowSums(wt_mat)
  result   <- num / den
  result[(den / full_den) < min_weight_fraction] <- NA
  result
}

pred_cols <- c("avg0t1hi", "avg0t1lo", "avg10t11", "avg12", "avg13", "avg14",
               "avg15t16hi", "avg15t16lo", "avg2t3hi", "avg2t3lo",
               "avg4t5hi", "avg4t5lo", "avg6", "avg7", "avg8", "avg9")
pred_mat <- as.matrix(Indices[, pred_cols])

el_wt_mat <- with(Indices, cbind(
  el_Urban, el_Rural, wt_el_1011, el_Sparse_population, el_Remote, 1 - el_Indigenous,
  1 - el_Ruling_party_strong, 1 - el_Ruling_party_weak, el_More_development, el_Less_development,
  1 - el_Inside_capital, 1 - el_Outside_capital, el_North, el_South, el_West, el_East))

cl_wt_mat <- with(Indices, cbind(
  cl_Urban, cl_Rural, wt_cl_1011, cl_Sparse_population, cl_Remote, 1 - cl_Indigenous,
  1 - cl_Ruling_party_strong, 1 - cl_Ruling_party_weak, cl_More_development, cl_Less_development,
  1 - cl_Inside_capital, 1 - cl_Outside_capital, cl_North, cl_South, cl_West, cl_East))

Indices_both <- Indices %>%
  mutate(
    snelect  = weighted_avg_narm(pred_mat, el_wt_mat),
    sncivlib = weighted_avg_narm(pred_mat, cl_wt_mat),
    # --- COMPOSITE INDEX ---
    sndem = 0.5 * (snelect + sncivlib)
  )


#---- Diagnostics ----
summary(Indices_both$snelect)
summary(Indices_both$sncivlib)
summary(Indices_both$sndem)

# Verify remaining NAs in the indices (expected: the sub-50%-weight-mass tail only, ~44 rows --
# not the full 682 from before the na.rm + renormalize fix).
cat("NAs in snelect:", sum(is.na(Indices_both$snelect)), "\n")
cat("NAs in sncivlib:", sum(is.na(Indices_both$sncivlib)), "\n")

p4 <- ggplot(Indices_both, aes(x = snelect, y = sncivlib)) +
  geom_point(alpha = .1) +
  theme_light() +
  labs(title = "Electoral vs. Civil Liberties Index (pre-benchmarking)")
p4
ggsave(file.path(img_dir, "snelect_sncivlib_prebenchmark_scatter.png"), plot = p4,
       device = "png", height = 6, width = 7, units = "in", dpi = 300)


#---- Write output ----
write_rds(Indices_both,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/07_final_snvdem_data/snvdem_col_weighted.rds")
