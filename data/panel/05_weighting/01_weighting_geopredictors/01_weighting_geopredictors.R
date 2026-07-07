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
# Step 7: Compare unbenchmarked/benchmarked data, run diagnostics (Folder "07_snvdem-col_diagnostics")

# Author: MC; revised by PM (Jan 11, 2026)

library(tidyverse)
library(haven)

#---- Paths ----
# 05_weighting reorganized 2026-07-03 into numbered subfolders (script+MC legacy / images),
# mirroring 04_vdem_data's structure. No "01_source_files" here -- unlike 01_empirical_data and
# 04_vdem_data, this step has no raw *external* source files of its own; both of its inputs
# (ELCLweights_wide.dta, CDF_averages.rds) are already-canonical outputs of earlier pipeline
# steps, read cross-folder like every other inter-step dependency in this pipeline.
# 03_output/ added 2026-07-06: this step's own output (snvdem_col_weighted.rds) now lives here
# instead of in 07_*, so the "unbenchmarked" and "benchmarked" datasets are never sitting in the
# same folder -- 07_snvdem-col_diagnostics is now a comparison workspace, not a storage location
# for either one.
panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
img_dir   <- file.path(panel_dir, "05_weighting", "02_images")
out_dir   <- file.path(panel_dir, "05_weighting", "03_output")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#---- Load weights (V-Dem coder-level) ----
# Path fixed 2026-07-03: "coder-level/MC/" has never existed on disk. The live file was in
# 04_vdem_data/02_vdem_weighting/MC/ earlier the same day, then promoted to 04_vdem_data's own
# 03_outputs/ once 04_vdem_data was reorganized further (03_outputs holds this script's canonical
# *output*, not legacy MC material). See 04_vdem_data/weighting_bug_log_2026-07-03.md.
weights <- read_dta(file.path(panel_dir, "04_vdem_data", "03_outputs", "ELCLweights_wide.dta"))
weights_col <- filter(weights, country_text_id == "COL" & year > 1999)

# REMOVED 2026-07-06: manual fill for cl_Less_development NAs in 2000-2002/2004 (was hardcoding
# the mean, 0.2443505, over those years). Confirmed against the current ELCLweights_wide.dta
# (post 2026-07-03 criterion-weight and merge fixes) that those years are no longer NA -- COL now
# has real values throughout 2000-2024 (e.g. 0.40/0.50/0.50/0.50 for 2000-2002/2004, all above the
# old fill value). Line had already become a no-op (is.na() matched zero rows), but left it in
# would misdescribe the current data as still missing. See weighting_bug_log_2026-07-06.md.

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
# renormalizing the denominator to match. pred_mat and wt_mat must have the same dimensions
# and the same column *order* -- this function has no column names to check against, it works
# purely by matrix position (see pred_cols/el_wt_mat/cl_wt_mat below, which is where the
# ordering is actually established and where a mismatch would have to be caught by eye).
weighted_avg_narm <- function(pred_mat, wt_mat, min_weight_fraction = MIN_WEIGHT_FRACTION) {
  den_mat <- wt_mat
  # Blank out (to NA) the weight for any (row, criterion) cell whose predictor is missing, so
  # the next two lines' na.rm=TRUE drops that criterion from BOTH the numerator and the
  # denominator together -- a missing predictor never gets treated as 0, and its weight never
  # gets counted as if it had been satisfied.
  den_mat[is.na(pred_mat)] <- NA
  num      <- rowSums(pred_mat * wt_mat, na.rm = TRUE)  # sum(predictor_k * weight_k), available k only
  den      <- rowSums(den_mat, na.rm = TRUE)             # sum(weight_k), same available k only
  full_den <- rowSums(wt_mat)                            # sum(weight_k) if nothing were missing
  result   <- num / den                                  # renormalized weighted average, available k only
  # den/full_den = fraction of the total weight mass this row's answer is actually resting on.
  # Below MIN_WEIGHT_FRACTION (0.5), too much of the relevance weighting was thrown out along
  # with the missing predictors for the renormalized average to mean much, so it reverts to NA
  # instead of quietly reporting a score built from a handful of criteria.
  result[(den / full_den) < min_weight_fraction] <- NA
  result
}

# The 16 geocoded predictor columns, in the exact order they must appear in el_wt_mat/cl_wt_mat
# below (weighted_avg_narm() matches them up by position, column 1 to column 1, column 2 to
# column 2, etc.) -- five of V-Dem's 22 relevance criteria (avg1/Urban, avg0/Rural are one pair;
# similarly Development, Capital, Ruling_party) were split into hi/lo predictor columns above,
# so this list has 16 entries even though only ~13 distinct geographic concepts are represented.
pred_cols <- c("avg0t1hi", "avg0t1lo", "avg10t11", "avg12", "avg13", "avg14",
               "avg15t16hi", "avg15t16lo", "avg2t3hi", "avg2t3lo",
               "avg4t5hi", "avg4t5lo", "avg6", "avg7", "avg8", "avg9")
pred_mat <- as.matrix(Indices[, pred_cols])

# el_wt_mat/cl_wt_mat: the 16 relevance weights lined up column-for-column against pred_cols
# above (avg0t1hi -> el_Urban, avg0t1lo -> el_Rural, avg10t11 -> wt_el_1011, ... avg9 -> el_East).
# For the hi/lo pairs this deliberately uses a DIFFERENT weight for each side -- e.g. a
# municipality's "very urban" (avg0t1hi) contribution is scaled by how relevant "Urban" is as a
# criterion (el_Urban), while its "very rural" (avg0t1lo) contribution is scaled by el_Rural --
# rather than one symmetric weight for the whole avg0t1 variable, which is what lets the index
# respond asymmetrically to the two ends of each predictor.
#
# NON-OBVIOUS, UNRESOLVED: 5 of the 16 (Indigenous, Ruling_party_strong, Ruling_party_weak,
# Inside_capital, Outside_capital) use `1 - weight` instead of the raw relevance weight; the
# other 11 don't. This is inherited verbatim from MC's original
# (05_weighting/01_weighting_geopredictors/MC/wts_predictors_v2.R, el_num/el_den), not something
# introduced by this rewrite -- and that script's own lead-in comment before the equivalent code
# ("Which paired variables have high values that disfavor democracy?") suggests it was a
# deliberate correction, not an accident. But neither version explains *why* those specific 5
# needed the flip and the other 11 didn't, and the operational strategy doc
# (06_benchmark/Revised operational strategy_Jan2026.docx) stops at the HPD-range calculation in
# 04_vdem_data -- it doesn't cover this predictor-weighting stage at all. Worth confirming with
# MC directly before treating this pattern as settled if it's going in the memo.
el_wt_mat <- with(Indices, cbind(
  el_Urban, el_Rural, wt_el_1011, el_Sparse_population, el_Remote, 1 - el_Indigenous,
  1 - el_Ruling_party_strong, 1 - el_Ruling_party_weak, el_More_development, el_Less_development,
  1 - el_Inside_capital, 1 - el_Outside_capital, el_North, el_South, el_West, el_East))

cl_wt_mat <- with(Indices, cbind(
  cl_Urban, cl_Rural, wt_cl_1011, cl_Sparse_population, cl_Remote, 1 - cl_Indigenous,
  1 - cl_Ruling_party_strong, 1 - cl_Ruling_party_weak, cl_More_development, cl_Less_development,
  1 - cl_Inside_capital, 1 - cl_Outside_capital, cl_North, cl_South, cl_West, cl_East))

# snelect/sncivlib: the actual per-municipality-year weighted averages, one call of
# weighted_avg_narm() per index, sharing the same pred_mat (predictors don't depend on
# elections vs. CL) but each with its own weight matrix. sndem is not itself weighted by
# anything further -- it's a plain unweighted 50/50 average of the two sub-indices.
Indices_both <- Indices %>%
  mutate(
    snelect  = weighted_avg_narm(pred_mat, el_wt_mat),
    sncivlib = weighted_avg_narm(pred_mat, cl_wt_mat),
    # --- COMPOSITE INDEX ---
    sndem = 0.5 * (snelect + sncivlib)
  )


#---- Diagnostics ----
# Quick distributional sanity check (range/quartiles look plausible?) before the harder NA-count
# check below.
summary(Indices_both$snelect)
summary(Indices_both$sncivlib)
summary(Indices_both$sndem)

# Verify remaining NAs in the indices. Originally expected a sub-50%-weight-mass tail of ~44
# rows here (the Santa Rosalia/99624 + Cumaribo/99773 cluster noted above, missing 12 of 16
# predictors every year). Re-checked 2026-07-06: both municipalities now have 0 missing
# predictors in CDF_averages.rds (all 48 rows complete) -- that residual Step 3 geocoding gap
# has since been closed, so the actual count below is 0/0, not ~44.
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
# 2026-07-06: moved from 07_final_snvdem_data (now 07_snvdem-col_diagnostics, a comparison
# workspace, not a storage location) to this step's own 03_output/.
write_rds(Indices_both, file.path(out_dir, "snvdem_col_weighted.rds"))
