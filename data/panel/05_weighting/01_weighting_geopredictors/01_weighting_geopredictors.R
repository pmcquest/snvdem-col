#---- Step 5: Weight geocoded predictors with V-Dem coder-level data ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation", incl.
#         "02_imputation/03_merge_imputed" for the merge + CDF-standardize sub-stage)
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5 (this script): Flip and multiply geopredictors by V-Dem coder weights; normalize by weight sum
#         (Folder "05_weighting/01_weighting_geopredictors")
# Step 6: Benchmark using national V-Dem data (Folder "06_benchmark")
# Step 7: Compare unbenchmarked/benchmarked data, run diagnostics (Folder "07_snvdem-col_diagnostics")

# Author: MC (Jan 11, 2026); revised by PM (Jun 30, 2026)

library(tidyverse)
library(haven)

#---- Paths ----
# This step has no raw *external* source files of its own; both of its inputs
# (ELCLweights_wide.dta, CDF_averages.rds) are outputs of earlier pipeline steps.

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
img_dir   <- file.path(panel_dir, "05_weighting", "02_images")
out_dir   <- file.path(panel_dir, "05_weighting", "03_output")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#---- Load weights (V-Dem coder-level) ----
weights <- read_dta(file.path(panel_dir, "04_vdem_data", "03_outputs", "ELCLweights_wide.dta"))
weights_col <- filter(weights, country_text_id == "COL" & year > 1999)

# REMOVED 2026-07-06: manual fill for cl_Less_development NAs in 2000-2002/2004 (was hardcoding
# the mean, 0.2443505, over those years). Confirmed against the current ELCLweights_wide.dta
# (post 2026-07-03 criterion-weight and merge fixes) that those years are no longer NA -- COL now
# has real values throughout 2000-2024 (e.g. 0.40/0.50/0.50/0.50 for 2000-2002/2004, all above the
# old fill value). 

# Average civil unrest and illicit activity into a single criterion weight
weights_col <- weights_col %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity) / 2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity) / 2)

##----Visualize the variables ----
library(ggplot2)
library(patchwork) # To display plots side-by-side

# Scatter plot for 'el' variables 
# Using violence (civil unrest and illicit activity) but could be switched for others
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
ggsave(file.path(img_dir, "violence-el_cl_distribution_comparison.png"), plot = p3,
       device = "png", height = 5, width = 8, units = "in", dpi = 300)



#---- Load geocoded predictors (geopredictors) from CDF averages ----
predictors <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/03_geocoded_panel/01_clean_geocoded/CDF_averages.rds")
# Expected columns: MPIO_CDPMP, year, DPTO_CCDGO,
# avg0t1, avg2t3, avg4t5, avg10t11, avg12, avg14, avg15t16,
# avg6 = north, avg7 = south, avg8 = west, avg9 = east (separate directional variables)
# lAR_13pkm, DisMer_13 (separate Remoteness variables, bi-directional)

# PIPELINE-STRUCTURE NOTE (2026-07-26, team decision w/ MC): direction-correction happens HERE.
# CDF_averages.rds stores RAW (pre-flip) values for avg0t1,
# avg4t5, avg10t11, avg14, avg15t16, and the avg13 pair (lAR_13pkm/DisMer_13). 
# avg2t3 (PIB_2t3+IDF_2t3) is unaffected (its components were never flipped, already
# high=more-democratic).

predictors_flip <- predictors %>%
  mutate(
    avg0t1   = 1 - avg0t1,    # Urban_0t1       <- 1 - IndRur_0t1
    avg4t5   = 1 - avg4t5,    # BogProx_4t5     <- 1 - DisBog_4t5
    avg10t11 = 1 - avg10t11,  # nonviolent_10t11 <- 1 - ViolInd_1011
    avg14    = 1 - avg14,     # NonIndig_14     <- 1 - PropInd_14
    avg15t16 = 1 - avg15t16,  # Opposition_15t16   <- 1 - RulPar_15t16
    # avg13 combines two raw components, only one of which needs flipping -- DisMer_13 must
    # be flipped BEFORE averaging (flipping the average afterward would incorrectly also
    # flip lAR_13pkm, which was never supposed to be flipped).
    # avg13 <- (lAR_13pkm + MerProx_13) / 2, where MerProx_13 <- 1 - DisMer_13.
    avg13    = (lAR_13pkm + (1 - DisMer_13)) / 2
  ) %>%
  select(-lAR_13pkm, -DisMer_13)

##---- Visualize the flipped raw geopredictors ----
# Analogous to MC's `combined.png` (drafts/1025-v3/Coppedge/geopredictors/ColMaps.R, lines 26-172)
# Single reference year (2020, matching ColMaps.R) since the point is to check
# direction/labeling, not year-to-year change. 

library(sf)

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp",
                     quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

geopred_year_raw <- predictors_flip %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(year == 2020) %>%
  transmute(
    MPIO_CDPMP,
    `dist. capital (flipped)`       = avg4t5,
    `remote (flipped)`              = avg13,
    `development (unflipped)`       = avg2t3,
    `sparse population (unflipped)` = avg12,
    `indigenous (flipped)`          = avg14,
    `urban (flipped)`               = avg0t1,
    `unrest/illicit (flipped)`      = avg10t11,
    # Ruling party is NOT a plain flip because it's discontinuous. pmin(avg15t16, 1 - avg15t16)
    # creates a "tent": it peaks at 0.5 (moderate support) and decays toward both extremes, matching the
    # inverted-V from MC's MemoCalculations.docx ("yellow = less extreme support = more
    # democratic").
    `ruling party (tent)`           = 2*pmin(avg15t16, 1 - avg15t16),
    `north (unflipped)`             = avg6,
    `south (unflipped)`             = avg7,
    `west (unflipped)`              = avg8,
    `east (unflipped)`              = avg9
  )

geopred_map_raw <- muni_geo %>%
  left_join(geopred_year_raw, by = "MPIO_CDPMP") %>%
  select(MPIO_CDPMP, `dist. capital (flipped)`:`east (unflipped)`, geometry) %>%
  pivot_longer(cols = `dist. capital (flipped)`:`east (unflipped)`,
               names_to = "geopredictor", values_to = "value")

geopred_facet_raw <- ggplot(geopred_map_raw) +
  geom_sf(aes(fill = value), color = NA) +
  facet_wrap(~geopredictor, nrow = 3) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Geopredictors after direction-flip, 2020") +
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 8))
geopred_facet_raw

ggsave(geopred_facet_raw, filename = file.path(img_dir, "geopredictors_facet_2020_raw.png"),
       device = "png", width = 18, height = 11.25, units = "in", dpi = 300)


#---- Split paired criteria into high/low versions at the 0.5 median split ----
# This implements the asymmetric weighting function: low weight for low values,
# high weight for high values, allowing a nonlinear contribution to democracy.
predict_hilo <- predictors_flip %>%
  mutate(avg0t1hi  = ifelse(avg0t1   > .5, avg0t1,   0),
         avg0t1lo  = ifelse(avg0t1   <= .5, avg0t1,  0),
         avg2t3hi  = ifelse(avg2t3   > .5, avg2t3,   0),
         avg2t3lo  = ifelse(avg2t3   <= .5, avg2t3,  0),
         avg4t5hi  = ifelse(avg4t5   > .5, avg4t5,   0),
         avg4t5lo  = ifelse(avg4t5   <= .5, avg4t5,  0),
         avg10t11hi = ifelse(avg10t11 > .5, avg10t11, 0),
         avg10t11lo = ifelse(avg10t11 <= .5, avg10t11, 0),
         avg15t16hi = ifelse(avg15t16 > .5, avg15t16, 0),
         avg15t16lo = ifelse(avg15t16 <= .5, avg15t16, 0),
         # Ruling_party inverted-V (MC's MemoCalculations.docx): below 50% ruling-party vote
         # share (avg15t16 > .5, i.e. more competitive), more support helps democracy --
         # avg15t16lo is used as-is. Above 50% (avg15t16hi side, less competitive/more
         # dominant), more support HURTS democracy past that point, so this side is flipped;
         # avg15t16hi_flip is always defined (1 when avg15t16hi == 0, i.e. outside this
         # regime, so it contributes the full, un-diminished el_Ruling_party_strong weight
         # there -- this is not a mutually-exclusive 0-when-inapplicable column like the
         # other hi/lo pairs, it is the other half of one continuous tent-shaped function
         # together with avg15t16lo. Reproduces MC's own `(1-avg15t16hi)` term.
         avg15t16hi_flip = 1 - avg15t16hi)

##---- Reconstruct geopredictors post hi-low split (for the numeric check below) ----
# Every one of these 12 columns is expected to be identical to the raw map above.
# The numeric diff-check below confirms this is the casel

geopred_year <- predict_hilo %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(year == 2020) %>%
  transmute(
    MPIO_CDPMP,
    `dist. capital (flipped)`     = avg4t5hi + avg4t5lo,
    `remote (flipped)`            = avg13,
    `development (unflipped)`     = avg2t3hi + avg2t3lo,
    `sparse population (unflipped)` = avg12,
    `indigenous (flipped)`        = avg14,
    `urban (flipped)`             = avg0t1hi + avg0t1lo,
    `unrest/illicit (flipped)`    = avg10t11,
    # Same tent formula as the raw map above, computed straight from avg15t16 -- NOT from
    # avg15t16hi_flip/avg15t16lo (see note above; that sum is discontinuous and not a valid
    # combined predictor). 
    `ruling party (tent)`         = 2*pmin(avg15t16, 1 - avg15t16),
    `north (unflipped)`           = avg6,
    `south (unflipped)`           = avg7,
    `west (unflipped)`            = avg8,
    `east (unflipped)`            = avg9
  )

#---- Numeric check: raw (pre hi-low) vs. reconstructed (post hi-low) ----
# Reports max abs diff per panel over the same 1,122 municipalities, 2020. 
# Expect 0 everywhere.
geopred_compare <- geopred_year_raw %>%
  rename_with(~ paste0(., " [raw]"), -MPIO_CDPMP) %>%
  inner_join(geopred_year %>% rename_with(~ paste0(., " [hi-lo]"), -MPIO_CDPMP), by = "MPIO_CDPMP")

for (nm in names(geopred_year_raw)[-1]) {
  diff <- geopred_compare[[paste0(nm, " [raw]")]] - geopred_compare[[paste0(nm, " [hi-lo]")]]
  cat(sprintf("%-32s max abs diff = %.6f (n differing = %d/%d)\n",
              nm, max(abs(diff), na.rm = TRUE), sum(abs(diff) > 1e-9, na.rm = TRUE), nrow(geopred_compare)))
}


#---- Join predictors and weights ----
Indices <- predict_hilo %>%
  left_join(weights_col, by = "year")


#---- Calculate normalized weighted indices ----
# Formula: snelect = sum(predictor_k * weight_k) / sum(weight_k)
# Each sub-index is a weighted average, so the denominator varies by year
# as expert weights change. This normalizes scores to a comparable scale.
#
# Missing-predictor handling (decided 2026-07-03, see README "Open questions"): since Step 2's
# na.last = "keep" fix, real NAs can flow into the 16 avgXX predictor columns. A plain `+` sum
# would blank the WHOLE row's snelect/sncivlib whenever ANY one of the 16 criteria is missing,
# even if the other 15 are present. weighted_avg_narm() below instead drops missing (predictor,
# weight) pairs together and renormalizes over what's left, but only when at least
# MIN_WEIGHT_FRACTION of the original weight mass survives -- below that, a handful of criteria
# isn't a reliable basis for a score, so it stays NA rather than resting on 3-4 of 16 criteria.
#
# Current state (checked against the live CDF_averages.rds, 2026-07-21): 634 of ~26,928
# municipality-years have at least one missing predictor, but never more than 2 of 16 (>=88% of
# weight mass retained) -- comfortably above the floor, so nothing currently falls below
# MIN_WEIGHT_FRACTION. This used to be worse: Santa Rosalia (99624) and Cumaribo (99773) were
# missing 12 of 16 predictors in every year (only 18% weight mass, well below the floor -> NA),
# traced to the same legacy-DIVIPOLA-code gap flagged in the Step 1 empirical-merge fix. That
# gap was closed at Step 3 on 2026-07-06 (see README Step 5 note) -- both municipalities now have
# 0 missing predictors in every year, so this is no longer a live concern for them.
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
# Ruling_party_strong's slot uses avg15t16hi_flip (not raw avg15t16hi) -- see its construction
# above for why.
pred_cols <- c("avg0t1hi", "avg0t1lo", "avg10t11", "avg12", "avg13", "avg14",
               "avg15t16hi_flip", "avg15t16lo", "avg2t3hi", "avg2t3lo",
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
# PIPELINE-STRUCTURE NOTE (2026-07-26): weights below are all raw (no `1 - weight` flip) --
# per MC's `MemoCalculations.docx`, flipping the weight for Indigenous/Ruling_party/Capital
# was an error in the pipeline's prior formula (his "MPSA version"), not an intentional
# design; he confirms these relevance weights need no adjustment once the geopredictor side
# is already correctly oriented (handled above and, for Ruling_party, via avg15t16hi_flip in
# pred_cols). Direction for these criteria is carried entirely by the geopredictor, matching
# his own "July" formula in wts_predictors_v2.R. Prior version (weight-flipped, matching
# "MPSA") is preserved for reference/audit as
# 05_weighting/01_weighting_geopredictors/01_weighting_geopredictors_ORIGINAL_pre-julyformula_2026-07-26.R.
el_wt_mat <- with(Indices, cbind(
  el_Urban, el_Rural, wt_el_1011, el_Sparse_population, el_Remote, el_Indigenous,
  el_Ruling_party_strong, el_Ruling_party_weak, el_More_development, el_Less_development,
  el_Inside_capital, el_Outside_capital, el_North, el_South, el_West, el_East))

cl_wt_mat <- with(Indices, cbind(
  cl_Urban, cl_Rural, wt_cl_1011, cl_Sparse_population, cl_Remote, cl_Indigenous,
  cl_Ruling_party_strong, cl_Ruling_party_weak, cl_More_development, cl_Less_development,
  cl_Inside_capital, cl_Outside_capital, cl_North, cl_South, cl_West, cl_East))

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

# Verify remaining NAs in the indices. Per the missing-predictor note above, this should be 0/0
# on the current CDF_averages.rds.
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
