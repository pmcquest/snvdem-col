#---- Step 6: Benchmark municipal indices against national V-Dem subnational mean and range ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation", incl.
#         "02_imputation/03_merge_imputed" for the merge + CDF-standardize sub-stage)
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Weight Averaged CDF data by V-Dem data (Folder "05_weighting")
# Step 6 (this script): Combine subindices into sndem_final (Equation 1 in paper), (Folder "06_benchmark")
#         then compute calibrated positions on the V-Dem scale (EL_col_mt, CL_col_mt)
#         as supplementary outputs for cross-national comparison (paper pp. 14-16).
# Step 7: Compare unbenchmarked/benchmarked data, run diagnostics (Folder "07_snvdem-col_diagnostics")

# Formula:
#   EL_col_mt = v2elffelr + (snelect - country_mean_snelect) * weighted_range / ELrange_975_025
#   CL_col_mt = CLSNmean  + (sncivlib - country_mean_sncivlib) * wtdCL_range / CLrange_975_025
#
# Where weighted_range / wtdCL_range = national range estimated from HPD * unevenness multiplier
# and ELrange_975_025 / CLrange_975_025 = empirical 97.5-2.5 percentile range of municipal scores.

# Author: MC; revised by PM (June 2026), with assistance from Claude Code Sonnet 4.6

# 2026-07-06 reorg: 06_benchmark restructured to mirror 05_weighting -- this script now lives in
# 01_benchmark/ (alongside MC's original version in 01_benchmark/MC/); validate_map.R/
# trend_diagnostics.R/spatial_rank_check.R moved to 02_diagnostics/01_scripts/; this script's own
# output now goes to 03_output/; the memo chain lives in 04_memo/.

library(tidyverse)
library(haven)


#---- Load weighted municipal indices (from 05_weighting's own output folder) ----
index <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds")


#---- Load national-level V-Dem subnational means and ranges ----
# Both read cross-folder from 04_vdem_data/03_outputs -- their true origin (Step 4's own
# output). Previously copied into 06_benchmark directly; moved back to their source folder
# 2026-07-06 for the same reason ELCLweights_wide.dta already lived there.
# Elections: v2elffelr (subnational mean) and weighted_range (HPD * unevenness)
SNEL <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/04_vdem_data/03_outputs/snlsffHPD.dta")

# Civil liberties: CLSNmean (subnational mean) and wtdCL_range (HPD * unevenness)
SNCL <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/04_vdem_data/03_outputs/SNHPD.dta")


#---- Filter for Colombia 2000-2023 and merge ----
SNELcol <- filter(SNEL, country_text_id == "COL" & year > 1999)
SNCLcol <- filter(SNCL, country_text_id == "COL" & year > 1999)

SNcol <- merge(SNELcol, SNCLcol, by = c("country_text_id", "year"), all.y = TRUE) %>%
  select(country_text_id, year, CLSNmean, wtdCL_range, v2elffelr, weighted_range)

col_benchmark <- merge(SNcol, index, by = "year", all.y = TRUE)


#---- Calculate country-level summaries needed for rescaling ----
SNcol_by_year <- col_benchmark %>%
  group_by(year) %>%
  summarize(
    CLSNyrmean     = mean(CLSNmean, na.rm = TRUE),
    snelectyrmean  = mean(snelect,  na.rm = TRUE),
    CLrange_975_025 = quantile(sncivlib, 0.975, na.rm = TRUE) - quantile(sncivlib, 0.025, na.rm = TRUE),
    ELrange_975_025 = quantile(snelect,  0.975, na.rm = TRUE) - quantile(snelect,  0.025, na.rm = TRUE),
    .groups = "drop"
  )

col_benchmark <- merge(col_benchmark, SNcol_by_year, by = "year", all.x = TRUE)


#---- Global reference constants (fixed, NOT Colombia-specific) ----
# CRITICAL: benchmarking exists so Colombia's scores are comparable to other countries on
# the V-Dem scale. A per-country scale()/z-score (mean 0, sd 1 computed from Colombia's own
# panel) is ipsative -- it re-centers Colombia to itself and would NOT be comparable to
# another country's own self-centered z-score if this pipeline is ever extended. Instead, both dimensions are divided by a FIXED
# global sd computed once from the full V-Dem country-year panel (2000-2023, all countries),
# so the same constant would apply to any future country run through this pipeline.
v15_global <- vdemdata::vdem %>% filter(year >= 2000, year <= 2023)
EL_global_mean <- mean(v15_global$v2elffelr, na.rm = TRUE)
EL_global_sd   <- sd(v15_global$v2elffelr,   na.rm = TRUE)

# CL global reference must be built the SAME way as the CL municipal anchor (CLSNmean =
# v2x_civlib discounted by v2clsnlpct, the % of population under weaker-than-national civil
# liberties), or the two aren't comparable. An earlier version used raw, undiscounted
# v2x_civlib for the global reference while Colombia's municipalities were anchored to the
# discounted CLSNmean -- since v2clsnlpct > 0 for virtually every country (global mean ~37%),
# that mismatch alone pulled Colombia's CL_col_gz down by ~0.94 global SD, independent of any
# real difference in civil liberties. Found and fixed 2026-07-02.
v15_global      <- v15_global %>% mutate(CLSNmean_global = v2x_civlib * (100 - v2clsnlpct) / 100)
CLSNmean_valid  <- v15_global$CLSNmean_global[!is.na(v15_global$CLSNmean_global) &
                                                 v15_global$CLSNmean_global > 0 &
                                                 v15_global$CLSNmean_global < 1]
CLz_global      <- qnorm(CLSNmean_valid)
CL_global_mean  <- mean(CLz_global, na.rm = TRUE)
CL_global_sd    <- sd(CLz_global,   na.rm = TRUE)

cat("Global reference constants (full V-Dem panel, 2000-2023, all countries):\n")
cat("  v2elffelr:                mean =", round(EL_global_mean, 3), " sd =", round(EL_global_sd, 3), "\n")
cat("  qnorm(CLSNmean-style):    mean =", round(CL_global_mean, 3), " sd =", round(CL_global_sd, 3), "\n\n")

#---- Benchmark: calibrated positions and combined index ----
# Step 1: Map each municipality onto the V-Dem scale (paper pp. 14-16, Eqs 4-6).
#   EL is on the latent V-Dem scale (~[-3.5, 3.5]); CL is on [0,1] anchored to CLSNmean.
# Step 2: Put CL on the same latent scale as EL, per MC (email 2026-07-02): "convert 0-1 CDF
#   values to Z-scores with the qnorm() function." CL_col_mt is safely within [0.292, 0.581]
#   for Colombia -- no boundary values, so qnorm() is well-behaved (no +-Inf).
# Step 3: rebalance EL and CL to comparable magnitude using the FIXED global sd's above (not
#   Colombia's own), then average (paper Eq. 1). This keeps the result on a scale where 0 =
#   global country-year average and units are "global standard deviations" -- interpretable
#   and comparable if/when other countries are added, unlike a per-country scale().
snvdem_final <- col_benchmark %>%
  mutate(
    EL_col_mt = v2elffelr + (snelect - snelectyrmean) * weighted_range / ELrange_975_025,
    CL_col_mt = CLSNmean  + (sncivlib - CLSNyrmean)   * wtdCL_range   / CLrange_975_025
  ) %>%
  mutate(
    CL_col_z    = qnorm(CL_col_mt),
    EL_col_gz   = (EL_col_mt - EL_global_mean) / EL_global_sd,
    CL_col_gz   = (CL_col_z  - CL_global_mean) / CL_global_sd,
    sndem_final = 0.5 * (EL_col_gz + CL_col_gz)
  )


#---- Diagnostics ----
cat("CL_col_mt range check (must stay inside (0,1) for qnorm to be finite):\n")
print(round(range(snvdem_final$CL_col_mt, na.rm = TRUE), 3))

cat("\nEL_col_mt   (V-Dem elec/latent scale, raw):\n");     print(round(summary(snvdem_final$EL_col_mt),   3))
cat("CL_col_mt   (V-Dem CL scale, 0-1, raw):\n");           print(round(summary(snvdem_final$CL_col_mt),   3))
cat("EL_col_gz   (EL_col_mt, global-standardized):\n");     print(round(summary(snvdem_final$EL_col_gz),   3))
cat("CL_col_gz   (qnorm(CL_col_mt), global-standardized):\n"); print(round(summary(snvdem_final$CL_col_gz), 3))
cat("sndem_final (benchmarked, global-standardized scale):\n"); print(round(summary(snvdem_final$sndem_final), 3))

cat("\nColombia's mean/sd on the GLOBAL scale (NOT expected to be exactly mean 0/sd 1 --\n")
cat("that would only happen if Colombia's own distribution matched the global one; the\n")
cat("point of global standardization is to preserve where Colombia actually sits):\n")
cat("  EL_col_gz: mean =", round(mean(snvdem_final$EL_col_gz), 3),  " sd =", round(sd(snvdem_final$EL_col_gz), 3), "\n")
cat("  CL_col_gz: mean =", round(mean(snvdem_final$CL_col_gz, na.rm = TRUE), 3),
    " sd =", round(sd(snvdem_final$CL_col_gz, na.rm = TRUE), 3), "\n")

cat("\nNAs/Inf — EL_col_gz:", sum(!is.finite(snvdem_final$EL_col_gz)),
    " CL_col_gz:", sum(!is.finite(snvdem_final$CL_col_gz)),
    " sndem_final:", sum(!is.finite(snvdem_final$sndem_final)), "\n")

# Trend check: all three should show gradual improvement matching the box plots.
cat("\n--- National means by year ---\n")
snvdem_final %>%
  group_by(year) %>%
  summarize(
    EL_mean    = round(mean(EL_col_gz,  na.rm = TRUE), 3),
    CL_mean    = round(mean(CL_col_gz,  na.rm = TRUE), 3),
    sndem_mean = round(mean(sndem_final, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print(n = Inf)


#---- Write final output ----
# 2026-07-06: renamed snvdem_col_final.rds -> snvdem_col_benchmarked.rds and moved from
# 07_final_snvdem_data (now 07_snvdem-col_diagnostics, a comparison workspace) to this step's
# own 03_output/ -- mirrors 05_weighting's snvdem_col_weighted.rds living in its own 03_output/,
# so the unbenchmarked and benchmarked datasets are never stored in the same folder.
write_rds(snvdem_final,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/03_output/snvdem_col_benchmarked.rds")
