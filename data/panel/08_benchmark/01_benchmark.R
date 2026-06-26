#---- Step 4: Benchmark municipal indices against national V-Dem subnational mean and range ----

# Pipeline:
# Step 1: Wrangle raw data, clean it, impute missing values (Folders 01-04)
# Step 2: Calculate CDF averages for geocoded predictors (Folder 05)
# Step 3: Weight predictors by V-Dem coder weights (Folder 07_weighting)
# Step 4 (this script): Combine subindices into sndem_final (Equation 1 in paper),
#         then compute calibrated positions on the V-Dem scale (EL_col_mt, CL_col_mt)
#         as supplementary outputs for cross-national comparison (paper pp. 14-16).
# Step 5: Analysis (snvdem-col/scripts/)

# Formula:
#   EL_col_mt = v2elffelr + (snelect - country_mean_snelect) * weighted_range / ELrange_975_025
#   CL_col_mt = CLSNmean  + (sncivlib - country_mean_sncivlib) * wtdCL_range / CLrange_975_025
#
# Where weighted_range / wtdCL_range = national range estimated from HPD * unevenness multiplier
# and ELrange_975_025 / CLrange_975_025 = empirical 97.5-2.5 percentile range of municipal scores.

# Author: MC; revised by PM (June 2026), with assistance from Claude Code Sonnet 4.6

library(tidyverse)
library(haven)


#---- Load weighted municipal indices (from 07_weighting) ----
index <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_weighted.rds")


#---- Load national-level V-Dem subnational means and ranges ----
# Elections: v2elffelr (subnational mean) and weighted_range (HPD * unevenness)
SNEL <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/08_benchmark/snlsffHPD.dta")

# Civil liberties: CLSNmean (subnational mean) and wtdCL_range (HPD * unevenness)
SNCL <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/08_benchmark/SNHPD.dta")


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


#---- Benchmark: calibrated positions and combined index ----
# Step 1: Map each municipality onto the V-Dem scale (paper pp. 14-16, Eqs 4-6).
#   EL is on the latent V-Dem scale (~[-3.5, 3.5]); CL is on [0,1] anchored to CLSNmean.
# Step 2: Linear cross-panel normalization to a common [0,1] scale.
#   Bounds = 2.5th-97.5th percentiles computed across ALL years (not within year).
#   Linear (unlike pnorm) so the gradual trend in each dimension is preserved exactly.
#   Cross-panel (unlike within-year ecdf) so temporal improvement is retained.
# Step 3: sndem_final = average of the two normalized dimensions (paper Eq. 1).
snvdem_final <- col_benchmark %>%
  mutate(
    EL_col_mt = v2elffelr + (snelect - snelectyrmean) * weighted_range / ELrange_975_025,
    CL_col_mt = CLSNmean  + (sncivlib - CLSNyrmean)   * wtdCL_range   / CLrange_975_025
  ) %>%
  mutate(
    EL_col_01   = (EL_col_mt - quantile(EL_col_mt, 0.025, na.rm = TRUE)) /
                  (quantile(EL_col_mt, 0.975, na.rm = TRUE) -
                   quantile(EL_col_mt, 0.025, na.rm = TRUE)),
    CL_col_01   = (CL_col_mt - quantile(CL_col_mt, 0.025, na.rm = TRUE)) /
                  (quantile(CL_col_mt, 0.975, na.rm = TRUE) -
                   quantile(CL_col_mt, 0.025, na.rm = TRUE)),
    sndem_final = 0.5 * (EL_col_01 + CL_col_01)
  )


#---- Diagnostics ----
cat("EL_col_mt   (V-Dem elec scale):\n");       print(round(summary(snvdem_final$EL_col_mt),   3))
cat("CL_col_mt   (V-Dem CL scale):\n");         print(round(summary(snvdem_final$CL_col_mt),   3))
cat("EL_col_01   (cross-panel linear, ~0-1):\n"); print(round(summary(snvdem_final$EL_col_01), 3))
cat("CL_col_01   (cross-panel linear, ~0-1):\n"); print(round(summary(snvdem_final$CL_col_01), 3))
cat("sndem_final (benchmarked, ~0-1):\n");       print(round(summary(snvdem_final$sndem_final), 3))

cat("NAs — EL_col_mt:", sum(is.na(snvdem_final$EL_col_mt)),
    " CL_col_mt:", sum(is.na(snvdem_final$CL_col_mt)),
    " sndem_final:", sum(is.na(snvdem_final$sndem_final)), "\n")

# Trend check: all three should show gradual improvement matching the box plots.
cat("\n--- National means by year ---\n")
snvdem_final %>%
  group_by(year) %>%
  summarize(
    EL01_mean  = round(mean(EL_col_01,  na.rm = TRUE), 3),
    CL_mean    = round(mean(CL_col_mt,  na.rm = TRUE), 3),
    sndem_mean = round(mean(sndem_final, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print(n = Inf)


#---- Write final output ----
write_rds(snvdem_final,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_final.rds")
