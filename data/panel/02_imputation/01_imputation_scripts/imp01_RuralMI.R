#----Growth-based and Multiple imputation----
# For rurality (#0-1), we have a municipal index (available from CEDE-Los Andes) and population data differentiating between urban and rural. We also have 2 variables related to remoteness.

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyr)
library(naniar)
library(mice)
library(corrplot)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds")

# Subset rurality index
df01 <- df_all %>% select(MPIO_CDPMP, year, IndRur_0t1)

# Identify the specific municipalities and years with missing rurality data
missing_rurality_details <- df_all %>%
  filter(is.na(IndRur_0t1) | is.na(PobRur_0t1) | is.na(PobUrb_0t1)) %>%
  select(MPIO_CDPMP, year, depto, municipio, IndRur_0t1, PobRur_0t1, PobUrb_0t1) %>%
  arrange(MPIO_CDPMP, year)
print(missing_rurality_details)

# Summary: How many NAs per municipality?
missing_by_mpio <- missing_rurality_details %>%
  group_by(MPIO_CDPMP, municipio) %>%
  summarise(years_missing = n(), .groups = 'drop')
missing_by_mpio


# ---- Imputation ----

# Create a subset for imputation.
# NOTE 2026-07-05: previously passed MPIO_CDPMP straight into mice() as a raw ~1,122-level
# factor, left in the default predictorMatrix as a predictor for every other variable. mice's
# pmm builds a linear model with one dummy column per factor level for each of its 5 iterations
# x 5 imputed datasets x 3 target variables -- on a fresh run this did not finish in 25+ minutes
# of single-core CPU time (killed) for what is <0.5% missingness (142/29,172 for IndRur_0t1;
# 62/29,172 each for PobRur_0t1/PobUrb_0t1). Replaced with the same "group mean as personality"
# trick already used in imp23_FiscalCART_v3.R's IDF_mean: a per-municipality mean carries the
# municipality's identity into the predictor matrix without the 1,122-dummy blowup.
impute_vars <- df_all %>%
  select(MPIO_CDPMP, year, IndRur_0t1, PobRur_0t1, PobUrb_0t1) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    IndRur_mean = mean(IndRur_0t1, na.rm = TRUE),
    PobRur_mean = mean(PobRur_0t1, na.rm = TRUE),
    PobUrb_mean = mean(PobUrb_0t1, na.rm = TRUE)
  ) %>%
  ungroup()

# Turn off MPIO_CDPMP as a predictor/target; use only the group-mean columns to carry
# municipality identity (and don't impute the means themselves).
ini <- mice(impute_vars, maxit = 0)
pred <- ini$predictorMatrix
pred[, "MPIO_CDPMP"] <- 0
pred["MPIO_CDPMP", ] <- 0
pred[c("IndRur_mean", "PobRur_mean", "PobUrb_mean"), ] <- 0

# Perform multiple imputation (m = 5 datasets)
# We use "pmm" (Predictive Mean Matching)
imputed_data_obj <- mice(impute_vars, method = "pmm", m = 5, seed = 123,
                          predictorMatrix = pred, printFlag = FALSE)

# NOTE 2026-07-06: previously used a hardcoded problem_codes list of 5 municipality codes here
# as the "imputed" flag, instead of the row's actual pre-imputation NA status. Those 5 codes are
# legacy DIVIPOLA codes dropped entirely by the 2026-07-03 empirical-merge rewrite, so none of
# them exist in df_all any more -- the flag was "Observed" for all 29,172 rows regardless of what
# was truly missing, which is why the plot below only ever showed one category. Fixed to flag
# each row by whether IndRur_0t1 was actually NA before this script's imputation ran.
imputed_flag <- if_else(is.na(df_all$IndRur_0t1), "Imputed", "Observed")

# 3. Update the data with the imputed values (if not already done)
completed_df <- complete(imputed_data_obj, 1)
df_all$IndRur_0t1 <- completed_df$IndRur_0t1
df_all$imputed_flag <- imputed_flag

# 4. Run the visualization
library(ggplot2)
p_rurality <- ggplot(df_all, aes(x = IndRur_0t1, fill = imputed_flag)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("Imputed" = "#E41A1C", "Observed" = "#377EB8")) +
  labs(title = "Distribution of Observed vs. Imputed Rurality index",
       subtitle = "Imputation Method: Predictive Mean Matching (PMM)",
       x = "Rurality Score (0-1)", y = "Density", fill = "Data Type") +
  theme_minimal()
# NOTE 2026-07-06: previously never saved (just auto-printed to whatever the default graphics
# device is), so this plot was lost under non-interactive/batch runs. See also the consolidated,
# consistently-flagged version of this same comparison for every imputation script in
# 04_diagnostics/02_observed_vs_imputed.R.
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/01_imputation_scripts/imgs/imp01_rurality_observed_vs_imputed.png",
       p_rurality, width = 7, height = 5, dpi = 300, bg = "white")

#----Impute LOCF and save----
# Even with MICE, if a municipality was missing data for ALL years, 
# it might still have NAs if MPIO was the only predictor.
# This final pass ensures full coverage.
imp01 <- df_all %>%
  select(MPIO_CDPMP, year, IndRur_0t1) %>%
  filter(year >= 1998) %>%
  group_by(MPIO_CDPMP) %>%
  # Carry forward/backward any remaining outliers
  fill(IndRur_0t1, .direction = "downup") %>%
  ungroup()

# Check final missing counts
print(colSums(is.na(imp01)))




write_rds(imp01, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/imp01.rds")

