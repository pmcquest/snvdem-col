#----- Merge imputed data ----
# Step 2 of the panel pipeline, final sub-stage (folder 02_imputation/03_merge_imputed).
# Reads the per-variable imputed .rds files from 02_imputation/02_imputation_outputs/,
# merges them onto the municipality-year skeleton (MunYrs.rds), and writes the merged
# panel + CDF-standardized panel here.

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/")
# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(haven)
library(tidyverse)
library(naniar)


# Load cleaned and imputed datasets
impStatic <- readRDS("impStatic.rds")
imp01 <- readRDS("imp01.rds")
imp23 <- readRDS("imp23.rds")
imp23b <- readRDS("imp23b.rds") # Fiscal performance (IDF) imputed data
imp1011 <- readRDS("imp1011FA.rds") # Includes Factor Analysis scores
imp1214 <- readRDS("imp1214.rds") #
imp1516 <- readRDS("imp1516.rds")
imp13 <- readRDS("imp13.rds")

# check municipality count--none should be below 1122 (the DANE count)
n_distinct(impStatic$MPIO_CDPMP) 
n_distinct(imp01$MPIO_CDPMP) 
n_distinct(imp23$MPIO_CDPMP) 
n_distinct(imp23b$MPIO_CDPMP)
n_distinct(imp1011$MPIO_CDPMP)
n_distinct(imp1214$MPIO_CDPMP)
n_distinct(imp1516$MPIO_CDPMP)
n_distinct(imp13$MPIO_CDPMP) 

# Baseline: 1122 total
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/MunYrs.rds")
missing_mpio <- setdiff(unique(impStatic$MPIO_CDPMP), unique(MunYrs$MPIO_CDPMP))
print(missing_mpio)
# NOTE 2026-07-03: this used to print "27086" "27415" "99572" "99760" (legacy/disputed codes
# -- see 01_empirical_data/04_merge_empirical/01_merge_empirical.R's own notes for the full
# diagnosis). Fixed at the source: 01_merge_empirical.R now builds its master skeleton from
# MunYrs.rds directly and drops the legacy-code rows before merging, so impStatic (and every
# other imp0N.rds) should no longer carry these codes at all. This check is kept as a
# regression guard, not because a mismatch is expected.
extra_mpio <- setdiff(unique(MunYrs$MPIO_CDPMP), unique(impStatic$MPIO_CDPMP))
print(extra_mpio)

# list for joining
datasets <- list(
  static = impStatic, d01 = imp01, d23 = imp23, d23b = imp23b, 
  d1011 = imp1011, d1214 = imp1214, d13 = imp13, d1516 = imp1516
)

# Define valid codes
valid_mpio_codes <- unique(MunYrs$MPIO_CDPMP) 

# 1. Build the master panel with the DPTO code already defined
master_panel <- MunYrs %>%
  distinct(MPIO_CDPMP) %>%
  filter(MPIO_CDPMP %in% valid_mpio_codes) %>%
  # Generate DPTO_CCDGO immediately to avoid joining NAs later
  mutate(DPTO_CCDGO = substr(MPIO_CDPMP, 1, 2)) %>% 
  expand_grid(year = 2000:2023)

# 2. Clean and Join
# Use the master_panel (which now has MPIO, year, and DPTO) as the base
final_df <- map(datasets, function(df) {
  # Remove redundant grouping columns to prevent .x/.y suffixes
  df %>% select(-any_of(c("municipio", "depto", "provincia", "DPTO_CCDGO")))
}) %>%
  reduce(function(x, y) {
    join_vars <- if("year" %in% names(y)) c("MPIO_CDPMP", "year") else "MPIO_CDPMP"
    left_join(x, y, by = join_vars)
  }, .init = master_panel)

# 3. Validate
cat("Final MPIO count:", n_distinct(final_df$MPIO_CDPMP), "(Expected: 1122)\n")

# Re-run your check
unique_deptos_with_missing_pib <- final_df %>%
  filter(is.na(PIB_2t3)) %>%
  select(DPTO_CCDGO) %>%
  distinct() %>%
  pull(DPTO_CCDGO)

print(unique_deptos_with_missing_pib)
# [1] "88" "91" "94" "97" "99" -- San Andres, Amazonas, Guainia, Vaupes, Vichada. Colombia's
# smallest/most remote departments, for which DANE does not consistently publish
# municipal-level PIB estimates. Expected structural gap, not a join or imputation bug.
# 552 of 26928 rows (2.05%) -- see note below, this is the largest NA rate of any column.

# some NAs but proportionally few (<0.008)
# NOTE 2026-07-03: no longer accurate for every column. Checked against current data:
# most columns are 0.18-0.69% missing (consistent with "<0.008"), but PIB_2t3 is 2.05%
# missing (552/26928 rows) -- driven by the five departments above. Worth knowing before
# treating "<0.008" as a blanket guarantee downstream.
colSums(is.na(final_df))

# Filter out coordinates data
final_df <- final_df %>% select(-LATITUD, -LONGITUD)

##----Create standardized df (CDF)----
# NOTE 2026-07-03: rank()'s default na.last = TRUE sorts NAs to the END of the ranking,
# i.e. a missing predictor would silently become the HIGHEST percentile (~1.0) for that
# variable/year rather than staying NA -- confirmed rank(c(0.1,0.5,NA,0.9,NA)) gives NAs
# ranks 4 and 5 of 5. Decision (PM, 2026-07-03): this project wants missingness preserved
# rather than disguised as "best in Colombia," so na.last = "keep" is used instead --
# confirmed rank(c(0.1,0.5,NA,0.9,NA), na.last = "keep") returns NA for the NA positions,
# so rank(.)/length(.) is NA too. PIB_2t3 (552 rows, 2.05%, concentrated in San Andres/
# Amazonas/Guainia/Vaupes/Vichada -- Section 3 above) is the variable most affected, but
# every column with any missingness (see diagnostic_table below) is affected the same way.
# Consequence downstream: 05_weighting's snelect/sncivlib sums will propagate these NAs
# (a plain sum() with an NA term is NA) unless that script is updated to handle it
# explicitly -- decide there whether to na.rm and re-normalize the weight denominator,
# or leave those municipality-years NA. Not addressed in this script.
colvars_cdf <- final_df %>%
  mutate(across(4:32, ~ rank(., na.last = "keep") / length(.)))

summary(colvars_cdf) #2000-2023

#----Save and document (final)----
write_rds(final_df, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/03_merge_imputed/imputed_master_panel.rds")

write_rds(colvars_cdf, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/03_merge_imputed/imputed_cdf_panel.rds")


#----NAs----
# Missingness diagnostics on final_df -- the pre-CDF panel, since colvars_cdf's NAs are
# masked by rank() (see note above).
library(ggplot2)
library(stringr)

# --- Step 1: Calculate NA statistics ---
diagnostic_table <- final_df %>%
  summarise(
    total_obs = n(),
    across(where(is.numeric), ~ sum(is.na(.)), .names = "missing_{.col}")
  ) %>%
  pivot_longer(cols = starts_with("missing_"),
               names_to = "variable", values_to = "missing_count") %>%
  mutate(
    variable = str_remove(variable, "missing_"),
    missing_pct = round((missing_count / total_obs) * 100, 2)
  ) %>%
  select(variable, missing_count, missing_pct) %>%
  arrange(desc(missing_count))

cat("\n---- Missingness by variable (final_df, pre-CDF), sorted worst first ----\n")
print(diagnostic_table, n = Inf)

# --- Step 2: Identify Missing Years ---
missing_years <- final_df %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ sum(!is.na(.)), .names = "non_missing_{.col}")) %>%
  pivot_longer(cols = starts_with("non_missing_"),
               names_to = "variable", values_to = "non_missing_count") %>%
  mutate(variable = str_remove(variable, "non_missing_")) %>%
  filter(non_missing_count == 0) %>%
  group_by(variable) %>%
  summarise(missing_years = paste(unique(year), collapse = ", "), .groups = "drop")

diagnostic_table <- diagnostic_table %>%
  left_join(missing_years, by = "variable")

cat("\n---- Variables with at least one entirely-missing year (should be none) ----\n")
if (nrow(missing_years) == 0) cat("(none)\n") else print(missing_years, n = Inf)

cat("\n---- Full diagnostic table ----\n")
print(diagnostic_table, n = Inf)

# --- Step 3: Visualizing Distributions (Density Plots) ---
# We pivot the raw data to "long" format for ggplot
plot_data2 <- final_df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Create density plots faceted by variable
# Using scales = "free" because your variables (counts vs. percentages) have different ranges
distribution_plot <- ggplot(plot_data2, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distribution of Imputed Variables",
       subtitle = "Note: Free scales used to compare different units",
       x = "Value",
       y = "Density")

# Print outputs
print(distribution_plot)

library(naniar)
gg_miss_var(final_df, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness

# Save to CSV for review
write_csv(diagnostic_table,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/04_diagnostics/diagnostic_table_imputed.csv")


