#----- Merge imputed data ----

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/")
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

n_distinct(impStatic$MPIO_CDPMP) #1124
n_distinct(imp01$MPIO_CDPMP) #1124
n_distinct(imp23$MPIO_CDPMP) #1124
n_distinct(imp23b$MPIO_CDPMP) #1123
n_distinct(imp1011$MPIO_CDPMP) #1155
n_distinct(imp1214$MPIO_CDPMP) #1122
n_distinct(imp1516$MPIO_CDPMP) #1124
n_distinct(imp13$MPIO_CDPMP) #1122

# Baseline: 1125 total
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/MunYrs.rds")
missing_mpio <- setdiff(unique(impStatic$MPIO_CDPMP), unique(MunYrs$MPIO_CDPMP))
print(missing_mpio)
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
cat("Final MPIO count:", n_distinct(final_df$MPIO_CDPMP), "(Expected: 1125)\n")

# Re-run your check
unique_deptos_with_missing_pib <- final_df %>%
  filter(is.na(PIB_2t3)) %>%
  select(DPTO_CCDGO) %>%
  distinct() %>%
  pull(DPTO_CCDGO)

print(unique_deptos_with_missing_pib)

# some NAs but proportionally few (<0.008)
colSums(is.na(final_df))

# Filter out coordinates data
final_df <- final_df %>% select(-LATITUD, -LONGITUD)

##----Create standardized df (CDF)----
colvars_cdf <- final_df %>%
  mutate(across(4:32, ~ rank(.) / length(.)))

summary(colvars_cdf) #2000-2023

#----Save and document (final)----
write_rds(final_df, "G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/imputed_master_panel.rds")

write_rds(colvars_cdf, "G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/imputed_cdf_panel.rds")


#----NAs----
library(ggplot2)
library(stringr)

# --- Step 1: Calculate NA statistics ---
diagnostic_table <- imputed_master_panel %>%
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
  select(variable, missing_count, missing_pct)

# --- Step 2: Identify Missing Years ---
missing_years <- imputed_master_panel %>%
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

print(diagnostic_table)

# --- Step 3: Visualizing Distributions (Density Plots) ---
# We pivot the raw data to "long" format for ggplot
plot_data2 <- imputed_master_panel %>%
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
gg_miss_var(imputed_master_panel, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness


# Save to CSV for review
write.csv(diagnostic_table, "data/panel/05_geocoded_panel/diagnostics/diagnostic_table_imputed.csv", row.names = FALSE)


library(naniar)
gg_miss_var(df_balanced, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness


