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


# ---- Imputation (not necessary) ----

# Create a subset for imputation. 
impute_vars <- df_all %>%
  select(MPIO_CDPMP, year, IndRur_0t1, PobRur_0t1, PobUrb_0t1) %>%
  mutate(MPIO_CDPMP = as.factor(MPIO_CDPMP))

# Perform multiple imputation (m = 5 datasets) -- THIS IS VERY HEAVY (CPU)
# We use "pmm" (Predictive Mean Matching)
imputed_data_obj <- mice(impute_vars, method = "pmm", m = 5, seed = 123)

problem_codes <- c("23685", "27086", "27415", "99572", "99760")
# We use the MPIO_CDPMP codes to guarantee the flag exists
df_all <- df_all %>%
  mutate(imputed_flag = if_else(MPIO_CDPMP %in% problem_codes, "Imputed", "Observed"))

# 3. Update the data with the imputed values (if not already done)
completed_df <- complete(imputed_data_obj, 1)
df_all$IndRur_0t1 <- completed_df$IndRur_0t1

# 4. Run the visualization
library(ggplot2)
ggplot(df_all, aes(x = IndRur_0t1, fill = imputed_flag)) +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("Imputed" = "#E41A1C", "Observed" = "#377EB8")) +
  labs(title = "Distribution of Observed vs. Imputed Rurality index",
       subtitle = "Imputation Method: Predictive Mean Matching (PMM)",
       x = "Rurality Score (0-1)", y = "Density", fill = "Data Type") +
  theme_minimal()

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

