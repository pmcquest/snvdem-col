##----Predictive Mean Matching----
library(mice)
library(caret)
# Define variables to impute
vars_to_impute <- c("IndRur_0t1", "IDF_0t1",
                    "Pobre_2t3", "NBI_2t3", "IPM_2t3", "IPM_2t3", "IPMu_2t3", "IPMr_2t3",
                    "Desp_10", "AccSub_10", 
                    "Hurto_11", "Homic_11", "Errad_11", 
                    "HHomi_11", "HDesa_11", "HSecu_11", "HRecl_11", 
                    "DisMer_13")

# 1. Filter data for 2000-2023 (keeping all columns)
df_impute <- final_data %>%
  filter(year >= 2000) %>%
  select(year, MPIO_CDPMP, all_of(vars_to_impute))  # Keep year & municipality code for reference

# 2. Check missing values before imputation
na_summary <- colSums(is.na(df_impute))
print(na_summary[na_summary > 0])  # Show only variables with NAs

# 3. Run Multiple Imputation using Predictive Mean Matching (PMM)
mice_model <- mice(df_impute, method = "pmm", m = 5, seed = 123, printFlag = TRUE)

# 4. Inspect imputations
summary(mice_model)  # Summary of imputed variables
# densityplot(mice_model)  # Check distributions of imputed vs. observed data

# 5. Choose a completed dataset (e.g., first imputation)
df_imputed <- complete(mice_model, action = 1)

# 6. Merge imputed data back into full dataset (1993-2023)
final_data_imputed <- final_data %>%
  filter(year < 2000) %>%  # Keep original pre-2000 data
  bind_rows(df_imputed)  # Add imputed 2000-2023 data

