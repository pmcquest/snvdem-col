#----Growth-based interpolation with external data and Random Forest----
# For socioeconomic development (#2-3), external data such as regional or national-level GDP growth may facilitate imputation of missing years. We have municipal GDP growth from 2000-2009 (available from CEDE-Los Andes) and department-level GDP. For sensitivity analysis, we also ran linear regression imputation and Random Forest (RF) imputation. We found that they are all very comparable methods. RF was selected because it produced fewer NAs.

library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2) 
library(randomForest) 
library(stats)

# Load data (Assuming current working directory is set correctly)
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")
summary(df_all$PIB_2t3)


# Load external data: Department GDP (PIBd_2t3)
PBID <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", sheet = "Cuadro 1", range = "A10:AS36")

# ---- Convert PBID from wide to long format and Merge ---
PBID <- PBID %>%
  rename(DPTO_CCDGO = `Código Departamento (DIVIPOLA)`) %>%
  rename(`2020` = 43, `2021` = 44, `2022` = 45) %>%
  filter(!is.na(DPTO_CCDGO),      # Removes true NA values
         DPTO_CCDGO != "*")      # Removes literal "*" values

PBID_long <- PBID %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "year", values_to = "PIBd_2t3", values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(DPTO_CCDGO), !is.na(PIBd_2t3), !is.na(year))

# Step 1 & 2: Calculate Department Growth Rate (2000-2009) and Merge PIBd_2t3
department_growth <- PBID_long %>%
  filter(year >= 2000 & year <= 2009) %>%
  group_by(DPTO_CCDGO) %>%
  arrange(year) %>%
  mutate(Dgrowth_rate = (PIBd_2t3 / lag(PIBd_2t3)) - 1) %>% # Simpler growth calculation
  summarise(avg_Dgrowth_rate = mean(Dgrowth_rate, na.rm = TRUE), .groups = "drop")

df_all <- df_all %>%
  mutate(department_code = substr(MPIO_CDPMP, 1, 2)) %>%
  left_join(department_growth, by = c("department_code" = "DPTO_CCDGO")) %>%
  left_join(PBID_long %>% select(DPTO_CCDGO, year, PIBd_2t3),
            by = c("department_code" = "DPTO_CCDGO", "year" = "year")) %>%
  filter(!is.na(MPIO_CDPMP))
sum(is.na(df_all$PIBd_2t3)) #2999 NAs

# Step 3 & 4: Compute and Merge Municipal Growth Rates (2000-2009)
municipal_growth <- df_all %>%
  filter(year >= 2000 & year <= 2009) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(Mgrowth_rate = (PIB_2t3 / lag(PIB_2t3)) - 1) %>%
  summarise(avg_Mgrowth_rate = mean(Mgrowth_rate, na.rm = TRUE), .groups = "drop")

df_all <- df_all %>%
  left_join(municipal_growth, by = "MPIO_CDPMP") %>%
  mutate(final_growth_rate = coalesce(avg_Mgrowth_rate, avg_Dgrowth_rate)) 
sum(is.na(df_all$final_growth_rate)) # 546 NAs of 29224
    
# Step 5: Imputation using Growth-Based Method (Forward projection from 2009)
df_all <- df_all %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    last_known_PIB_2t3 = ifelse(all(is.na(PIB_2t3[year <= 2009])), NA_real_, max(PIB_2t3[year <= 2009], na.rm = TRUE)),
    PIB_2t3_imputed = ifelse(is.na(PIB_2t3) & year >= 2010 & year <= 2023 & !is.na(last_known_PIB_2t3),
                             last_known_PIB_2t3 * (1 + final_growth_rate)^(year - 2009),
                             PIB_2t3) 
  ) %>%
  ungroup()

# Combine Imputed and Observed data into the primary imputed column
df_all <- df_all %>%
  mutate(PIB_2t3_imputed = coalesce(PIB_2t3_imputed, PIB_2t3)) 

cat("\nGrowth-based Imputation Complete. Remaining NAs in PIB_2t3_imputed:", sum(is.na(df_all$PIB_2t3_imputed)), "\n")
#2896 NAs...




# ---- Sensitivity Analysis ----
## ---- Linear Regression Imputation (Panel-Corrected Prediction) ----
df_train_fe <- df_all %>%
  filter(!is.na(PIB_2t3))

dummies <- model.matrix(~ MPIO_CDPMP - 1, data = df_train_fe)

# Combine the response variable (y) and the predictors (X)
y_train <- df_train_fe$PIB_2t3
X_train <- data.frame(
  dummies, 
  year = df_train_fe$year, 
  PIBd_2t3 = df_train_fe$PIBd_2t3
)

# --- Step 2: Fit the Full Fixed Effects Model (lm_model_fe) ---
lm_model_fe <- lm(y_train ~ . - 1, data = X_train) # -1 removes the intercept

known_mpio_codes <- unique(df_train_fe$MPIO_CDPMP)

# --- Step 3: Fit the Pooled Model (for unknown MPIOs) ---
lm_model_pooled <- lm(PIB_2t3 ~ year + PIBd_2t3, data = df_train_fe)

# --- Step 4: Prepare the Prediction Dataset (Original NAs) ---
df_predict_na <- df_all %>%
  filter(is.na(PIB_2t3_imputed))

# --- Step 5: Apply Predictions ---
# Create prediction matrix for the NA data
dummies_predict_all <- model.matrix(~ MPIO_CDPMP - 1, data = df_predict_na)

# Filter the prediction dummies to match ONLY the columns (MPIOs) in the training data
known_dummy_cols <- colnames(dummies)[colnames(dummies) %in% colnames(dummies_predict_all)]
dummies_predict_known <- dummies_predict_all[, known_dummy_cols, drop = FALSE]

# Create the full prediction data frame (adding zero columns for missing dummies)
X_predict_known <- data.frame(
  dummies_predict_known,
  year = df_predict_na$year,
  PIBd_2t3 = df_predict_na$PIBd_2t3
)

# Add back any dummy columns that were in the model but are missing from the prediction data (value = 0)
missing_cols <- setdiff(colnames(X_train), colnames(X_predict_known))
X_predict_known[missing_cols] <- 0
X_predict_known <- X_predict_known[, colnames(X_train)] # Reorder to match training data

predicted_known_values <- predict(lm_model_fe, newdata = X_predict_known)

# Join the predictions back to the main df_all using MPIO_CDPMP and year
df_known_predictions <- df_predict_na %>%
  mutate(PIB_2t3_lm_predicted = predicted_known_values) %>%
  select(MPIO_CDPMP, year, PIB_2t3_lm_predicted)

df_all <- df_all %>%
  left_join(df_known_predictions, by = c("MPIO_CDPMP", "year"))


# 5b. Predict for UNKNOWN municipalities (using POOLED model)
df_unknown_predict <- df_predict_na %>%
  filter(!(MPIO_CDPMP %in% known_mpio_codes))

predicted_unknown <- predict(lm_model_pooled, newdata = df_unknown_predict)

df_unknown_predictions <- df_unknown_predict %>%
  mutate(PIB_2t3_lm_predicted_pooled = predicted_unknown) %>%
  select(MPIO_CDPMP, year, PIB_2t3_lm_predicted_pooled)

df_all <- df_all %>%
  left_join(df_unknown_predictions, by = c("MPIO_CDPMP", "year"))

# --- Step 6: Final Coalesce and Cleanup (Revised for Join Safety) ---
df_all <- df_all %>%
  mutate(
    PIB_2t3_lm_predicted = NA_real_,
    PIB_2t3_lm_predicted_pooled = NA_real_
  )

df_all <- df_all %>%
  rows_update(df_known_predictions, by = c("MPIO_CDPMP", "year"))

df_unknown_predictions_temp <- df_unknown_predictions %>% 
  rename(PIB_2t3_lm_predicted_pooled_new = PIB_2t3_lm_predicted_pooled)

df_all <- df_all %>%
  left_join(df_unknown_predictions_temp, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    PIB_2t3_lm_predicted_pooled = coalesce(PIB_2t3_lm_predicted_pooled_new, PIB_2t3_lm_predicted_pooled)
  ) %>%
  select(-PIB_2t3_lm_predicted_pooled_new)


# --- Final Coalesce and Cleanup ---
df_all <- df_all %>%
  mutate(
    PIB_2t3_final_lm_predicted = coalesce(PIB_2t3_lm_predicted, PIB_2t3_lm_predicted_pooled),
    PIB_2t3_imputed_lm = coalesce(PIB_2t3_imputed, PIB_2t3_final_lm_predicted)
  ) %>%
  select(-PIB_2t3_lm_predicted, -PIB_2t3_lm_predicted_pooled, -PIB_2t3_final_lm_predicted)

# 1077 NAs left
cat("\nLinear Regression Imputation (Join Fix Applied) Complete. Remaining NAs in PIB_2t3_imputed_lm:", sum(is.na(df_all$PIB_2t3_imputed_lm)), "\n")


## ---- Random Forest Imputation ----

# 1. Training data: Use only OBSERVED PIB_2t3 data AND filter out NAs in predictors
df_train_rf <- df_all %>%
  filter(!is.na(PIB_2t3)) %>%
  filter(!is.na(department_code), !is.na(year), !is.na(PIBd_2t3)) %>% 
  mutate(department_code = as.factor(department_code)) # Ensure factor for RF

training_levels <- levels(df_train_rf$department_code)

# Fit the RF model
rf_model <- randomForest(PIB_2t3 ~ department_code + year + PIBd_2t3, data = df_train_rf)

# 2. Prediction: Use the final, fixed prediction logic.
df_all <- df_all %>%
  mutate(department_code_factor = factor(department_code, levels = training_levels)) %>%
  rowwise() %>% 
  mutate(
    PIB_2t3_rf_predicted = predict(rf_model, newdata = list(
      department_code = department_code_factor, 
      year = year, 
      PIBd_2t3 = PIBd_2t3
    ))
  ) %>%
  ungroup() %>%
  mutate(
    PIB_2t3_imputed_rf = coalesce(PIB_2t3_imputed, PIB_2t3_rf_predicted)
  ) %>%
  select(-department_code_factor, -PIB_2t3_rf_predicted)

cat("Random Forest Imputation Complete. Remaining NAs in PIB_2t3_imputed_rf:", sum(is.na(df_all$PIB_2t3_imputed_rf)), "\n")

# ---- Comparison Plot ----
ggplot(df_all) +
  geom_density(aes(x = log1p(PIB_2t3_imputed_lm), fill = "Linear Regression"), alpha = 0.5) +
  geom_density(aes(x = log1p(PIB_2t3_imputed_rf), fill = "Random Forest"), alpha = 0.5) +
  geom_density(aes(x = log1p(PIB_2t3_imputed), fill = "Growth-Based"), alpha = 0.5) +
  labs(title = "Comparison of PIB Imputation Methods", x = "Log(PIB_2t3 + 1)", y = "Density", fill = "Imputation Method") +
  theme_minimal()

#----Save and document----
# Rationale: Selecting the Random Forest (RF) method because it produced the 
# fewest remaining missing values (589 NAs) while maintaining a plausible distribution
# compared to the other two methods.

# 1. Final selection and renaming
imp23 <- df_all %>%
  select(MPIO_CDPMP, year, PIB_2t3_imputed_rf) %>%
  rename(PIB_2t3 = PIB_2t3_imputed_rf)
imp23$PIB_2t3 <- as.vector(imp23$PIB_2t3)



# 2. Check for final NAs 
cat("\nFinal NAs in chosen PIB_2t3 column (RF Method):", sum(is.na(imp23$PIB_2t3)), "\n")

# 3. Save the final imputed data (assuming the path is correct)
write_rds(imp23, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp23.rds")
