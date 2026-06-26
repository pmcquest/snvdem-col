#----Multiple Imputation and Predictive Mean Matching----
# For crime data (#10, 11) which is highly variable, data from other variables and similar units can be used for multiple imputation (MI) and predictive mean matching (PMM). 

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(mice)
library(ggplot2)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds") %>%
  mutate(MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), width = 5, side = "left", pad = "0"))

# Subset only crime-related variables + covariates, keeping MPIO_CDPMP & year as predictors
crime_vars <- df_all %>%
  select(MPIO_CDPMP, year, Desp_1011, VDays_1011, HHomix_1011, PobTot_12)
colSums(is.na(crime_vars)) # relatively few NAs (~5%) for Desplazamientos

# Multiple imputation ----
library(mice)
# Define the variables to use in the imputation model
imputation_vars <- crime_vars

# Run the multiple imputation (e.g., creating 5 imputed datasets)
# Use 'pmm' (Predictive Mean Matching) which is good for non-negative count data.
imputed_data <- mice(imputation_vars, m = 5, method = 'pmm', seed = 42)

# To use the imputed data in an analysis:
fit <- with(imputed_data, lm(Desp_1011 ~ HHomix_1011 + VDays_1011))
summary(pool(fit)) # Combine the results from the 5 datasets


##----Compare observed vs. imputed----

# Compare the density of Desp_1011(observed) vs. Desp_1011 (imputed)
densityplot(imputed_data, ~Desp_1011)
imputed_desp <- complete(imputed_data, action = "long", include = TRUE) %>%
  as_tibble() %>%
  filter(.imp != 0) %>%
  pull(Desp_1011)
observed_desp <- imputed_data$data %>%
  filter(!is.na(Desp_1011)) %>%
  pull(Desp_1011)
print(summary(observed_desp))
print(summary(imputed_desp)) 
# Observe mean/SD of the variable across all 5 completed datasets
summary_comparison <- with(imputed_data, list(mean_desp = mean(Desp_1011), sd_desp = sd(Desp_1011)))
print(summary_comparison)


#----Sensitivity analysis----
library(plm)
library(zoo)


# Set up the results table for comparison
results_table <- tibble(
  Method = character(),
  Desp_1011_Estimate = numeric(),
  VDays_1011_Estimate = numeric(),
  HHomix_1011_Estimate = numeric(),
  Desp_1011_R2 = numeric()
)

## --- 1. MICE (PMM) Result (previously pooled result) ----
# Pooled summary:
# Desp_1011 ~ HHomix_1011 (10.28) + VDays_1011 (88.87)
mice_pooled_results <- list(
  Desp_1011_Estimate = 20.65, # Intercept
  VDays_1011_Estimate = 88.87, 
  HHomix_1011_Estimate = 10.28
)

results_table <- results_table %>% add_row(
  Method = "MICE (PMM)",
  Desp_1011_Estimate = mice_pooled_results$Desp_1011_Estimate,
  VDays_1011_Estimate = mice_pooled_results$VDays_1011_Estimate,
  HHomix_1011_Estimate = mice_pooled_results$HHomix_1011_Estimate
)

print(results_table)

## --- 2. Mean Imputation ----
crime_mean <- crime_vars %>%
  mutate(Desp_1011_imputed = replace_na(Desp_1011, mean(Desp_1011, na.rm = TRUE)))
fit_mean <- lm(Desp_1011_imputed ~ HHomix_1011 + VDays_1011, data = crime_mean)

results_table <- results_table %>% add_row(
  Method = "Mean Imputation",
  Desp_1011_Estimate = coef(fit_mean)[1],
  VDays_1011_Estimate = coef(fit_mean)[3], # VDays_1011 is the third coefficient
  HHomix_1011_Estimate = coef(fit_mean)[2]
)

## --- 3. Panel Imputation: Locf/Nocf (Last/Next Observation Carried Forward) ----
crime_locf <- crime_vars %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    Desp_1011_locf = na.locf(Desp_1011, na.rm = FALSE),
    Desp_1011_imputed = na.locf(Desp_1011_locf, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

# Check if any NAs remain (if the entire time series is missing)
if (any(is.na(crime_locf$Desp_1011_imputed))) {
  global_mean <- mean(crime_vars$Desp_1011, na.rm = TRUE)
  crime_locf <- crime_locf %>%
    mutate(Desp_1011_imputed = replace_na(Desp_1011_imputed, global_mean))
}

fit_locf <- lm(Desp_1011_imputed ~ HHomix_1011 + VDays_1011, data = crime_locf)

results_table <- results_table %>% add_row(
  Method = "Locf/Nocf Imputation",
  Desp_1011_Estimate = coef(fit_locf)[1],
  VDays_1011_Estimate = coef(fit_locf)[3],
  HHomix_1011_Estimate = coef(fit_locf)[2]
)

## --- 4. Panel Imputation: Fixed Effects (FE) Regression ----
# The goal is to predict the missing Desp_1011 values using a FE model's components.
# 1. Create a complete case set to train the model
fe_train_data <- crime_vars %>% 
  filter(!is.na(Desp_1011) & !is.na(PobTot_12)) # Filter out NAs in all model vars

# Fit the FE model (lm)
fe_model <- lm(Desp_1011 ~ factor(MPIO_CDPMP) + factor(year) + PobTot_12 + HHomix_1011 + VDays_1011,
               data = fe_train_data)
# Note: Added factor(year) to make it a true Two-Way Fixed Effects model.

# --- 2. Predict for ALL rows using the FE model ---
# This uses the fitted model to generate a prediction for every row in crime_vars.
# R's predict() function handles the fixed effects (MPIO_CDPMP) internally,
# applying the estimated alpha_i for known units and NA for new/unseen units.

valid_municipalities <- levels(fe_model$model$MPIO_CDPMP)

# 2. Filter and Predict
crime_fe <- crime_vars %>%
  # Change: Filter to ONLY include municipalities the model knows
  filter(MPIO_CDPMP %in% valid_municipalities) %>%
  mutate(
    year_for_predict = ifelse(year > 2020, 2020, year),
    year_for_predict = factor(year_for_predict, levels = levels(fe_model$model$year))
  ) %>%
  # Use a simpler approach for the prediction
  mutate(
    Desp_1011_pred = predict(
      fe_model, 
      newdata = mutate(., year = year_for_predict)
    )
  )


# --- 3. Handle 'Unseen' Municipalities (Prediction NAs) ---
# Municipalities not in fe_train_data will have NA predictions.
# Fallback: Impute the remaining NA predictions with the global mean of the training data.
fe_train_data <- crime_vars %>% 
  filter(!is.na(Desp_1011)) %>%
  mutate(
    MPIO_CDPMP = factor(MPIO_CDPMP),
    year       = factor(year)
  )
# Fit the FE model
fe_model <- lm(Desp_1011 ~ MPIO_CDPMP + year + PobTot_12 + HHomix_1011 + VDays_1011,
               data = fe_train_data)

# 3. Extract model levels to ensure consistency during prediction
valid_year_levels <- levels(fe_model$model$year)
valid_mpio_levels <- levels(fe_model$model$MPIO_CDPMP)

# 4. Predict for all rows using the "on-the-fly" factor transformation
prediction_data <- crime_vars %>%
  mutate(
    year       = factor(year, levels = valid_year_levels),
    MPIO_CDPMP = factor(MPIO_CDPMP, levels = valid_mpio_levels)
  )

# Use the temporary dataframe to generate predictions
crime_fe <- crime_vars %>%
  mutate(
    Desp_1011_pred = predict(fe_model, newdata = prediction_data)
  )


# 5. Final Imputation Logic
global_mean_desp <- mean(fe_train_data$Desp_1011, na.rm = TRUE)

crime_fe <- crime_fe %>%
  mutate(
    # A: Fill original NAs with predictions
    Desp_1011_imputed = ifelse(is.na(Desp_1011), Desp_1011_pred, Desp_1011),
    
    # B: Fallback for rows where prediction failed (unseen municipalities)
    Desp_1011_imputed = replace_na(Desp_1011_imputed, global_mean_desp),
    
    # C: Logical constraint: Displacement cannot be negative
    Desp_1011_imputed = pmax(0, Desp_1011_imputed)
  )

# --- 4. Run LM on the FE-Imputed Data for Comparison ---
fit_fe <- lm(Desp_1011_imputed ~ HHomix_1011 + VDays_1011, data = crime_fe)

# This tells you exactly which column is empty
summary(crime_fe[c("Desp_1011_imputed", "HHomix_1011", "VDays_1011")])

# This counts how many rows have a complete set of data
sum(complete.cases(crime_fe[c("Desp_1011_imputed", "HHomix_1011", "VDays_1011")]))

results_table <- results_table %>% add_row(
  Method = "FE Regression Imputation",
  Desp_1011_Estimate = coef(fit_fe)[1],
  VDays_1011_Estimate = coef(fit_fe)["VDays_1011"],
  HHomix_1011_Estimate = coef(fit_fe)["HHomi_1011"]
)

# --- Final Check ---
cat("\nFE Imputation Complete. NA count in imputed column:\n")
print(sum(is.na(crime_fe$Desp_1011_imputed)))
print(head(crime_fe))


## --- Final Comparison Table ---
print(results_table %>%
        select(-any_of("Desp_1011_R2")) %>%
        mutate(across(where(is.numeric), ~round(., 3)))
)

# Inspect MICE data ----
# --- Extract the First Imputed Dataset ---
# The complete() function extracts a specified imputed dataset.
# action = 1 extracts the first of the M=5 datasets created by MICE.
imp1011 <- complete(imputed_data, action = 1) %>%
  as_tibble() 

imp1011 <- imp1011 %>%
  select(MPIO_CDPMP, year, Desp_1011, VDays_1011, HHomix_1011)

# Visualize how many imputations were made per year
is_imputed <- is.na(crime_vars$Desp_1011)
imputed_municipalities <- imp1011[is_imputed, ] %>%
  mutate(Desp_1011_Original = crime_vars$Desp_1011[is_imputed]) %>%
  select(MPIO_CDPMP, year, Desp_1011_Original, Desp_1011, everything()) %>%
  rename(Desp_1011_Imputed = Desp_1011)
cat(paste("Total rows with imputed data:", nrow(imputed_municipalities), "\n"))
table(imputed_municipalities$year)

# Summary stats: observed
stats_observed <- crime_vars %>%
  filter(!is.na(Desp_1011)) %>%
  summarise(
    N = n(),
    Mean = mean(Desp_1011, na.rm = TRUE),
    SD = sd(Desp_1011, na.rm = TRUE),
    Min = min(Desp_1011, na.rm = TRUE),
    Q1 = quantile(Desp_1011, 0.25, na.rm = TRUE),
    Median = median(Desp_1011, na.rm = TRUE),
    Q3 = quantile(Desp_1011, 0.75, na.rm = TRUE),
    Max = max(Desp_1011, na.rm = TRUE)
  ) %>%
  mutate(Data_Type = "Observed Data (N=24,472)")

# Summary stats: imputed
stats_imputed <- imp1011 %>%
  summarise(
    N = n(),
    Mean = mean(Desp_1011, na.rm = TRUE),
    SD = sd(Desp_1011, na.rm = TRUE),
    Min = min(Desp_1011, na.rm = TRUE),
    Q1 = quantile(Desp_1011, 0.25, na.rm = TRUE),
    Median = median(Desp_1011, na.rm = TRUE),
    Q3 = quantile(Desp_1011, 0.75, na.rm = TRUE),
    Max = max(Desp_1011, na.rm = TRUE)
  ) %>%
  mutate(Data_Type = "Imputed Data (N=25,804)")


comparison_summary <- bind_rows(stats_observed, stats_imputed) %>%
  pivot_longer(
    cols = c(Mean, SD, Min, Q1, Median, Q3, Max),
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Data_Type,
    values_from = Value
  )

cat("\n## 📋 Summary Statistics Comparison for Desp_1011\n")
print(comparison_summary %>%
        mutate(across(where(is.numeric), ~round(., 2))) %>%
        select(Statistic, everything())
)
# the two sets look very similar (that's a good thing)

# --- Check for any other missing variables ----
na_counts <- colSums(is.na(imp1011))
print(na_counts)

summary(imp1011)



#----Save and document----
write_rds(imp1011, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp1011.rds")



# Visualizations ----


library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Prepare data for plotting ----
# We tag rows as "Observed" or "Imputed" based on the original crime_vars
plot_data <- imp1011 %>%
  mutate(
    Type = ifelse(is.na(crime_vars$Desp_1011), "Imputed", "Observed")
  ) %>%
  # Pivot to long format for faceted plotting
  pivot_longer(
    cols = c(Desp_1011, VDays_1011, HHomix_1011),
    names_to = "Variable",
    values_to = "Value"
  )

# 2. Calculate annual means for the trend lines ----
trend_data <- plot_data %>%
  group_by(year, Variable, Type) %>%
  summarise(
    Mean_Value = mean(Value, na.rm = TRUE),
    se = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 3. Create the Plot ----
ggplot(trend_data, aes(x = year, y = Mean_Value, color = Type, fill = Type)) +
  # Add ribbon for standard error (shows uncertainty in imputation)
  geom_ribbon(aes(ymin = Mean_Value - se, ymax = Mean_Value + se), alpha = 0.2, color = NA) +
  # Add trend line
  geom_line(size = 1) +
  # Separate plots for each variable with independent y-axes
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Comparison of Observed vs. MICE Imputed Trends (1998-2023)",
    subtitle = "Imputation density increases significantly after 2021",
    x = "Year",
    y = "Annual Mean Value",
    color = "Data Status",
    fill = "Data Status"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Imputed" = "#E41A1C", "Observed" = "#377EB8")) +
  scale_fill_manual(values = c("Imputed" = "#E41A1C", "Observed" = "#377EB8")) +
  theme(legend.position = "bottom")

# Density plot (heavily right-skewed)
# It shows the observed data in blue and the 'm' imputations in red
densityplot(imputed_data, ~Desp_1011 + VDays_1011 + HHomix_1011, 
            main = "MICE Density Plot: Observed vs. Imputed",
            layout = c(1, 3)) # Stack them vertically

# Log transform
# 1. Extract all 5 imputations + the original data
# .imp == 0 is the original data (with NAs)
# .imp 1 through 5 are the completed datasets
all_imps_long <- complete(imputed_data, action = "long", include = TRUE) %>%
  select(.imp, .id, Desp_1011, VDays_1011, HHomix_1011) %>%
  pivot_longer(cols = c(Desp_1011, VDays_1011, HHomix_1011), 
               names_to = "Variable", values_to = "Value")

# 2. Apply log1p transformation: log(x + 1)
# This handles the 0s and reduces the skewness for visualization
all_imps_long <- all_imps_long %>%
  mutate(log_value = log1p(Value))

# 3. Create the Plot
ggplot(all_imps_long, aes(x = log_value, group = .imp, color = factor(.imp == 0))) +
  # Draw density lines: Thick blue for Observed, thin red for Imputations
  geom_density(aes(size = factor(.imp == 0), alpha = factor(.imp == 0))) +
  facet_wrap(~Variable, scales = "free", ncol = 1) +
  # Customizing scales and colors
  scale_color_manual(values = c("TRUE" = "#377EB8", "FALSE" = "#E41A1C"), 
                     labels = c("TRUE" = "Observed Data", "FALSE" = "MICE Imputations"),
                     name = "Data Source") +
  scale_size_manual(values = c("TRUE" = 1.2, "FALSE" = 0.5), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = "none") +
  labs(
    title = "Log-Transformed Density Comparison: Observed vs. Imputed",
    subtitle = "Transformation: log(Value + 1) | Shows if imputations match the distribution of real data",
    x = "Log(Value + 1)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )


