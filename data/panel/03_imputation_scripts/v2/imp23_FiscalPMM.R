##----Predictive Mean Matching----
# For fiscal performance (#2-3), relatively few NAs. 

library(mice)
library(tidyverse)
library(dplyr)
library(lattice)
library(caret)

#load df with "cleaned" data
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")

mice_data <- df_all %>%
  select(MPIO_CDPMP, year, IDF_2t3, IndRur_0t1, PIB_2t3, PobTot_12, DenPob_12)


# Check for successful join and missing pattern (optional but recommended)
print(md.pattern(mice_data[, c("IDF_2t3", "MPIO_CDPMP", "year", "IndRur_0t1", "PIB_2t3", "PobTot_12", "DenPob_12")])) 

# Prepare the data for MICE
# Define the set of variables to be used in the imputation model.
imputation_vars <- c("MPIO_CDPMP", "year", "IDF_2t3", "IndRur_0t1", "PIB_2t3", "PobTot_12", "DenPob_12")

mice_data_sub <- mice_data %>%
  select(all_of(imputation_vars))

# Convert ID and Year to factors for the imputation model
mice_data_sub$MPIO_CDPMP <- as.factor(mice_data_sub$MPIO_CDPMP)
mice_data_sub$year <- as.factor(mice_data_sub$year)

# Re-create the Unit Mean
unit_mean <- mice_data_sub %>%
  group_by(MPIO_CDPMP) %>%
  summarise(IDF_2t3_mean = mean(IDF_2t3, na.rm = TRUE)) %>%
  ungroup()

# Prepare the data for the new, simplified MICE run
mice_data_final <- mice_data_sub %>%
  left_join(unit_mean, by = "MPIO_CDPMP") %>%
  select(-MPIO_CDPMP) 

# Convert the year factor back to numeric for the trend variable
mice_data_final$year <- as.numeric(as.character(mice_data_final$year))

# Check Multicollinearity and Simplify Predictors
# Get only the numeric columns (including the target, mean, and year)
numeric_data <- mice_data_final %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(cor_matrix)

# --- MICE ----
# Initialize the imputation settings
ini_final <- mice(mice_data_final, maxit = 0)
pred_final <- ini_final$predictorMatrix

# Ensure the IDF_2t3_mean (which is complete) is NOT imputed
pred_final["IDF_2t3_mean", ] <- 0

# Non-parametric CART method
# CART is robust to multicollinearity and singular matrices.
imputed_data_success <- mice(mice_data_final, 
                             m = 5, 
                             maxit = 20, 
                             method = 'cart', # <-- THE CRITICAL CHANGE
                             predictorMatrix = pred_final, 
                             seed = 42,
                             printFlag = FALSE)

## Sensitivity Analyses and Visualization ----
# These plots will help you assess the plausibility of the imputed values from CART.
cat("\nPlotting Convergence (Should show intermingled lines for CART):\n")
plot(imputed_data_success, c("IDF_2t3")) 
cat("\nPlotting Density of Imputed vs. Observed Data (Should align closely):\n")
densityplot(imputed_data_success, ~IDF_2t3)
# The five red lines (imputed data) are clustered tightly together, which is good. Crucially, their overall distribution closely follows the shape of the observed blue line but is slightly narrower and taller (less variance in the imputed values). This is common for imputation models, as they often predict values closer to the mean, slightly shrinking the variance of the imputed data.

# Extract the full imputed dataset in long format (all 5 sets)
imputed_long_df <- complete(imputed_data_success, action = "long")

# Create a key map for MPIO_CDPMP from the original data (mice_data_sub)
mpio_key_map <- mice_data_sub %>%
  mutate(.id = row_number()) %>%
  select(.id, MPIO_CDPMP) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))

# Select ONLY the first imputation set (.imp = 1) AND join MPIO_CDPMP
df_single_imputation <- imputed_long_df %>%
  filter(.imp == 1) %>%
  left_join(mpio_key_map, by = ".id") %>%
  select(-.imp, -.id) %>%
  select(MPIO_CDPMP, year, IDF_2t3)


# Clean municipal data----
# Baseline: 1125 total
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/MunYrs.rds")
missing_mpio <- setdiff(unique(df_single_imputation$MPIO_CDPMP), unique(MunYrs$MPIO_CDPMP))
print(missing_mpio)
imp23b <- df_single_imputation %>%
  filter(!MPIO_CDPMP %in% missing_mpio)

# Visualize ----
observed_avg <- df_all %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(year) %>%
  summarise(avg_idf = mean(IDF_2t3, na.rm = TRUE), .groups = "drop") %>%
  mutate(Source = "Observed (Original)")

# 3. Summarize Imputed Data (from imp23b - complete)
imputed_avg <- imp23b %>%
  filter(year >= 2000 & year <= 2023) %>%
  group_by(year) %>%
  summarise(avg_idf = mean(IDF_2t3, na.rm = TRUE), .groups = "drop") %>%
  mutate(Source = "Imputed (Final)")

# 4. Combine for plotting
comparison_data <- bind_rows(observed_avg, imputed_avg)

# 5. Generate the Plot
ggplot(comparison_data, aes(x = year, y = avg_idf, color = Source, linetype = Source)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  # Styling the colors and lines
  scale_color_manual(values = c("Observed (Original)" = "red", "Imputed (Final)" = "darkgreen")) +
  scale_linetype_manual(values = c("Observed (Original)" = "dashed", "Imputed (Final)" = "solid")) +
  # Formatting
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  labs(
    title = "Fiscal Performance Index (IDF): Observed vs. Imputed",
    subtitle = "Municipal Averages (2000-2023)",
    x = "Year",
    y = "Average IDF Score",
    caption = "Red line represents averages of non-missing values; Green represents the full panel after imputation."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- Save as rds file ----
saveRDS(imp23b, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp23b.rds")



