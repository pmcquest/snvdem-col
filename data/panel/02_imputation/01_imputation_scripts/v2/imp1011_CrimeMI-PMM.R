#----Multiple Imputation and Predictive Mean Matching----
# For crime data (#10, 11) which is highly variable, data from other variables and similar units can be used for multiple imputation (MI) and predictive mean matching (PMM). 

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/data_cleaned/")
# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(mice)
library(ggplot2)

# Load cleaned dataset
df_all <- read_rds("df_all_clean.rds")


# Step 1: Select only crime-related variables + covariates, keeping MPIO_CDPMP & year as predictors
crime_vars <- df_all %>%
  select(MPIO_CDPMP, year, Desp_10, VDays_1011, HHomi_11)
colSums(is.na(crime_vars))


# Step 2: Define the predictor matrix to ensure MPIO_CDPMP and year are used as predictors but NOT imputed
pred_matrix <- make.predictorMatrix(crime_vars)
pred_matrix[, c("MPIO_CDPMP", "year")] <- 0  # Do not impute these

# Step 3: Run Multiple Imputation with PMM
imp <- mice(crime_vars, method = "pmm", m = 5, seed = 123, predictorMatrix = pred_matrix)

# Step 4: 
# Extract imputed datasets in long format
df_imputed_long <- complete(imp, action = "long")

# Compute mean imputed values per MPIO_CDPMP-year
df_imputed <- df_imputed_long %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(across(-c(.imp, .id), mean, na.rm = TRUE), .groups = "drop")  # Mean across imputations

# Identify imputed variables (excluding MPIO_CDPMP and year)
imputed_vars <- setdiff(names(df_imputed), c("MPIO_CDPMP", "year"))

# Drop old versions of imputed variables from df_all before merging
df_all_imputed <- df_all %>%
  select(-one_of(imputed_vars)) %>%  # Remove old versions of imputed variables
  left_join(df_imputed, by = c("MPIO_CDPMP", "year"))  # Merge back with imputed values


#----Compare observed vs. imputed----

# Create a flag for imputed vs. observed data
df_all_imputed$imputed_flag <- ifelse(is.na(df_all$Homic_11), "Imputed", "Observed")

# Gather variables into long format
df_longdp <- df_all_imputed %>%
  pivot_longer(cols = c(Desp_10, Errad_10, Hurto_11, Homic_11,
                        HHomi_11, HDesa_11, HSecu_11, HRecl_11, PobTot_12, IDF_2t3),
               names_to = "variable", values_to = "value")

# Density plots with faceting
ggplot(df_longdp, aes(x = log1p(value), fill = imputed_flag)) +  # log1p to handle skewness
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +  # Separate plots for each variable
  labs(title = "Density Plots of Observed vs. Imputed Data (Log Scale)",
       x = "Log(Value + 1)", fill = "Data Type") +
  theme_minimal()

imp1011 <- df_all_imputed %>%
  select(MPIO_CDPMP, year, Desp_10, Errad_10, Hurto_11, Homic_11,
         HHomi_11, HDesa_11, HSecu_11, HRecl_11, PobTot_12, IDF_2t3)


#----Sensitivity analysis----
# Let's perform a sensitivity analysis with two different imputation methods: mean and LOESS
library(imputeTS)  # For time series imputation
library(dplyr)

# Impute using mean
imp1011_mean <- imp1011 %>%
  mutate(across(c(Desp_10, Errad_10, Hurto_11, Homic_11,
                  HHomi_11, HDesa_11, HSecu_11, HRecl_11, PobTot_12, IDF_2t3), 
                ~imputeTS::na_mean(.)))

# Impute using LOESS (if appropriate for your data)
imp1011_loess <- imp1011 %>%
  mutate(across(c(Desp_10, Errad_10, Hurto_11, Homic_11,
                  HHomi_11, HDesa_11, HSecu_11, HRecl_11, PobTot_12, IDF_2t3), 
                ~imputeTS::na_locf(.)))  # LOESS (Last Observation Carried Forward) as an example

# You can compare distributions, or run any statistical tests between these two methods
summary(imp1011_mean)
summary(imp1011_loess)

library(ggplot2)

# Reshape the data for plotting
imp1011_long <- bind_rows(
  imp1011 %>%
    mutate(imputation_method = "Original"),
  imp1011_mean %>%
    mutate(imputation_method = "Mean Imputation"),
  imp1011_loess %>%
    mutate(imputation_method = "LOESS Imputation")
)

# Plot the density for all three datasets
ggplot(imp1011_long, aes(x = HRecl_11, fill = imputation_method)) +
  geom_density(alpha = 0.5) +  # Add transparency to help with overlap visibility
  scale_fill_manual(values = c("blue", "red", "green")) +  # Customize colors
  labs(title = "Density Plot: HRecl for Different Imputation Methods",
       x = "Log-transformed HCoca_10 (log scale)",
       y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank())



#----Save and document----
# Select the subset of variables



# Check for any other missing variables
colSums(is.na(imp1011))



write_rds(imp1011, "G:/Shared drives/snvdem/snvdem-col/data/panel/data_imputed/imp1011.rds")

