#----Growth-based interpolation----
# Used for demographic variables such as Indigenous population (#14). We assume that birth and death rates are largely more predictable than socioeconomic or violence trends. 

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")

# Sort data for interpolation
new_levels <- c("01001", "05000", "08000", "13000", "15000", "17000", 
                "18000", "19000", "20000", "23000", "25000", "27000", 
                "41000", "44000", "47000", "50000", "52000", "54000", 
                "63000", "66000", "68000", "70000", "73000", "76000", 
                "81000", "85000", "86000", "88000", "91000", "94000", 
                "95000", "97000", "99000")

df_all <- df_all %>%
  arrange(MPIO_CDPMP, year) %>%
  filter(!MPIO_CDPMP %in% new_levels)

imp14 <- df_all %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    ind_growth_factor = ifelse(!is.na(PobInd_14[year == 2018]) & !is.na(PobInd_14[year == 2005]), 
                               (PobInd_14[year == 2018] - PobInd_14[year == 2005]) / 13, 
                               NA),
    etn_growth_factor = ifelse(!is.na(PobEtn_14[year == 2018]) & !is.na(PobEtn_14[year == 2005]), 
                               (PobEtn_14[year == 2018] - PobEtn_14[year == 2005]) / 13, 
                               NA),
    # Apply imputation using calculated growth factors, ensuring non-negative values
    PobInd_14 = ifelse(is.na(PobInd_14) & !is.na(PobInd_14[year == 2005]), 
                       pmax(PobInd_14[year == 2005] + ind_growth_factor * (year - 2005), 0),  # Ensure non-negative values
                       PobInd_14),
    
    PobEtn_14 = ifelse(is.na(PobEtn_14) & !is.na(PobEtn_14[year == 2005]), 
                       pmax(PobEtn_14[year == 2005] + etn_growth_factor * (year - 2005), 0),  # Ensure non-negative values
                       PobEtn_14)
  ) %>%
  ungroup()


## ---- Impute last gaps with Median Imputation ----
# some municipalities have NAs because they are recently incorporated or never had data
imp14 <- imp14 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    PobInd_14 = ifelse(is.na(PobInd_14), median(PobInd_14, na.rm = TRUE), PobInd_14),
    PobEtn_14 = ifelse(is.na(PobEtn_14), median(PobEtn_14, na.rm = TRUE), PobEtn_14),
    PobTot_0t1 = ifelse(is.na(PobTot_0t1), median(PobTot_0t1, na.rm = TRUE), PobTot_0t1)
  ) %>%
  ungroup()

# Check for any other missing variables
colSums(is.na(imp14))
summary(imp14$PobEtn_14)
summary(imp14$PobInd_14)


# ---- Visualize the data ----
# Aggregate by year (mean values for visualization)
df_plot <- imp14 %>%
  group_by(year) %>%
  summarise(
    PobInd_14 = mean(PobInd_14, na.rm = TRUE),
    PobEtn_14 = mean(PobEtn_14, na.rm = TRUE),
    PobTot_12 = mean(PobTot_12, na.rm = TRUE)  # Include total population
  ) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")
# Plot
ggplot(df_plot, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Trends in Indigenous, Ethnic, and Total Population (2000-2020)",
       x = "Year", y = "Population", color = "Variable") +
  theme(legend.position = "bottom")


#----Sensitivity analysis----

##---- 1) LOESS ----
# Apply LOESS imputation for each variable (sensitivity analysis)
imp14_loess <- imp14 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    # LOESS imputation for PobInd_14 (indigenous population), ensuring non-negative values
    PobInd_14 = ifelse(is.na(PobInd_14), 
                       pmax(predict(loess(PobInd_14 ~ year, data = .)), 0),  # Ensure non-negative values
                       PobInd_14),
    
    # LOESS imputation for PobEtn_14 (ethnic population), ensuring non-negative values
    PobEtn_14 = ifelse(is.na(PobEtn_14), 
                       pmax(predict(loess(PobEtn_14 ~ year, data = .)), 0),  # Ensure non-negative values
                       PobEtn_14)
  ) %>%
  ungroup()


colSums(is.na(imp14_loess))
summary(imp14_loess$PobEtn_14)
summary(imp14_loess$PobInd_14)

##---- 2) Bayesian (heavy...) ----
library(brms)

# Define Bayesian mixed-effects model for imputation (heavy)
bayes_model <- brm(
  PobInd_14 ~ s(year) + (1 | MPIO_CDPMP),  # Smooth function for time + random effects
  data = df_all,
  family = gaussian(),
  prior = c(set_prior("normal(0, 5)", class = "b")),  # Weakly informative prior
  iter = 2000, warmup = 500, chains = 4, cores = 4
)

# Generate predictions using the brms model
predictions <- predict(bayes_model, newdata = df_all, summary = FALSE)

# The predictions object is usually a matrix where each row corresponds to a different posterior draw and each column corresponds to a different observation.

# For imputation, you may want to use the mean of the posterior predictions for each observation
imp14$PobInd_14_imputed <- rowMeans(predictions)  # Mean of posterior samples for PobInd_14
imp14$PobEtn_14_imputed <- rowMeans(predictions)  # Mean of posterior samples for PobEtn_14


#---- Compare original to LOESS imputed values ----

library(ggplot2)
# Create a long-format dataframe for both baseline and LOESS (Ensure both datasets are properly included)
imp14_comparison <- bind_rows(
  imp14 %>%
    gather(key = "variable", value = "value", PobInd_14, PobEtn_14, PobTot_0t1) %>%
    mutate(status = "Baseline"),  # Assign status to Baseline data
  
  imp14_loess %>%
    gather(key = "variable", value = "value", PobInd_14, PobEtn_14, PobTot_0t1) %>%
    mutate(status = "LOESS")  # Assign status to LOESS imputed data
)

ggplot(imp14_comparison, aes(x = log(value + 1), fill = status)) +
  geom_density(alpha = 0.3, color = NA) +  # Increase transparency for better overlap visibility
  facet_wrap(~variable, scales = "free") +
  labs(title = "Log-Transformed Density Plot: Baseline vs LOESS Imputation",
       x = "Log(Value)",
       y = "Density") +
  theme_minimal()





#----Save and document----
imp14 <- imp14 %>% 
  mutate(PropInd_14 = PobInd_14/PobTot_12) %>% # Create proportions
  mutate(PropEtn_14 = PobEtn_14/PobTot_12)

imp1214 <- imp14 %>%
  select(MPIO_CDPMP, year, DenPob_12, PropInd_14, PropEtn_14)
# Check to see if there are still missing values
colSums(is.na(imp1214))


write_rds(imp1214, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp1214.rds")

