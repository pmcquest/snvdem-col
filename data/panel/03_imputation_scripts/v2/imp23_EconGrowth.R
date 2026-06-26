#----Growth-based interpolation with external data----
# For socioeconomic development (#2-3), external data such as regional or national-level GDP growth may facilitate imputation of missing years. We have municipal GDP growth from 2000-2009 (available from CEDE-Los Andes) and department-level GDP.

setwd("G:/Shared drives/snvdem/snvdem-col")
# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyr)

# Load cleaned dataset
df_all <- read_rds("data/panel/data_cleaned/df_all_clean.rds")

# Load external data: Department GDP
PBID <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", sheet = "Cuadro 1", range = "A10:AS36")

# ---- Imputation ----
## ---- Convert PBID from wide to long format ----
# rename
PBID <- PBID %>%
  rename(DPTO_CCDGO = `Código Departamento (DIVIPOLA)`) %>%
  rename(`2020` = 43, `2021` = 44, `2022` = 45)
PBID_long <- PBID %>%
  pivot_longer(cols = `1980`:`2022`, names_to = "year", values_to = "PIBd_2t3", values_drop_na = TRUE) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(DPTO_CCDGO), !is.na(PIBd_2t3), !is.na(year))


## ---- Step 1: Calculate Department-Level Growth Rates (2000-2009) ----
department_growth <- PBID_long %>%
  filter(year >= 2000 & year <= 2009) %>%
  group_by(DPTO_CCDGO) %>%
  arrange(year) %>%
  mutate(Dgrowth_rate = (PIBd_2t3 - lag(PIBd_2t3)) / lag(PIBd_2t3)) %>%
  summarise(avg_Dgrowth_rate = mean(Dgrowth_rate, na.rm = TRUE))


## ---- Step 2: Merge Department Growth Rates and PIBd_2t3 into df_all ----
df_all <- df_all %>%
  mutate(department_code = substr(MPIO_CDPMP, 1, 2)) %>%
  left_join(department_growth, by = c("department_code" = "DPTO_CCDGO")) %>%
  left_join(PBID_long %>% select(DPTO_CCDGO, year, PIBd_2t3), 
            by = c("department_code" = "DPTO_CCDGO", "year" = "year"))

## ---- Step 3: Compute Municipal Growth Rates (2000-2009) ----
municipal_growth <- df_all %>%
  filter(year >= 2000 & year <= 2009) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(Mgrowth_rate = (PIB_2t3 - lag(PIB_2t3)) / lag(PIB_2t3)) %>%
  summarise(avg_Mgrowth_rate = mean(Mgrowth_rate, na.rm = TRUE), .groups = "drop")

## ---- Step 4: Merge and Handle Missing Growth Rates ----
df_all <- df_all %>%
  left_join(municipal_growth, by = "MPIO_CDPMP") %>%
  mutate(final_growth_rate = coalesce(avg_Mgrowth_rate, avg_Dgrowth_rate))  # Prefer municipal growth


## ---- Step 5: Imputation using Growth-Based Method ----
df_all <- df_all %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    last_known_PIB_2t3 = ifelse(any(!is.na(PIB_2t3) & year <= 2009),
                                max(PIB_2t3[year <= 2009], na.rm = TRUE), 
                                NA_real_),
    PIB_2t3_imputed = ifelse(is.na(PIB_2t3) & year >= 2010 & year <= 2020 & !is.na(last_known_PIB_2t3),
                             last_known_PIB_2t3 * (1 + final_growth_rate)^(year - 2009),
                             PIB_2t3)
  ) %>%
  ungroup()

# Summary of missing values per variable
colSums(is.na(df_all))

## ---- Impute for stubborn NAs in PIB_2t3 ----
# There are some municipalities with NAs still, probably because they are new and lack prior data
# These include: Arauca (81), Casanare (85), Putumayo (86), Amazonas (91), Guainía (94), Guaviare (95), Vaupés (97), and Vichada (99), which are mostly peripheral or sparsely populated regions

# Using first mean growth rate per municipality - reduces NAs slightly
df_all <- df_all %>%
  filter(year >= 2000) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    mean_growth_rate = mean((PIB_2t3_imputed - lag(PIB_2t3_imputed)) / lag(PIB_2t3_imputed), na.rm = TRUE),
    PIB_2t3_imputed = ifelse(is.na(PIB_2t3_imputed), lag(PIB_2t3_imputed) * (1 + mean_growth_rate), PIB_2t3_imputed)
  ) %>%
  ungroup()

# Using Dept-level data for stubborn NAs - barely any more
df_all %>% filter(is.na(PIB_2t3_imputed)) %>% count(year, department_code)
df_all <- df_all %>%
  group_by(department_code, year) %>%
  mutate(
    avg_municipal_share = ifelse(all(is.na(PIB_2t3_imputed / PIBd_2t3)), NA, mean(PIB_2t3_imputed / PIBd_2t3, na.rm = TRUE)),
    PIB_2t3_imputed = ifelse(is.na(PIB_2t3_imputed) & !is.na(PIBd_2t3) & !is.na(avg_municipal_share), 
                     PIBd_2t3 * avg_municipal_share, PIB_2t3_imputed)
  ) %>%
  ungroup()




# ---- Visualization ----
## ---- Create a Flag for Observed vs. Imputed Data ----
df_all <- df_all %>%
  mutate(imputed_flag = ifelse(is.na(PIB_2t3), "Imputed", "Observed"))

## ---- Density Plot for Growth-Based Imputed vs Observed ----
ggplot(df_all, aes(x = log1p(PIB_2t3_imputed), fill = imputed_flag)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Observed vs. Imputed Municipal GDP",
       x = "Log(PIB_2t3 + 1)", y = "Density", fill = "Data Type") +
  theme_minimal()


## ---- Line Plot for Growth-Based Trends ----
df_plot <- df_all %>%
  filter(year <= 2020) %>%
  group_by(year) %>%
  summarise(original_PIB = mean(PIB_2t3, na.rm = TRUE),
            imputed_PIB = mean(PIB_2t3_imputed, na.rm = TRUE))

ggplot(df_plot, aes(x = year)) +
  geom_line(aes(y = original_PIB, color = "Original"), size = 1) +
  geom_point(aes(y = original_PIB, color = "Original"), size = 3) +
  geom_line(aes(y = imputed_PIB, color = "Imputed"), size = 1, linetype = "dashed") +
  geom_point(aes(y = imputed_PIB, color = "Imputed"), size = 3, shape = 1) +
  scale_color_manual(values = c("Original" = "blue", "Imputed" = "red")) +
  theme_minimal() +
  labs(title = "Trends in PIB_2t3 (Original and Imputed) for 2010-2020",
       x = "Year", y = "PIB_2t3 (in total pesos)", color = "Data Type") +
  theme(legend.position = "bottom")

# ---- Sensitivity Analysis ----

## ---- Linear regression imputation ----
# Select only numeric columns from df_all
numeric_columns <- df_all %>%
  select(where(is.numeric)) %>%
  filter(year <=2009)

# Step 1: Fit a linear regression model on the available data (2000-2009)
lm_model <- lm(PIB_2t3 ~ year + PIBd_2t3, data = df_all)

# Step 2: Predict the missing values for the years 2010-2020
df_all <- df_all %>%
  mutate(PIB_2t3_imputed_lm = ifelse(is.na(PIB_2t3) & year >= 2010 & year <= 2020, 
                                     predict(lm_model, newdata = df_all), PIB_2t3))

## ---- Random Forest ----
library(randomForest)

# Remove rows with missing values in PIB_2t3, year, or PIBd_2t3 before fitting the model
df_all_clean <- df_all %>%
  filter(!is.na(PIB_2t3) & !is.na(year) & !is.na(PIBd_2t3))

# Fit the random forest model using the cleaned data
rf_model <- randomForest(PIB_2t3 ~ year + PIBd_2t3, data = df_all_clean)

# Predict missing values for years 2010-2020 using the random forest model
df_all <- df_all %>%
  mutate(PIB_2t3_imputed_rf = ifelse(is.na(PIB_2t3) & year >= 2010 & year <= 2020, 
                                     predict(rf_model, newdata = df_all), PIB_2t3))

## ---- LOESS Imputation (?) ----
loess_model <- loess(PIB_2t3 ~ year, data = df_all, span = 0.5)

# Predict LOESS-based values for df_all years
df_all_loess <- df_all %>% select(MPIO_CDPMP, year) %>% distinct()
df_all_loess$PIB_2t3_loess <- predict(loess_model, newdata = df_all_loess)

# Merge LOESS-based imputation into df_all using both MPIO_CDPMP and year
df_all <- df_all %>%
  left_join(df_all_loess, by = c("MPIO_CDPMP", "year")) %>%
  filter(year >= 2000)

# ---- Comparison ----
ggplot(df_all) +
  # Plot for Linear Regression Imputation
  geom_density(aes(x = log1p(PIB_2t3_imputed_lm), fill = "Linear Regression"), alpha = 0.5) +
  
  # Plot for Random Forest Imputation
  geom_density(aes(x = log1p(PIB_2t3_imputed_rf), fill = "Random Forest"), alpha = 0.5) +
  
  # Plot for Growth-Based Imputation
  geom_density(aes(x = log1p(PIB_2t3_imputed), fill = "Growth-Based"), alpha = 0.5) +
  
  # Plot for LOESS Imputation # this looks off. will keep out for now...
  #geom_density(aes(x = log1p(PIB_2t3_loess), fill = "LOESS"), alpha = 0.5) +
  
  # Add labels and title
  labs(title = "Comparison of Imputation Methods", 
       x = "Log(PIB_2t3 + 1)", 
       y = "Density", 
       fill = "Imputation Method") +
  
  # Add minimal theme
  theme_minimal()



#----Save and document----
# Check for any other missing variables
# Filter data for NA values in PIB_2t3_imputed and years between 2000 and 2020
imp23_na <- df_all %>%
  filter(is.na(PIB_2t3_imputed) & year >= 2000 & year <= 2020) %>%
  select(MPIO_CDPMP, year, PIB_2t3_imputed)
# Print the result to check the rows with NAs
imp23_na


imp23 <- df_all %>%
  select(MPIO_CDPMP, year, PIB_2t3_imputed, IDF_2t3) %>% # be sure to include Fiscal capacity (low NAs)
  rename(PIB_2t3 = PIB_2t3_imputed) 
write_rds(imp23, "G:/Shared drives/snvdem/snvdem-col/data/panel/data_imputed/imp23.rds")

