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
library(stringr)

# Load data (Assuming current working directory is set correctly)
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds")
summary(df_all$PIB_2t3)


# Load external data: Department GDP (PIBd_2t3)
PBID <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", sheet = "Cuadro 1", range = "A10:AS36")

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


# --- Step 0: Pre-process Department Data & Forecast 2023 ---
# 1. Fix the DIVIPOLA codes (leading zeros)
PBID_long <- PBID_long %>%
  mutate(DPTO_CCDGO = str_pad(as.character(DPTO_CCDGO), width = 2, pad = "0"))

# 2. Forecast 2023 Departmental GDP (using the avg growth of last 3 years)
dpto_2023_forecast <- PBID_long %>%
  filter(year >= 2019) %>%
  group_by(DPTO_CCDGO) %>%
  arrange(year) %>%
  mutate(growth = (PIBd_2t3 / lag(PIBd_2t3)) - 1) %>%
  summarise(
    avg_recent_growth = mean(growth, na.rm = TRUE),
    last_val = last(PIBd_2t3),
    .groups = "drop"
  ) %>%
  mutate(
    year = 2023,
    PIBd_2t3 = last_val * (1 + avg_recent_growth)
  ) %>%
  select(DPTO_CCDGO, year, PIBd_2t3)

# 3. Combine original and forecast
PBID_long_extended <- bind_rows(PBID_long, dpto_2023_forecast)

# --- Step 1: Prepare Main Data ---
df_all <- df_all %>%
  mutate(department_code = str_pad(substr(MPIO_CDPMP, 1, 2), width = 2, pad = "0")) %>%
  left_join(PBID_long_extended %>% select(DPTO_CCDGO, year, PIBd_2t3), 
            by = c("department_code" = "DPTO_CCDGO", "year" = "year"))

# --- Step 2: Growth-Based Imputation ----
# Calculate growth rates using 2000-2009 anchor
department_growth <- PBID_long_extended %>%
  filter(year >= 2000 & year <= 2009) %>%
  mutate(DPTO_CCDGO = str_pad(DPTO_CCDGO, width = 2, pad = "0")) %>%
  group_by(DPTO_CCDGO) %>%
  arrange(year) %>%
  mutate(Dgrowth = (PIBd_2t3 / lag(PIBd_2t3)) - 1) %>%
  summarise(avg_Dgrowth_rate = mean(Dgrowth, na.rm = TRUE), .groups = "drop")

municipal_growth <- df_all %>%
  filter(year >= 2000 & year <= 2009) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  summarise(
    avg_Mgrowth_rate = mean((PIB_2t3 / lag(PIB_2t3)) - 1, na.rm = TRUE),
    last_known_PIB = last(na.omit(PIB_2t3)),
    .groups = "drop"
  )

## Dynamic imputation----
df_all <- df_all %>%
  left_join(municipal_growth, by = "MPIO_CDPMP") %>%
  left_join(department_growth, by = c("department_code" = "DPTO_CCDGO")) %>%
  mutate(final_growth_rate = coalesce(avg_Mgrowth_rate, avg_Dgrowth_rate)) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    dpto_annual_multiplier = PIBd_2t3 / lag(PIBd_2t3),
    # For 2023 or any missing years, fallback to the average rate
    dpto_annual_multiplier = coalesce(dpto_annual_multiplier, 1 + final_growth_rate),
    # Create a cumulative chain starting from 1 in 2009
    growth_chain = if_else(year <= 2009, 1, dpto_annual_multiplier),
    cum_multiplier = cumprod(growth_chain),
    # Project forward: 2009_Value * (Product of all yearly changes since 2009)
    PIB_2t3_growth_imp = if_else(
      is.na(PIB_2t3) & year > 2009 & !is.na(last_known_PIB),
      last_known_PIB * cum_multiplier,
      PIB_2t3
    )
  ) %>%
  ungroup()

# --- Step 3: Random Forest Refinement ----
# This uses your preferred method to 'polish' the growth-based estimates
df_train_rf <- df_all %>%
  filter(!is.na(PIB_2t3), !is.na(PIBd_2t3)) %>%
  mutate(department_code = as.factor(department_code))

rf_model <- randomForest(PIB_2t3 ~ department_code + year + PIBd_2t3, 
                         data = df_train_rf, ntree = 100)

df_all <- df_all %>%
  mutate(department_code_f = factor(department_code, levels = levels(df_train_rf$department_code))) %>%
  mutate(
    PIB_rf_pred = predict(rf_model, newdata = data.frame(
      department_code = department_code_f,
      year = year,
      PIBd_2t3 = PIBd_2t3
    ))
  ) %>%
  mutate(
    # Final Coalesce Hierarchy: Observed > Growth-Based > RF
    PIB_2t3_final = coalesce(PIB_2t3, PIB_2t3_growth_imp, PIB_rf_pred)
  )

# Step 4: Create a weighted average----
# 50% Dynamic Growth Trend, 50% Random Forest Prediction
# This keeps the '2020 dip' but adds municipal-level variety from the RF
df_all <- df_all %>%
  rowwise() %>% 
  mutate(
    # 1. Create a safe average that ignores NAs in individual components
    # If both exist, it's a 50/50 blend. If only one exists, it takes that one.
    PIB_2t3_blended = mean(c(PIB_2t3_growth_imp, PIB_rf_pred), na.rm = TRUE),
    # 2. Final hierarchy: 
    # Use real observed data first. If missing, use the smart blend.
    PIB_2t3_final = coalesce(PIB_2t3, PIB_2t3_blended)
  ) %>%
  ungroup()

# --- Verify the result ---
cat("Remaining NAs in 2023:", sum(is.na(df_all$PIB_2t3_final[df_all$year == 2023])))
cat("\nTotal NAs in dataset:", sum(is.na(df_all$PIB_2t3_final)))

# --- Clean & Save ----
imp23 <- df_all %>%
  select(MPIO_CDPMP, year, PIB_2t3 = PIB_2t3_final)

# Save the final imputed data (assuming the path is correct)
write_rds(imp23, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/imp23.rds")

# Visualizations----
# Department-level trend check (2018-2023)
plot_data <- df_all %>%
  filter(year >= 2018) %>%
  group_by(department_code, year) %>%
  summarise(mean_GDP = mean(PIB_2t3_final, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type = if_else(year == 2023, "Forecast", "Actual"))

# trend plot
ggplot(plot_data, aes(x = year, y = mean_GDP, group = department_code)) +
  geom_line(aes(color = department_code), alpha = 0.5) +
  geom_point(aes(shape = Type, color = department_code), size = 2) +
  scale_y_log10() + # Use log scale because GDP varies wildly by dept
  labs(
    title = "GDP Continuity: 2022 Actual vs 2023 Forecast",
    subtitle = "Aggregated by Department (Log Scale)",
    x = "Year",
    y = "Mean Municipal GDP",
    color = "Dept Code"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend if too many depts

# Distribution Comparison (Density Plot)
ggplot(df_all) +
  geom_density(aes(x = log1p(PIB_2t3), fill = "Original Observed"), alpha = 0.4) +
  geom_density(aes(x = log1p(PIB_2t3_final), fill = "Final Imputed (RF + Growth)"), alpha = 0.4) +
  labs(
    title = "Distribution Check: Observed vs. Imputed GDP",
    x = "Log(GDP + 1)",
    y = "Density",
    fill = "Data Source"
  ) +
  theme_minimal()

# Yearly municipal growth rates
library(scales)

growth_data <- df_all %>%
  filter(year >= 1999 & year <= 2023) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    growth_rate = (PIB_2t3_final / lag(PIB_2t3_final)) - 1
  ) %>%
  filter(!is.na(growth_rate)) %>% # Remove the first year (NA lag)
  ungroup()

# 2. Calculate the National Average Growth per Year
yearly_avg_growth <- growth_data %>%
  group_by(year) %>%
  summarise(
    mean_growth = mean(growth_rate, na.rm = TRUE),
    sd_growth = sd(growth_rate, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(Type = if_else(year == 2023, "Forecast", "Observed/Imputed"))

# 3. Create the Visualization
ggplot(yearly_avg_growth, aes(x = year, y = mean_growth)) +
  # Add a ribbon for the variation (Standard Deviation)
  geom_ribbon(aes(ymin = mean_growth - (0.2 * sd_growth), 
                  ymax = mean_growth + (0.2 * sd_growth)), 
              fill = "steelblue", alpha = 0.2) +
  # Line and points
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(aes(color = Type), size = 3) +
  # Reference line at 0%
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Formatting
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  scale_color_manual(values = c("Forecast" = "orange", "Observed/Imputed" = "steelblue")) +
  labs(
    title = "Average Municipal GDP Growth Rate (2000-2023)",
    subtitle = "Blue line shows mean; Shaded area represents ±0.2 SD (Municipal Variation)",
    x = "Year",
    y = "Average Annual Growth (%)",
    caption = "Source: DANE & Imputation Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# Top 10 municipal economic growth in 2022
top_10_growth <- df_all %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    growth_2022 = (PIB_2t3_final / lag(PIB_2t3_final)) - 1
  ) %>%
  filter(year == 2022) %>%
  ungroup() %>%
  # 2. Select identifying columns and sort
  select(MPIO_CDPMP, municipio, depto, growth_2022, PIB_2t3_final) %>%
  arrange(desc(growth_2022)) %>%
  slice_head(n = 10)

# 3. Display the results formatted
top_10_growth %>%
  mutate(growth_2022 = percent(growth_2022, accuracy = 0.1)) %>%
  print()

