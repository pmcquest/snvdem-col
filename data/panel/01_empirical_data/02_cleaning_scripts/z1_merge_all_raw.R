
#---- Merge all ----

setwd("G:/Shared drives/snvdem/snvdem-col")
# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# Load cleaned datasets with data for all relevant criteria
df01 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df01_clean.rds") # 0-1, 2-3, 3-4, 13
df02 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df02_clean.rds") # 2-3
df03 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df03_clean.rds") # 5-9*
df04 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df04_clean_v4.rds") # 10-11
df05 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df05_clean.rds") # 12, 14
df06 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df06_clean.rds") # 15-16
df07 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df07_clean.rds") # 13

n_distinct(df07$MPIO_CDPMP)


# Rename homicides (extended)
df04 <- df04 %>% rename(HHomix_1011 = 7)

# Merge all datasets by MPIO_CDPMP and year
df_all <- list(df01, df02, df03, df04, df05, df06, df07) %>%
  reduce(full_join, by = c("MPIO_CDPMP", "year")) %>%
  filter(year >=1998, year <=2023, !is.na(MPIO_CDPMP)) %>% # adjust accordingly: captures both 2005 and 2018 Censuses, as well as 1998 elections
  arrange(MPIO_CDPMP, year)
n_distinct(df_all$MPIO_CDPMP)

summary(df_all)

##----Correct depto names----
unique(df_all$depto)
library(stringr)

df_all <- df_all %>%
  mutate(
    depto = str_squish(depto),
    temp_dpto_code = substr(MPIO_CDPMP, 1, 2),
    depto = case_when(
      temp_dpto_code == "05" ~ "Antioquia",
      temp_dpto_code == "08" ~ "Atlántico",
      temp_dpto_code == "11" ~ "Bogotá, D.C.",
      temp_dpto_code == "13" ~ "Bolívar",
      temp_dpto_code == "15" ~ "Boyacá",
      temp_dpto_code == "17" ~ "Caldas",
      temp_dpto_code == "18" ~ "Caquetá",
      temp_dpto_code == "19" ~ "Cauca",
      temp_dpto_code == "20" ~ "Cesar",
      temp_dpto_code == "23" ~ "Córdoba",
      temp_dpto_code == "25" ~ "Cundinamarca",
      temp_dpto_code == "27" ~ "Chocó",
      temp_dpto_code == "41" ~ "Huila",
      temp_dpto_code == "44" ~ "La Guajira",
      temp_dpto_code == "47" ~ "Magdalena",
      temp_dpto_code == "50" ~ "Meta",
      temp_dpto_code == "52" ~ "Nariño",
      temp_dpto_code == "54" ~ "Norte de Santander",
      temp_dpto_code == "63" ~ "Quindío",
      temp_dpto_code == "66" ~ "Risaralda",
      temp_dpto_code == "68" ~ "Santander",
      temp_dpto_code == "70" ~ "Sucre",
      temp_dpto_code == "73" ~ "Tolima",
      temp_dpto_code == "76" ~ "Valle del Cauca",
      temp_dpto_code == "81" ~ "Arauca",
      temp_dpto_code == "85" ~ "Casanare",
      temp_dpto_code == "86" ~ "Putumayo",
      temp_dpto_code == "88" ~ "Archipiélago de San Andrés, Providencia y Santa Catalina",
      temp_dpto_code == "91" ~ "Amazonas",
      temp_dpto_code == "94" ~ "Guainía",
      temp_dpto_code == "95" ~ "Guaviare",
      temp_dpto_code == "97" ~ "Vaupés",
      temp_dpto_code == "99" ~ "Vichada",
      TRUE ~ depto # Fallback for anything that doesn't match a code
    )
  ) %>%
  select(-temp_dpto_code)
# Final Verification
unique(df_all$depto)

# Municipal issues
df_final <- df_all %>%
  mutate(MPIO_CDPMP = case_when(
    MPIO_CDPMP == "23685" ~ "23580", # Puerto Libertador (Confirmed)
    MPIO_CDPMP == "99624" ~ "99572", # Santa Rosalía (Confirmed)
    MPIO_CDPMP == "99773" ~ "99760", # Cumaribo (Confirmed)
    TRUE ~ MPIO_CDPMP
  )) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    municipio = if(all(is.na(municipio))) NA_character_ else names(sort(table(municipio), decreasing = TRUE))[1],
    depto     = if(all(is.na(depto)))     NA_character_ else names(sort(table(depto), decreasing = TRUE))[1]
  ) %>%
  ungroup() %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(across(everything(), ~ if(all(is.na(.))) NA else .[!is.na(.)][1]), .groups = "drop") %>%
  complete(MPIO_CDPMP, year = 1998:2023)


# Baseline: 1125 total
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/MunYrs.rds")

missing_mpio <- setdiff(unique(MunYrs$MPIO_CDPMP), unique(df_final$MPIO_CDPMP))
print(missing_mpio)


# Define the target years
target_years <- 1998:2023

# Balance the panel
df_balanced <- df_final %>%
  filter(!MPIO_CDPMP %in% c("99624", "99773")) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  complete(MPIO_CDPMP, year = target_years) %>%
  group_by(MPIO_CDPMP) %>%
  fill(municipio, depto, provincia, .direction = "downup") %>%
  ungroup()

# VERIFICATION: Total rows should be 1124 * 26 = 29,224
print(paste("Final Row Count:", nrow(df_balanced)))
print(paste("Unique Municipalities:", n_distinct(df_balanced$MPIO_CDPMP)))

# Find the 2 codes that are in df_all but missing from MunYrs
extra_in_df_all <- setdiff(df_balanced$MPIO_CDPMP, MunYrs$MPIO_CDPMP)
# 27415 = Medio Atrato, Choco, created in 1999 
print(extra_in_df_all) # may be an issue in MunYrs
extra_in_df_all2 <- setdiff(MunYrs$MPIO_CDPMP, df_balanced$MPIO_CDPMP)
print(extra_in_df_all2)

df_balanced <- df_balanced %>%
  mutate(municipio = case_when(
    MPIO_CDPMP == "27415" ~ "Medio Atrato",
    MPIO_CDPMP == "27086" ~ "Belén de Bajirá", # Often confused in this region
    TRUE ~ municipio
  ))

#----NAs----
library(ggplot2)
library(stringr)

# --- Step 1: Calculate NA statistics ---
diagnostic_table <- df_balanced %>%
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
missing_years <- df_balanced %>%
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

# --- Step 3 & 4: Classify and Recommend ---
diagnostic_table <- diagnostic_table %>%
  mutate(
    missing_type = case_when(
      missing_pct == 0 ~ "No Missingness",
      missing_pct < 10 ~ "Minor Gaps (Random Missingness)",
      missing_pct >= 10 & missing_pct < 50 ~ "Moderate Gaps (Possible MAR)",
      missing_pct >= 50 & !is.na(missing_years) ~ "Structural Gaps (e.g., Census Years)",
      missing_pct >= 50 & is.na(missing_years) ~ "High Missingness (MNAR)",
      TRUE ~ "Unclassified"
    ),
    imputation_method = case_when(
      missing_type == "No Missingness" ~ "None",
      missing_type == "Minor Gaps (Random Missingness)" ~ "Linear Interpolation",
      missing_type == "Moderate Gaps (Possible MAR)" ~ "Multiple Imputation (PMM)",
      missing_type == "Structural Gaps (e.g., Census Years)" ~ "Mixed-Effects Models / Bayesian",
      missing_type == "High Missingness (MNAR)" ~ "Assess Mechanism / Consider Excluding",
      TRUE ~ "Manual Review Needed"
    )
  )

print(diagnostic_table)

# --- Step 5: Visualizing Distributions (Density Plots) ---
# We pivot the raw data to "long" format for ggplot
plot_data1 <- df_balanced %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Create density plots faceted by variable
# Using scales = "free" because your variables (counts vs. percentages) have different ranges
distribution_plot <- ggplot(plot_data1, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distribution of Raw Variables",
       subtitle = "Note: Free scales used to compare different units",
       x = "Value",
       y = "Density")

# Print outputs
print(distribution_plot)

# Save to CSV for review
write.csv(diagnostic_table, "data/panel/01_empirical_data/05_diagnostics/diagnostic_table.csv", row.names = FALSE)


##---- Visualizing NAs----
library(naniar)
gg_miss_var(df_balanced, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness


#---- Save cleaned dataset: 2000-2020 ----
write_rds(df_balanced, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds")


