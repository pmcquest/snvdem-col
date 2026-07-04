
#---- Merge all ----

setwd("G:/Shared drives/snvdem/snvdem-col")
# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# 1. Load cleaned datasets
df01 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df01_clean.rds") 
df02 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df02_clean.rds") 
df03 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df03_clean.rds") 
df04 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df04_clean_v4.rds") 
df05 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df05_clean.rds") 
df06 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df06_clean.rds") 
df07 <- read_rds("data/panel/01_empirical_data/03_clean_outputs/df07_clean.rds") 

# 2. Define Exclusion Levels and Unique Municipalities
new_levels <- c("01001", "05000", "08000", "13000", "15000", "17000", 
                "18000", "19000", "20000", "23000", "25000", "27000", 
                "41000", "44000", "47000", "50000", "52000", "54000", 
                "63000", "66000", "68000", "70000", "73000", "76000", 
                "81000", "85000", "86000", "88000", "91000", "94000", 
                "95000", "97000", "99000")

all_mpio <- list(df01, df02, df03, df04, df05, df06, df07) %>%
  map(~pull(.x, MPIO_CDPMP)) %>%
  unlist() %>%
  unique()

# 3. Create the Master Skeleton (1998-2023)
df_all <- crossing(
  MPIO_CDPMP = all_mpio, 
  year = 1998:2023
) %>%
  filter(!MPIO_CDPMP %in% new_levels) %>%
  mutate(year = as.numeric(year))

# 4. Helper function to clean sub-datasets before joining
prep_df <- function(df) {
  # Standardize year if it exists
  if ("year" %in% names(df)) {
    df <- df %>% mutate(year = as.numeric(year))
  }
  # Remove redundant metadata columns to prevent .x / .y suffixes
  df %>%
    select(MPIO_CDPMP, 
           any_of("year"), 
           everything(), 
           -any_of(c("depto", "provincia", "municipio", "DPTO_CCDGO", "AREA", "AREAkm")))
}

# 5. Join all datasets into the skeleton
df_final <- df_all %>%
  # Panel Data Joins (Matches on Municipality AND Year)
  left_join(prep_df(df01), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df02), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df04), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df05), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df06), by = c("MPIO_CDPMP", "year")) %>%
  # Static/Snapshot Data Joins (Matches on Municipality only - broadcasts to all years)
  left_join(prep_df(df03) %>% select(-any_of("year")), by = "MPIO_CDPMP") %>% 
  left_join(prep_df(df07) %>% select(-any_of("year")), by = "MPIO_CDPMP")

# 6. Create clean Static Metadata reference
meta_names <- df01 %>%
  select(MPIO_CDPMP, depto, provincia, municipio, DPTO_CCDGO) %>%
  distinct(MPIO_CDPMP, .keep_all = TRUE)

meta_area <- df05 %>%
  select(MPIO_CDPMP, AREAkm) %>%
  distinct(MPIO_CDPMP, .keep_all = TRUE)

static_meta <- meta_names %>%
  left_join(meta_area, by = "MPIO_CDPMP")

# 7. Finalize and Sort
df_final <- df_final %>%
  left_join(static_meta, by = "MPIO_CDPMP") %>%
  arrange(MPIO_CDPMP, year)

# Final Check
head(df_final)
summary(df_final$year)



##----Correct depto names----
unique(df_all$depto)
library(stringr)

df_final <- df_final %>%
  mutate(
    depto = case_when(
      # --- Departments with Accents/Tildes ---
      depto == "Atlantico" ~ "Atlántico",
      depto == "Bolivar" ~ "Bolívar",
      depto == "Boyaca" ~ "Boyacá",
      depto == "Caqueta" ~ "Caquetá",
      depto == "Cordoba" ~ "Córdoba",
      depto == "Choco" ~ "Chocó",
      depto == "Narino" ~ "Nariño",
      depto == "Guainia" ~ "Guainía",
      depto == "Vaupes" ~ "Vaupés",
      # --- Punctuation, Spacing, and Abbreviations ---
      depto == "Bogota Dc" ~ "Bogotá, D.C.",
      depto == "Norte Santander" ~ "Norte de Santander",
      depto == "Valle Cauca" ~ "Valle del Cauca",
      # --- La Guajira (Standardize to "La Guajira") ---
      depto == "Guajira" ~ "La Guajira",
      # --- San Andrés Archipelago (Consolidate variations) ---
      depto == "Archipiélago de San Andrés" | 
        depto == "Archipielago San Andres Providencia Y Santa Catalina" ~ "Archipiélago de San Andrés, Providencia y Santa Catalina",
      TRUE ~ depto
    )
  )
    

#----NAs----
# We used ChatGPT here to gather recommendations for imputation techniques based on the number of NAs in our data. 
colnames(df_all)

# Step 1: Calculate NA statistics
diagnostic_table <- df_all %>%
  summarise(
    total_obs = n(),
    across(where(is.numeric), ~ sum(is.na(.)), .names = "missing_{.col}")
  ) %>%
  pivot_longer(cols = starts_with("missing_"), 
               names_to = "variable", values_to = "missing_count") %>%
  mutate(
    variable = gsub("missing_", "", variable),  # Clean column names
    missing_pct = round((missing_count / total_obs) * 100, 2)
  ) %>%
  select(variable, missing_count, missing_pct)

# Step 2: Identify Missing Years
missing_years <- df_all %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ sum(!is.na(.)), .names = "non_missing_{.col}")) %>%
  pivot_longer(cols = starts_with("non_missing_"), 
               names_to = "variable", values_to = "non_missing_count") %>%
  mutate(variable = gsub("non_missing_", "", variable)) %>%
  filter(non_missing_count == 0) %>%  # Years where all values for that variable are NA
  group_by(variable) %>%
  summarise(missing_years = paste(unique(year), collapse = ", "))

# Merge updated missing year info into the diagnostic table
diagnostic_table <- diagnostic_table %>%
  left_join(missing_years, by = "variable")


# Step 3: Classify Missingness Type
diagnostic_table <- diagnostic_table %>%
  mutate(
    missing_type = case_when(
      missing_pct == 0 ~ "No Missingness",
      missing_pct < 10 ~ "Minor Gaps (Random Missingness)",
      missing_pct >= 10 & missing_pct < 50 ~ "Moderate Gaps (Possible MAR)",
      missing_pct >= 50 & !is.na(missing_years) ~ "Structural Gaps (e.g., Census Years)",
      missing_pct >= 50 & is.na(missing_years) ~ "High Missingness (MNAR)",
      TRUE ~ "Unclassified"
    )
  )


# Step 4: Assign Recommended Imputation Method
diagnostic_table <- diagnostic_table %>%
  mutate(
    imputation_method = case_when(
      missing_type == "No Missingness" ~ "None",
      missing_type == "Minor Gaps (Random Missingness)" ~ "Linear Interpolation",
      missing_type == "Moderate Gaps (Possible MAR)" ~ "Multiple Imputation (PMM)",
      missing_type == "Structural Gaps (e.g., Census Years)" ~ "Mixed-Effects Models / Bayesian",
      missing_type == "High Missingness (MNAR)" ~ "Assess Mechanism / Consider Excluding",
      TRUE ~ "Manual Review Needed"
    )
  )

# Print final diagnostic table
print(diagnostic_table)

# Save to CSV for review
write.csv(diagnostic_table, "data/panel/01_empirical_data/05_diagnostics/diagnostic_table.csv", row.names = FALSE)


##---- Visualizing NAs----
library(naniar)
gg_miss_var(df_all, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness



#---- Save cleaned dataset: 2000-2020 ----
write_rds(df_all, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds")




