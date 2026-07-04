# Script for cleaning up Colombia outcome data

library(tidyverse)
library(vdemdata)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readxl)

# Descriptive data ----

## Total Population and physical area ----
pob <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df05_clean.rds")
pob <- pob %>% select(1:3|5) # total population (time-variant) and surface area (km^2, time-invariant)
### Altitude and descriptive data ----
CEDE01 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/source_files/a2-CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx", 
                     col_types = c("numeric", "numeric", "numeric", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))
colnames(CEDE01)[1] = "DPTO_CCDGO"
colnames(CEDE01)[3] = "MPIO_CDPMP"
colnames(CEDE01)[7] = "year"
CEDE01$MPIO_CDPMP <- as.character(CEDE01$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE01$MPIO_CDPMP <- ifelse(nchar(CEDE01$MPIO_CDPMP) == 4, paste0("0", CEDE01$MPIO_CDPMP), CEDE01$MPIO_CDPMP)
# use these new values to extract the dept. codes
CEDE01$DPTO_CCDGO <- substr(as.character(CEDE01$MPIO_CDPMP), 1, 2)
CEDE01 <- CEDE01[c("DPTO_CCDGO", "MPIO_CDPMP", "year", "depto", "provincia", "municipio", "altura")]

## Census data: Education, Health, Services ----
### 1. Health ----
## interested in infant mortality, disease, and vaccination 
## Note: all are rates: 
### infant mortality is <5 years and general mortality is by 1000 inhabitants; 
### tuberculosis or dengue by 100k inhabitants
### vaccination in pentavalent for <1 year olds (rate)
SaludTD <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/source_files/Devt/TerriData_Dim5_salud.xlsx") 
colnames(SaludTD)[3] = "MPIO_CDPMP"
colnames(SaludTD)[8:10] = c("DATOn","DATOc","year") # Qualitative data (DATOc) may be relevant for CDA in other Terridata datasets
SaludTD = SaludTD[ , c("MPIO_CDPMP", "Indicador","DATOn","year")] # Keep only relevant variables here
Salud_unique = unique(SaludTD$Indicador) # Check the list of available indicators (many have no Quantitative data associated)
Salud_unique
SaludKeep = c("Tasa de mortalidad (x cada 1.000 habitantes)", "Tasa de fecundidad específica en mujeres de 10 a 19 años", "Tasa de mortalidad infantil en menores de 5 años", "Tasa de mortalidad infantil en menores de 1 año (x cada 1.000 nacidos vivos)", "Cobertura vacunación pentavalente en menores de 1 año")
SaludTD1 = SaludTD[(SaludTD$Indicador %in% SaludKeep), ] # Drop the indicators that are not relevant

# Pivot Wider the relevant indicators
SaludTDl = SaludTD1 %>%
  pivot_wider(names_from = Indicador, values_from = DATOn)
colnames(SaludTDl)[3:7] = c("Mortalidad", "MortInfantil1", "VaxPenta", "AdolMothers", "MortInfantil5")
SaludTDl = SaludTDl %>%
  mutate_all(as.numeric) # Make sure the quantitative data is numeric

SaludTDl$MPIO_CDPMP <- as.character(SaludTDl$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
SaludTDl$MPIO_CDPMP <- ifelse(nchar(SaludTDl$MPIO_CDPMP) == 4, paste0("0", SaludTDl$MPIO_CDPMP), SaludTDl$MPIO_CDPMP)


### 2. Education----
# A. Coverage: includes key population of interest (5-16 years old) and corresponding rates of enrollment, class-size, internet access, dropout, graduation, and repetition. Dates are 2011-2022. 
Edu_Coverage = read_excel("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/Education/MEN/MEN_ESTADISTICAS_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_POR_MUNICIPIO.xlsx")
colnames(Edu_Coverage)[1:2] = c("year", "MPIO_CDPMP")
Edu_Coverage = Edu_Coverage[c("year", "MPIO_CDPMP", "POBLACION_5_16", "TASA_MATRICULACION_5_16", "COBERTURA_NETA", "COBERTURA_BRUTA", "DESERCION", "APROBACION", "REPITENCIA")]
colnames(Edu_Coverage)[3:9] = c("Pob5_16", "Enroll5_16", "NetCover", "GrossCover", "Dropout", "Grad", "Repeat")

Edu_Coverage$MPIO_CDPMP <- as.character(Edu_Coverage$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
Edu_Coverage$MPIO_CDPMP <- ifelse(nchar(Edu_Coverage$MPIO_CDPMP) == 4, paste0("0", Edu_Coverage$MPIO_CDPMP), Edu_Coverage$MPIO_CDPMP)


# B. Educational Infrastructure
CEDE21_Edu = read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/source_files/a2-CEDE_PM/2022/Edu/PANEL_DE_EDUCACION(2021).xlsx")
colnames(CEDE21_Edu)[1] = "MPIO_CDPMP"
colnames(CEDE21_Edu)[2] = "year"
CEDE21_Edu <- CEDE21_Edu[c("MPIO_CDPMP", "year", 
                           "t_establ", #Total establecimientos educativos por municipio
                           "jornada_total", #Total de jornadas escolares: preschool, primary, secondary, and middle/college
                           "admin_total", #Total personal administrativo de los establecimientos educativos.
                           "docen_total", #Total docentes
                           "alumn_total", #Total alumnos
                           "s11_total", #Promedio del puntaje total de los resultados en la prueba Saber 11
                           "s11_mate")] #Puntaje en matemáticas 
CEDE21_Edu1 <- CEDE21_Edu %>%
  filter(year > 1999)
colnames(CEDE21_Edu1)[3:9] = c("EdEst", "Sessions", "Admins", "Teachers", "Students", "AvgS11", "AvgS11_Math")


CEDE21_Edu1$MPIO_CDPMP <- as.character(CEDE21_Edu1$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE21_Edu1$MPIO_CDPMP <- ifelse(nchar(CEDE21_Edu1$MPIO_CDPMP) == 4, paste0("0", CEDE21_Edu1$MPIO_CDPMP), CEDE21_Edu1$MPIO_CDPMP)

#NAs high for admin_total (~29%)
summary(CEDE21_Edu1)


### 3. Services----
Servicios <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/source_files/Devt/TerriData_Dim3_servicios.xlsx") 
colnames(Servicios)[3] = "MPIO_CDPMP"
colnames(Servicios)[8:10] = c("DATOn","DATOc","year") # Qualitative data (DATOc) may be relevant for CDA in other Terridata datasets
Servicios = Servicios[ , c("MPIO_CDPMP", "Indicador","DATOn","year")] # Keep only relevant variables here
Serv_unique = unique(Servicios$Indicador) # Check the list of available indicators (many have no Quantitative data associated)
Serv_unique
ServKeep = c("Cobertura de acueducto (Censo)", "Cobertura de acueducto (REC)", "Cobertura de alcantarillado (Censo)", "Cobertura de alcantarillado (REC)", "Cobertura de aseo (REC)", "Cobertura de aseo (Censo)", "Cobertura de Energía Eléctrica (Censo)","Cobertura de Gas Natural (Censo)", "Cobertura de Internet (Censo)")
Servicios1 = Servicios[(Servicios$Indicador %in% ServKeep), ] # Drop the indicators that are not relevant

Servicios1 %>%
  dplyr::summarise(n = dplyr::n(), .by = c(MPIO_CDPMP, year, Indicador)) |>
  dplyr::filter(n > 1L)
Serviciosl = Servicios1 %>%
  dplyr::group_by(MPIO_CDPMP, year, Indicador) %>%
  dplyr::summarise(DATOn_unique = mean(DATOn, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Indicador, values_from = DATOn_unique)

Servicios_final <- Serviciosl %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    Acueducto = mean(c(`Cobertura de acueducto (Censo)`, `Cobertura de acueducto (REC)`), na.rm = TRUE),
    Alcantarillado = mean(c(`Cobertura de alcantarillado (Censo)`, `Cobertura de alcantarillado (REC)`), na.rm = TRUE),
    Aseo = mean(c(`Cobertura de aseo (REC)`, `Cobertura de aseo (Censo)`), na.rm = TRUE),
    Electricidad = mean(`Cobertura de Energía Eléctrica (Censo)`, na.rm = TRUE),
    Gas = mean(`Cobertura de Gas Natural (Censo)`, na.rm = TRUE),
    Internet = mean(`Cobertura de Internet (Censo)`, na.rm = TRUE),
    .groups = "drop"
  )

summary(Servicios_final)

# too many NAs for Electricity, Gas, Internet...
Servicios_final <- Servicios_final %>%
  mutate(year = as.numeric(year)) %>%
  select(-Electricidad, -Gas, -Internet)
Servicios_final <- Servicios_final %>%
  mutate(across(c(Acueducto, Alcantarillado, Aseo), ~ifelse(is.nan(.), NA, .)))


## Combine the Census data----
PanelData <- Servicios_final %>%
  full_join(SaludTDl, by = c("MPIO_CDPMP", "year")) %>%
  full_join(Edu_Coverage, by = c("MPIO_CDPMP", "year")) %>%
  full_join(CEDE21_Edu1, by = c("MPIO_CDPMP", "year")) %>%
  arrange(MPIO_CDPMP, year) %>%
  filter(year < 2022)


## Merge all covariate data----
CEDE01_clean <- CEDE01 %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP), DPTO_CCDGO = as.character(DPTO_CCDGO))
pob_clean <- pob %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))
PanelData_clean <- PanelData %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))

# We use left_join to keep all records from our primary reference (CEDE01)
master_joined <- pob_clean %>%
  filter(year >= 2000 & year <= 2023) %>%
  left_join(CEDE01_clean, by = c("MPIO_CDPMP", "year")) %>%
  left_join(PanelData_clean, by = c("MPIO_CDPMP", "year"))
summary(master_joined)

#----NAs----
# We used Gemini here to gather recommendations for imputation techniques based on the number of NAs in our data. 
colnames(master_joined)
# Step 1: Calculate NA statistics
diagnostic_table <- master_joined %>%
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
missing_years <- master_joined %>%
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
write.csv(diagnostic_table, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/outcome_diagnostics.csv", row.names = FALSE)


##---- Visualizing NAs----
library(naniar)
gg_miss_var(master_joined, facet = year) # clearly 2005 and 2018 (Census years) have the lowest missingness


##---- Raw Panel: Write to rds----
write_rds(master_joined, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_raw.rds")


# Imputation ----
library(imputeTS)

# 1. Setup Variable Groups
static_vars <- c("DPTO_CCDGO", "depto", "provincia", "municipio", "altura")
nocb_vars   <- c("Acueducto", "Alcantarillado", "Aseo")
interp_vars <- c("Pob5_16", "Enroll5_16", "NetCover", "GrossCover", "Dropout", 
                 "Grad", "Repeat", "Mortalidad", "MortInfantil1", "VaxPenta", 
                 "AdolMothers", "MortInfantil5", "Admins")
mpio_to_drop <- c(
  "88001", "91263", "91405", "91407", "91430", "91460", "91530", "91536", 
  "91669", "91798", "94343", "94663", "94883", "94884", "94885", "94886", 
  "94887", "94888", "97511", "97777", "97889"
)

# Function
fast_interp <- function(x) {
  if (sum(!is.na(x)) >= 2) na_interpolation(x, option = "linear") else x
}

# 3. Streamlined Process
df_imputed_final <- master_joined %>%
  # Filter
  filter(!MPIO_CDPMP %in% mpio_to_drop, !grepl("000$", MPIO_CDPMP)) %>%
  # Create Quality Flags
  mutate(across(all_of(c(interp_vars, nocb_vars)), is.na, .names = "{.col}_is_imputed")) %>%
  # Grouped Operations
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    # Fill Static Metadata
    across(all_of(static_vars), ~fill(data.frame(x=.), x, .direction="updown")$x),
    # Interpolate Health/Edu
    across(all_of(interp_vars), fast_interp)
  ) %>%
  # Single 'fill' for everything (tails and services)
  fill(all_of(c(static_vars, interp_vars, nocb_vars)), .direction = "updown") %>%
  ungroup()

# 4. Quick Quality Audit
df_imputed_final %>%
  summarise(across(all_of(c(static_vars, interp_vars)), ~sum(is.na(.))))


# 4. Verification Plot ----
# Ensure the plot shows up to 2023
df_plot <- df_imputed_final %>%
  select(MPIO_CDPMP, year, VaxPenta_imputed = VaxPenta) %>%
  left_join(PanelData %>% select(MPIO_CDPMP, year, VaxPenta_original = VaxPenta), 
            by = c("MPIO_CDPMP", "year")) %>%
  mutate(is_imputed = is.na(VaxPenta_original) & !is.na(VaxPenta_imputed))

sample_mpio <- sample(unique(df_plot$MPIO_CDPMP), 5)

ggplot(filter(df_plot, MPIO_CDPMP %in% sample_mpio), aes(x = year, y = VaxPenta_imputed)) +
  geom_line(color = "gray70") +
  geom_point(aes(color = is_imputed), size = 2) +
  facet_wrap(~MPIO_CDPMP, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  labs(title = "Imputation Extended to 2023",
       subtitle = "Red dots show imputed gaps and 2021-2023 extensions",
       color = "Imputed")

# Moderately Reliable (Approx. 25-35% Imputed)
# These are your health and education outcomes from PanelData. Because this source had significant internal gaps even before 2021, these variables have more "red dots" in the middle of the time series.
## Health: Mortalidad, VaxPenta, AdolMothers.
## Education: Enroll5_16, NetCover, Dropout.

# Calculate the percentage of imputed data per variable
imputation_report <- df_imputed_final %>%
  summarise(across(ends_with("_is_imputed"), ~mean(.) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Percent_Imputed") %>%
  mutate(Variable = str_remove(Variable, "_is_imputed")) %>%
  arrange(Percent_Imputed)
print(imputation_report)


#---- Write to rds----
summary(df_imputed_final)
write_rds(df_imputed_final, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")

