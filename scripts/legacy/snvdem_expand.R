# Factor analysis of snvdem data

# Which criteria are most influential in the snvdem index?
library(dplyr)
library(tidyverse)

## 1. Load Data ----
# snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SNDEM_tentative.rds")
panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")

## 2. Join Panel Data ----
snvdem <- snvdem %>%
  left_join(panel_imputed, by = c("MPIO_CDPMP", "year"))

## 3. Apply Manual Patch for Missing Names ----
# Define the patch for the specific codes (mostly Amazonas and Guainía areas)
manual_patch <- data.frame(
  MPIO_CDPMP = c("27086", "88001", "91263", "91405", "91407", "91430", "91460", "91530", "91536", "91669", "91798", "94343", "94663", "94883", "94884", "94885", "94886", "94887", "94888", "97511", "97777", "97889", "99572", "99760"),
  municipio_patch = c("Belén de Bajirá", "San Andrés", "El Encanto", "La Chorrera", "La Pedrera", "La Victoria", "Mirití-Paraná", "Puerto Alegría", "Puerto Arica", "Puerto Santander", "Tarapacá", "Barranco Minas", "Mapiripana", "San Felipe", "Puerto Colombia", "La Guadalupe", "Cacahual", "Pana Pana", "Morichal", "Pacoa", "Papunaua", "Yavaraté", "Santa Rita", "San José de Ocuné"),
  depto_patch = c("Chocó", "San Andrés", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Amazonas", "Guainía", "Guainía", "Guainía", "Guainía", "Guainía", "Guainía", "Guainía", "Guainía", "Vaupés", "Vaupés", "Vaupés", "Vichada", "Vichada")
)

# Use coalesce to fill the NAs in municipio and depto with the patch values
snvdem <- snvdem %>%
  left_join(manual_patch, by = "MPIO_CDPMP") %>%
  mutate(
    municipio = coalesce(municipio, municipio_patch),
    depto = coalesce(depto, depto_patch)
  ) %>%
  select(-municipio_patch, -depto_patch)

## 4. Verification ----
# Check if any municipio names remain missing
missing_after_patch <- sum(is.na(snvdem$municipio))
print(paste("Missing municipio names:", missing_after_patch))

#---- Write the expanded dataframe to rds ----
write_rds(snvdem, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/01_Expand/snvdem2.rds")
