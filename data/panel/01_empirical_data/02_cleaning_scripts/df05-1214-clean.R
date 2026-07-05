
#---- Census (ColOpenData) ----

##---- Criteria measured ----
# 12: Areas that are very sparsely populated. (0=No, 1=Yes) [v2*_12] 
# 14: Areas where there are indigenous populations. (0=No, 1=Yes) [v2*_14] 

##---- Data Sources ----
# Data 1: National Census data from Los Andes Epiverse-TRACE initiative (https://github.com/epiverse-trace/ColOpenData)
#pak::pak("epiverse-trace/ColOpenData")
#library(ColOpenData)
# Data 2: Terridata for 2005 census data on ethnicity (https://terridata.dnp.gov.co/index-app.html#/descargas)

# Note: we can calculate a rough estimate of density by dividing: total population / total area. For time-series, I tried to follow these instructions: https://epiverse-trace.github.io/ColOpenData/articles/population_projections.html. However, the code didn't work. So I needed to find the source code here: https://github.com/epiverse-trace/ColOpenData/tree/main/R. I ran 'retrieve.R' which allowed me to run the function in 'download_population_projections.R' (see R script: G:/Shared drives/snvdem/snvdem-col/data/panel/ColOpenData/). Then, I exported the dataframe of municipal-level population from 1985-2030 to an Excel file.
# Note 2: Census data for Indigenous population (#14) is only available 2018-2030, so we can use 2005 Census data to impute missing values since then. In the future we can try to impute dtat before 2005.

#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

##---- 12 Sparse population density ----

###---- Population Density (1985-2030) ----
SP_12 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/ColOpenData/population_projections.xlsx")

SP_12 <- SP_12 %>%
  filter(area == "total") %>%
  # For 2005-2019, codigo_municipio and municipio are swapped in the source file (codigo_municipio
  # holds the name, municipio holds the 5-digit code) -- un-swap before renaming so no data is lost.
  mutate(
    codigo_municipio_fixed = ifelse(grepl("^[0-9]{5}$", codigo_municipio), codigo_municipio, municipio),
    municipio_fixed = ifelse(grepl("^[0-9]{5}$", codigo_municipio), municipio, codigo_municipio)
  ) %>%
  select(-codigo_municipio, -municipio) %>%
  rename(codigo_municipio = codigo_municipio_fixed, municipio = municipio_fixed) %>%
  rename(MPIO_CDPMP = `codigo_municipio`) %>%
  rename(year = `ano`) %>%
  rename(area_tipo = `area`) %>% # we exclude this for now, but could be relevant later
  rename(PobTot_12 = `total`) %>%
  select(MPIO_CDPMP, year, PobTot_12)


# Integrate data on municipal area size
MGN18 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN18.xls") 
MGN18 <- MGN18 %>%
  select(5|7) %>%
  mutate(AREAkm = AREA / 1000)
SP_12 <- merge(SP_12, MGN18, by = "MPIO_CDPMP", all = TRUE)

# calculate density (inhabitants per km^2)
SP_12 <- SP_12 %>%
  mutate(DenPob_12 = PobTot_12 / AREAkm)

##---- 14 Indigenous population (2005|2018) ----

###---- Ethnic population (2005 Census) ----

IP_14_05 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/14_Indigenous/TerriData_Dim25_Sub5_pobetn.xlsx") 

IP_14_05 <- IP_14_05 %>%
  rename(DPTO_CCDGO = `Código Departamento`) %>%
  rename(MPIO_CDPMP = `Código Entidad`) %>%
  rename(DatoN = `Dato Numérico`) %>%
  rename(year = `Año`) %>%
  select(1|3|7:8|10) %>%
  mutate(DatoN = as.numeric(gsub(",", ".", gsub("\\.", "", DatoN)))) # DANE exports DatoN as Spanish-formatted text ("1.139,00"); convert to numeric


IP05u <- unique(IP_14_05$Indicador) # Check the list of available indicators
IP_Keep = c("Población indígena", "Población negra, mulata o afrocolombiana", "Población raizal", "Población rom", "Población palenquero", "Población étnica total")
IP_14_05 = IP_14_05[(IP_14_05$Indicador %in% IP_Keep), ] # Drop the indicators that are not relevant
# Pivot Wider the relevant indicators
IP_14_05l = IP_14_05 %>%
  pivot_wider(names_from = Indicador, values_from = DatoN) %>%
  mutate(year = as.numeric(year))

supra_manual <- c("01001", "05000", "08000", "13000", "15000", "17000", 
                  "18000", "19000", "20000", "23000", "25000", "27000", 
                  "41000", "44000", "47000", "50000", "52000", "54000", 
                  "63000", "66000", "68000", "70000", "73000", "76000", 
                  "81000", "85000", "86000", "88000", "91000", "94000", 
                  "95000", "97000", "99000")

# Pivot Wider the relevant indicators
IP_14_05l <- IP_14_05l %>% filter(!MPIO_CDPMP %in% supra_manual)

# join total population data (SP_12, from above)
IP_14_05l <- IP_14_05l %>%
  left_join(SP_12 %>% select(MPIO_CDPMP, year, PobTot_12), 
            by = c("MPIO_CDPMP", "year"))

n_distinct(IP_14_05l$MPIO_CDPMP)

# create new variable for total ethnic groups
IP_14_05 <- IP_14_05l %>%
  rename(PobInd_14 = 4, Afro_14 = 5, Raiz_14 = 6, Rrom_14 = 7, Palen_14 = 8,  PobEtn_14 = 9) %>%
  mutate(PobInd_14p = PobInd_14 / PobTot_12) %>%
  mutate(PobEtn_14p = PobEtn_14 / PobTot_12) %>%
  select(2:4|9|11:12)


###---- *Projections (2018-2030) ----
# *Data for Indigenous population (#14) from National Census data from Los Andes Epiverse-TRACE initiative is only available 2018-2030, so need to look for historical data (DANE Census?)
IP_14_18 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/ColOpenData/population_projections_ethnic2018-30.xlsx")

# Select relevant data and shape
IP_14_18 <- IP_14_18 %>%
  select(-area) %>%
  pivot_wider(names_from = pertenencia_etnico_racial, values_from = total, values_fn = sum)
# create new variable for total ethnic groups
IP_14_18 <- IP_14_18 %>%
  rename(MPIO_CDPMP = 3, year = 5, total_14 = 6, PobInd_14 = 7, Rrom_14 = 8, Raiz_14 = 9, Palen_14 = 10, Afro_14 = 11, Ningun_14 = 12) %>%
  mutate(PobEtn_14 = total_14 - Ningun_14) %>%
  mutate(PobInd_14p = PobInd_14 / total_14) %>%
  mutate(PobEtn_14p = PobEtn_14 / total_14) %>%
  select(3|5|7|13:15)
n_distinct(IP_14_18$MPIO_CDPMP)

IP_14 <- bind_rows(IP_14_05, IP_14_18)
n_distinct(IP_14$MPIO_CDPMP)

#---- Merge df----
df05 <- merge(SP_12, IP_14, by = c("MPIO_CDPMP", "year"), all = TRUE)
n_distinct(df05$MPIO_CDPMP)




##----- Completeness ----
# Calculate completeness across all variables per year
completeness_summary <- df05 %>%
  mutate(
    # Count non-missing values for each row excluding 'MPIO_CDPMP' and 'year'
    non_missing_values = rowSums(!is.na(select(., -MPIO_CDPMP, -year))),  
    
    # Calculate total possible values (excluding 'MPIO_CDPMP' and 'year')
    total_values = ncol(.) - 2,  # Total number of variables excluding 'MPIO_CDPMP' and 'year'
    
    # Calculate completeness percentage for each observation
    completeness_percentage = (non_missing_values / total_values) * 100
  )
print(completeness_summary)
summary(completeness_summary$completeness_percentage)

# Average completeness by year
completeness_by_year <- completeness_summary %>%
  group_by(year) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_year)

# average completeness by municipality
completeness_by_municipality <- completeness_summary %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_municipality)

library(ggplot2)
ggplot(completeness_by_year, aes(x = year, y = avg_completeness)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Data Completeness by Year", x = "Year", y = "Completeness (%)") +
  theme_minimal()

# Check for NA's 
na_counts <- colSums(is.na(df05))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))

##----- Imputation (?) ----

#---- Save cleaned dataset ----
write_rds(df05, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df05_clean.rds")


