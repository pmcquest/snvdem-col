
#---- CEDE (Gen.) and Census (Dem.) ----

##---- Criteria measured ----
# 0: Rural. (0=No, 1=Yes) [v2*_0]
# 1: Urban. (0=No, 1=Yes) [v2*_1]
# 2: Areas that are less economically developed. (0=No, 1=Yes) [v2*_2] 
# 3: Areas that are more economically developed. (0=No, 1=Yes) [v2*_3]
# 4: Inside the capital city. (0=No, 1=Yes) [v2*_4] 
# 5: Outside the capital city. (0=No, 1=Yes) [v2*_5] 
# 13: Areas that are remote (difficult to reach by available transportation, for example). (0=No, 1=Yes) [v2*_13] 

##---- Data Sources ----
# Data 1: CEDE (U de Los Andes) Panel Municipal (2022) (https://datoscede.uniandes.edu.co/catalogo-de-datos/). We draw data from General panel.
# Data 2: ColOpenData population projections (Los Andes Epiverse-TRACE, https://github.com/epiverse-trace/ColOpenData), used to patch 2021-2023 (CEDE's panel ends in 2020). See rurality_validation.R for why this replaced TerriData's 2018 census export and for the validation of this patch against CEDE's own rurality index.


#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)


##---- 0-1 Rurality ----
# Rurality can be measured in many ways. The World Bank’s Rural Access Index (RAI) or European Commission’s Global Human Settlement Layer (GHSL)  are exemplary GIS-based approaches. In Colombia, DANE uses a classification based on the Organization for Economic Co-operation and Development (OECD) criteria that defines rural territories as those with 150 or fewer persons per kilometer squared (km^2). Using this criteria, the National Planning Department (DNP) found in 2014 that over 75% of Colombian municipalities do not meet the threshold of even 100 persons per km^2. From another perspective, DNP estimates that close to 4/5 of the population lives in urban centers. (Departamento Nacional de Planeación 2014)

###---- Rurality index (1993-2020) ----
# CEDE data contains the "Raw" dataset for merging

CEDE01 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/a2-CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx", 
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
# range(CEDE01$year, na.rm = TRUE) # 1993-2020
# this data set also contains important descriptive information (department, province) and data related to rural population [#0-1], poverty [#2-3], Bogota [#4-5], and remoteness [#13] that we will hang onto
CEDE01 <- CEDE01[c("DPTO_CCDGO", "MPIO_CDPMP", "year", "depto", "provincia", "municipio", #location
                   "pobl_rur", "pobl_urb", "pobl_tot", "indrural", # rurality
                   "pobreza", "nbi", "nbicabecera", "nbiresto", "IPM", "IPM_urb", "IPM_rur", "pib_percapita", #development (poverty)
                   "distancia_mercado", "disbogota")] #distance to capital and remoteness
# Because the import creates a numeric field for municipal and dept. DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CEDE01$MPIO_CDPMP <- as.character(CEDE01$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE01$MPIO_CDPMP <- ifelse(nchar(CEDE01$MPIO_CDPMP) == 4, paste0("0", CEDE01$MPIO_CDPMP), CEDE01$MPIO_CDPMP)
# use these new values to extract the dept. codes
CEDE01$DPTO_CCDGO <- substr(as.character(CEDE01$MPIO_CDPMP), 1, 2)

# Rurality Index is limited to 1993. Prior to this, we would need to find an alternative measure, or somehow calculate the ratio with demographic data

CEDE01 <- CEDE01 %>%
  rename(PobRur_0t1 = 7, PobUrb_0t1 = 8, PobTot_0t1 = 9, IndRur_0t1 = 10, 
         Pobre_2t3 = 11, NBI_2t3 = 12, NBIu_2t3 = 13, NBIr_2t3 = 14, 
         IPM_2t3 = 15, IPMu_2t3 = 16, IPMr_2t3 = 17, PIB_2t3 = 18,
         DisMer_13 = 19, DisBog_4t5 = 20)

###---- Rural population patch (2021-2023) ----
# CEDE's panel ends in 2020 (see IndRur_0t1 etc. above), so we patch 2021-2023
# from ColOpenData's population_projections.xlsx (Los Andes Epiverse-TRACE),
# already used in df05-1214-clean.R for criterion #12. This replaced an
# earlier version of this patch sourced from TerriData's 2018 census export,
# which was missing 20 municipalities (19 Areas No Municipalizadas in
# Amazonas/Guainia/Vaupes + San Andres) that ColOpenData covers completely.
# See rurality_validation.R for the correlation check of this ColOpenData-
# derived index against CEDE's own IndRur_0t1 over their 1993-2020 overlap.
MunYrs <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/MunYrs.rds")

# area == "cabecera_municipal" is urban, "centros_poblados_y_rural_disperso" is
# rural (ColOpenData's own terms); "total" is dropped since we recompute it.
# codigo_municipio/municipio are swapped for 2005-2019 in the source file
# (codigo_municipio holds the name, municipio holds the 5-digit code) --
# un-swap before use so no data is lost.
ColOp_2123 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/ColOpenData/population_projections.xlsx") %>%
  filter(area %in% c("cabecera_municipal", "centros_poblados_y_rural_disperso")) %>%
  mutate(MPIO_CDPMP = ifelse(grepl("^[0-9]{5}$", codigo_municipio), codigo_municipio, municipio)) %>%
  select(MPIO_CDPMP, year = ano, area, total) %>%
  filter(MPIO_CDPMP %in% MunYrs$MPIO_CDPMP, year %in% 2021:2023) %>%
  pivot_wider(names_from = area, values_from = total) %>%
  rename(PobUrb_0t1 = cabecera_municipal, PobRur_0t1 = centros_poblados_y_rural_disperso) %>%
  mutate(
    PobTot_0t1 = PobUrb_0t1 + PobRur_0t1,
    IndRur_0t1b = PobRur_0t1 / PobTot_0t1 # Indice Rural (calculo CEDE), from ColOpenData population
  )

##---- 2-3 Economic development ----
# CEDE contains different measures of poverty based on 1993, 2005, and 2018 Census data
###---- Poverty (1993 | 2005) ----
# Unclear how CEDE measures this. It's based on Census data
###---- NBI (1993 | 1995 | 2000 | 2005 | 2018) ----
# Unsatisfied basic needs. Includes rates for Cabecera (urban) and non-Cabecera (rural) segments of the municipality
###---- IPM (2005 | 2018) ----
# Multi-dimensional poverty. Includes rates for urban and rural zones in each municipality.

##---- 4-5 Distance to capital city ----
###---- Distance to Bogota (1993-2020) ----
# Linear distance to Bogota in km
# This variable is imported with the initial df. Prior years could be imputed for most municipalities.


##---- 13 Remoteness ----
# *many ways to conceptualize remoteness, including distance to market, accessibility via roads or airports. In this script, we will collect data on distance to wholesale market, but in a later script we will incorporate road density.
# Check the standard deviation of the variable within each municipality
check_variance <- CEDE01 %>%
  group_by(MPIO_CDPMP) %>%
  summarize(st_dev = sd(DisMer_13, na.rm = TRUE)) %>%
  filter(st_dev > 0) # This looks for any municipality where the value changes

# If the resulting dataframe has 0 rows, the variable is perfectly time-invariant.
nrow(check_variance)


###---- Distance to market (1993-2020)* ----
# This variable is "static" for all years, which is strange. Presumably markets are appearing or shuttering with some variation. What explains this?

#---- Merge df ----
df01 <- full_join(CEDE01, ColOp_2123, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    # Combine the Population columns (only these three overlap in name
    # between CEDE01 and ColOp_2123, since the years don't overlap; DPTO_CCDGO
    # is CEDE01-only here and gets forward-filled per municipality below)
    PobRur_0t1 = coalesce(PobRur_0t1.x, PobRur_0t1.y),
    PobUrb_0t1 = coalesce(PobUrb_0t1.x, PobUrb_0t1.y),
    PobTot_0t1 = coalesce(PobTot_0t1.x, PobTot_0t1.y),

    # Crucial: Combine the index variables (different names in your summary)
    # We use coalesce to fill CEDE01's column with ColOp_2123's 'b' version
    IndRur_0t1 = coalesce(IndRur_0t1, IndRur_0t1b)
  ) %>%
  # 2. Drop all the residual .x and .y columns + the 'b' version
  select(-ends_with(".x"), -ends_with(".y"), -IndRur_0t1b) %>%
  # 3. Optional: Filter to your desired range
  filter(year >= 1998) %>%
  arrange(MPIO_CDPMP, year)

##----- Static variables ----
# we can "fill in" some NAs for "static" variables:
df01 <- df01 %>%
  group_by(MPIO_CDPMP) %>%  # Group by municipality
  mutate(DPTO_CCDGO = ifelse(is.na(DPTO_CCDGO), first(na.omit(DPTO_CCDGO)), DPTO_CCDGO),   # Replace NAs with first non-NA value within each group
         depto = ifelse(is.na(depto), first(na.omit(depto)), depto),
         provincia = ifelse(is.na(provincia), first(na.omit(provincia)), provincia),
         municipio = ifelse(is.na(municipio), first(na.omit(municipio)), municipio),
         DisBog_4t5 = ifelse(is.na(DisBog_4t5), first(na.omit(DisBog_4t5)), DisBog_4t5)) %>%
  ungroup() # Remove grouping

# Missing values for IndRural and DisMercado (2021-2023)

##----- Completeness ----
# Calculate completeness across all variables per year
# Calculate completeness for each row in the dataframe
completeness_summary <- df01 %>%
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
na_counts <- colSums(is.na(df01))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))

##----- Imputation (?) ----

#---- Save cleaned dataset ----
write_rds(df01, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df01_clean.rds")
