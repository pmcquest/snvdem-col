#----- Wrangling time-series data for Colombia (snvdem): Thick index -----
# This analysis uses time-series, municipal-level data (unless noted otherwise). The time-series analysis for Colombia can be divided into "thick" and "thin" indices.

# Thick index: includes variables for which ample data is available over time. This often refers to municipal-level data after 1990. 
# Thin index (see script "1b_wrangle_thick-ts.R"): mostly includes variables for which less ample data is available. This often refers to non-municipal or regional data pre-1990. 

## Current gaps: 
## 2-3: "Econ Devt" gaps may be addressed by including municipal-level historical Census data (this requires reaching out to DANE). There are additional conceptual issues, e.g., is it even possible to scale down PBI from the department to the municipal level? Additionally, "multidimensional poverty" levels are only available in cross-sections; is it worth using imputation to calculate yearly IPM?
## 10-11: "Civil unrest" and "Illicit activity" are complicated concepts to observe empirically. For now we are using displacement and confinement reports from MinDefense, and robbery and eradication reported by  MinDefense, in addition to HRDAG data on direct violence compiled from NGOs around the country (homicides, disappearances, sequesters, and recruitment). We will need to compare and measure correlation between MinDefense and HRDAG reports of homicide.  
## 13: "Remoteness" could be measured using spatial data, potentially. For example, can we calculate municipal centroid distances from main roads, or some use of elevation data?
## 14: "Indigenous population" data is missing before the 2018 Census. Supposedly, micro-data exists from the 2005 Census and potentially even the 1993 Census, but not likely prior to that. This would be an additional request for DANE.

# Note: IPUMS data may provide some data, however there may be too much "missingness" because they work with a stratified sample provided by DANE that is equivalent to 10% of all Census data. See here for 2005 Census data for example: https://international.ipums.org/international-action/sample_details/country/co#tab_co2005a. Census data could provide richer data for the following criteria:
## (0-1) rural/urban
## (2-3) socio-economic
## (12) population density
## (14) indigenous

setwd("G:/Shared drives/snvdem/snvdem-col")

# In general terms, the data collection process for snvdem will follow these 4 steps: 
# Step 1 (this script): create country data df and ecdf country data df (standardized)
# Step 2: Data reduction (calculate factor scores)
# Step 3: merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

#---- Raw dataset for merging ----

col_ts <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx", 
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
colnames(col_ts)[1] = "DPTO_CCDGO"
colnames(col_ts)[3] = "MPIO_CDPMP"
colnames(col_ts)[7] = "year"
# range(col_ts$year, na.rm = TRUE) # 1993-2020
# this data set also contains important descriptive information (department, province, distance to market [criteria #13], Bogota [#4-5], poverty index) that we will hang onto
col_ts <- col_ts[c("DPTO_CCDGO", "MPIO_CDPMP", "year", "depto", "provincia", "municipio", "indrural", "distancia_mercado", "disbogota")]
# Because the import creates a numeric field for municipal and dept. DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
col_ts$MPIO_CDPMP <- as.character(col_ts$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
col_ts$MPIO_CDPMP <- ifelse(nchar(col_ts$MPIO_CDPMP) == 4, paste0("0", col_ts$MPIO_CDPMP), col_ts$MPIO_CDPMP)
# use these new values to extract the dept. codes
col_ts$DPTO_CCDGO <- substr(as.character(col_ts$MPIO_CDPMP), 1, 2)

##---- 0-1 Rurality ----
###---- Rurality index (1993-2020) ----
# this is limited to 1993. Prior to this, we would need to find an alternative measure, or somehow calculate the ratio with demographic data
col_ts <- col_ts %>%
  rename(IndRur_0t1 = 7, DisMer_13 = 8, DisBog_4t5 = 9)

##---- 2-3 Economic development (?) ----
# We have used three different measures--VAM (municipal), PBI (departmental), and Multi-dimensional poverty (municipal)--but there may be many more (e.g., nightlights).
# IPUMS-Col. data includes variables such as: home ownership, access to electricity, water supply, floor material, wall material, and number of families. 

###---- VAM (2011-2020) ----
VAM <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/2-3_EconDevt/anexo-2020-2021-provisional-valor-agregado-municipio-2011-2021.xlsx", 
                  sheet = "Cuadro 1", range = "A11:O1133")
# rename columns to facilitate merging shapefile
VAM <- VAM %>%
  rename(MPIO_CDPMP = `Código Municipio`) %>%
  rename(DPTO_CCDGO = `Código Departamento`) %>%
  rename(`2020` = `2020p`) %>%
  rename(`2021` = `2021p`)
VAM <- VAM %>%
  pivot_longer(cols = 5:15,    # Columns 5 to 15 contain the values for the new variable 'year'
               names_to = "year",  # New variable name
               values_to = "VAM_2t3") %>% # Name of the column for values
  mutate(year = as.numeric(year))
VAM <- VAM %>%
  select(1|5:6)
col_ts <- merge(col_ts, VAM, by = c("MPIO_CDPMP", "year"), all = TRUE)


##---- 4-5 Distance to capital city ----
# this is somewhat of a static variable (Bogota does not change over time). 
###---- Distance to Bogota (1993-2020) ----
# This variable is imported with the initial df. Prior years could be imputed for most municipalities.



##---- 6-9 NSWE ----
# this is somewhat of a static variable (distance does not change over time). 
###---- Distance from center-point (ratio) ----
# rather than simply categorical data (which is arbitrary), we use continuous distances from Matt Sisk
reg <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/geospatial/6-9_NSWE/COL_NSEW.csv")
reg <- reg %>%
  mutate(ns_6t9 = north - south) %>%
  mutate(ew_6t9 = east - west) %>%
  mutate(nsv_6t9 = north + south) %>%
  mutate(ewv_6t9 = east + west)
col_ts <- merge(col_ts, reg, by = "MPIO_CDPMP", all.x = TRUE)


##---- 10 Civil unrest ----
# Data on violence are tricky. Official data include reports of displacement or confinements to MinDefense, but there may be reporting issues. CEDE also offers historic dummy variables of presence of La Violencia and Land Conflicts, which are very thin. CEV-HRDAG in Colombia has produced estimates of recruitment (in addition to more violent variables like homicide, disappearances, and sequesters) based on reports from hundreds of NGOs between 1985-2018. 

# First, we look at CEDE data:
CU <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")
CU <- CU %>%
  rename(MPIO_CDPMP = 1, year = 2)

# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CU$MPIO_CDPMP <- as.character(CU$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CU$MPIO_CDPMP <- ifelse(nchar(CU$MPIO_CDPMP) == 4, paste0("0", CU$MPIO_CDPMP), CU$MPIO_CDPMP)

# check to see what variables have the most data... 
#variations <- CU %>%
#  summarise_all(~sd(., na.rm = TRUE)) %>%
#  gather() %>%
#  arrange(desc(value))
#print(variations) 

# we hold on to illicit crop eradication, robberies, and homicide for next variable "illicit activity"
CU <- CU[c("MPIO_CDPMP", "year", "d_desplaza", "e_confina", "hurto", "homicidios", "errad_manual")]

CU <- CU %>%
  rename(Desp_10 = 3, Conf_10 = 4, Hurto_11 = 5, Homic_11 = 6, Errad_11 = 7) %>%
  mutate(year = as.numeric(year))

###---- Displacement (1993-2021) and Confinements (1993-2020) ----
CU_10 <- CU[c("MPIO_CDPMP", "year", "Desp_10", "Conf_10")] # use the other variables for the next criterion: Illicit activity
col_ts <- merge(col_ts, CU_10, by = c("MPIO_CDPMP", "year"), all = TRUE)

###---- HRDAG variables (1985-2022) ----
# HRDAG data import: homicides, disapperances, sequesters, and recruitment -- applies to #11 as well
HRDAG <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/10-11_HRDAG/td_HRDAG_ym.csv")
HRDAG1 <- HRDAG %>%
  select(1:3|6|9|12|15:18) %>% # raw obs. (counts) and CDFs already calculated
  rename(HHomi_11 = 3, HDesa_11 = 4, HSecu_11 = 5, HRecl_11 = 6, HHomi_11c = 7, HDesa_11c = 8, HSecu_11c = 9, HRecl_11c = 10)
col_ts <- merge(col_ts, HRDAG1, by = c("MPIO_CDPMP", "year"), all = TRUE)

##---- 11 Illicit activity ----
###---- Robbery (2003-2020), Homicides (2003-2020), Eradication (1998-2020) ----
# note: transform Crime by 100k (see penultimate section)
IA_11 <- CU[c("MPIO_CDPMP", "year", "Hurto_11", "Homic_11", "Errad_11")]
col_ts <- merge(col_ts, IA_11, by = c("MPIO_CDPMP", "year"), all = TRUE)


##---- 12 Sparse population density ----
# we can calculate a rough estimate of density by dividing: total population / total area.

# Step 1: Import Population data
# We can use National Census data from Los Andes Epiverse-TRACE initiative (https://github.com/epiverse-trace/ColOpenData)
#pak::pak("epiverse-trace/ColOpenData")
#library(ColOpenData)

# 06/12/24 PM// For time-series, I tried to follow these instructions: https://epiverse-trace.github.io/ColOpenData/articles/population_projections.html
# However, the code didn't work. So I needed to find the source code here: https://github.com/epiverse-trace/ColOpenData/tree/main/R
# I ran 'retrieve.R' which allowed me to run the function in 'download_population_projections.R' 
# (see R script: G:/Shared drives/snvdem/snvdem-col/data/panel/ColOpenData/)
# Then, I exported the dataframe of municipal-level population from 1985-2030 to an Excel file

###---- Density (1985-2030) ----
SP_12 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/ColOpenData/population_projections.xlsx")
# Note: Data for Indigenous population (#14) is only available 2018-2030, so we must find historical data (DANE Census?) 

SP_12 <- SP_12 %>%
  filter(area == "total") %>%
  rename(MPIO_CDPMP = `codigo_municipio`) %>%
  rename(year = `ano`) %>%
  rename(area_tipo = `area`) %>% # we exclude this for now, but could be relevant later
  rename(PobTot_12 = `total`) %>%
  select(3|5|7)

  
# Step 2: integrate data on municipal area size
MGN18 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN18.xls") 
MGN18 <- MGN18 %>%
  select(5|7) %>%
  mutate(AREAkm = AREA / 1000)
SP_12 <- merge(SP_12, MGN18, by = "MPIO_CDPMP", all = TRUE)

# calculate density (inhabitants per km^2)
SP_12 <- SP_12 %>%
  mutate(DenPob_12 = PobTot_12 / AREAkm)

# Merge raw data for population (Note: this creates rows for years 1985-93 when merging all)
col_ts <- merge(col_ts, SP_12, by = c("MPIO_CDPMP", "year"), all = TRUE)

##---- 13 Remoteness (?) ----
# *many ways to conceptualize remoteness, including distance to market, accessibility via roads or airports
###---- Distance to market (1993-2020) ----
# This variable is imported with the initial df.


##---- 14 Indigenous population (2005|2018) ----

###---- Ethnic population (2005 Census) ----
# Primary source is [Terridata](https://terridata.dnp.gov.co/index-app.html#/descargas)

IP_14_05 <- read_excel("data/panel/14_Indigenous/TerriData_Dim25_Sub5_pobetn.xlsx") 
#          col_types = c("numeric", "text", "numeric", 
#"text", "text", "text", "text", "numeric", 
#"numeric", "numeric", "numeric", 
#"text", "text")) # These are the general variable types
IP_14_05 <- IP_14_05 %>%
  rename(DPTO_CCDGO = `Código Departamento`) %>%
  rename(MPIO_CDPMP = `Código Entidad`) %>%
  rename(DatoN = `Dato Numérico`) %>%
  rename(year = `Año`) %>%
  select(1|3|7:8|10)

IP05u <- unique(IP_14_05$Indicador) # Check the list of available indicators
IP_Keep = c("Población indígena", "Población negra, mulata o afrocolombiana", "Población raizal", "Población rom", "Población palenquero", "Población étnica total")
IP_14_05 = IP_14_05[(IP_14_05$Indicador %in% IP_Keep), ] # Drop the indicators that are not relevant
# Pivot Wider the relevant indicators
IP_14_05l = IP_14_05 %>%
  pivot_wider(names_from = Indicador, values_from = DatoN) %>%
  mutate(year = as.numeric(year))
# join total population data (SP_12, from above)
IP_14_05l <- IP_14_05l %>%
  left_join(SP_12 %>% select(MPIO_CDPMP, year, PobTot_12), 
            by = c("MPIO_CDPMP", "year"))

# create new variable for total ethnic groups
IP_14_05 <- IP_14_05l %>%
  rename(PobInd_14 = 4, Afro_14 = 5, Raiz_14 = 6, Rrom_14 = 7, Palen_14 = 8,  PobEtn_14 = 9) %>%
  mutate(PobInd_14p = PobInd_14 / PobTot_12) %>%
  mutate(PobEtn_14p = PobEtn_14 / PobTot_12) %>%
  select(2:4|9|11:12)

col_ts <- merge(col_ts, IP_14_05, by = c("MPIO_CDPMP", "year"), all = TRUE)


###---- *Projections (2018-2030) ----
# *Data for Indigenous population (#14) from National Census data from Los Andes Epiverse-TRACE initiative is only available 2018-2030, so need to look for historical data (DANE Census?)

IP_14_18 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/ColOpenData/population_projections_ethnic2018-30.xlsx")

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



col_ts <- merge(IP_14, IP_14_18, by = c("MPIO_CDPMP", "year"), all = TRUE)


##---- 15-16 Support for National ruling party ----
# Voting data can be found online on Registraduria (RNEC) website. 

###---- Presidential votes (2002-2022) ----
# While direct elections for mayors and governors appeared in the 1980s, major changes--such as introduction of nationwide distict for senatorial elections--emerged with the 1991 Const. 
RP_15t16 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/15-16_RulingParty/Presidencia/p0322_15t16.csv")

col_ts <- merge(col_ts, RP_15t16, by = c("MPIO_CDPMP", "year"), all = TRUE)




#----- Review df (?) -----
summary(col_ts) # for reference, the "thick"  time-series df at the moment contains 52019 obs. of 42 variables, from 1985-2030 

##----- Static variables ----
# we can "fill in" some NAs for "static" variables:
col_ts <- col_ts %>%
  group_by(MPIO_CDPMP) %>%  # Group by municipality
  mutate(DPTO_CCDGO = ifelse(is.na(DPTO_CCDGO), first(na.omit(DPTO_CCDGO)), DPTO_CCDGO),   # Replace NAs with first non-NA value within each group
         depto = ifelse(is.na(depto), first(na.omit(depto)), depto),
         provincia = ifelse(is.na(provincia), first(na.omit(provincia)), provincia),
         municipio = ifelse(is.na(municipio), first(na.omit(municipio)), municipio),
         DisBog_4t5 = ifelse(is.na(DisBog_4t5), first(na.omit(DisBog_4t5)), DisBog_4t5),
         north = ifelse(is.na(north), first(na.omit(north)), north),
         south = ifelse(is.na(south), first(na.omit(south)), south),
         east = ifelse(is.na(east), first(na.omit(east)), east),
         west = ifelse(is.na(west), first(na.omit(west)), west),
         ns_6t9 = ifelse(is.na(ns_6t9), first(na.omit(ns_6t9)), ns_6t9),
         ew_6t9 = ifelse(is.na(ew_6t9), first(na.omit(ew_6t9)), ew_6t9),
         nsv_6t9 = ifelse(is.na(nsv_6t9), first(na.omit(nsv_6t9)), nsv_6t9),
         ewv_6t9 = ifelse(is.na(ewv_6t9), first(na.omit(ewv_6t9)), ewv_6t9)) %>%
  ungroup() %>% # Remove grouping
  filter(year <= 2023)

##----- Dispersed variables ----
# we can "fill in" some NAs for "dispersed" variables, like Census data (2-3 Econ Devt; 14 Indigenous) and 15-16 Ruling Party support:

# 15-16: Function to extend values for the next 3 years after each election year
# i have a timeseries dataset (col_ts), with municipality-year obs between 2002-2022. I have a variable, RulPar15t16, with NA values for some municipality-years. It has values for years 2002, 2006, 2010, 2014, 2018, and 2022, but I want to extend the values for to the subsequent 3 years. As a result, the values would be the same for years 2002-2005, 2006-2009, 2010-2013, 2014-2017, 2018-2021, and 2022. how can i do this?

# *Apply function to col_ts for variable RulPar_15t16 
col_ts1 <- extend_values(col_ts, "RulPar_15t16")



#----- Completeness ----
# Calculate completeness for each row in the dataframe
completeness_summary <- col_ts %>%
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
na_counts <- colSums(is.na(col_ts))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))
# there are many NAs due to missing data in some variables, most notably: #2-3 Econ Devt. (2011-2020), #11 Illicit activity (2003-2020, MinDefense), and #14 Indigenous (2018-) ... 

# ---- Merge CDF data ----
# Create standardized dataset for merging criteria in Cumulative Distribution Function (CDF) format
## should the eCDF values be calculated by year? MC// For now, no.

## Calculate CDFs ----
#0-1
eCDF_0t1 <- ecdf(col_ts$IndRur_0t1)
col_ts$IndRur_0t1c <- eCDF_0t1(col_ts$IndRur_0t1)

#2-3
eCDF_2t3 <- ecdf(col_ts$VAM_2t3)
col_ts$VAM_2t3c <- eCDF_2t3(col_ts$VAM_2t3)

#4-5
eCDF_4t5 <- ecdf(col_ts$DisBog_4t5)
col_ts$DisBog_4t5c <- eCDF_4t5(col_ts$DisBog_4t5) 

#6-9
# (check to see whether to include all 4...?)
eCDF_ns6t9 <- ecdf(col_ts$ns_6t9)
col_ts$ns_6t9c <- eCDF_ns6t9(col_ts$ns_6t9) #north-south
eCDF_ew6t9 <- ecdf(col_ts$ew_6t9)
col_ts$ew_6t9c <- eCDF_ew6t9(col_ts$ew_6t9) #east-west
eCDF_nsv6t9 <- ecdf(col_ts$nsv_6t9)
col_ts$nvs_6t9c <- eCDF_nsv6t9(col_ts$nsv_6t9) #north-south v-shape
eCDF_ewv6t9 <- ecdf(col_ts$ewv_6t9)
col_ts$ewv_6t9c <- eCDF_ewv6t9(col_ts$ewv_6t9) #east-west v-shape

#10
eCDF_10a <- ecdf(col_ts$Desp_10)
col_ts$Desp_10c <- eCDF_10a(col_ts$Desp_10) # 2 more obs than main df
eCDF_10b <- ecdf(col_ts$Conf_10)
col_ts$Conf_10c <- eCDF_10b(col_ts$Conf_10) # 2 more obs than main df

#11 
eCDF_11a <- ecdf(col_ts$Hurto_11)
col_ts$Hurto_11c <- eCDF_11a(col_ts$Hurto_11)
eCDF_11b <- ecdf(col_ts$Homic_11)
col_ts$Homic_11c <- eCDF_11b(col_ts$Homic_11)
eCDF_11c <- ecdf(col_ts$Errad_11)
col_ts$Errad_11c <- eCDF_11c(col_ts$Errad_11) 

#12
eCDF_12c <- ecdf(col_ts$DenPob_12)
col_ts$DenPob_12c <- eCDF_12c(col_ts$DenPob_12)

#13
eCDF_13 <- ecdf(col_ts$DisMer_13)
col_ts$DisMer_13c <- eCDF_13(col_ts$DisMer_13)

#14
eCDF_14i <- ecdf(col_ts$PobInd_14)
col_ts$PobInd_14c <- eCDF_14i(col_ts$PobInd_14)
eCDF_14e <- ecdf(col_ts$PobEtn_14)
col_ts$PobEtn_14c <- eCDF_14e(col_ts$PobEtn_14)
eCDF_14t <- ecdf(col_ts$total_14)
col_ts$total_14c <- eCDF_14t(col_ts$total_14)
# proportions
eCDF_14ip <- ecdf(col_ts$PobInd_14p)
col_ts$PobInd_14cp <- eCDF_14ip(col_ts$PobInd_14p)
eCDF_14ep <- ecdf(col_ts$PobEtn_14p)
col_ts$PobEtn_14cp <- eCDF_14ep(col_ts$PobEtn_14p)

#15-16


#2018
eCDF_15t16_2018 <- ecdf(col_ts$RulPar_15t16_2018)
col_ts$RulPar_15t16c_2018 <- eCDF_15t16_2018(col_ts$RulPar_15t16_2018)


## Select municipal-year CDF values ----
col_tse <- col_ts %>%
  select(1:2|) #pending compile

#---- Adding per capita measures ----
# some variables are typically measured per capita: 
## 10 unrest: displacement reported, confinement reported
## 11 illicit activity: robberies, homicides, manual eradication (proportion of hectares / total area)
col_ts <- col_ts %>%
  mutate(Desp_10pc = Desp_10 / PobTot_12) %>%
  mutate(Conf_10pc = Conf_10 / PobTot_12) %>%
  mutate(Hurto_11pc = Hurto_11 / PobTot_12) %>%
  mutate(Homic_11pc = Homic_11 / PobTot_12) %>%
  mutate(Errad_11pkm = (Errad_11*100) / AREAkm) #100 has = 1 km^2

eCDF_10ap <- ecdf(col18$Desp_10pc)
col18e$Desp_10pcc <- eCDF_10ap(col18$Desp_10pc)
eCDF_10bp <- ecdf(col18$Conf_10pc)
col18e$Conf_10pcc <- eCDF_10bp(col18$Conf_10pc)
eCDF_11ap <- ecdf(col18$Hurto_11pc)
col18e$Hurto_11pcc <- eCDF_11ap(col18$Hurto_11pc)
eCDF_11bp <- ecdf(col18$Homic_11pc)
col18e$Homic_11pcc <- eCDF_11bp(col18$Homic_11pc)
eCDF_11cp <- ecdf(col18$Errad_11pkm)
col18e$Errad_11pkmc <- eCDF_11cp(col18$Errad_11pkm)


# Hoping Pop Dens and Income will be on a different factor than crime data... 

# 2) calculate population change, then correlate that with displacement measure
# Displacement in the origin or destination municipalities. 
# There may be a few places at the lowest end of the distribution. 
## 1980s Peru: flood of people leaving highlands
## Check REPSAL information for armed groups. 



#----- Export dfs to .csv -----

write.csv(col18, file = "G:/Shared drives/snvdem/snvdem-col/report/analysis/1_col18.csv", row.names = FALSE)
write.csv(col18e, file = "G:/Shared drives/snvdem/snvdem-col/report/analysis/1_col18e.csv", row.names = FALSE)

