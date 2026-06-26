#----- Wrangling country data for subnational V-Dem project -----

setwd("G:/Shared drives/snvdem/snvdem24")


# Step 1 (this script): create country data df and ecdf country data df (standardized)
# Step 2: Data reduction (calculate factor scores)
# Step 3: merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

##---- 0-1 Rurality ----
# municipal-level data for rurality index (CEDE)
IR18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx", 
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
colnames(IR18)[3] = "MPIO_CDPMP"
colnames(IR18)[7] = "Year"
IR18 <- IR18 %>%
  filter(Year == 2018)
# this data set also contains important descriptive information (department, province, distance to market (criteria #13), Bogota (#4-5)) that we will hang onto
IR18 <- IR18[c("MPIO_CDPMP", "depto", "provincia", "municipio", "indrural", "distancia_mercado", "disbogota", "IPM")]
# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
IR18$MPIO_CDPMP <- as.character(IR18$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
IR18$MPIO_CDPMP <- ifelse(nchar(IR18$MPIO_CDPMP) == 4, paste0("0", IR18$MPIO_CDPMP), IR18$MPIO_CDPMP)
IR18 <- IR18 %>%
  rename(IndRur_0t1 = 5, DisMer_13 = 6, DisBog_4t5 = 7, IPM18_2t3 = 8)
IR18_0 <- IR18 %>%
  select(1:5)
# Create Raw dataset for merging following criteria
col18 <- IR18_0

# Create standardized dataset for merging subsequent criteria
col18e <- col18 %>%
  select(1:4)
eCDF_0t1 <- ecdf(IR18$IndRur_0t1)
col18e$IndRur_0t1c <- eCDF_0t1(IR18$IndRur_0t1)
#hist(col18e$IndRur_0t1c)

##---- 2-3 Economic development ----
# we will include three different measures: VAM, PBI departmental, and Multi-dimensional poverty
# municipal-level data for "municipal value added"
VAM18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/2-3_EconDevt/anexo-2020-2021-provisional-valor-agregado-municipio-2011-2021.xlsx", sheet = "Cuadro 9", range = "A11:I1133")
# rename columns to facilitate merging shapefile
VAM18 <- VAM18 %>%
  rename(MPIO_CDPMP = `Código Municipio`) %>%
  rename(DPTO_CCDGO = `Código Departamento`) %>%
  rename(VAM18_2t3 = `Valor agregado\r\n`) %>%
  select(1|3|8)
#hist(log(VAM18$VAM18_2t3))



# department-level data for "PBI per capita"
PBID18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", sheet = "Cuadro 1", range = "A10:AS36")
PBID18 <- PBID18 %>%
  rename(DPTO_CCDGO = `Código Departamento (DIVIPOLA)`) %>%
  select(DPTO_CCDGO, `2018`) %>% #keep only 2018 data 
  rename(PBID18_2t3 = `2018`)

ED18 <- merge(VAM18, PBID18, by = "DPTO_CCDGO", all.x = TRUE)
ED18 <- subset(ED18, select = -DPTO_CCDGO) #remove repetitive variable

#hist(ED18$VAM18_2t3) # heavily skewed by few cases with much higher values
#hist(ED18$PBID18_2t3) #right-skewed, major outlier is Bogota (25% of Colombian GDP and VAM), so the data could be logged

# multi-dimensional poverty
# something is off with Terridata--21 obs missing--so we use CEDE
# IPM (b) from CEDE
IPM18 = IR18 %>%
  select(1|8)
sum(is.na(IPM18$IPM18_2t3))
ED18 <- merge(ED18, IPM18, by = "MPIO_CDPMP", all.x = TRUE)

# merge raw data
col18 <- merge(col18, ED18, by = "MPIO_CDPMP", all.x = TRUE)


# Merge CDF data to CDF df
eCDF_2t3m <- ecdf(ED18$VAM18_2t3)
col18e$VAM18_2t3c <- eCDF_2t3m(ED18$VAM18_2t3)
eCDF_2t3d <- ecdf(ED18$PBID18_2t3) 
col18e$PBID18_2t3c <- eCDF_2t3d(ED18$PBID18_2t3)
eCDF_2t3p <- ecdf(ED18$IPM18_2t3) 
col18e$IPM18_2t3c <- eCDF_2t3p(ED18$IPM18_2t3)


#clean things up for next variable
df2rm = c("IR18_0", "ED18", "IPM18", "PBID18", "VAM18", "df2rm")
rm(list = df2rm)


##---- 4-5 Distance to capital city ----
# municipal-level data for distance to capital city
#hist(IR18$DisBog_4t5) # skewed right (more distant)
Bog18 = IR18 %>%
  select(1|7)
col18 <- merge(col18, Bog18, by = "MPIO_CDPMP", all.x = TRUE)

# now make variables for ECDF
eCDF_4t5 <- ecdf(Bog18$DisBog_4t5)
col18e$DisBog_4t5c <- eCDF_4t5(Bog18$DisBog_4t5) 



#clean things up for next variable
df2rm = c("Bog18", "df2rm")
rm(list = df2rm)

##---- 6-9 NSWE ----

#data from Matt Sisk
reg <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/6-9_NSWE/COL_NSEW.csv")

reg <- reg %>%
  mutate(ns_6t9 = north - south) %>%
  mutate(ew_6t9 = east - west) %>%
  mutate(nsv_6t9 = north + south) %>%
  mutate(ewv_6t9 = east + west)

col18 <- merge(col18, reg, by = "MPIO_CDPMP", all.x = TRUE)

# variables for ECDF
eCDF_n6t9 <- ecdf(col18$n_6t9)
col18e$n_6t9c <- eCDF_n6t9(col18$n_6t9) #north
eCDF_s6t9 <- ecdf(col18$s_6t9)
col18e$s_6t9c <- eCDF_s6t9(col18$s_6t9) #south
eCDF_e6t9 <- ecdf(col18$e_6t9)
col18e$e_6t9c <- eCDF_e6t9(col18$e_6t9) #east
eCDF_w6t9 <- ecdf(col18$w_6t9)
col18e$w_6t9c <- eCDF_w6t9(col18$w_6t9) #west



#clean things up for next variable
df2rm = c("reg", "df2rm")
rm(list = df2rm)

##---- 10 Civil unrest ----
CU18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")
CU18 <- CU18 %>%
  rename(MPIO_CDPMP = 1, Year = 2) %>%
  filter(Year == 2018)
# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CU18$MPIO_CDPMP <- as.character(CU18$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CU18$MPIO_CDPMP <- ifelse(nchar(CU18$MPIO_CDPMP) == 4, paste0("0", CU18$MPIO_CDPMP), CU18$MPIO_CDPMP)

# check to see what variables have the most data... 
variations <- CU18 %>%
  summarise_all(~sd(., na.rm = TRUE)) %>%
  gather() %>%
  arrange(desc(value))
print(variations) 

# Various measures of civil unrest, but we will choose forced displacement (declared by citizens). We also hold on to hurto and illicit crop erradication for next variable
CU18 <- CU18[c("MPIO_CDPMP", "hurto", "homicidios", "errad_manual", "d_desplaza", "e_confina")] # use the other variables for the next criterion: Illicit activity
CU18 <- CU18 %>%
  mutate(errad_manual = ifelse(is.na(errad_manual), 0, errad_manual)) %>% #remove NAs
  mutate(d_desplaza = ifelse(is.na(d_desplaza), 0, d_desplaza)) %>%
  mutate(e_confina = ifelse(is.na(e_confina), 0, e_confina)) %>%
  rename(Hurto_11 = 2, Homic_11 = 3, Errad_11 = 4, Desp_10 = 5, Conf_10 = 6) 
#hist(CU18$Desp_10) # very right-skewed. Medellin most reported cases of displacement
#hist(CU18$Conf_10)

CU18_10 <- CU18[c("MPIO_CDPMP", "Desp_10", "Conf_10")]
col18 <- merge(col18, CU18_10, by = "MPIO_CDPMP", all.x = TRUE)

# ECDF df
eCDF_10a <- ecdf(CU18_10$Desp_10)
CU18_10$Desp_10c <- eCDF_10a(CU18_10$Desp_10) #2 more obs than main df
eCDF_10b <- ecdf(CU18_10$Conf_10)
CU18_10$Conf_10c <- eCDF_10b(CU18_10$Conf_10) #2 more obs than main df

CU18_10e <- CU18_10 %>%
  select(1|4:5)
col18e <- merge(col18e, CU18_10e, by = "MPIO_CDPMP", all.x = TRUE)


##---- 11 Illicit activity ----
# transform Crime by 100k

CU18_11 <- CU18[c("MPIO_CDPMP", "Hurto_11", "Homic_11", "Errad_11")]
col18 <- merge(col18, CU18_11, by = "MPIO_CDPMP", all.x = TRUE)

# ECDF df
eCDF_11a <- ecdf(CU18_11$Hurto_11)
CU18_11$Hurto_11c <- eCDF_11a(CU18_11$Hurto_11)
eCDF_11b <- ecdf(CU18_11$Homic_11)
CU18_11$Homic_11c <- eCDF_11b(CU18_11$Homic_11)
eCDF_11c <- ecdf(CU18_11$Errad_11)
CU18_11$Errad_11c <- eCDF_11c(CU18_11$Errad_11) 

CU18_11e <- CU18_11 %>%
  select(1|5:7)
col18e <- merge(col18e, CU18_11e, by = "MPIO_CDPMP", all.x = TRUE)



#clean things up for next variables
df2rm = c("CU18", "CU18_10", "CU18_10e", "CU18_11", "CU18_11e", "variations", "df2rm")
rm(list = df2rm)


##---- 12 Sparse population density ----
#NAs from Terridata: DenPob (criteria 12) for newer departamentos: San Andres, Amazonas, Guainia, and Vaupes
#To correct for NAs, we can calculate population density using total population / area (km)

# for population, use CNPV directly: Los Andes Epiverse-TRACE initiative (https://github.com/epiverse-trace/ColOpenData)
#pak::pak("epiverse-trace/ColOpenData")
library(ColOpenData)
datasets_dem <- list_datasets("demographic")
# head(datasets_dem)
CNPV18_12PM <- download_demographic("DANE_CNPVPD_2018_12PM")
# head(CNPV18_12PM)
# Step 1: Exclude rows where 'codigo_departamento' is "total", keep only totals (not by age group), 
CNPV18_12PM <- CNPV18_12PM %>%
  filter(codigo_departamento != "total") %>%
  filter(grupo_de_edad == "total") %>%
  select(-area) %>%
  pivot_wider(names_from = auto_reconocimiento_etnico, values_from = total, values_fn = sum)
# create new variable for total ethnic groups
CNPV18_12PM <- CNPV18_12PM %>%
  rename(MPIO_CDPMP = 3, PobTot_12 = 6, PobInd_14 = 7) %>%
  mutate(PobEtn_14 = PobTot_12 - ningun_grupo_etnico) %>%
  mutate(PobInd_14p = PobInd_14 / PobTot_12) %>%
  mutate(PobEtn_14p = PobEtn_14 / PobTot_12)
CNPV18 = CNPV18_12PM %>%
  select(3|6:7|14:16)

#Merge raw data
col18 <- merge(col18, CNPV18, by = "MPIO_CDPMP", all.x = TRUE)


MGN18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/MGN_ANM_MPIOS/MGN18.xls") 
MGN18 <- MGN18 %>%
  select(5|7) %>%
  mutate(AREAkm = AREA / 1000)
col18 <- merge(col18, MGN18, by = "MPIO_CDPMP", all.x = TRUE)

col18 <- col18 %>%
  mutate(DenPob_12 = PobTot_12 / AREAkm)

eCDF_12 <- ecdf(col18$DenPob_12)
col18e$DenPob_12c <- eCDF_12(col18$DenPob_12)


df2rm = c("MGN18", "df2rm")
rm(list = df2rm)

# ECDF
eCDF_12 <- ecdf(col18$DenPob_12)
col18e$DenPob_12c <- eCDF_12(col18$DenPob_12)


##---- 13 Remoteness ----
# a) distance to market (CEDE)
DM18 = IR18 %>%
  select(1|6)
col18 <- merge(col18, DM18, by = "MPIO_CDPMP", all.x = TRUE)
# now make variables for ECDF
eCDF_13 <- ecdf(IR18$DisMer_13)
col18e$DisMer_13c <- eCDF_4t5(IR18$DisMer_13) 

# b) road density
RD18 <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/13_Remote/roads_updated.csv")
RD18 <- RD18 %>%
  rename(nAllRds_13 = 3, lAllRds_13 = 4, lRds_13 = 5, lMjRds_13 = 6) %>%
  select(1|3:6)
col18 <- merge(col18, RD18, by = "MPIO_CDPMP", all.x = TRUE)

# create proportions for Road density (number or length of roads / km^2)
col18 <- col18 %>%
  mutate(nAR_13pkm = nAllRds_13 / AREAkm) %>% #number all roads
  mutate(lAR_13pkm = lAllRds_13 / AREAkm) %>% #length all roads
  mutate(lRds_13pkm = lRds_13 / AREAkm) %>% #length "roads"
  mutate(lMjRds_13pkm = lMjRds_13 / AREAkm) #length major roads

# per capita measures can be found below, before export


# variables for ECDF, using the km proportion
eCDF_13a <- ecdf(col18$nAR_13pkm)
col18e$nARpkm_13c <- eCDF_13a(col18$nAR_13pkm) #number all roads
eCDF_13b <- ecdf(col18$lAR_13pkm)
col18e$lARpkm_13c <- eCDF_13b(col18$lAR_13pkm) #length all roads
eCDF_13c <- ecdf(col18$lRds_13pkm)
col18e$lRpkm_13c <- eCDF_13c(col18$lRds_13pkm) #length "roads"
eCDF_13d <- ecdf(col18$lMjRds_13pkm)
col18e$lMRpkm_13c <- eCDF_13d(col18$lMjRds_13pkm) #length major roads

#clean things up for next variable
df2rm = c("DM18", "RD18",  "df2rm")
rm(list = df2rm)


##---- 14 Indigenous population ----

# raw values calculated under criteria 12


#ECDF
eCDF_14i <- ecdf(col18$PobInd_14)
col18e$PobInd_14c <- eCDF_14i(col18$PobInd_14)
eCDF_14e <- ecdf(col18$PobEtn_14)
col18e$PobEtn_14c <- eCDF_14e(col18$PobEtn_14)
eCDF_14t <- ecdf(col18$PobTot_12)
col18e$PobTot_12c <- eCDF_14t(col18$PobTot_12)
#ECDF proportions
eCDF_14ip <- ecdf(col18$PobInd_14p)
col18e$PobInd_14cp <- eCDF_14ip(col18$PobInd_14p)
eCDF_14ep <- ecdf(col18$PobEtn_14p)
col18e$PobEtn_14cp <- eCDF_14ep(col18$PobEtn_14p)

#clean things up
df2rm = c("CNPV18_12PM", "CNPV18", "datasets_dem", "df2rm")
rm(list = df2rm)





##---- 15-16 Support for National ruling party ----
# Import voting data from Registraduria (RNEC).
rp18 <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/15-16_RulingParty/2018_presidencia_segunda_vuelta_dta_c27d4515ed.csv") # this long dataset contains only 2 candidates because it was a second-round election. other datasets will include more rows depending on the number of candidates running.
#Codigo municipio should have 5 digits
rp18$MPIO_CDPMP <- as.character(rp18$codmpio)
rp18$MPIO_CDPMP <- ifelse(nchar(rp18$MPIO_CDPMP) == 4, sprintf("0%s", rp18$MPIO_CDPMP), rp18$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp18 <- rp18 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
p18d0 <- rp18 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Petro = `1`,
         Duque = `2`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p18d2 <- p18d0 %>%
  select(MPIO_CDPMP, Petro) %>%
  filter(!is.na(Petro))
p18d2 <- p18d2[!duplicated(p18d2$MPIO_CDPMP), ]
p18d3 <- p18d0 %>%
  select(MPIO_CDPMP, Duque) %>%
  filter(!is.na(Duque))
p18d3 <- p18d3[!duplicated(p18d3$MPIO_CDPMP), ]
p18d4 <- p18d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p18d4 <- p18d4[!duplicated(p18d4$MPIO_CDPMP), ]
p18d5 <- p18d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p18d5 <- p18d5[!duplicated(p18d5$MPIO_CDPMP), ]
p18d6 <- p18d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p18d6 <- p18d6[!duplicated(p18d6$MPIO_CDPMP), ]
p18d7 <- p18d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p18d7 <- p18d7[!duplicated(p18d7$MPIO_CDPMP), ]

RPs <- list(p18d2, p18d3, p18d4, p18d5, p18d6, p18d7)
merged_RP <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RPs)

df2rm2 = c("rp18", "p18d0", "p18d2", "p18d3", "p18d4", "p18d5", "p18d6", "p18d7", "df2rm2", "RPs")
rm(list = df2rm2)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP18 <- merged_RP %>%
  mutate(MOV_top2 = Duque-Petro) %>% #Raw vote-count margin of victory
  mutate(Duque_pct = Duque/vtotal) %>%
  mutate(Petro_pct = Petro/vtotal) %>%
  mutate(MOV_pct = Duque_pct-Petro_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP18$Duque)/sum(RP18$vtotal))*100, 2) # 52.3% for Duque
round((sum(RP18$Petro)/sum(RP18$vtotal))*100, 2) # 40.8% for Petro
round(mean(RP18$MOV_pct)*100, 2) #Duque municipal-level margin is 26.3% on average (quite high).
round(sd(RP18$MOV_pct), 3) #SD for the margin is larger than the mean (39.7%) suggesting abnormal distribution
# hist(RP18$MOV_pct, main = "Histogram of MOV", xlab = "MOV", col = "skyblue", border = "black") #left-tail long (margin for Petro)

RP18_15t16 <- RP18 %>%
  select(1|12)
col18 <- merge(col18, RP18_15t16, by = "MPIO_CDPMP", all.x = TRUE)


#ECDF
eCDF_15t16 <- ecdf(RP18$RulPar_15t16)
RP18$RulPar_15t16c <- eCDF_15t16(RP18$RulPar_15t16)
RP18_15t16e <- RP18 %>%
  select(1|13)
col18e <- merge(col18e, RP18_15t16e, by = "MPIO_CDPMP", all.x = TRUE)


#clean things up
df2rm = c("RP18", "RP18_15t16", "RP18_15t16e", "IR18", "merged_RP", "df2rm")
rm(list = df2rm)

#----- Check for NAs -----

na_counts <- colSums(is.na(col18))
# Print the number of NAs for each variable
print(na_counts) #PBID18_2t3 has 75 NAs due to level of analysis


##---- Adding per capita measures ----
# 1. some variables are typically measured per capita: 
# 10 unrest: displacement reported, confinement reported
# 11 illicit activity: robberies, homicides, manual eradication (proportion of hectares / total area)
# 13 remoteness: #it's not clear we will want to use per capita for road density
col18 <- col18 %>%
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

# 2. calculate population change, then correlate that with displacement measure
# Displacement in the origin or destination municipalities. 
# There may be a few places at the lowest end of the distribution. 
## 1980s Peru: flood of people leaving highlands
## Check REPSAL information for armed groups. 





#----- Export dfs to .csv -----

write.csv(col18, file = "G:/Shared drives/snvdem/snvdem24/report/analysis/1_col18_v2.csv", row.names = FALSE)
write.csv(col18e, file = "G:/Shared drives/snvdem/snvdem24/report/analysis/1_col18e_v2.csv", row.names = FALSE)

