#----- Wrangling time-series data for Colombia (snvdem) -----
# time-series

# Thick index: all variables since ~2000
## gaps: 
## 2-3: imputation for IPM? scaling down PBID?
## 13: 2000s road maps?
## 14: 

# Thin index: variables pre-2000s
## gaps:
## 1: rurality pre-1993?
## 2-3: 
## 10: CEV?
## 11: CEV?
## 13: Major roads only?
## 14: no census pre-1993?
## 15-16: direct elections began in 1988...


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
###---- Rurality index (1993-2020) ----
# this is limited to 1993. Prior to this, we would need to find an alternative measure, or somehow calculate the ratio with demographic data
IR <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx", 
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
colnames(IR)[1] = "DPTO_CCDGO"
colnames(IR)[3] = "MPIO_CDPMP"
colnames(IR)[7] = "year"
# this data set also contains important descriptive information (department, province, distance to market [criteria #13], Bogota [#4-5], poverty index) that we will hang onto
IR <- IR[c("DPTO_CCDGO", "MPIO_CDPMP", "year", "depto", "provincia", "municipio", "indrural", "distancia_mercado", "disbogota", "IPM")]
# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
IR$MPIO_CDPMP <- as.character(IR$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
IR$MPIO_CDPMP <- ifelse(nchar(IR$MPIO_CDPMP) == 4, paste0("0", IR$MPIO_CDPMP), IR$MPIO_CDPMP)
IR <- IR %>%
  rename(IndRur_0t1 = 7, DisMer_13 = 8, DisBog_4t5 = 9, IPM_2t3 = 10)

# Rurality index only
IR_0 <- IR %>%
  select(1:7)

####---- Raw datasets for merging ----
col_ts <- IR_0
# range(col_ts$year, na.rm = TRUE) # 1993-2020

# Create standardized dataset for merging subsequent criteria

col_tse <- col_ts %>%
  select(1:6)

eCDF_0t1 <- ecdf(IR$IndRur_0t1)
col_tse$IndRur_0t1c <- eCDF_0t1(IR$IndRur_0t1)

## should the eCDF values be calculated by year? MC// For now, no.


##---- 2-3 Economic development ----
# we will include three different measures: VAM, PBI departmental, and Multi-dimensional poverty

###---- VAM (2011-2020) ----
# data available only between 2011-2020
VAM <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/2-3_EconDevt/anexo-2020-2021-provisional-valor-agregado-municipio-2011-2021.xlsx", 
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


###---- PBI departamental (1980-2022) ----
PBID <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", 
                     sheet = "Cuadro 1", range = "A10:AS36")
PBID <- PBID %>%
  rename(DPTO_CCDGO = `Código Departamento (DIVIPOLA)`) %>%
  rename(`2020` = `2020p`) %>%
  rename(`2021` = `2021p`) %>%
  rename(`2022` = `2022pr`)
PBID <- PBID %>%
  pivot_longer(cols = 3:45,    # Columns 5 to 15 contain the values for the new variable 'year'
               names_to = "year",  # New variable name
               values_to = "PBID_2t3") %>% # Name of the column for values
  mutate(year = as.numeric(year))


ED <- merge(VAM, PBID, by = c("DPTO_CCDGO", "year"), all = TRUE)
# ED <- subset(ED, select = -DPTO_CCDGO) #remove repetitive variable


###---- Multi-dimensional poverty (2005 & 2018) ----
# IPM from CEDE
IPM <- IR %>%
  select(2:3|10)
#sum(is.na(IPM$IPM_2t3))
ED <- merge(ED, IPM, by = c("MPIO_CDPMP", "year"), all = TRUE)
ED <- ED %>%
  select(1:3|6|8:9)

# merge raw data
col_ts <- merge(col_ts, ED, by = c("DPTO_CCDGO", "MPIO_CDPMP", "year"), all = TRUE)


# Merge CDF data to CDF df
eCDF_2t3m <- ecdf(ED$VAM_2t3)
ED$VAM_2t3c <- eCDF_2t3m(ED$VAM_2t3)
eCDF_2t3d <- ecdf(ED$PBID_2t3) 
ED$PBID_2t3c <- eCDF_2t3d(ED$PBID_2t3)
eCDF_2t3p <- ecdf(ED$IPM_2t3) 
ED$IPM_2t3c <- eCDF_2t3p(ED$IPM_2t3)
EDc = ED %>%
  select(1:3|6|8:9)

col_tse <- merge(col_tse, EDc, by = c("DPTO_CCDGO", "MPIO_CDPMP", "year"), all = TRUE)




#clean things up for next variable
df2rm = c("IR_0", "ED", "EDc", "IPM", "PBID", "VAM", "df2rm")
rm(list = df2rm)


##---- 4-5 Distance to capital city ----
###---- Distance to Bogota (1993-2020) ----
# prior years could be imputed for most municipalities
Bog <- IR %>%
  select(2:3|9)
col_ts <- merge(col_ts, Bog, by = c("MPIO_CDPMP", "year"), all = TRUE)

# now make variables for ECDF
eCDF_4t5 <- ecdf(Bog$DisBog_4t5)
Bog$DisBog_4t5c <- eCDF_4t5(Bog$DisBog_4t5) 
Bogc <- Bog %>%
  select(1:2|4)
col_tse <- merge(col_tse, Bogc, by = c("MPIO_CDPMP", "year"), all = TRUE)


#clean things up for next variable
df2rm = c("Bog", "Bogc", "df2rm")
rm(list = df2rm)

##---- 6-9 NSWE (categorical) ----

#data from DANE regions requires some formatting before merging
reg <- read_csv("G:/Shared drives/snvdem/snvdem24/data/panel/6-9_Cardinal/Departamentos_y_municipios_de_Colombia_20240312.csv")
reg <- reg %>%
  rename(mpio = `CÓDIGO DANE DEL MUNICIPIO`)
reg$MPIO_CDPMP <- sprintf("%.3f", reg$mpio) #Codigo municipio should have 5 digits
reg$MPIO_CDPMP <- gsub("\\.", "", reg$MPIO_CDPMP)
reg$MPIO_CDPMP <- ifelse(nchar(reg$MPIO_CDPMP) == 4, sprintf("0%s", reg$MPIO_CDPMP), reg$MPIO_CDPMP)

# categories chosen here
unique(reg$REGION)
reg <- reg %>%
  mutate(NSWE_6t9 = case_when(
    REGION %in% c('Región Caribe', 'Región Eje Cafetero - Antioquia') ~ 'North',
    REGION %in% c('Región Centro Oriente', 'Región Llano') ~ 'East',
    REGION == 'Región Centro Sur' ~ 'South',
    REGION == 'Región Pacífico' ~ 'West',
    TRUE ~ NA_character_
  ))

reg <- reg[c("MPIO_CDPMP", "REGION", "NSWE_6t9")]

reg <- reg %>%
  mutate(North_6 = as.integer(NSWE_6t9 == "North"),
         South_7 = as.integer(NSWE_6t9 == "South"),
         West_8 = as.integer(NSWE_6t9 == "West"),
         East_9 = as.integer(NSWE_6t9 == "East")) 

# mutate(EjeCafe_6t9 = as.integer(REGION == "Región Eje Cafetero - Antioquia"),
#         Caribe_6t9 = as.integer(REGION == "Región Caribe"),
#         CSur_6t9 = as.integer(REGION == "Región Centro Sur"),
#         Pacif_6t9 = as.integer(REGION == "Región Pacífico"),
#         COrient_6t9 = as.integer(REGION == "Región Centro Oriente"), 
#         Llano_6t9 = as.integer(REGION == "Región Llano"))

reg <- reg %>%
  select(1|4:7)

#not sure whether to include dichotomous variable in the CDF df
col_ts <- merge(col_ts, reg, by = "MPIO_CDPMP", all = TRUE)
col_tse <- merge(col_tse, reg, by = "MPIO_CDPMP", all.x = TRUE)

#clean things up for next variable
df2rm = c("reg", "reg2", "df2rm")
rm(list = df2rm)


##---- 10 Civil unrest ----

CU <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")
CU <- CU %>%
  rename(MPIO_CDPMP = 1, year = 2)

# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CU$MPIO_CDPMP <- as.character(CU$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CU$MPIO_CDPMP <- ifelse(nchar(CU$MPIO_CDPMP) == 4, paste0("0", CU$MPIO_CDPMP), CU$MPIO_CDPMP)

# check to see what variables have the most data... 
variations <- CU %>%
  summarise_all(~sd(., na.rm = TRUE)) %>%
  gather() %>%
  arrange(desc(value))
print(variations) 

CU <- CU[c("MPIO_CDPMP", "year", "d_desplaza", "e_confina", "hurto", "homicidios", "errad_manual")]

CU <- CU %>%
  #mutate(errad_manual = ifelse(is.na(errad_manual), 0, errad_manual)) %>% #remove NAs
  #mutate(d_desplaza = ifelse(is.na(d_desplaza), 0, d_desplaza)) %>%
  #mutate(e_confina = ifelse(is.na(e_confina), 0, e_confina)) %>%
  rename(Desp_10 = 3, Conf_10 = 4, Hurto_11 = 5, Homic_11 = 6, Errad_11 = 7) %>%
  mutate(year = as.numeric(year))

# Various measures of civil unrest, but we will choose the following:
###---- Displacement (1993-2021) and Confinements (1993-2020) ----
# we hold on to illicit crop eradication, robberies, and homicide for next variable "illicit activity"
CU_10 <- CU[c("MPIO_CDPMP", "year", "Desp_10", "Conf_10")] # use the other variables for the next criterion: Illicit activity

col_ts <- merge(col_ts, CU_10, by = c("MPIO_CDPMP", "year"), all = TRUE)

# ECDF df
eCDF_10a <- ecdf(CU_10$Desp_10)
CU_10$Desp_10c <- eCDF_10a(CU_10$Desp_10) #2 more obs than main df
eCDF_10b <- ecdf(CU_10$Conf_10)
CU_10$Conf_10c <- eCDF_10b(CU_10$Conf_10) #2 more obs than main df

CU_10e <- CU_10 %>%
  select(1:2|5:6)
col_tse <- merge(col_tse, CU_10e, by = c("MPIO_CDPMP", "year"), all = TRUE)


##---- 11 Illicit activity ----
###---- Robbery (2003-2020), Homicides (2003-2020), Eradication (1998-2020) ----
# note: transform Crime by 100k (see penultimate section)
IA_11 <- CU[c("MPIO_CDPMP", "year", "Hurto_11", "Homic_11", "Errad_11")]

col_ts <- merge(col_ts, IA_11, by = c("MPIO_CDPMP", "year"), all = TRUE)

# ECDF df
eCDF_11a <- ecdf(IA_11$Hurto_11)
IA_11$Hurto_11c <- eCDF_11a(IA_11$Hurto_11)
eCDF_11b <- ecdf(IA_11$Homic_11)
IA_11$Homic_11c <- eCDF_11b(IA_11$Homic_11)
eCDF_11c <- ecdf(IA_11$Errad_11)
IA_11$Errad_11c <- eCDF_11c(IA_11$Errad_11) 

IA_11e <- IA_11 %>%
  select(1:2|6:8)
col_tse <- merge(col_tse, IA_11e, by = c("MPIO_CDPMP", "year"), all = TRUE)



#clean things up for next variables
df2rm = c("CU", "CU_10", "CU_10e", "IA_11", "IA_11e", "variations", "df2rm")
rm(list = df2rm)


##---- 12 Sparse population density (1985-2030) ----
# we can calculate a rough estimate of density by dividing: total population / total area.

# Step 1: Import Population data
# We can use National Census data from Los Andes Epiverse-TRACE initiative (https://github.com/epiverse-trace/ColOpenData)
#pak::pak("epiverse-trace/ColOpenData")
#library(ColOpenData)

# 06/12/24 PM// For time-series, I tried to follow these instructions: https://epiverse-trace.github.io/ColOpenData/articles/population_projections.html
# However, the code didn't work. So I needed to find the source code here: https://github.com/epiverse-trace/ColOpenData/tree/main/R
# I ran 'retrieve.R' which allowed me to run the function in 'download_population_projections.R' 
# (see R script: G:/Shared drives/snvdem/snvdem24/data/panel/ColOpenData/)
# Then, I exported the dataframe of municipal-level population from 1985-2030 to an Excel file
SP_12 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/panel/ColOpenData/population_projections.xlsx")
# Data for Indigenous population (#14) is only available 2018-2030, so we can use CEDE projections (1993-2017)
# If necessary, we can expand the CEDE projections with the ColOpenData data.

SP_12 <- SP_12 %>%
  filter(area == "total") %>%
  rename(MPIO_CDPMP = `codigo_municipio`) %>%
  rename(year = `ano`) %>%
  rename(area_tipo = `area`) %>% # we exclude this for now, but could be relevant later
  rename(PobTot_12 = `total`) %>%
  select(3|5|7)
  
#Step 2: integrate data on municipal area size
MGN18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/MGN_ANM_MPIOS/MGN18.xls") 
MGN18 <- MGN18 %>%
  select(5|7) %>%
  mutate(AREAkm = AREA / 1000)
SP_12 <- merge(SP_12, MGN18, by = "MPIO_CDPMP", all = TRUE)

#calculate density
SP_12 <- SP_12 %>%
  mutate(DenPob_12 = PobTot_12 / AREAkm)

#Merge raw data for population
col_ts <- merge(col_ts, SP_12, by = c("MPIO_CDPMP", "year"), all = TRUE)


# eCDF
eCDF_12c <- ecdf(SP_12$DenPob_12)
SP_12$DenPob_12c <- eCDF_12c(SP_12$DenPob_12)

SP_12e <- SP_12 %>%
  select(1:2|7)

col_tse <- merge(col_tse, SP_12e, by = c("MPIO_CDPMP", "year"), all = TRUE)



df2rm = c("MGN18", "df2rm")
rm(list = df2rm)


##---- 13 Remoteness ----

DM18 = IR18 %>%
  select(1|6)
col18 <- merge(col18, DM18, by = "MPIO_CDPMP", all.x = TRUE)

# now make variables for ECDF
eCDF_13 <- ecdf(IR18$DisMer_13)
col18e$DisMer_13c <- eCDF_4t5(IR18$DisMer_13) 


#clean things up for next variable
df2rm = c("DM18",  "df2rm")
rm(list = df2rm)


##---- 14 Indigenous population ----

#NAs from Terridata: DenPob (criteria 12) for newer departamentos: San Andres, Amazonas, Guainia, and Vaupes
#To correct for NAs, we can calculate population density using total population / area (km)

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
print(na_counts)


##---- Adding per capita measures ----
# 1. some variables are typically measured per capita: 
# 10 unrest: displacement reported, confinement reported
# 11 illicit activity: robberies, homicides, manual eradication (proportion of hectares / total area)
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

# 2. calculate population change, then correlate that with displacement measure
# Displacement in the origin or destination municipalities. 
# There may be a few places at the lowest end of the distribution. 
## 1980s Peru: flood of people leaving highlands
## Check REPSAL information for armed groups. 





#----- Export dfs to .csv -----

write.csv(col18, file = "G:/Shared drives/snvdem/snvdem24/report/analysis/1_col18.csv", row.names = FALSE)
write.csv(col18e, file = "G:/Shared drives/snvdem/snvdem24/report/analysis/1_col18e.csv", row.names = FALSE)

