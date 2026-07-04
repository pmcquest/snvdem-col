#----- Wrangling country data for subnational V-Dem project -----

setwd("G:/Shared drives/snvdem/snvdem24")


# Step 1 (this script): create country data df (x3 test)
# Step 2: create ecdf country data df (standardized)
# Step 3: Data reduction (calculate factor scores)
# Step 4: merge-in V-Dem data (weighted by coder-level analysis)
# Step 5: merge geolocational data


library(readxl)
library(dplyr)
library(readr)

##---- 0-1 Rurality ----

# municipal-level data for rurality index (CEDE)
IR18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/0-1_Rurality/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx")
colnames(IR18)[3] = "MPIO_CDPMP"
colnames(IR18)[7] = "Year"
IR18 <- IR18 %>%
  filter(Year == 2018)
# this data set also contains important descriptive information (department, province, distance to market) that we will hang onto
IR18 <- IR18[c("MPIO_CDPMP", "depto", "provincia", "indrural", "distancia_mercado")]
# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
IR18$MPIO_CDPMP <- as.character(IR18$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
IR18$MPIO_CDPMP <- ifelse(nchar(IR18$MPIO_CDPMP) == 4, paste0("0", IR18$MPIO_CDPMP), IR18$MPIO_CDPMP)

hist(IR18$indrural) # mostly normal, but skewed left (more rural)
# Option #1: interact V-Dem with rural index score
IR18$CDF_0t1 <- pnorm(IR18$indrural) #normal distribution, so we use pnorm (CDF)
hist(IR18$CDF_0t1) # wrong?

eCDF_0t1 <- ecdf(IR18$indrural)
IR18$eCDF_0t1 <- eCDF_0t1(IR18$indrural)
hist(IR18$eCDF_0t1)





# For sf (R), merge this data to shapefile
col <- merge(col, IR18, by = "MPIO_CDPMP", all.x = TRUE)

#clean things up for next variable (leave IR18 for #13 Remoteness)
df2rm = c("df2rm", "ap_0t1", "c0", "d0")
rm(list = df2rm)

##---- 2-3 Economic development ----

# municipal-level data for "value added"
VAM18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/2-3_EconDevt/anexo-2020-2021-provisional-valor-agregado-municipio-2011-2021.xlsx", sheet = "Cuadro 9", range = "A11:I1133")
# rename columns to facilitate merging shapefile
VAM18 <- VAM18 %>%
  rename(MPIO_CDPMP = `Código Municipio`) %>%
  rename(DPTO_CCDGO = `Código Departamento`) %>%
  rename(VAM18_2t3 = `Valor agregado\r\n`) %>%
  select(1|3|8)

# merge base-layer dataset to obtain FID variable (for later integration with ArcMaps)
ED18 <- merge(VAM18, MGN18[, c("MPIO_CDPMP", "FID")], by = "MPIO_CDPMP", all.x = TRUE)
ED18 <- ED18 %>%
  rename(FID_2t3 = FID)

# department-level data for "PBI per capita"
PBID <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/2-3_EconDevt/anex-PIBDep-RetropolacionDepartamento-2022pr.xlsx", sheet = "Cuadro 1", range = "A10:AS36")
PBID <- PBID %>%
  rename(DPTO_CCDGO = `Código Departamento (DIVIPOLA)`) %>%
  select(DPTO_CCDGO, `2018`) %>% #keep only 2018 data 
  rename(PBID18_2t3 = `2018`)

ED18 <- merge(ED18, PBID, by = "DPTO_CCDGO", all.x = TRUE)

ED18 <- subset(ED18, select = -DPTO_CCDGO) #remove repetitive variable

hist(ED18$PBID18_2t3) #right-skewed, major outlier is Bogota (25% of Colombian GDP and VAM), so the data could be logged
hist(ED18$VAM18_2t3) # heavily skewed by few cases with much higher values
# Because the data is not normally distributed, we use the Empirical CDF
eCDF_2t3d <- ecdf(ED18$PBID18_2t3) 
ED18$eCDF_2t3d <- eCDF_2t3d(ED18$PBID18_2t3) # departments
eCDF_2t3m <- ecdf(ED18$VAM18_2t3)
ED18$eCDF_2t3m <- eCDF_2t3m(ED18$VAM18_2t3) # municipalities



#---- 2-3 Interaction ----#
# interact V-Dem with ECDF variables

# Department-level data
# 2: proportion who selected less economically developed
ED18 <- ED18 %>% 
  mutate(el_c2d = eCDF_2t3d*v13_col_sn_2018$v2elsnlfc_2) %>%
  mutate(em_c2d = eCDF_2t3d*v13_col_sn_2018$v2elsnmrfc_2) %>%
  mutate(cs_c2d = eCDF_2t3d*v13_col_sn_2018$v2clrgstch_2) %>%
  mutate(cw_c2d = eCDF_2t3d*v13_col_sn_2018$v2clrgwkch_2)
# 3: proportion who selected more economically developed
ED18 <- ED18 %>% 
  mutate(el_c3d = eCDF_2t3d*v13_col_sn_2018$v2elsnlfc_3) %>%
  mutate(em_c3d = eCDF_2t3d*v13_col_sn_2018$v2elsnmrfc_3) %>%
  mutate(cs_c3d = eCDF_2t3d*v13_col_sn_2018$v2clrgstch_3) %>%
  mutate(cw_c3d = eCDF_2t3d*v13_col_sn_2018$v2clrgwkch_3)

# Municipal-level data
# 2: proportion who selected less economically developed
ED18 <- ED18 %>% 
  mutate(el_c2m = eCDF_2t3m*v13_col_sn_2018$v2elsnlfc_2) %>%
  mutate(em_c2m = eCDF_2t3m*v13_col_sn_2018$v2elsnmrfc_2) %>%
  mutate(cs_c2m = eCDF_2t3m*v13_col_sn_2018$v2clrgstch_2) %>%
  mutate(cw_c2m = eCDF_2t3m*v13_col_sn_2018$v2clrgwkch_2)
# 3: proportion who selected more economically developed
ED18 <- ED18 %>% 
  mutate(el_c3m = eCDF_2t3m*v13_col_sn_2018$v2elsnlfc_3) %>%
  mutate(em_c3m = eCDF_2t3m*v13_col_sn_2018$v2elsnmrfc_3) %>%
  mutate(cs_c3m = eCDF_2t3m*v13_col_sn_2018$v2clrgstch_3) %>%
  mutate(cw_c3m = eCDF_2t3m*v13_col_sn_2018$v2clrgwkch_3)

# For sf (R), merge this data to shapefile
col <- merge(col, ED18, by = "MPIO_CDPMP", all.x = TRUE)

#clean things up for next variable
df2rm = c("ED18", "ED18_2t3", "eCDF_2t3d", "eCDF_2t3m", "PBID", "VAM18", "ap_2t3", "df2rm")
rm(list = df2rm)


##---- 4-5 Distance to capital city ----

# municipal-level data for distance to capital city
CEDE18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/4-5_DisCapital/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx")
colnames(CEDE18)[3] = "MPIO_CDPMP"
colnames(CEDE18)[7] = "Year"
CEDE18 <- CEDE18 %>%
  filter(Year == 2018)
# this data set also contains important descriptive information (department, province, rurality index) that we will hang onto
CEDE18 <- CEDE18[c("MPIO_CDPMP", "discapital", "disbogota")]

# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CEDE18$MPIO_CDPMP <- as.character(CEDE18$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE18$MPIO_CDPMP <- ifelse(nchar(CEDE18$MPIO_CDPMP) == 4, paste0("0", CEDE18$MPIO_CDPMP), CEDE18$MPIO_CDPMP)

# Note: It may be possible to calculate this variable in ArcMaps using the field calculator, but this requires advanced techniques.

hist(CEDE18$disbogota) # skewed right (more distant)
# Option #1: interact V-Dem with rural index score
eCDF_4t5 <- ecdf(CEDE18$disbogota) #normal distribution, so we use pnorm (CDF)
CEDE18$eCDF_4t5 <- eCDF_4t5(CEDE18$disbogota) # municipalities

#---- 4-5 Interaction ----#

# 4-5. proportion who selected inside capital city and outside
CEDE18 <- CEDE18 %>% 
  mutate(el_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2elsnlfc_4, v13_col_sn_2018$v2elsnlfc_5*eCDF_4t5)) %>%
  mutate(em_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2elsnmrfc_4, v13_col_sn_2018$v2elsnmrfc_5*eCDF_4t5)) %>%
  mutate(cs_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2clrgstch_4, v13_col_sn_2018$v2clrgstch_5*eCDF_4t5)) %>%
  mutate(cw_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2clrgwkch_4, v13_col_sn_2018$v2clrgwkch_5*eCDF_4t5))


# For sf (R), merge this data to shapefile
col <- merge(col, CEDE18, by = "MPIO_CDPMP", all.x = TRUE)

# merge base-layer dataset to obtain FID variable
CEDE18 <- merge(CEDE18, MGN18[, c("MPIO_CDPMP", "FID")], by = "MPIO_CDPMP", all.x = TRUE)
CEDE18 <- CEDE18 %>%
  rename(discap4t5 = discapital) %>%
  rename(disbog4t5 = disbogota) %>% 
  rename(FID_4t5 = FID)

#clean things up for next variable
df2rm = c("CEDE18", "c5el", "c5cw", "df2rm", "ap_4t5")
rm(list = df2rm)

##---- 6-9 NESW ----

#data from DANE regions requires some formatting before merging
reg <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/6-9_Cardinal/Departamentos_y_municipios_de_Colombia_20240312.csv")
reg <- reg %>%
  rename(mpio = `CÓDIGO DANE DEL MUNICIPIO`)
reg$MPIO_CDPMP <- sprintf("%.3f", reg$mpio) #Codigo municipio should have 5 digits
reg$MPIO_CDPMP <- gsub("\\.", "", reg$MPIO_CDPMP)
reg$MPIO_CDPMP <- ifelse(nchar(reg$MPIO_CDPMP) == 4, sprintf("0%s", reg$MPIO_CDPMP), reg$MPIO_CDPMP)

# categories chosen here
unique(reg$REGION)
reg <- reg %>%
  mutate(cdir6t9 = case_when(
    REGION %in% c('Región Caribe', 'Región Eje Cafetero - Antioquia') ~ 'North',
    REGION %in% c('Región Centro Oriente', 'Región Llano') ~ 'East',
    REGION == 'Región Centro Sur' ~ 'South',
    REGION == 'Región Pacífico' ~ 'West',
    TRUE ~ NA_character_
  ))

#---- 6-9 Interaction ----#

# "less free and fair subnational elections"
reg <- reg %>% 
  mutate(el_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2elsnlfc_6,  
    cdir6t9 == "South" ~ v13_col_sn_2018$v2elsnlfc_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2elsnlfc_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2elsnlfc_9,
    TRUE ~ NA_real_ 
  ))
# "more free and fair subnational elections"
reg <- reg %>% 
  mutate(em_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2elsnmrfc_6,
    cdir6t9 == "South" ~ v13_col_sn_2018$v2elsnmrfc_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2elsnmrfc_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2elsnmrfc_9,
    TRUE ~ NA_real_ 
  ))
# "stronger civil liberties"
reg <- reg %>% 
  mutate(cs_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2clrgstch_6, 
    cdir6t9 == "South" ~ v13_col_sn_2018$v2clrgstch_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2clrgstch_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2clrgstch_9,
    TRUE ~ NA_real_
  ))
# "weaker civil liberties"
reg <- reg %>% 
  mutate(cw_t6t9 = case_when( 
    cdir6t9 == "North" ~ v13_col_sn_2018$v2clrgwkch_6,  
    cdir6t9 == "South" ~ v13_col_sn_2018$v2clrgwkch_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2clrgwkch_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2clrgwkch_9,
    TRUE ~ NA_real_ 
  ))

# For sf (R), merge this data to shapefile
col <- merge(col, reg, by = "MPIO_CDPMP", all.x = TRUE)

#for preliminary mapping... 
col <- col %>%
  mutate(m1_t6t9 = (el_t6t9 + cw_t6t9) / 2) %>% # weak sn democracy
  mutate(m2_t6t9 = (em_t6t9 + cs_t6t9) / 2) # strong sn democracy

# For ArcMaps, merge base-layer dataset to obtain FID variable (for later integration with ArcMaps)
reg <- merge(reg, MGN18[, c("MPIO_CDPMP", "FID")], by = "MPIO_CDPMP", all.x = TRUE)
reg <- reg %>% 
  rename(FID_6t9 = FID)
#Belen de Bajira (Choco) does not have an FID number, which causes problems in ArcMap when merging. This municipality was only recently inaugurated.
reg <- reg[complete.cases(reg$FID_6t9), ] 

#clean things up for next variable
df2rm = c("reg", "elcw6t9", "emcs6t9", "ap_6t9", "df2rm")
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
CU18 <- CU18[c("MPIO_CDPMP", "hurto", "errad_manual", "d_desplaza")]

hist(CU18$d_desplaza) # very right-skewed. Medellin most reported cases of displacement
# Because the data is not normally distributed, we use the Empirical CDF
eCDF_10 <- ecdf(CU18$d_desplaza) 
CU18$eCDF_10 <- eCDF_10(CU18$d_desplaza) 
hist(CU18$eCDF_10) # still shows a right-leaning distribution, but less so. About 20% observations (220) are 0 or NA. What repercussions does this have on the factor analysis?

IA18 <- CU18[c("MPIO_CDPMP", "hurto", "errad_manual")] #before removing these variables, create new df for next criterion
CU18 <- CU18[c("MPIO_CDPMP", "d_desplaza", "eCDF_10")]




##---- 11 Illicit activity ----

hist(CU18$hurto) # very right-skewed 
hist(CU18$errad_manual) # very right-skewed 

eCDF_11a <- ecdf(IA18$hurto)
IA18$eCDF_11a <- eCDF_11a(IA18$hurto) 
eCDF_11b <- ecdf(IA18$errad_manual)
IA18$eCDF_11b <- eCDF_11b(IA18$errad_manual) 

hist(IA18$eCDF_11a) # almost normal. Will keep as our indicator for illicit activity. Only 45 observations of 0
hist(IA18$eCDF_11b) # more normal, but way more NAs (crop substitution is rural and focalized)

IA18 <- IA18[c("MPIO_CDPMP", "hurto", "eCDF_11a")]
col <- merge(col, IA18, by = "MPIO_CDPMP", all.x = TRUE)

#clean things up for next variables
df2rm = c("CU18", "IA18", "variations", "df2rm")
rm(list = df2rm)


##---- 12 Sparse population density ----
SP18 <- read_excel("data/geospatial/2018pmq/12_SparsePop/TerriData_Dim1_gen.xlsx", 
                   col_types = c("text", "text", "text", "text", "text", "text", 
                                 "text", "numeric", "text", "text", "text", "text", "text"))
SP18 <- SP18 %>%
  rename(MPIO_CDPMP = 3, Valor = 8, Year = 10) %>%
  filter(Year == 2018)
SP18 <- SP18[c("MPIO_CDPMP", "Entidad", "Indicador", "Valor", "Year", "Fuente")]

SP18_unique = unique(SP18$Indicador) # Check the list of available indicators (many have no Quantitative data associated)
SP18_unique
SP2keep = c("Población total", "Densidad poblacional")
SP18 = SP18[(SP18$Indicador %in% SP2keep), ] # Keep only the indicators that are relevant
SP18 <- SP18 %>%
  filter(MPIO_CDPMP != "01001")

# Pivot Wider the relevant indicators
SP18 <- SP18 %>%
  pivot_wider(names_from = Indicador, values_from = Valor) %>%
  group_by(MPIO_CDPMP) %>%
  summarise(across(everything(), ~first(na.omit(.))))
colnames(SP18)[5:6] = c("pobl_tot", "pobl_dens")


hist(SP18$pobl_tot) # very right-skewed 
hist(SP18$pobl_dens) # very right-skewed 

eCDF_12 <- ecdf(SP18$pobl_dens)
SP18$eCDF_12 <- eCDF_12(SP18$pobl_dens)

hist(SP18$eCDF_12) # looks good


SP18 <- SP18[c("MPIO_CDPMP", "pobl_tot", "pobl_dens", "eCDF_12")]
col <- merge(col, SP18, by = "MPIO_CDPMP", all.x = TRUE)


##---- 13 Remoteness ----



##---- 14 Indigenous population ----
## 2.2.2 Terridata "General": contains Population data up to 2023
IN18 <- read_excel("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/14_Indigenous/TerriData_Dim2_Sub5_etnica.xlsx")
IN18 <- IN18 %>%
  rename(MPIO_CDPMP = 3, Valor = 8, Year = 10) %>%
  filter(Year == 2018)
IN18 <- IN18[c("MPIO_CDPMP", "Entidad", "Indicador", "Valor", "Year", "Fuente")]


IN18_unique = unique(IN18$Indicador) # Check the list of available indicators (many have no Quantitative data associated)
IN18_unique
IN2keep = c("Población indígena","Población étnica total")
IN18 = IN18[(IN18$Indicador %in% IN2keep), ] # Keep only the indicators that are relevant
# Pivot Wider the relevant indicators
IN18 = IN18 %>%
  pivot_wider(names_from = Indicador, values_from = Valor)
colnames(IN18)[5:6] = c("pobl_ind", "pobl_etn")

hist(IN18$pobl_ind) # very right-skewed 
hist(IN18$pobl_etn) # very right-skewed 

eCDF_14i <- ecdf(IN18$pobl_ind)
IN18$eCDF_14i <- eCDF_14i(IN18$pobl_ind)
eCDF_14e <- ecdf(IN18$pobl_etn)
IN18$eCDF_14e <- eCDF_14e(IN18$pobl_etn)


IN18 <- IN18[c("MPIO_CDPMP", "pobl_ind", "pobl_etn", "eCDF_14i", "eCDF_14e")]
col <- merge(col, IN18, by = "MPIO_CDPMP", all.x = TRUE)



#create indigenous population proportion over total population
#col = col %>%
#  mutate(pobl_indp = pobl_ind / pobl_tot) %>%
#  mutate(pobl_etnp = pobl_etn / pobl_tot)





##---- 15-16 Support for National ruling party ----
# Import voting data from Registraduria (RNEC).

rp18 <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/15-16_RulingParty/2018_presidencia_segunda_vuelta_dta_c27d4515ed.csv") # this long dataset contains only 2 candidates because it was a second-round election. other datasets will include more rows depending on the number of candidates running.

#Codigo municipio should have 5 digits
rp18$MPIO_CDPMP <- as.character(rp18$codmpio)
rp18$MPIO_CDPMP <- ifelse(nchar(rp18$MPIO_CDPMP) == 4, sprintf("0%s", rp18$MPIO_CDPMP), rp18$MPIO_CDPMP)
# merge base-layer dataset to obtain FID variable (for later integration with ArcMaps)
rp18 <- merge(rp18, MGN18[, c("MPIO_CDPMP", "FID")], by = "MPIO_CDPMP", all.x = TRUE)

# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp18 <- rp18 %>%
  group_by(FID) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 

# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
p18d0 <- rp18 %>%
  group_by(FID, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Petro = `1`,
         Duque = `2`,
         nomark = `997`,
         null = `998`,
         blank = `999`)

## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p18d2 <- p18d0 %>%
  select(FID, Petro) %>%
  filter(!is.na(Petro))
p18d2 <- p18d2[!duplicated(p18d2$FID), ]
p18d3 <- p18d0 %>%
  select(FID, Duque) %>%
  filter(!is.na(Duque))
p18d3 <- p18d3[!duplicated(p18d3$FID), ]
p18d4 <- p18d0 %>%
  select(FID, nomark) %>%
  filter(!is.na(nomark))
p18d4 <- p18d4[!duplicated(p18d4$FID), ]
p18d5 <- p18d0 %>%
  select(FID, null) %>%
  filter(!is.na(null))
p18d5 <- p18d5[!duplicated(p18d5$FID), ]
p18d6 <- p18d0 %>%
  select(FID, blank) %>%
  filter(!is.na(blank))
p18d6 <- p18d6[!duplicated(p18d6$FID), ]
p18d7 <- p18d0 %>%
  group_by(FID) %>%
  summarise(vtotal = first(votototal))
p18d7 <- p18d7[!duplicated(p18d7$FID), ]

RPs <- list(p18d2, p18d3, p18d4, p18d5, p18d6, p18d7)
merged_RP <- Reduce(function(x, y) merge(x, y, by = "FID", all = TRUE), RPs)

df2rm2 = c("rp18", "p18d0", "p18d2", "p18d3", "p18d4", "p18d5", "p18d6", "p18d7", "df2rm2", "RPs")
rm(list = df2rm2)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP18 <- merged_RP %>%
  mutate(MOV_top2 = Duque-Petro) %>% #Raw vote-count margin of victory
  mutate(Duque_pct = Duque/vtotal) %>%
  mutate(Petro_pct = Petro/vtotal) %>%
  mutate(MOV_pct = Duque_pct-Petro_pct) #Standardized percent margin of victory

round((sum(RP18$Duque)/sum(RP18$vtotal))*100, 2) # 52.3% for Duque
round((sum(RP18$Petro)/sum(RP18$vtotal))*100, 2) # 40.8% for Petro

round(mean(RP18$MOV_pct)*100, 2) #Duque municipal-level margin is 26.3% on average (quite high).
round(sd(RP18$MOV_pct), 3) #SD for the margin is larger than the mean (39.7%) suggesting abnormal distribution
hist(RP18$MOV_pct, main = "Histogram of MOV", xlab = "MOV", col = "skyblue", border = "black") #left-tail long (margin for Petro)


#---- 15-16 Interaction ----#

# Option #1: interact V-Dem with margin-of-victory
eCDF_15t6 <- ecdf(RP18$MOV_pct) #abnormal distribution, so we use eCDF
RP18$eCDF_15t6 <- eCDF_15t6(RP18$MOV_pct) # municipalities

# 15-16. proportion votes for ruling party (Duque) 
# 15: proportion of respondents who selected "Areas where the national ruling party or group is strong":
RP18 <- RP18 %>% 
  mutate(el_c15 = eCDF_15t6*v13_col_sn_2018$v2elsnlfc_15) %>%
  mutate(em_c15 = eCDF_15t6*v13_col_sn_2018$v2elsnmrfc_15) %>%
  mutate(cs_c15 = eCDF_15t6*v13_col_sn_2018$v2clrgstch_15) %>%
  mutate(cw_c15 = eCDF_15t6*v13_col_sn_2018$v2clrgwkch_15)
# 16: proportion of respondents who selected "Areas where the national ruling party or group is weak":
RP18 <- RP18 %>% 
  mutate(el_c16 = eCDF_15t6*v13_col_sn_2018$v2elsnlfc_16) %>%
  mutate(em_c16 = eCDF_15t6*v13_col_sn_2018$v2elsnmrfc_16) %>%
  mutate(cs_c16 = eCDF_15t6*v13_col_sn_2018$v2clrgstch_16) %>%
  mutate(cw_c16 = eCDF_15t6*v13_col_sn_2018$v2clrgwkch_16)


# For sf (R), merge this data to shapefile
col <- merge(col, RP18, by = "FID", all.x = TRUE)


#clean things up
df2rm = c("RP18", "merged_RP", "c15cs", "c16cw", "ap_15t6", "df2rm")
rm(list = df2rm)


#---- Step 4: Summary measures ----#

# calculate the arithmetic mean for the continuous variables
col$el_i = (col$el_c0+col$el_c1+col$el_c2m+col$el_c3m+col$el_c4t5+col$el_t6t9+col$el_c15+col$el_c16) / 8
col$em_i = (col$em_c0+col$em_c1+col$em_c2m+col$em_c3m+col$em_c4t5+col$em_t6t9+col$em_c15+col$em_c16) / 8
col$cs_i = (col$cs_c0+col$cs_c1+col$cs_c2m+col$cs_c3m+col$cs_c4t5+col$cs_t6t9+col$cs_c15+col$cs_c16) / 8
col$cw_i = (col$cw_c0+col$cw_c1+col$cw_c2m+col$cw_c3m+col$cw_c4t5+col$cw_t6t9+col$cw_c15+col$cw_c16) / 8



col <- merge(col, CU18, by = "MPIO_CDPMP", all.x = TRUE)
