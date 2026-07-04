# Case study for snvdem draft v3

# Diff-in-diff with two treatments
# DV: predicted levels of subnational democracy
# IV1: 2005 JyP territory (5 departments?)
# IV2: 2016 CPA territory (


# ----Setup ----
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(haven)
library(ggplot2)


# selecting treatment municipalities...

# 2005 JyP----
# In order to measure the treatment effects of the 2005 accord, we have to identify municipalities that received the "treatment". This is tricky because the JyP law didn't define geographic locations where it would be implemented (a major flaw of the process was inadequate reincorporation monitoring and assistance). However, demobilization under the process followed a territorial strategy, occurring over time across several regions (see: Vásquez Delgado and Barrera Ramírez, 2016). The majority of demobilizations occurred in 2005 and 2006, involving both collective as well as individual mobilization, and among separate armed groups (AUC, FARC, ELN, GAO, etc.).

## A. Demobilizing municipalities----
# Locations where AUC demobilized starting in 2003 as treated municipalities. [Could be a one-time treatment (e.g., 2005 with the law passage) or a staggered treatment (2003-2006).] AUC leadership offered a schedule for collective demobilizations of regional blocs beginning in 2003 with two pilot processes: Bloque Cacique Nutibara (in Medellin) and Autodefensas Campesinas de Ortega (en Cajibio, Cauca). More detail on the demobilizations here: https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf
# Source: Oficina Alto Comisionado para la Paz (Dec. 2006)--https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf (information extracted and put into table format).
Bloques <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/Bloques.xlsx")
Bloques <- Bloques %>%
  rename(year = 7, AUC_demob = 8)

# Based on the OACP report (2006), there were 37 unique municipal observations between 2003-2006 (22 unique departments mentioned). Antioquia was by far the leader in demobilizations (10), followed by Cordoba (Tierralta--3), then Choco and Magdalena (2 each)
summary(Bloques)
table(Bloques$DPTO_CCDGO)
# several observations in Tierralta, Cordoba, where Santa Fe de Ralito is located. 


## B. Total demobilizations----
# Select locations of all demobilized (individual or collective) according to ARN data. This data is already municipal-level time-series. Source: Agencia para la Reincorporacion y la Normalizacion (2025)--https://www.datos.gov.co/Inclusi-n-Social-y-Reconciliaci-n/ESTAD-STICAS-DE-LAS-PERSONAS-DESMOVILIZADAS-QUE-HA/39pj-dba6/about_data
ARN <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/EstadisticasARN_20251220.csv", 
                locale = locale(encoding = "ISO-8859-1"))
ARN <- ARN %>%
  rename(TipoDesmovil = 1, ExGrupo = 2, year = 3, depto = 9, municipio = 10, DPTO_CCDGO = 11, MPIO_CDPMP = 12) %>%
  select(1:5|9:12) %>%
  filter(year < 2021)
# 56627 individual-level observations between 2001-2025
# nearly all municipalities mentioned, even between 2005 and 2006 when majority demobilized
summary(ARN)

## Aggregate to municipality-year data
demob_summary <- ARN %>%
  mutate(
    ExGrupo = trimws(ExGrupo),
    TipoDesmovil = trimws(TipoDesmovil),
    MPIO_CDPMP = trimws(MPIO_CDPMP), 
    municipio = trimws(municipio),
    depto = trimws(depto)
  ) %>%
  filter(year >= 2001, MPIO_CDPMP != "<No Registra>") %>%
  # Adding municipio and depto to the grouping
  group_by(MPIO_CDPMP, municipio, depto, year) %>%
  summarise(
    total_demob = n(),
    AUC_total = sum(ExGrupo == "AUC", na.rm = TRUE),
    colectiva = sum(TipoDesmovil == "Colectiva", na.rm = TRUE),
    individual = sum(TipoDesmovil == "Individual", na.rm = TRUE),
    AUC_colectiva = sum(ExGrupo == "AUC" & TipoDesmovil == "Colectiva", na.rm = TRUE),
    AUC_individual = sum(ExGrupo == "AUC" & TipoDesmovil == "Individual", na.rm = TRUE),
    .groups = "drop"
  )
# Create full df (all 1122 municipalities for all available years 2001-2020)
demob_complete <- demob_summary %>%
  complete(
    # nesting ensures MPIO_CDPMP stays linked to its specific municipio and depto
    nesting(MPIO_CDPMP, municipio, depto), 
    year = 2001:max(year), 
    fill = list(
      total_demob = 0, 
      AUC_total = 0, 
      colectiva = 0, 
      individual = 0, 
      AUC_colectiva = 0, 
      AUC_individual = 0
    )
  ) %>%
  select(1|4:10)
# Verify the result
summary(demob_complete)


### Visualize----
yearly_trends <- demob_complete %>%
  group_by(year) %>%
  summarise(
    All_Groups = sum(total_demob),
    AUC = sum(AUC_total)
  ) %>%
  pivot_longer(cols = -year, names_to = "Category", values_to = "Count")

ggplot(yearly_trends, aes(x = year, y = Count, color = Category)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Verification of Demobilization Totals",
       subtitle = "Total All Groups vs. AUC Subset",
       y = "Number of People") +
  theme_minimal()



# 2016 CPA----

## A. 2017 ZVTN data ----
# Locations of the 26 Zonas Veredales Transitorias de Normalización (ZVTN)--subsequently labeled Espacios Territoriales de Capacitación y Reincorporación (ETCR)--according to Defensoría del Pueblo (2017). Analogous to the demobilization sites of the AUC between 2003-2006 ("Bloques").
ZVTN <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/ZVTN.xlsx")
ZVTN <- ZVTN %>%
  rename(municipio = 2, depto = 3, year = 6)

# 26 unique municipal observations (all in 2017) in 13 unique departments mentioned. Again, Antioquia is the leader in demobilizing sites for FARC (6 total), followed by Cauca and Meta (3 each). Cordoba and Magdalena (northern region) only contain 1 site, in contrast to the AUC process.
summary(ZVTN)
table(ZVTN$DPTO_CCDGO)


## B. 2016 PDET data----
PDET <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta")
PDET <- PDET %>%
  rename(DPTO_CCDGO = 3, depto = 4, MPIO_CDPMP = 5, municipio = 6) %>%
  select(3:8)

# Many municipalities: 170 total
table(PDET$PDET)
# similar amount of unique departments as OACP 2006 report: 19
PDET %>% 
  filter(PDET == 1) %>% 
  summarise(n_deptos_unicos = n_distinct(DPTO_CCDGO))



# Merge data (2000-2020)----

# 2000-2020 snvdem data
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

# Standardize MPIO_CDPMP to character in all relevant dataframes
snvdem <- snvdem %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))
PDET <- PDET %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))
demob_complete <- demob_complete %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))
Bloques <- Bloques %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))
ZVTN <- ZVTN %>% mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))

## ARN treatment (demobilized) ----
# We select TipoDesmovil and ExGrupo 
snvdem_paz <- snvdem %>%
  left_join(demob_complete, by = c("MPIO_CDPMP", "year"))
table(snvdem_paz$AUC_total, snvdem_paz$year)


## 'Bloque' treatment (2005-)----
snvdem_paz <- snvdem_paz %>%
  left_join(Bloques2, by = c("MPIO_CDPMP", "year")) %>%
  mutate(Desmovilizados2 = coalesce(Desmovilizados, 0))
# 4. Re-create the 'Bloque' treatment indicator
# This uses the unique list of municipalities from the Bloques dataset
bloque_mpio_list <- unique(Bloques$MPIO_CDPMP)

snvdem_paz <- snvdem_paz %>%
  mutate(Bloque = ifelse(MPIO_CDPMP %in% bloque_mpio_list & year >= 2005, 1, 0)) %>%
  distinct() # Final safety check to remove any identical redundant rows
# 36 municipios in 2005, but 34 for the rest
table(snvdem_paz$Bloque, snvdem_paz$year)

# Find which codes appear more than once in 2005
duplicates_2005 <- snvdem_paz %>%
  filter(year == 2005) %>%
  group_by(MPIO_CDPMP) %>%
  tally() %>%
  filter(n > 1)

print(duplicates_2005)



## PDET treatment (2016-)----
pdet_codes <- PDET %>%
  filter(PDET == 1) %>%
  mutate(MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), width = 5, side = "left", pad = "0")) %>%
  pull(MPIO_CDPMP) %>%
  unique()
snvdem_paz <- snvdem_paz %>%
  mutate(PDET_mun = ifelse(MPIO_CDPMP %in% pdet_codes & year >= 2016, 1, 0))

# 170 municipios starting in 2016
table(snvdem_paz$PDET_mun, snvdem_paz$year)

## 'ZVTN' treatment (2017-)----
treated_mpio_codes2 <- unique(ZVTN$MPIO_CDPMP)
snvdem_paz <- snvdem_paz %>%
  mutate(ZVTN_mun = ifelse(MPIO_CDPMP %in% treated_mpio_codes2 & year >= 2017, 1, 0))
# 36 municipios in 2005, but 34 for the rest
table(snvdem_paz$ZVTN_mun, snvdem_paz$year)



# PanelMatch----


# @imaietat2021 provide a matching method for generalizing the DiD estimator, matching treated and untreated observations based on similar outcome or covariate histories. 
# Double treatment...
#In this case a lag of 4 is used to match pre-treatment periods (2000-2004; 2012-2016), while the lead window is 4 post-treatment periods (2017-2020). Then, the ATT and dynamic treatment effects are estimated using pooling and block bootstrapping. 

# The following pipeline is based on the instructions by Liu and Xu in the fect user manual: "5. New DID Methods" (https://yiqingxu.org/packages/fect/05-panel.html#with-treatment-reversals)

# install packages from CRAN
packages <- c("dplyr", "fixest", "did", "didimputation", 
              "panelView", "ggplot2", "bacondecomp", "HonestDiD",
              "DIDmultiplegtDYN", "PanelMatch", "readstata13")
install.packages(setdiff(packages, rownames(installed.packages())))  

# install most up-to-date "fect" from Github
if ("fect" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("xuyiqing/fect")
}

# install forked "HonestDiD" package compatible with "fect"
if ("HonestDiDFEct" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("lzy318/HonestDiDFEct")
}

# load libraries
library(dplyr)
library(readstata13)
library(fixest)
library(did)
library(fect)
library(panelView)
library(PanelMatch)
library(ggplot2)
library(bacondecomp)
library(fect)
library(didimputation)
library(doParallel)
library(HonestDiD)
library(HonestDiDFEct)
# library(DIDmultiplegtDYN) # may require XQuartz 


## Visualizing treatments ----
# What are the effects of the 2005 and 2016 peace processes on subnational democracy in Colombia? 
# 4 municipality categories with 2 treatments (2000-2020): non-treated, once treated then not-treated, not-treated then treated (staggered?), twice treated in 2005 and 2016.

# Data visualization: provides a visual that is more informative for staggered treatments, but also a graphic that shows the DiD estimates

#REVISE
# Dropout rates
panelview(DESERCION ~ PDET, data = CMPD, index = c("CODIGODANE","Year"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)

p1 <- panelview(data = CMPD, Y='DESERCION',
                D='PDET',index=c("CODIGODANE","Year"),
                by.timing = T, display.all = T,
                type = "outcome",by.cohort = T)

# Graduation rates
p2 <- panelview(data = CMPD, Y='APROBACION',
                D='PDET',index=c("CODIGODANE","Year"),
                by.timing = T, display.all = T,
                type = "outcome",by.cohort = T)

# Coverage rates
p3 <- panelview(data = CMPD, Y='COBERTURA_NETA',
                D='PDET',index=c("CODIGODANE","Year"),
                by.timing = T, display.all = T,
                type = "outcome",by.cohort = T)

# Test scores
panelview(data = CMPD, Y='s11_total',
          D='PDET',index=c("CODIGODANE","Year"),
          by.timing = T, display.all = T,
          type = "outcome",by.cohort = T)




## PanelMatch ----
df.pm <- df.use
# we need to convert the unit and time indicator to integer
df.pm[,"CODIGODANE"] <- as.integer(as.factor(df.pm[,"CODIGODANE"]))
df.pm[,"Year"] <- as.integer(as.factor(df.pm[,"Year"]))
df.pm <- df.pm[,c("CODIGODANE","Year","DESERCION","PDET")]

PM.results <- PanelMatch(lag=5, 
                         time.id="Year", 
                         unit.id = "CODIGODANE", 
                         treatment = 'PDET', 
                         refinement.method = "none", 
                         data = df.pm, 
                         qoi = "att", 
                         lead = c(0:3), 
                         outcome.var = 'DESERCION', 
                         match.missing = TRUE)

## For pre-treatment dynamic effects
PM.results.placebo <- PanelMatch(lag=5, 
                                 time.id="Year", 
                                 unit.id = "CODIGODANE", 
                                 treatment = 'PDET', 
                                 refinement.method = "none", 
                                 data = df.pm, 
                                 qoi = "att", 
                                 lead = c(0:3), 
                                 outcome.var = 'DESERCION', 
                                 match.missing = TRUE,
                                 placebo.test = TRUE)


PE.results.pool <- PanelEstimate(PM.results, data = df.pm, pooled = TRUE)
summary(PE.results.pool)
# Dynamic Treatment Effects
PE.results <- PanelEstimate(PM.results, data = df.pm)
PE.results.placebo <- placebo_test(PM.results.placebo, data = df.pm, plot = F)


#not working...
est_lead <- as.vector(PE.results$estimates)
est_lag <- as.vector(PE.results.placebo$estimates)
sd_lead <- apply(PE.results$bootstrapped.estimates,2,sd)
sd_lag <- apply(PE.results.placebo$bootstrapped.estimates,2,sd)
coef <- c(est_lag, 0, est_lead)
sd <- c(sd_lag, 0, sd_lead)
pm.output <- cbind.data.frame(ATT=coef, se=sd, t=c(-2:4))
p.pm <- esplot(data = pm.output,Period = 't',
               Estimate = 'ATT',SE = 'se')
p.pm

out.fect.balance <- fect(DESERCION~PDET, data = df, index = c("CODIGODANE","Year"),
                         method = 'fe', se = TRUE, balance.period = c(-2,4))
print(out.fect.balance$est.balance.avg)

fect.balance.output <- as.data.frame(out.fect.balance$est.balance.att)
fect.balance.output$Time <- c(-2:4)
p.fect.balance <- esplot(fect.balance.output,Period = 'Time',Estimate = 'ATT',
                         SE = 'S.E.',CI.lower = "CI.lower", 
                         CI.upper = 'CI.upper')
p.fect.balance

