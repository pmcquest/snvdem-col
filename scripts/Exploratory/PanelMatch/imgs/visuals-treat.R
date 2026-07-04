# ==============================================================================#
# VISUALIZATION: COLOMBIAN MUNICIPAL DEMOCRACY & PEACE PROCESSES (2023)   #
# ==============================================================================#

library(tidyverse)
library(dplyr)
library(readxl)
library(haven)
library(stringr)
library(sf)
library(spdep)
library(PanelMatch)
library(did)
library(zoo)
library(ggplot2)
library(viridis)

# 0. Load data -----------------------------------------------------------------
## SNVDEM Panel (2000-2023)----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

## AUC Demobilization Sites (2003-2006)----
## Locations (37 municipalities) where AUC demobilized starting in 2003 as treated municipalities. [Could be a one-time treatment (e.g., 2005 with the law passage) or a staggered treatment (2003-2006).] AUC leadership offered a schedule for collective demobilizations of regional blocs beginning in 2003 with two pilot processes: Bloque Cacique Nutibara (in Medellin) and Autodefensas Campesinas de Ortega (en Cajibio, Cauca). More detail on the demobilizations here: https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf
# Source: Oficina Alto Comisionado para la Paz (Dec. 2006)--https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf (information extracted and put into table format).
Bloques <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2005_2017-compare/Bloques.xlsx")

## ZVTN Sites (2017-) ----
## Locations of the 26 Zonas Veredales Transitorias de Normalización (ZVTN)--subsequently labeled Espacios Territoriales de Capacitación y Reincorporación (ETCR)--according to Defensoría del Pueblo (2017). Analogous to the demobilization sites of the AUC between 2003-2006 ("Bloques").
ZVTN <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2005_2017-compare/ZVTN.xlsx")
## PDET municipalities (2017-) ----
## ART identified 170 municipalities for the PDET program.
PDET <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta")
PDET <- PDET %>%
  rename(DPTO_CCDGO = 3, depto = 4, MPIO_CDPMP = 5, municipio = 6) %>%
  select(3:8)
PDET$MPIO_CDPMP <- as.character(PDET$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
PDET$MPIO_CDPMP <- ifelse(nchar(PDET$MPIO_CDPMP) == 4, paste0("0", PDET$MPIO_CDPMP), PDET$MPIO_CDPMP)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

##Geospatial data----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# Function to get unique neighbor codes
get_neighbors <- function(target_list, geo_obj) {
  nb <- poly2nb(geo_obj, queen = TRUE)
  idx <- which(geo_obj$MPIO_CDPMP %in% target_list)
  neighbor_idx <- unlist(nb[idx])
  neighbor_codes <- geo_obj$MPIO_CDPMP[neighbor_idx]
  return(setdiff(unique(neighbor_codes), target_list))
}

# Generate neighbor lists 
jyp_list      <- clean_mpio(Bloques$MPIO_CDPMP)
zvtn_list     <- clean_mpio(ZVTN$MPIO_CDPMP)
pdet_list     <- clean_mpio(PDET$MPIO_CDPMP[PDET$PDET == 1])

jyp_neighbors <- get_neighbors(jyp_list, muni_geo)
zvtn_neighbors <- get_neighbors(zvtn_list, muni_geo)
pdet_neighbors <- get_neighbors(pdet_list, muni_geo)


## Covariate data ----
# For regression-based estimations (e.g., PanelMatch), it is useful to have time-variant and time-invariant data. We selected variables that are not included in the dependent variable (snvdem index) or independent variables (demobilizations or territorial programs).
# See 09_analysis_scripts/Outcomes/outcome_wrangle.R for more information
panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")
panel_imputed <- panel_imputed %>% select(1:9|14|19) # selecting covariates

# Treatments----
master_panel <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(mutate(panel_imputed, MPIO_CDPMP = clean_mpio(MPIO_CDPMP)), 
            by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    MPIO_ID_numeric = as.integer(as.factor(MPIO_CDPMP)),
    year = as.integer(year),
    log_pop = log(PobTot_12 + 1),
    # Core logical flags
    is_jyp  = MPIO_CDPMP %in% jyp_list,
    is_zvtn = MPIO_CDPMP %in% zvtn_list,
    is_pdet = MPIO_CDPMP %in% pdet_list,
    is_jyp_nb  = MPIO_CDPMP %in% jyp_neighbors,
    is_zvtn_nb = MPIO_CDPMP %in% zvtn_neighbors,
    is_pdet_nb = MPIO_CDPMP %in% pdet_neighbors
  ) %>%
  group_by(MPIO_ID_numeric) %>%
  # 1. Staggered DiD Cohorts (Sticky)
  mutate(first_treat = case_when(
    is_jyp | is_jyp_nb ~ 2005,
    is_pdet ~ 2017,
    TRUE ~ 0
  )) %>%
  # 2. Broad Event Indicators for PanelMatch (Single Year Pulse)
  mutate(
    jyp_broad_event  = as.integer((is_jyp | is_jyp_nb) & year == 2005),
    zvtn_broad_event = as.integer((is_zvtn | is_zvtn_nb) & year == 2017)
  ) %>%
  # Clean up missing data (linear interpolation)
  mutate(across(c(sndem_index, log_pop, MortInfantil1, Enroll5_16), 
                ~ na.approx(., na.rm = FALSE))) %>%
  fill(starts_with("is_"), first_treat, .direction = "downup") %>%
  ungroup()
master_panel$MPIO_ID_numeric <- as.numeric(as.character(master_panel$MPIO_ID_numeric))

## Table of treatments ----

# Full table
master_panel_did <- master_panel %>%
  mutate(
    treatment_profile = case_when(
      # 1. Complex Overlaps First
      is_jyp & is_zvtn & is_pdet      ~ "Triple Overlap (JyP + ZVTN + PDET)",
      is_jyp_nb & is_pdet             ~ "JyP Nb + PDET Overlap", # Move this up!
      is_zvtn_nb & is_pdet            ~ "ZVTN Nb + PDET Overlap", # Move this up!
      is_zvtn & is_pdet               ~ "ZVTN + PDET Overlap",
      is_jyp & is_zvtn                ~ "JyP + ZVTN Overlap",
      is_jyp & is_pdet                ~ "JyP + PDET Overlap",
      
      # 2. Individual Categories Second
      is_jyp                          ~ "JyP Only",
      is_zvtn                         ~ "ZVTN Only",
      is_pdet                         ~ "PDET Only",
      is_jyp_nb                       ~ "JyP Neighbor",
      is_zvtn_nb                      ~ "ZVTN Neighbor",
      is_pdet_nb                      ~ "PDET Neighbor",
      
      # 3. Baseline
      TRUE                            ~ "Control (None)"
    )
  )

# Trimmed table
master_panel_did2 <- master_panel %>%
  mutate(
    treatment_profile = case_when(
      is_jyp & is_pdet                ~ "JyP + PDET Overlap",
      is_jyp & !is_pdet               ~ "JyP Only",
      is_zvtn & !is_pdet              ~ "ZVTN Only",
      is_pdet                         ~ "PDET Only", # will include JyP and ZVTN overlaps
      is_jyp_nb                       ~ "JyP Neighbor",
      TRUE                            ~ "Control (None)"
    )
  )

## Summary counts ----
library(janitor)
library(kableExtra)

# All treatments
muni_counts <- master_panel_did %>%
  filter(year == 2023) %>%
  count(treatment_profile) %>%
  mutate(percent = n / sum(n) * 100) %>%
  adorn_totals("row")
print(muni_counts)

muni_counts %>%
  kbl(
    caption = "Table 1: Distribution of Peace Agreement Treatment Profiles (2023)",
    booktabs = TRUE,           # Professional thin/thick horizontal lines
    align = "lc",              # Left-align labels, Center-align numbers
    col.names = c("Treatment Profile", "N", "Percentage (%)"),
    digits = 2
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  row_spec(0, bold = TRUE) %>% # Bold header
  row_spec(nrow(muni_counts), bold = TRUE, italic = TRUE) %>% # Bold/Italic Total row
  footnote(general = "Data reflects the 1,125 municipalities in the Colombian national panel for the year 2023.",
           general_title = "Note: ", 
           footnote_as_chunk = TRUE) %>%
  # 3. Save directly to your working directory
  as_image(file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/imgs/Table_Treatment_Profiles.png")

# Trimmed treatments
muni_counts2 <- master_panel_did2 %>%
  filter(year == 2023) %>%
  count(treatment_profile) %>%
  mutate(percent = n / sum(n) * 100) %>%
  adorn_totals("row")
print(muni_counts2)

muni_counts2 %>%
  kbl(
    caption = "Table 1: Distribution of Peacebuilding Treatment Profiles (2023)",
    booktabs = TRUE,           # Professional thin/thick horizontal lines
    align = "lc",              # Left-align labels, Center-align numbers
    col.names = c("Treatment Profile", "N", "Percentage (%)"),
    digits = 2
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  row_spec(0, bold = TRUE) %>% # Bold header
  row_spec(nrow(muni_counts2), bold = TRUE, italic = TRUE) %>% # Bold/Italic Total row
  footnote(general = "Data reflects the 1,125 municipalities in the Colombian national panel for the year 2023.",
           general_title = "Note: ", 
           footnote_as_chunk = TRUE) %>%
  # 3. Save directly to your working directory
  as_image(file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/imgs/Table_Treatment_Profiles-trimmed.png")

# Maps ----
# Full data
map_data <- master_panel_did %>%
  filter(year == 2023) %>%
  select(MPIO_CDPMP, treatment_profile)

muni_geo_plot <- muni_geo %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(map_data, by = "MPIO_CDPMP") %>%
  # Replace NA profiles (if any) with Control
  mutate(treatment_profile = ifelse(is.na(treatment_profile), "Control (None)", treatment_profile))

ggplot(data = muni_geo_plot) +
  geom_sf(aes(fill = treatment_profile), color = "white", size = 0.05) +
  scale_fill_manual(values = c(
    "Control (None)" = "grey90",
    "JyP Only" = "#FDE725FF",
    "PDET Only" = "#35B779FF",
    "ZVTN Only" = "#440154FF",
    "JyP + PDET Overlap" = "#31688EFF",
    "ZVTN + PDET Overlap" = "#8FD744FF",
    "Triple Overlap (JyP + ZVTN + PDET)" = "#D55E00" # Bright orange for the critical group
  )) +
  labs(
    title = "Geography of the 'Hard Places' in Colombia",
    subtitle = "Overlap of JyP (2005), ZVTN (2017), and PDET Territorial Programs",
    fill = "Treatment Category",
    caption = "Source: Panel Data based on OACP and PDET registries."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Trimmed data
# Full data
map_data2 <- master_panel_did2 %>%
  filter(year == 2023) %>%
  select(MPIO_CDPMP, treatment_profile)

muni_geo_plot2 <- muni_geo %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(map_data2, by = "MPIO_CDPMP") %>%
  # Replace NA profiles (if any) with Control
  mutate(treatment_profile = ifelse(is.na(treatment_profile), "Control (None)", treatment_profile))

ggplot(data = muni_geo_plot2) +
  geom_sf(aes(fill = treatment_profile), color = "white", size = 0.05) +
  scale_fill_manual(values = c(
    "Control (None)" = "grey90",
    "JyP Only" = "#FDE725FF",
    "PDET Only" = "#35B779FF",
    "ZVTN Only" = "#440154FF",
    "JyP Neighbor" = "#31688EFF",
    "JyP + PDET Overlap" = "#D55E00"
  )) +
  labs(
    title = "Geography of treatments in Colombia",
    subtitle = "JyP (2005), ZVTN (2017), and PDET municipalities",
    fill = "Treatment Category",
    caption = "Source: Panel Data based on OACP and PDET registries."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

#---- Write the dataframe to rds ----
write_rds(master_panel_did, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/snvdem_treated.rds")
