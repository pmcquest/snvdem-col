# ==============================================================================#
# INTEGRATED PIPELINE: COLOMBIAN MUNICIPAL DEMOCRACY & PEACE PROCESSES (2023)   #
# ==============================================================================#
library(tidyverse)
library(readxl)
library(haven)
library(stringr)
library(sf)
library(spdep)

# 1. Load data -----------------------------------------------------------------
## SNVDEM Panel (2000-2023)----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

## AUC Demobilization Sites (2003-2006)----
## Locations (37 municipalities) where AUC demobilized starting in 2003 as treated municipalities. [Could be a one-time treatment (e.g., 2005 with the law passage) or a staggered treatment (2003-2006).] AUC leadership offered a schedule for collective demobilizations of regional blocs beginning in 2003 with two pilot processes: Bloque Cacique Nutibara (in Medellin) and Autodefensas Campesinas de Ortega (en Cajibio, Cauca). More detail on the demobilizations here: https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf
# Source: Oficina Alto Comisionado para la Paz (Dec. 2006)--https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf (information extracted and put into table format).
Bloques <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/Bloques.xlsx")

## ARN Statistics (Individual/Collective Demobilizations)----
## Select locations of all demobilized (individual or collective) according to ARN data. This data is already municipal-level time-series. Source: Agencia para la Reincorporacion y la Normalizacion (2025)--https://www.datos.gov.co/Inclusi-n-Social-y-Reconciliaci-n/ESTAD-STICAS-DE-LAS-PERSONAS-DESMOVILIZADAS-QUE-HA/39pj-dba6/about_data
ARN <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/EstadisticasARN_20251220.csv", 
                locale = locale(encoding = "ISO-8859-1"))

## ZVTN Sites (2017-) ----
## Locations of the 26 Zonas Veredales Transitorias de Normalización (ZVTN)--subsequently labeled Espacios Territoriales de Capacitación y Reincorporación (ETCR)--according to Defensoría del Pueblo (2017). Analogous to the demobilization sites of the AUC between 2003-2006 ("Bloques").
ZVTN <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/ZVTN.xlsx")
## PDET municipalities (2017-) ----
## ART identified 170 municipalities for the PDET program.
PDET <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta")
PDET <- PDET %>%
  rename(DPTO_CCDGO = 3, depto = 4, MPIO_CDPMP = 5, municipio = 6) %>%
  select(3:8)
PDET$MPIO_CDPMP <- as.character(PDET$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
PDET$MPIO_CDPMP <- ifelse(nchar(PDET$MPIO_CDPMP) == 4, paste0("0", PDET$MPIO_CDPMP), PDET$MPIO_CDPMP)


# 2. Clean treatment datasets & Spatial Clustering -----------------------------
clean_mpio <- function(x) str_pad(as.character(x), width = 5, side = "left", pad = "0")

# AUC BLOQUES (2005 Process): 37 municipalities
Bloques_clean <- Bloques %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  group_by(MPIO_CDPMP, year = Año) %>%
  summarise(AUC_site_demob = sum(Desmovilizados, na.rm = TRUE), .groups = "drop")

# FARC ZVTN (2016 Process): 25 municipalities
ZVTN_clean <- ZVTN %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP) %>%
  mutate(ZVTN_site = 1) %>%
  distinct()

# PDET (Territorial Program): 170 municipalities
PDET_clean <- PDET %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(PDET == 1) %>%
  select(MPIO_CDPMP) %>%
  mutate(is_pdet = 1)

# ARN (Continuous Demobilization Counts): 5707 municipality-year observations
# We use the ARN summary statistics to identify the correct columns
ARN_agg <- ARN %>%
  mutate(MPIO_CDPMP = clean_mpio(`Municipio de residencia`),
         year = as.numeric(Desmovilizacion)) %>%
  filter(year >= 2000 & year <= 2023) %>% # Updated to 2023
  group_by(MPIO_CDPMP, year) %>%
  summarise(total_demob_arn = n(), .groups = "drop")

## Spatial neighbor identification ----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
muni_geo <- muni_geo %>%
  select(1:8)

# Define treated list for spatial analysis
treated_mpio_all <- unique(c(Bloques$MPIO_CDPMP, ZVTN$MPIO_CDPMP, PDET$MPIO_CDPMP[PDET$PDET==1]))

# Find neighbors (Queen Contiguity)
nb <- poly2nb(muni_geo, queen = TRUE)
# Extract codes of municipalities that border any treated municipality
neighbor_indices <- unlist(nb[muni_geo$MPIO_CDPMP %in% clean_mpio(treated_mpio_all)])
neighbor_codes <- unique(muni_geo$MPIO_CDPMP[neighbor_indices])
# Exclude the treated themselves from the neighbor list
pure_neighbor_codes <- setdiff(neighbor_codes, clean_mpio(treated_mpio_all))


## Visualize treated and neighbors----
treated_clean <- clean_mpio(treated_mpio_all)
neighbor_clean <- clean_mpio(pure_neighbor_codes)

muni_map_data <- muni_geo %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>% # Ensure ID consistency
  mutate(status = case_when(
    MPIO_CDPMP %in% treated_clean ~ "Treated (Host)",
    MPIO_CDPMP %in% neighbor_clean ~ "Neighbor (Buffer)",
    TRUE ~ "Control"
  ))

colombia_spatial_plot <- ggplot(data = muni_map_data) +
  geom_sf(aes(fill = status), color = "white", size = 0.05) +
  scale_fill_manual(values = c(
    "Treated (Host)" = "#e41a1c",   # Red
    "Neighbor (Buffer)" = "#377eb8", # Blue
    "Control" = "#f0f0f0"           # Light Grey
  )) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of Peace Process Treatments",
    subtitle = "Hosts (Core) vs. Contiguous Neighbors (Buffer)",
    fill = "Municipality Status",
    caption = "Buffer defined by Queen Contiguity"
  ) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

print(colombia_spatial_plot)

# ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/DiD/Colombia_Spatial_Treatment_Map.png", width = 8, height = 10, dpi = 300)


## Different treated and neighbors 
muni_map_data <- muni_geo %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  mutate(Status_Detail = case_when(
    MPIO_CDPMP %in% clean_mpio(zvtn_codes) ~ "FARC ZVTN Host",
    MPIO_CDPMP %in% clean_mpio(bloque_mpio_list) & MPIO_CDPMP %in% clean_mpio(pdet_codes) ~ "AUC + PDET Overlap",
    MPIO_CDPMP %in% clean_mpio(bloque_mpio_list) ~ "AUC Process Only",
    MPIO_CDPMP %in% clean_mpio(pdet_codes) ~ "PDET Process Only",
    MPIO_CDPMP %in% neighbor_clean ~ "Neighbor (Buffer)",
    TRUE ~ "Pure Control"
  ))

# 2. Define a color palette that distinguishes cohorts
# Highlighting PDET and ZVTN in distinct shades helps track the 'J-curve' regions
treatment_colors <- c(
  "FARC ZVTN Host"     = "#e41a1c", # Bright Red
  "AUC + PDET Overlap" = "#984ea3", # Purple
  "AUC Process Only"   = "#ff7f00", # Orange
  "PDET Process Only"  = "#377eb8", # Blue
  "Neighbor (Buffer)"  = "#a6cee3", # Light Blue
  "Control"            = "#f0f0f0"  # Grey
)

ggplot(data = muni_map_data) +
  geom_sf(aes(fill = Status_Detail), color = "white", size = 0.05) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  labs(
    title = "Peace Process Typology in Colombia",
    subtitle = "Distinguishing between 2005 (AUC) and 2016 (PDET/ZVTN) Jurisdictions",
    fill = "Treatment Category"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/DiD/Multiple_Treatments_Map.png", width = 8, height = 10, dpi = 300)

# 3. Merge into a master panel -------------------------------------------------
snvdem_paz <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(ARN_agg, by = c("MPIO_CDPMP", "year")) %>%
  left_join(Bloques_clean, by = c("MPIO_CDPMP", "year")) %>%
  left_join(ZVTN_clean, by = "MPIO_CDPMP") %>%
  left_join(PDET_clean, by = "MPIO_CDPMP") %>%
  mutate(across(c(total_demob_arn, AUC_site_demob, ZVTN_site, is_pdet), ~coalesce(., 0)))

snvdem_paz <- snvdem_paz %>%
  mutate(
    is_neighbor = ifelse(MPIO_CDPMP %in% pure_neighbor_codes, 1, 0)
  )

# 4. Create time-variant treatment variables -----------------------------------
# Assumption: rough treatment years
snvdem_paz <- snvdem_paz %>%
  mutate(
    # 2005 Justicia y Paz law passed, but most demobilizations occurred between 2003-2006
    treat_AUC  = ifelse(MPIO_CDPMP %in% unique(Bloques_clean$MPIO_CDPMP) & year >= 2005, 1, 0),
    # 2016 accord implementation (demobilization and PDET selection) began in 2017
    treat_ZVTN = ifelse(ZVTN_site == 1 & year >= 2017, 1, 0),
    treat_PDET = ifelse(is_pdet == 1 & year >= 2017, 1, 0)
  )

# 5. Define Treatment Groups with Spatial Buffers -------------------------------
snvdem_paz <- snvdem_paz %>%
  mutate(Group = case_when(
    MPIO_CDPMP %in% unique(Bloques_clean$MPIO_CDPMP) & is_pdet == 1 ~ "AUC + PDET Overlap",
    MPIO_CDPMP %in% unique(Bloques_clean$MPIO_CDPMP) ~ "AUC Process Only",
    is_pdet == 1 ~ "PDET Process Only",
    TRUE ~ "Control"
  ))

# Simple Visual Check
snvdem_paz %>%
  group_by(year, Group) %>%
  summarise(avg_democracy = mean(sndem_mean, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_democracy, color = Group)) +
  geom_line() +
  geom_vline(xintercept = c(2005, 2017), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Democracy Trends by Peace Process Group")

snvdem_paz <- snvdem_paz %>%
  mutate(Group_Spatial = case_when(
    MPIO_CDPMP %in% clean_mpio(treated_mpio_all) ~ "Treated (Core)",
    is_neighbor == 1 ~ "Neighbor (Buffer)",
    TRUE ~ "Pure Control"
  ))



# 6. STAGGERED DIFF-IN-DIFF (UPDATED FOR 2023 & SPATIAL) -----------------------
# To use spatial logic, we drop 'Neighbors' from the control pool to ensure 
# the 'Never Treated' group is truly unaffected by spillovers.

snvdem_paz_filtered <- snvdem_paz %>% 
  filter(Group_Spatial != "Neighbor (Buffer)") # Removes 'contaminated' controls

# 1. Create the treatment year variable (Group)
snvdem_paz_filtered <- snvdem_paz_filtered %>%
  mutate(
    MPIO_ID_numeric = as.numeric(as.factor(MPIO_CDPMP)),
    first_treat = case_when(
      MPIO_CDPMP %in% clean_mpio(bloque_mpio_list) ~ 2005,
      MPIO_CDPMP %in% clean_mpio(pdet_codes) ~ 2017,
      TRUE ~ 0
    )
  )

# 2. Run the Model (through 2023)
atts_2023 <- att_gt(yname = "sndem_mean",
                    tname = "year",
                    idname = "MPIO_ID_numeric",
                    gname = "first_treat",
                    data = snvdem_paz_filtered,
                    allow_unbalanced_panel = TRUE, 
                    control_group = "nevertreated",
                    est_method = "dr")

# 7. DYNAMIC REBOUND ANALYSIS (THROUGH 2023) -----------------------------------
# With data to 2023, the 2017 cohort now has 6 post-treatment years (e=6)
dynamic_2023 <- aggte(atts_2023, type = "dynamic", na.rm = TRUE, max_e = 6)

plot(ggdid(dynamic_2023) + labs(title = "Democracy J-Curve"))
