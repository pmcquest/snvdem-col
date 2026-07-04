# ==============================================================================#
# INTEGRATED PIPELINE: COLOMBIAN MUNICIPAL DEMOCRACY & PEACE PROCESSES          #
# ==============================================================================#
library(tidyverse)
library(readxl)
library(haven)
library(stringr)

# Note: this script was heavily influenced by Gemini. See here for description of the process: https://docs.google.com/document/d/1dBvEybgIpcs8LTulChVoNpqRccUFXcI1Mm1A6F6Faww/edit?pli=1&tab=t.0


# 1. Load data -----------------------------------------------------------------

## SNVDEM Panel (2000-2020)----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

## AUC Demobilization Sites (2003-2006)----
## Locations where AUC demobilized starting in 2003 as treated municipalities. [Could be a one-time treatment (e.g., 2005 with the law passage) or a staggered treatment (2003-2006).] AUC leadership offered a schedule for collective demobilizations of regional blocs beginning in 2003 with two pilot processes: Bloque Cacique Nutibara (in Medellin) and Autodefensas Campesinas de Ortega (en Cajibio, Cauca). More detail on the demobilizations here: https://cja.org/cja/downloads/Proceso%20de%20Paz%20con%20las%20Autodefensas.pdf
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


# 2. Clean treatment datasets --------------------------------------------------
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
# Optimized Cleaning Function
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

# Update ARN aggregation to include years through 2023
ARN_agg <- ARN %>%
  mutate(MPIO_CDPMP = clean_mpio(`Municipio de residencia`),
         year = as.numeric(Desmovilizacion)) %>%
  filter(year >= 2000 & year <= 2023) %>% # Updated to 2023
  group_by(MPIO_CDPMP, year) %>%
  summarise(total_demob_arn = n(), .groups = "drop")

# Ensure the population data covers the full timeline
# If df05_clean only goes to 2020, you may need a newer projection file

# 3. Merge into a master panel -------------------------------------------------
snvdem_paz <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(ARN_agg, by = c("MPIO_CDPMP", "year")) %>%
  left_join(Bloques_clean, by = c("MPIO_CDPMP", "year")) %>%
  left_join(ZVTN_clean, by = "MPIO_CDPMP") %>%
  left_join(PDET_clean, by = "MPIO_CDPMP") %>%
  mutate(across(c(total_demob_arn, AUC_site_demob, ZVTN_site, is_pdet), ~coalesce(., 0)))

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

# 5. 2005 and 2017 treatment overlaps ------------------------------------------
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

# 6. STAGGERED DIFF-IN-DIFF ----------------------------------------------------
library(did)

bloque_mpio_list <- unique(Bloques$MPIO_CDPMP)
pdet_codes       <- unique(PDET$MPIO_CDPMP[PDET$PDET == 1])
zvtn_codes       <- unique(ZVTN$MPIO_CDPMP)

bloque_mpio_list <- as.character(bloque_mpio_list)
pdet_codes       <- as.character(pdet_codes)
zvtn_codes       <- as.character(zvtn_codes)


# 1. Create the treatment year variable (Group)
snvdem_paz <- snvdem_paz %>%
  mutate(
    MPIO_ID_numeric = as.numeric(as.factor(MPIO_CDPMP)),
    # Identify the FIRST year of treatment
    first_treat = case_when(
      MPIO_CDPMP %in% bloque_mpio_list ~ 2005, #Group 2005: 34 "JyP" municipalities.
      MPIO_CDPMP %in% pdet_codes       ~ 2017, #Group 2017: 170 "PDET" municipalities.
      MPIO_CDPMP %in% zvtn_codes       ~ 2017, #Group 2017: 26 "ZVTN" municipalities.
      TRUE ~ 0
    )
  )

# 2. Run the Model
atts <- att_gt(yname = "sndem_mean",
               tname = "year",
               idname = "MPIO_ID_numeric",
               gname = "first_treat",
               data = snvdem_paz,
               allow_unbalanced_panel = TRUE, 
               control_group = "nevertreated",
               est_method = "dr") # Doubly Robust

# 3. View Results
summary(aggte(atts, type = "group"))

# 4. Add covariates: Regression-based estimation (more stable)
pob <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df05_clean.rds")
pob <- pob %>% select(1:3)
snvdem_paz <- snvdem_paz %>%
  left_join(pob, by = c("MPIO_CDPMP", "year"))

atts_controls <- att_gt(yname = "sndem_mean",
                        tname = "year",
                        idname = "MPIO_ID_numeric",
                        gname = "first_treat",
                        xformla = ~ PobTot_12, # Substitute for a more stable variable
                        data = snvdem_paz,
                        allow_unbalanced_panel = TRUE,
                        control_group = "nevertreated",
                        est_method = "reg")

# 5. View Results
summary(aggte(atts_controls, type = "group"))


# 6. Visualization
# Extract Group Effects for plotting
res_df <- data.frame(
  Cohort = c("AUC (2005)", "PDET (2017)", "ZVTN (2017)"),
  Estimate = c(0.0045, -0.0053, 0.0096),
  StdError = c(0.0021, 0.0011, 0.0004)
)

ggplot(res_df, aes(x = Cohort, y = Estimate, fill = Cohort)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Estimate - 1.96*StdError, ymax = Estimate + 1.96*StdError), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Comparative Impact on Subnational Democracy",
       subtitle = "ATT by Treatment Cohort",
       y = "Effect Size (sndem_mean)") +
  theme_minimal()


## Dynamic event study----

# 1. Aggregate the results to show dynamic (year-by-year) effects
dynamic_results <- aggte(atts, 
                         type = "dynamic", 
                         na.rm = TRUE, 
                         min_e = -10, # Look back up to 10 years before treatment
                         max_e = 4)   # Look forward up to 5 years after treatment

# 2. Summary of the event study coefficients
summary(dynamic_results)

# 3. Plot the Event Study
# This will show 'Leads' (pre-trends) and 'Lags' (post-treatment effects)
ggdid(dynamic_results) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Event Study: Impact of Peace Processes on Subnational Democracy",
       caption = "Vertical 0 represents the year of treatment. Points show ATT(e).",
       x = "Years Relative to Treatment",
       y = "ATT (Estimate)")



# Separate event study plots

# 1. Dynamic Effects for the 2005 AUC Cohort
# We limit this to municipalities treated in 2005
dynamic_2005 <- aggte(atts, 
                      type = "dynamic", 
                      balance_e = 15, # AUC has a long history
                      min_e = -5, 
                      max_e = 15)

# 2. Dynamic Effects for the 2017 PDET Cohort
# We limit the balance to 4 years because your data ends in 2023
dynamic_2017 <- aggte(atts, 
                      type = "dynamic", 
                      balance_e = 7, 
                      min_e = -5, 
                      max_e = 4)

# 3. Plotting them side-by-side
library(gridExtra)

plot_2005 <- ggdid(dynamic_2005) + 
  labs(title = "AUC Process (2005 Cohort)", y = "ATT") +
  theme_minimal()

plot_2016 <- ggdid(dynamic_2016) + 
  labs(title = "PDET/FARC Process (2016 Cohort)", y = "ATT") +
  theme_minimal()

grid.arrange(plot_2005, plot_2016, ncol = 2)

# Extract group effects
group_effects <- aggte(atts, type = "group")

# Create a summary table
summary_table <- data.frame(
  Cohort = group_effects$egt,
  ATT = group_effects$att,
  SE = group_effects$se,
  P_Value = 2 * (1 - pnorm(abs(group_effects$att / group_effects$se)))
)

print(summary_table)

# Final model specification with controls

# 1. Prepare stable control variables
snvdem_paz <- snvdem_paz %>%
  mutate(
    log_pop = log(PobTot_12 + 1),
    log_demob = log(total_demob_arn + 1)
  )

# Final specification: Dropping the sparse covariate to allow the model to run
atts_final <- att_gt(yname = "sndem_mean",
                     tname = "year",
                     idname = "MPIO_ID_numeric",
                     gname = "first_treat",
                     xformla = ~ log_pop, # Keep population, drop demob counts
                     data = snvdem_paz,
                     allow_unbalanced_panel = TRUE,
                     control_group = "nevertreated",
                     est_method = "reg")

# Extract the proof of the J-Curve
dynamic_summary <- aggte(atts_final, type = "dynamic", na.rm = TRUE)


