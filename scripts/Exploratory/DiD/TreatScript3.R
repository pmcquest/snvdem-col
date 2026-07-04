# ==============================================================================#
# INTEGRATED PIPELINE: COLOMBIAN MUNICIPAL DEMOCRACY & PEACE PROCESSES (2023)   #
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

# Generate neighbor lists early
jyp_list      <- clean_mpio(Bloques$MPIO_CDPMP)
zvtn_list     <- clean_mpio(ZVTN$MPIO_CDPMP)
pdet_list     <- clean_mpio(PDET$MPIO_CDPMP[PDET$PDET == 1])

jyp_neighbors <- get_neighbors(jyp_list, muni_geo)
zvtn_neighbors <- get_neighbors(zvtn_list, muni_geo)
pdet_neighbors <- get_neighbors(pdet_list, muni_geo)

## Covariate data ----
# for regression-based estimations (e.g., PanelMatch), it is useful to have time-variant and time-invariant data. I will select variables that are not included in the dependent variable (snvdem index) or independent variables (demobilizations or territorial programs).

# See 09_analysis_scripts/Outcomes/outcome_wrangle.R for more information
panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")
panel_imputed <- panel_imputed %>% select(1:9|14|19) # selecting covariates

# 0a. Treatments----
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

## Visualize ----
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


# 1. Triple-treated municipalities ----
# Question: Does the "Triple Overlap" group have a statistically significant 'Dip' compared to the 'PDET Only' group? Spoiler: no.

# Extract 'Dip' directly from the full source to avoid the '24 years' filter
dip_data <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(year %in% c(2016, 2019)) %>%
  select(MPIO_CDPMP, year, sndem_mean) %>%
  pivot_wider(names_from = year, values_from = sndem_mean, names_prefix = "yr_") %>%
  mutate(dip_magnitude = yr_2019 - yr_2016)

muni_labels <- data.frame(MPIO_CDPMP = unique(clean_mpio(snvdem$MPIO_CDPMP))) %>%
  mutate(
    is_jyp = MPIO_CDPMP %in% jyp_list,
    is_zvtn = MPIO_CDPMP %in% zvtn_list,
    is_pdet = MPIO_CDPMP %in% pdet_list,
    treatment_profile = case_when(
      is_jyp & is_zvtn & is_pdet ~ "Triple Overlap (JyP + ZVTN + PDET)",
      is_zvtn & is_pdet           ~ "ZVTN + PDET Overlap",
      is_jyp & is_zvtn            ~ "JyP + ZVTN Overlap",
      is_jyp & is_pdet            ~ "JyP + PDET Overlap",
      is_jyp                      ~ "JyP Only",
      is_zvtn                     ~ "ZVTN Only",
      is_pdet                     ~ "PDET Only",
      TRUE                        ~ "Control (None)"
    )
  )

dip_data_final <- dip_data %>%
  left_join(muni_labels, by = "MPIO_CDPMP") %>%
  # Bring in covariates from the panel_imputed (just the average to avoid NA issues)
  left_join(
    panel_imputed %>% 
      mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
      group_by(MPIO_CDPMP) %>%
      summarise(log_pop = mean(log(PobTot_12 + 1), na.rm=TRUE),
                altura = mean(altura, na.rm=TRUE)),
    by = "MPIO_CDPMP"
  )

table(dip_data_final$treatment_profile)

# Targeted T-Test: Triple Overlap vs. PDET Only
# This tests if 'Hard Places' fared worse than municipalities that 
# received development (PDET) but didn't have the double-demobilization history.
triple_vs_pdet <- dip_data_final %>%
  filter(treatment_profile %in% c("Triple Overlap (JyP + ZVTN + PDET)", "PDET Only")) %>%
  mutate(treatment_profile = as.factor(treatment_profile)) %>% 
  mutate(treatment_profile = droplevels(treatment_profile))

# Run the T-Test
t_test_results <- t.test(dip_magnitude ~ treatment_profile, data = triple_vs_pdet)
print(t_test_results)

# "While the Triple Overlap municipalities (the 'Hard Places') showed a lower mean improvement in democratic quality compared to standard PDET zones (0.027 vs 0.029), the difference was not statistically significant in a Welch T-test ($p=0.72$). This is likely due to the small sample size ($N=7$) and the high internal variance within these historically complex territories."

# 2. Demobilization effects ----
#  I would like to use PanelMatch first to compare each demobilization treatment effect, and then possibly a staggered DiD to examine municipalities that received both demobilization treatments (staggered). That would help me see the unique treatment effect of each demobilization process, and the staggered treatment effect on specific municipalities.
# However, there are too few observations for either JyP (n = 35) or ZVTN (n = 25). So I will take the neighbors of these municipalities to see if a spillover effect can be detected.

run_pm_analysis <- function(data, treat_var, outcome_var = "sndem_mean") {
  df_clean <- as.data.frame(data)
  df_clean$MPIO_ID_numeric <- as.numeric(as.factor(df_clean$MPIO_CDPMP))
  df_clean$year <- as.numeric(df_clean$year)
  df_clean <- df_clean[!is.na(df_clean$MPIO_ID_numeric) & !is.na(df_clean$year), ]
  p_obj <- PanelData(df_clean, 
                     unit.id = "MPIO_ID_numeric", 
                     time.id = "year", 
                     treatment = treat_var, 
                     outcome = outcome_var)
  m_obj <- PanelMatch(panel.data = p_obj, 
                      lag = 4, 
                      refinement.method = "mahalanobis", 
                      qoi = "att", 
                      size.match = 5, 
                      lead = 0:5, 
                      forbid.treatment.reversal = FALSE,
                      covs.formula = ~ log_pop + altura + MortInfantil1 + Enroll5_16)
  PanelEstimate(sets = m_obj, panel.data = p_obj)
}

# Using your universal plot function, now with broad indicators
PE_JyP_Broad  <- run_pm_analysis(master_panel, "jyp_broad_event")
PE_ZVTN_Broad <- run_pm_analysis(master_panel, "zvtn_broad_event")


## Plotting ----
print(PE_JyP_Broad)
print(PE_ZVTN_Broad)

get_data_universal <- function(pe_obj, label) {
  s <- summary(pe_obj, verbose = FALSE)
  res_matrix <- if (is.list(s)) s$summary else s
  res_df <- as.data.frame(res_matrix)
  data.frame(
    time = 0:(nrow(res_df) - 1),
    estimate = as.numeric(res_df[, 1]),
    se = as.numeric(res_df[, 2]),
    process = label,
    stringsAsFactors = FALSE
  )
}

# Map both to see comparative trajectories
plot_df <- list(PE_JyP_Broad, PE_ZVTN_Broad) %>%
  map2_dfr(c("AUC (2005) + Spillovers", "FARC (2017) + Spillovers"), get_data_universal)
ggplot(plot_df, aes(x = time, y = estimate, color = process, fill = process)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), 
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00", "#009E73")) +
  scale_fill_manual(values = c("#D55E00", "#009E73")) +
  labs(
    title = "Regional Democratic Shock: AUC vs. FARC",
    subtitle = "Post-Demobilization Impact including Neighbor Spillovers",
    x = "Years Since Treatment Pulse (t=0)",
    y = "Estimated ATT (sndem_mean)",
    caption = "Confidence intervals (95%) based on bootstrap standard errors."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Check the matched sets for the AUC group
print(PE_JyP_Broad$matched.sets[1:5]) 

# See the distribution of how many controls were matched per unit
summary(PE_JyP_Broad$matched.sets)

## 2a. Demobilization effect by dimensions ----
# Run PanelMatch for Electoral Quality (EMEL)
PE_JyP_emel  <- run_pm_analysis(master_panel, "jyp_broad_event", outcome_var = "emel_score")
PE_ZVTN_emel <- run_pm_analysis(master_panel, "zvtn_broad_event", outcome_var = "emel_score")

# Run PanelMatch for Civil Liberties/Security (CSCW)
PE_JyP_cscw  <- run_pm_analysis(master_panel, "jyp_broad_event", outcome_var = "cscw_score")
PE_ZVTN_cscw <- run_pm_analysis(master_panel, "zvtn_broad_event", outcome_var = "cscw_score")

# Combine EMEL Data
plot_df_emel <- list(PE_JyP_emel, PE_ZVTN_emel) %>%
  map2_dfr(c("AUC (Electoral)", "FARC (Electoral)"), get_data_universal)

# Combine CSCW Data
plot_df_cscw <- list(PE_JyP_cscw, PE_ZVTN_cscw) %>%
  map2_dfr(c("AUC (Civil Libs)", "FARC (Civil Libs)"), get_data_universal)

# Dual-Panel Plot
library(patchwork) # For side-by-side plotting

p_emel <- ggplot(plot_df_emel, aes(x = time, y = estimate, color = process, fill = process)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.1) +
  geom_line(size = 1) + labs(title = "Electoral Quality (EMEL)", y = "ATT") +
  theme_minimal() + theme(legend.position = "none")

p_cscw <- ggplot(plot_df_cscw, aes(x = time, y = estimate, color = process, fill = process)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.1) +
  geom_line(size = 1) + labs(title = "Civil Liberties (CSCW)", y = "ATT") +
  theme_minimal()

p_emel / p_cscw # Stacked visualization

# 3. PDET: Territorial focus ----
# A more "general" DiD analysis should be used to capture the broader "Territorial Focus" (2017 PDET) treatment effect on democracy. I think PanelMatch would be suitable to compare the JyP+neighbor treatment and matching control groups, and the 170 PDET treatment and (170 matching) control groups. Some municipalities, especially neighbors from JyP, may overlap with PDET; if so, I want to also have code for the staggered DiD analysis to see if intentional or unintentional treatment matters.



## 2A. Spatial Neighbor Identification for JyP ----
# incorporate the "Queen" spatial neighbors to compare the unintentional spillover (JyP neighbors) with the intentional treatment (PDET)

# Ensure spatial neighbors and PDETs are mutually exclusive for cohort analysis
master_panel_did <- master_panel_did %>%
  group_by(MPIO_ID_numeric) %>%
  mutate(
    is_jyp_neighbor = ifelse(MPIO_CDPMP %in% jyp_neighbor_codes, 1, 0),
    # Define Cohorts: 0 for never treated, or the year treatment began
    first_treat = case_when(
      is_jyp_muni == 1 | is_jyp_neighbor == 1 ~ 2005,
      is_pdet_muni == 1 ~ 2017,
      TRUE ~ 0
    ),
    # Define Group Labels for comparison later
    treatment_type = case_when(
      is_pdet_muni == 1 & (is_jyp_muni == 1 | is_jyp_neighbor == 1) ~ "Overlapping (Both)",
      is_pdet_muni == 1 ~ "Intentional (PDET Only)",
      is_jyp_muni == 1 | is_jyp_neighbor == 1 ~ "Unintentional (JyP/Neighbor)",
      TRUE ~ "Never Treated"
    )
  ) %>%
  ungroup()

# Check the distribution:
# 82 PDETs (170 minus 88) were "Already Treated" in 2005 (either as JyP sites or neighbors).
# 88 PDETs are "New Territorial Focus" areas. These municipalities were prioritized in the 2017 Peace Agreement but did not have a formal paramilitary demobilization (JyP) footprint or immediate proximity to one in 2005.
table(master_panel_did$first_treat[master_panel_did$year == 2023])

## 2B. Staggered DiD ----
# Did the 88 "New" PDETs (2017) perform better than the 82 PDETs that were already "Hard Places" (2005)?
## Spoiler: "The staggered Difference-in-Differences analysis confirms a significant 'Democratic Dip' following the 2017 peace agreement. Municipalities newly designated as PDET zones saw a highly significant decline in democracy index scores (ATT = -0.0046, p < 0.01), a negative effect nearly double the size of the long-term penalty observed in municipalities from the 2005 paramilitary demobilization cohort (ATT = -0.0026, p = 0.037)."

out <- att_gt(
  yname = "sndem_mean",
  tname = "year",
  idname = "MPIO_ID_numeric",
  gname = "first_treat",
  xformla = ~ log_pop + altura,
  data = master_panel_did,
  control_group = "nevertreated",
  base_period = "varying",
  est_method = "reg",
  print_details = FALSE
)

# Extract Group-Specific Effects (Comparing 2005 vs 2017)
group_effects <- aggte(out, type = "group", na.rm = TRUE)

# Format the Coefficients for Interpretation
cohort_results <- data.frame(
  Cohort = group_effects$egt,
  ATT = group_effects$att.egt,
  Std_Error = group_effects$se.egt,
  t_stat = group_effects$att.egt / group_effects$se.egt
) %>%
  mutate(p_value = round(2 * (1 - pnorm(abs(t_stat))), 4))

print(cohort_results)


### Visualization ----
# Overall effect
dynamic_effects <- aggte(out, type = "dynamic", na.rm = TRUE)

ggdid(dynamic_effects) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Event Study: The Evolution of the Democratic Dip",
       subtitle = "Negative ATT values after Time 0 confirm the post-peace decline",
       x = "Years Since Treatment Start",
       y = "Average Treatment Effect (ATT)") +
  theme_minimal()

# Group-specific effect
plot_cohorts <- ggdid(out, facet = TRUE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  labs(
    title = "Event Study: 2005 Cohort vs. 2017 Cohort",
    subtitle = "Comparing the 'Democratic Dip' across Peace Eras",
    x = "Years Relative to Treatment Start",
    y = "ATT (Effect on Democracy Index)"
  )

print(plot_cohorts)

## 2C. Disaggregating snvdem ----
# what dimension of democracy drives the "Democratic Dip": institutional resistance (electoral) or security vacuum (civil liberties)?

# --- A. Electoral Quality Model ---
out_emel <- att_gt(
  yname = "emel_score", # The competitiveness/integration dimension
  tname = "year",
  idname = "MPIO_ID_numeric",
  gname = "first_treat",
  xformla = ~ log_pop + altura,
  data = master_panel_did,
  control_group = "nevertreated",
  base_period = "varying",
  est_method = "reg"
)

# --- B. Civil Liberties / Security Model ---
out_cscw <- att_gt(
  yname = "cscw_score", # The violence/civil liberties index
  tname = "year",
  idname = "MPIO_ID_numeric",
  gname = "first_treat",
  xformla = ~ log_pop + altura,
  data = master_panel_did,
  control_group = "nevertreated",
  base_period = "varying",
  est_method = "reg"
)

# Hypothesis 1: 2017 PDETs show a sharper initial dip as elites resist new representation.
# Aggregate Electoral results by group
group_emel <- aggte(out_emel, type = "group")
print("--- Electoral Results (EMEL) ---")
summary(group_emel)


# Hypothesis 2: ZVTN/Hard Place municipalities show lower scores due to persistent security spillovers.
# Aggregate Civil Liberties results by group
group_cscw <- aggte(out_cscw, type = "group")
print("--- Civil Liberties/Security Results (CSCW) ---")
summary(group_cscw)

### Visualizations ----

# A. Comparison chart
# Aggregate Dynamic Effects for both models
dyn_emel <- aggte(out_emel, type = "dynamic", na.rm = TRUE)
dyn_cscw <- aggte(out_cscw, type = "dynamic", na.rm = TRUE)

# Plot Electoral Quality
p1 <- ggdid(dyn_emel) + labs(title = "Electoral Quality (EMEL) Trajectory")
# Plot Civil Liberties
p2 <- ggdid(dyn_cscw) + labs(title = "Civil Liberties (CSCW) Trajectory")

library(gridExtra)
grid.arrange(p1, p2, ncol = 1)


# B. Combined chart
# Extract and Combine Data for Plotting
plot_data_emel <- data.frame(
  event_time = dyn_emel$egt,
  att = dyn_emel$att.egt,
  se = dyn_emel$se.egt,
  Dimension = "Electoral Quality (EMEL)"
)

plot_data_cscw <- data.frame(
  event_time = dyn_cscw$egt,
  att = dyn_cscw$att.egt,
  se = dyn_cscw$se.egt,
  Dimension = "Civil Liberties (CSCW)"
)

combined_plot_data <- rbind(plot_data_emel, plot_data_cscw) %>%
  # Filter for the relevant 'Dip' window (5 years before/after)
  filter(event_time >= -5 & event_time <= 5)

# Create the Decoupling Plot
ggplot(combined_plot_data, aes(x = event_time, y = att, color = Dimension, fill = Dimension)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey70") +
  geom_ribbon(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) + # Orange for EMEL, Green for CSCW
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  labs(
    title = "The Decoupling of Local Democracy",
    subtitle = "Comparing Institutional Quality vs. Civil Liberties (Post-Treatment)",
    x = "Years Since Peace Process Start (t=0)",
    y = "Average Treatment Effect (ATT)",
    caption = "Note: Ribbons represent 95% Confidence Intervals. \nElectoral Quality shows the 'Democratic Dip', while Civil Liberties remain stable."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "italic")
  )

# "The disaggregated analysis reveals that the post-2017 'Democratic Dip' is fundamentally an institutional crisis. While civil liberties (CSCW) in the 88 new PDET municipalities actually showed a slight relative improvement following the peace agreement, electoral quality (EMEL) plummeted by an estimate of -0.0135. This suggests that the 'Hard Place' challenge is not a failure of security, but a failure of the local political system to integrate new territorial representation without a significant loss in competitiveness and quality."
