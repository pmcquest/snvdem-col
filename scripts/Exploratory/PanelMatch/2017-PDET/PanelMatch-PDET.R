# COLOMBIAN MUNICIPAL DEMOCRACY & THE 2016 PEACE PROCESS (2000-2023) ----
# PanelMatch method to measuring the causal impact of PDET on subnational democracy

# Core Libraries
library(tidyverse)
library(readxl)
library(haven)
library(PanelMatch)
library(fect)
library(panelView)
library(gridExtra)

# Manual: https://cran.r-project.org/web/packages/PanelMatch/vignettes/panelmatch-overview.pdf

# --- 0. Load & Clean Data ----
snvdem        <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")
# Function for Min-Max Normalization
normalize_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply to your main dataframe
snvdem <- snvdem %>%
  mutate(
    snelect_norm = normalize_01(snelect),
    sncivlib_norm = normalize_01(sncivlib),
    sndem_norm   = normalize_01(sndem)
  )

panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")
pdet_raw      <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta")

# Unified DANE code cleaner (Ensures 5-digit strings)
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

pdet_raw <- pdet_raw %>% rename(MPIO_CDPMP = MunCode)

pdet_list <- pdet_raw %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(PDET == 1) %>%
  pull(MPIO_CDPMP)

# Unified Master Panel Construction
master_panel <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(mutate(panel_imputed, MPIO_CDPMP = clean_mpio(MPIO_CDPMP)), by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    MPIO_integer   = as.integer(as.factor(MPIO_CDPMP)),
    year_integer   = as.integer(as.factor(year)), # Ensures consecutive integers for PanelMatch
    log_pop        = log(PobTot_12 + 1),
    pdet_treatment = as.integer(MPIO_CDPMP %in% pdet_list & year >= 2017)
  ) %>%
  filter(!is.na(log_pop), !is.na(sndem_norm)) %>%
  as.data.frame()

# --- 1. Visualization (Raw FEct Trends) ----
run_fect <- function(y_var, main_title) {
  res <- fect(as.formula(paste(y_var, "~ pdet_treatment + log_pop + MortInfantil1 + Enroll5_16")), 
              data = master_panel, index = c("MPIO_CDPMP", "year"),
              method = "fe", force = "two-way", se = TRUE, nboots = 100)
  plot(res, main = main_title, cex.main = 0.8, stats = "F.p")
}

p1 <- run_fect("sndem_norm", "SN Democracy")
p2 <- run_fect("snelect_norm", "Electoral Fairness")
p3 <- run_fect("sncivlib_norm", "Civil Liberties")
grid.arrange(p1, p2, p3, ncol = 1, top = "Estimated ATT (FEct Raw Trends)")

# 011326: The updated index using MC's weighting has produced different results for FE. Here we see Ashenfelter's dips for all three outcomes at t-3. The f-test p-value is 0 too. This suggests we can reject the null hypothesis that the pre-trends are 0. Let's try PM.

# --- 2. PanelMatch Base: Propensity Score Weighting ----

# We can use this base to calculate Mahalanobis later. But we use it now because it is more robust to possible selection bias inherent in lower-than-average democratic levels in PDET zones (selected for being impoverished, violent, with coca presence, and institutional weakness). PS Weighting ensures parallel trends assumption by mirroring average pre-treatment trajectory of treated units. PS weighting creates a synthetic control where municipalities that look like PDET zones are more influential. For this reason, the placebo test is different than the Mahalanobis one because it uses all control municipalities (n=931) with unique weights rather than matching 170 treated with control municipalities. 


get_weighted_results <- function(outcome_name, data) {
  p.data <- PanelData(panel.data = data, 
                      unit.id = "MPIO_integer", time.id = "year_integer", 
                      treatment = "pdet_treatment", outcome = outcome_name)
  pm.weighted <- PanelMatch(lag = 4, panel.data = p.data, 
                            refinement.method = "ps.weight", 
                            covs.formula = as.formula(paste0("~ I(lag(", outcome_name, ", 1:4)) + I(lag(log_pop, 1:4))")),
                            qoi = "att", 
                            lead = 0:6, # looking 6 years ahead
                            match.missing = FALSE)
  
  m_estimate <- PanelEstimate(sets = pm.weighted, panel.data = p.data, number.iterations = 1000)
  return(list(pm_obj = pm.weighted, pdata = p.data, estimate = m_estimate))
}

# Run refined models for all three pillars
res_sn <- get_weighted_results("sndem_norm", master_panel)
res_em <- get_weighted_results("snelect_norm", master_panel)
res_cs <- get_weighted_results("sncivlib_norm", master_panel)

# --- 3. PM Results & Visualization ----
## PS Weights ----
# Extract trajectories and calculate significance
extract_full_path <- function(res_obj, outcome_label) {
  data.frame(
    Outcome   = outcome_label,
    Time      = names(res_obj$estimate$estimate),
    Estimate  = as.numeric(res_obj$estimate$estimate),
    Std_Error = as.numeric(res_obj$estimate$standard.error)
  )
}

results_df <- rbind(
  extract_full_path(res_sn, "SN Democracy"),
  extract_full_path(res_em, "Electoral Fairness"),
  extract_full_path(res_cs, "Civil Liberties")
) %>%
  mutate(
    t_stat = Estimate / Std_Error,
    p_val  = 2 * (1 - pnorm(abs(t_stat))),
    sig    = case_when(p_val < 0.01 ~ "***", p_val < 0.05 ~ "**", p_val < 0.1 ~ "*", TRUE ~ "")
  )

# Print Summary Table
print(results_df)

# Causal impact plot
ggplot(results_df, aes(x = Time, y = Estimate, group = Outcome, color = Outcome)) +
  geom_line(size = 1.2) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96*Std_Error, ymax = Estimate + 1.96*Std_Error), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Estimated impact of the PDET Program on subnational democracy",
       subtitle = "Weights: Propensity Score | Bars: 95% Confidence Intervals",
       y = "ATT (Point Estimate)",
       x = "Years Since Designation")

## Mahalanobis ----
# create PM object with Mahalanobis refinement for comparison
PM.maha <- PanelMatch(panel.data = res_sn$pdata,
                      lag = 4,
                      refinement.method = "mahalanobis",
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(log_pop, 1:4)) +
                        I(lag(sndem_norm, 1:4)),
                      size.match = 5,
                      qoi = "att",
                      lead = 0:6,
                      use.diagonal.variance.matrix = TRUE,
                      forbid.treatment.reversal = FALSE,
                      placebo.test = TRUE)
# PanelEstimate object
PE.maha.results <- PanelEstimate(sets = PM.maha,
                            panel.data = res_sn$pdata,
                            se.method = "bootstrap")
plot(PE.maha.results)

## Comparison ----

# Mahalanobis
maha_sum <- summary(PE.maha.results)
df_maha <- data.frame(
  Time = rownames(maha_sum),
  Estimate = as.numeric(maha_sum[, "estimate"]),
  Std_Error = as.numeric(maha_sum[, "std.error"]),
  Method = "Mahalanobis Matching"
)

# PS weighting
ps_sum <- summary(res_sn$estimate)

df_ps <- data.frame(
  Time = rownames(ps_sum),
  Estimate = as.numeric(ps_sum[, "estimate"]),
  Std_Error = as.numeric(ps_sum[, "std.error"]),
  Method = "Propensity Score Weighting"
)

# Combine
comparison_df <- rbind(df_maha, df_ps)
print(comparison_df)
comparison_df$p_val <- 2 * (1 - pnorm(abs(comparison_df$Estimate / comparison_df$Std_Error)))

# It appears that Maha standard error (0.008) at t+6 is about the same PS 
write.csv(comparison_df, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/Robustness_Matching_vs_Weighting.csv", row.names = FALSE)

# Plot the comparison
ggplot(comparison_df, aes(x = Time, y = Estimate, group = Method, color = Method)) +
  geom_line(size = 1.1, alpha = 0.8) + 
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std_Error, 
                    ymax = Estimate + 1.96 * Std_Error), 
                width = 0.15, alpha = 0.6, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Mahalanobis Matching" = "#d95f02", 
                                "Propensity Score Weighting" = "#1f78b4")) +
  labs(title = "Sensitivity Analysis: Matching vs. Weighting",
       subtitle = "Subnational Democracy (ATT estimates from t+0 to t+6)",
       y = "ATT (Point Estimate)",
       x = "Years Since Designation",
       color = "Refinement Method") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# 3.1 Covariate balance ----
# Calculate balance using the sub-objects in list and additional PM Mahalanobis
covbal <- get_covariate_balance(res_sn$pm_obj, PM.maha,
                                panel.data = res_sn$pdata, 
                                covariates = c("sndem_norm", "log_pop"), 
                                include.unrefined = TRUE)
summary(covbal)
# results look balanced around 0
plot(covbal, type = "panel",
     include.unrefined.panel = FALSE, ylim = c(-.5, .5))
# (not working...)
plot(get_unrefined_balance(covbal)[2],
     include.unrefined.panel = FALSE, ylim = c(-.5, .5))

# 4. Placebo tests ----

# PS Weighting: "Are the 931 weighted control and 170 PDET zones similar enough to compare?"
# Mahalanobis: "If we only compare the most similar municipalities, do they still show zero difference before 2017?"

## PS Weights placebo ----
# Note: do NOT include the outcome in the formula for a placebo test
pm_placebo <- PanelMatch(lag = 4, 
                         panel.data = p.data_pre, 
                         refinement.method = "none", 
                         qoi = "att", 
                         lead = 0:3, 
                         match.missing = FALSE,
                         placebo.test = TRUE)


pe_placebo <- PanelEstimate(sets = pm_placebo, panel.data = p.data_pre)
plot(pe_placebo) # Ideally, these estimates should all be near zero

# This looks at the 2 years BEFORE the designation (t-2 and t-1)
# Run the placebo test using the PanelData object [HIGH COMPUTATIONAL POWER--load instead]
# placebo_results <- placebo_test(pm_placebo, 
                              #  panel.data = p.data_pre, 
                              #  number.iterations = 1000)
placebo_results <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/ps_weight_placebo.rds")
# Check the pre-treatment coefficients (shows t-2 and before due to lags)
print(placebo_results)
summary(placebo_results)
#saveRDS(placebo_results, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/ps_weight_placebo.rds")

## Mahalanobis placebo ----
# Run the placebo test [HIGH COMPUTATIONAL POWER--load instead]
# PE.maha.placebo <- placebo_test(PM.maha, panel.data = res_sn$pdata, plot = F)
# saveRDS(PE.maha.placebo, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/maha_placebo.rds")
PE.maha.placebo <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/maha_placebo.rds")

# obtain lead and lag (placebo) estimates
est_lead <- as.vector(PE.maha.results$estimate)
est_lag <- as.vector(PE.maha.placebo$estimates)
sd_lead <- apply(PE.maha.results$bootstrapped.estimates,2,sd)
sd_lag <- apply(PE.maha.placebo$bootstrapped.estimates,2,sd)
coef <- c(est_lag, 0, est_lead)
sd <- c(sd_lag, 0, sd_lead)

# Define 11-year timeline because placebo years are different than Maha estimates
# t-4, t-3, t-2 (Placebo) | t-1 (Baseline) | t+0 to t+6 (Results)
full_timeline <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6)

# Rebuild the data frame with the correct length
pm.output <- data.frame(
  t = full_timeline,
  ATT = coef, 
  se = sd
)
# Add Confidence Intervals for the plot
pm.output$lower <- pm.output$ATT - (1.96 * pm.output$se)
pm.output$upper <- pm.output$ATT + (1.96 * pm.output$se)

print(pm.output)
p.pm <- esplot(data = pm.output, Period = 't',
               Estimate = 'ATT',SE = 'se')
p.pm
# ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/DynamicTreatment-mahaMC.pdf", width = 8, height = 5, device = "pdf")

# Results indicate that pre-treatment trends among PDET and non-PDET were not divergent (non-significant at t-2). Selection process is valid. 
# Dip at t+0 captures initial issues and disruption on the ground. But by t+3, trend is positive, and by t+6 confidence intervals are tighter and above the 0 line. The gain is around 1% on the 0-1 subnational democracy scale.
# Finding: PDET has had a statistically significant positive impact on subnational democracy but only after some time (5-6 years). This despite country's overall dip in democracy in 2023.

## Table ----
# Load the saved RDS files
ps_placebo <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/ps_weight_placebo.rds") #NA
maha_placebo <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/maha_placebo.rds")

# Extract the critical t-2 estimates for both
# Propensity Score (Unrefined)
ps_t2_est <- ps_placebo$estimates["t-2"]
ps_t2_se  <- ps_placebo$standard.errors["t-2"]
# Mahalanobis (Refined)
maha_t2_est <- maha_placebo$estimates["t-2"]
maha_t2_se  <- maha_placebo$standard.errors["t-2"]

# Create the Comparison Table
placebo_robustness <- data.frame(
  Method = c("Propensity Score (Unrefined)", "Mahalanobis Matching (Refined)"),
  Estimate_t2 = c(as.numeric(ps_t2_est), as.numeric(maha_t2_est)),
  Std_Error = c(as.numeric(ps_t2_se), as.numeric(maha_t2_se))
)

# Add p-values to prove non-significance (Balance)
placebo_robustness$p_val <- 2 * (1 - pnorm(abs(placebo_robustness$Estimate_t2 / placebo_robustness$Std_Error)))
# Round
placebo_robustness[, 2:4] <- round(placebo_robustness[, 2:4], 5)

print(placebo_robustness)

# High p-values indicate no statistically significant "pre-trend."
# Placebo $t-2$: $p = 0.508$ (No difference before treatment).
# Main Effect $t+6$: $p = 0.02$ (Clear difference after treatment).

# Export to CSV
write.csv(placebo_robustness, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/Placebo_Robustness_Comparison.csv", row.names = FALSE)


# Identifying the controls used for matching ----
# Extract the matched sets from the 'att' attribute
msets_att <- attr(PM.maha, "att")

print(msets_att[[1]])

# Extract the specific matched set for unit 60 (at time 15)
mset_60 <- PM.maha[["att"]][["60.15"]]

# Get the IDs of the units that actually have weights (the matches)
# We look for weights > 0
all_weights <- attr(mset_60, "weights")
matched_control_ids <- as.numeric(names(all_weights[all_weights > 0]))

lookup <- master_panel %>% 
  select(MPIO_integer, MPIO_CDPMP) %>% 
  distinct()

# Identify the names using your lookup table
treated_name <- lookup$MPIO_CDPMP[lookup$MPIO_integer == 60]
control_names <- lookup$MPIO_CDPMP[lookup$MPIO_integer %in% matched_control_ids]

cat("PDET Municipality:", treated_name, "\n")
cat("The 5 Mahalanobis Matches:", paste(control_names, collapse = ", "), "\n")


library(dplyr)
library(tidyr)

# Access 'att' as a list element, not an attribute
msets_att <- PM.maha$att 

# Now run the extraction loop
all_matched_ids <- unlist(lapply(msets_att, function(x) {
  # 'x' represents each double[931] set seen in your screenshot
  weights <- attr(x, "weights")
  # Extract IDs where weight is 0.2 (indicating a Mahalanobis match)
  as.numeric(names(weights[weights > 0]))
}))

# Check if it worked
length(all_matched_ids) # Should be 170 * 5 = 850

# Tally
control_tally <- as.data.frame(table(all_matched_ids))
colnames(control_tally) <- c("MPIO_integer", "Match_Frequency")

# 2. Convert IDs to numeric for joining
control_tally$MPIO_integer <- as.numeric(as.character(control_tally$MPIO_integer))

# 3. Extract the 170 Treated Unit IDs from the names of msets_att
treated_ids <- as.numeric(sapply(names(msets_att), function(x) strsplit(x, "\\.")[[1]][1]))

# 4. Create the Master List for Mapping
map_data <- lookup %>%
  mutate(Role = case_when(
    MPIO_integer %in% treated_ids ~ "PDET (Treated)",
    MPIO_integer %in% control_tally$MPIO_integer ~ "Matched Control",
    TRUE ~ "Other (Untreated)"
  )) %>%
  left_join(control_tally, by = "MPIO_integer")

# Preview the top "Super-Controls"
print(head(arrange(map_data, desc(Match_Frequency)), 10))


## Mapping ----
library(sf)
library(ggplot2)
library(RColorBrewer)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))


# 1. Join your roles and frequency data to the shapefile
muni_map <- muni_geo %>%
  left_join(map_data, by = "MPIO_CDPMP") %>%
  mutate(Role = ifelse(is.na(Role), "Other (Untreated)", Role))

# 2. Create the Regional Cluster Visualization
# We use a distinct color for the 16 PDET subregions to see the clusters
ggplot(data = muni_map) +
  # Draw all municipalities in light gray
  geom_sf(fill = "#f7f7f7", color = "white", size = 0.05) +
  # Highlight the Matched Controls in Blue
  geom_sf(data = filter(muni_map, Role == "Matched Control"), 
          aes(fill = Role), color = "white", size = 0.1) +
  # Highlight the PDETs in Red
  geom_sf(data = filter(muni_map, Role == "PDET (Treated)"), 
          aes(fill = Role), color = "white", size = 0.1) +
  scale_fill_manual(values = c("PDET (Treated)" = "#e41a1c", 
                               "Matched Control" = "#377eb8")) +
  theme_void() +
  labs(title = "Spatial Distribution of PDET and Matched Control Municipalities",
       subtitle = "Mahalanobis matching identifies local counterfactuals to fix pre-treatment bias.",
       fill = "Municipality Role")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/PanelMatch/2017-PDET/MC/map_maha-treat-control.png", device = "png", height=6.5, width=6.5, units = "in")


# Tallying matches by Department to see where the algorithm 'shopped' for controls
regional_balance <- muni_map %>%
  filter(Role != "Other (Untreated)") %>%
  group_by(DPTO_NAME, Role) %>%
  tally() %>%
  spread(Role, n, fill = 0)

print(regional_balance)

# Identifying outliers...
get_unit_effects_safe <- function(pm_obj, p_data, outcome_name, lead_time = 5) { # Reduced lead to 5 to ensure data exists
  m_sets <- pm_obj$att
  results_list <- list()
  
  df <- p_data$panel.data
  # Ensure these are numeric for comparison
  df[[p_data$unit.id]] <- as.numeric(df[[p_data$unit.id]])
  df[[p_data$time.id]] <- as.numeric(df[[p_data$time.id]])
  
  for(i in seq_along(m_sets)) {
    set_name <- names(m_sets)[i]
    treated_id <- as.numeric(strsplit(set_name, "\\.")[[1]][1])
    t_year <- as.numeric(strsplit(set_name, "\\.")[[1]][2])
    target_year <- t_year + lead_time
    
    # 1. Extract Treated value
    val_treated <- df[df[[p_data$unit.id]] == treated_id & 
                        df[[p_data$time.id]] == target_year, outcome_name]
    
    if(length(val_treated) == 0) next
    
    # 2. Extract Control values
    controls <- m_sets[[i]]
    control_ids <- as.numeric(names(controls))
    weights <- as.numeric(controls)
    
    control_outcomes <- sapply(seq_along(control_ids), function(j) {
      val_c <- df[df[[p_data$unit.id]] == control_ids[j] & 
                    df[[p_data$time.id]] == target_year, outcome_name]
      if(length(val_c) == 0) return(NA)
      return(as.numeric(val_c) * weights[j])
    })
    
    if(all(is.na(control_outcomes))) next
    
    results_list[[i]] <- data.frame(
      MPIO_integer = treated_id,
      treatment_year = t_year,
      effect = as.numeric(val_treated) - sum(control_outcomes, na.rm = TRUE)
    )
  }
  
  if(length(results_list) == 0) {
    stop("No matches found. Check if (Treatment Year + Lead Time) exists in your master_panel years.")
  }
  
  return(do.call(rbind, results_list))
}

# Run with a slightly shorter lead (5 years instead of 6) to ensure coverage
unit_level_impact <- get_unit_effects_safe(res_cs$pm_obj, res_cs$pdata, "sncivlib_norm", lead_time = 5)
