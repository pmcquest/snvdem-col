# ==============================================================================#
#     COLOMBIAN MUNICIPAL DEMOCRACY & THE 2016 PEACE PROCESS (2000-2023)        #
# ==============================================================================#

# Question: Did the 2016 peace process improve levels of subnational democracy? 
# If so, where?
## One approach to answering this question is to measure the impact of the PDET program--a territorial component of the agreement under Chapter 1 of the accord--on treated municipalities (n=170). This program has been criticized for lacking institutional support, but by providing financial resources to under-developed conflict-affected areas, it may have provided some relief between 2017-2023. 
## I think PanelMatch would be suitable to analyze the 170 PDET treatment and (170 matching) control groups. Some neighboring municipalities may be affected by PDET; to examine diffusion, I want to measure if unintentional treatment matters.
## I follow the vignette provided by the authors of PanelMatch: https://cran.r-project.org/web/packages/PanelMatch/vignettes/panelmatch-overview.pdf

library(tidyverse)
library(dplyr)
library(readxl)
library(haven)
library(stringr)
library(PanelMatch)
library(did)
library(zoo)
library(ggplot2)
library(viridis)
library(sf)
library(fect)
library(panelView)
library(bacondecomp)
library(gridExtra)
library(grid)

# 0. Load data -----------------------------------------------------------------
## SNVDEM Panel (2000-2023)----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

## PDET municipalities (2017-) ----
## ART identified 170 municipalities for the PDET program, which was formally introduced into law in 2017.
PDET <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta")
PDET <- PDET %>%
  rename(DPTO_CCDGO = 3, depto = 4, MPIO_CDPMP = 5, municipio = 6) %>%
  select(3:8)
PDET$MPIO_CDPMP <- as.character(PDET$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
PDET$MPIO_CDPMP <- ifelse(nchar(PDET$MPIO_CDPMP) == 4, paste0("0", PDET$MPIO_CDPMP), PDET$MPIO_CDPMP)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

## Geospatial data----
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
pdet_list     <- clean_mpio(PDET$MPIO_CDPMP[PDET$PDET == 1])
# pdet_neighbors <- get_neighbors(pdet_list, muni_geo)

## Covariate data ----
# for regression-based estimations (e.g., PanelMatch), it is useful to have time-variant and time-invariant data. I wrangled variables that are not included in the dependent variable (snvdem index) or independent variables (demobilizations or territorial programs).

# See 09_analysis_scripts/Outcomes/outcome_wrangle.R for more information
panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")
panel_imputed <- panel_imputed %>% select(1:9|14|19) # selecting covariates

# 1. Treatment assignment ----
master_panel <- snvdem %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  left_join(mutate(panel_imputed, MPIO_CDPMP = clean_mpio(MPIO_CDPMP)), 
            by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    MPIO_integer = as.numeric(as.factor(MPIO_CDPMP)),
    year_integer = as.numeric(year),
    log_pop = log(PobTot_12 + 1),
    pdet_treatment = as.integer(MPIO_CDPMP %in% pdet_list & year >= 2017)
  ) %>%
  as.data.frame()

# View treatment (not very informative)
panelview(sndem_mean ~ pdet_treatment, data = master_panel, index = c("MPIO_CDPMP","year"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)


# 2. Parallel trends ----
sn <- panelview(data = master_panel, Y='sndem_mean',
                D='pdet_treatment',index=c("MPIO_CDPMP","year"),
                by.timing = T, display.all = T,
                type = "outcome", by.cohort = T, 
                main = "Mean SN Dem score", xlab = "Year", 
                ylab = "SN Dem", xlim=c(2000,2023))
emel <- panelview(data = master_panel, Y='emel_score',
                 D='pdet_treatment',index=c("MPIO_CDPMP","year"),
                 by.timing = T, display.all = T,
                 type = "outcome", by.cohort = T, 
                 main = "Mean EMEL score", xlab = "Year", 
                 ylab = "EMEL score", xlim=c(2000,2023))
cscw <- panelview(data = master_panel, Y='cscw_score',
                 D='pdet_treatment',index=c("MPIO_CDPMP","year"),
                 by.timing = T, display.all = T,
                 type = "outcome", by.cohort = T, 
                 main = "Mean CSCW score", xlab = "Year", 
                 ylab = "CSCW score", xlim=c(2000,2023))

sndem_PT <- grid.arrange(sn, emel, cscw, ncol = 1)

# 3. FECT ----
## ATTs on Democracy 
sn.fect <- fect(sndem_mean ~ pdet_treatment + log_pop + MortInfantil1 + Enroll5_16, 
                data = master_panel, index=c("MPIO_CDPMP", "year"),
                method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200)
sn.f <- plot(sn.fect, main = "SN Dem Score (FEct)", 
             ylab = "PDET on SN Dem", 
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, cex.text = .6, stats = "F.p")
emel.fect <- fect(emel_score ~ pdet_treatment + log_pop + MortInfantil1 + Enroll5_16, 
                data = master_panel, index=c("MPIO_CDPMP", "year"),
                method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200)
em.f <- plot(emel.fect, main = "Electoral fairness (FEct)", 
             ylab = "PDET on EMEL", 
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, cex.text = .6, stats = "F.p")
cscw.fect <- fect(cscw_score ~ pdet_treatment + log_pop + MortInfantil1 + Enroll5_16, 
                data = master_panel, index=c("MPIO_CDPMP", "year"),
                method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200)
cs.f <- plot(cscw.fect, main = "Civil liberties strength (FEct)", 
             ylab = "PDET on CSCW",
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, cex.text = .6, stats = "F.p")

#for just FEct results on sndem
sndem_FEct <- grid.arrange(sn.f, em.f, cs.f, ncol = 1, 
                         top=textGrob("Estimated ATT on SN Dem (FEct)", gp=gpar(fontsize=16,font=3)))

# 4. PanelMatch estimation ----
df.pm <- master_panel
df.pm <- df.pm[,c("MPIO_integer","year_integer", "pdet_treatment", "sndem_mean", "emel_score", "cscw_score", "log_pop", "MortInfantil1","Enroll5_16", "altura")] 
length(unique(df.pm$MPIO_integer)) # includes 1125 observations
# Adjust "refinement.method" to make pre-treatment estimates closer to 0: "mahalanobis", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight" --> figure out which one provides a better pre-treatment trend

library(PanelMatch)
library(ggplot2)

# Ensure data is sorted
df.pm <- df.pm[order(df.pm$MPIO_integer, df.pm$year_integer), ]

# Helper function to create the three objects per outcome
get_pm_objects <- function(outcome_name, data) {
  # 1. Create PanelData Object
  p.data <- PanelData(panel.data = data,
                      unit.id = "MPIO_integer",
                      time.id = "year_integer",
                      treatment = "pdet_treatment",
                      outcome = outcome_name)
  
  # 2. Define formula dynamically to include the specific outcome lags
  formula_str <- paste0("~ I(lag(", outcome_name, ", 1:6)) + 
                          I(lag(log_pop, 1:6)) + 
                          I(lag(altura, 1:6)) + 
                          I(lag(MortInfantil1, 1:6)) + 
                          I(lag(Enroll5_16, 1:6))")
  
  # 3. Create Mahalanobis Match
  pm.maha <- PanelMatch(lag = 6, panel.data = p.data, match.missing = TRUE,
                        refinement.method = "mahalanobis", size.match = 10,
                        covs.formula = as.formula(formula_str),
                        qoi = "att", lead = 0:6, use.diagonal.variance.matrix = TRUE)
  
  # 4. Create Propensity Score Match (PS Match is more selective than PS Weight)
  pm.ps <- PanelMatch(lag = 6, panel.data = p.data, match.missing = TRUE,
                      refinement.method = "ps.match", size.match = 10,
                      covs.formula = as.formula(formula_str),
                      qoi = "att", lead = 0:6, use.diagonal.variance.matrix = TRUE)
  
  return(list(maha = pm.maha, ps = pm.ps, pdata = p.data))
}

# Generate objects for all three
results_sn <- get_pm_objects("sndem_mean", df.pm)
results_em <- get_pm_objects("emel_score", df.pm)
results_cs <- get_pm_objects("cscw_score", df.pm)


## Plot ----
plot_balance_clean <- function(pm_list, title) {
  # 1. Calculate balance for all variables included in formula
  # Ensure we include the specific outcome to verify parallel trends
  target_outcome <- attr(pm_list$pdata, "outcome")
  covs_to_plot <- c(target_outcome, "log_pop", "MortInfantil1", "Enroll5_16")
  
  cb <- get_covariate_balance(pm_list$maha, pm_list$ps,
                              panel.data = pm_list$pdata,
                              covariates = covs_to_plot,
                              include.unrefined = TRUE)
  
  # 2. Adjust margins: bottom margin (first number) must be large (e.g., 10)
  par(mar = c(10, 4, 4, 2) + 0.1)
  
  # 3. Plot with a valid legend.position but invisible text (cex.legend = 0)
  plot(cb, type = "panel", 
       include.unrefined.panel = FALSE,
       ylim = c(-0.3, 0.3), 
       main = title, 
       legend.position = "topleft", 
       cex.legend = 0) # This bypasses the error while hiding the box
  
  # 4. Add the manual legend in the margin space
  # We use 'ncol' to spread it out horizontally
  legend(x = "bottom", 
         inset = c(0, -0.8), # Pushes the legend into the margin area
         legend = covs_to_plot,
         col = c("black", "red", "darkblue", "cyan"), 
         lty = 1, 
         pch = 19, 
         ncol = 2, 
         cex = 0.7, 
         xpd = TRUE, # Vital: allows drawing outside plot axes
         bty = "n")
}

# Run diagnostics
plot_balance_clean(results_sn, "Balance: Subnational Democracy")
plot_balance_clean(results_em, "Balance: Electoral Fairness")
plot_balance_clean(results_cs, "Balance: Civil Liberties")

## SN Dem scores ----
sn.panel <- PanelData(panel.data = df.pm,
                       unit.id = "MPIO_CDPMP",
                       time.id = "year",
                       treatment = "pdet_treatment",
                       outcome = "sndem_mean")
time_map <- attr(sn.panel, "time.data.map") # look at new integers--fine.

# Mahalanobis 
sn.PM.maha <- PanelMatch(lag = 6, panel.data = sn.panel, match.missing = TRUE,
                    refinement.method = "mahalanobis",
                    size.match = 10,
                    covs.formula = ~ I(lag(sndem_mean, 1:6)) + 
                      I(lag(log_pop, 1:6)) + 
                      I(lag(altura, 1:6)) + 
                      I(lag(MortInfantil1, 1:6)) + 
                      I(lag(Enroll5_16, 1:6)),
                    qoi = "att", 
                    lead = 0:6,
                    use.diagonal.variance.matrix = TRUE)
plot(sn.PM.maha) # uninformative...
# Propensity score weighting
sn.PM.ps <- PanelMatch(lag = 6, panel.data = dem.panel, match.missing = TRUE,
                         refinement.method = "ps.match", 
                       size.match = 10,
                         covs.formula = ~ I(lag(sndem_mean, 1:6)) + 
                           I(lag(log_pop, 1:6)) + 
                           I(lag(altura, 1:6)) + 
                           I(lag(MortInfantil1, 1:6)) + 
                           I(lag(Enroll5_16, 1:6)),
                         qoi = "att", 
                         lead = 0:6,
                         use.diagonal.variance.matrix = TRUE)
plot(sn.PM.ps) # uninformative...

### Compare methods
covbal <- get_covariate_balance(sn.PM.maha, sn.PM.ps,
                                panel.data = dem.panel,
                                covariates = c("sndem_mean", "log_pop"),
                                include.unrefined = TRUE)
plot(covbal, type = "panel",
     include.unrefined.panel = FALSE, ylim = c(-.7, .7),
     legend.position = "bottomleft")

## Use "topleft" but make the text tiny so it doesn't overlap
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(4, 1))
par(mar = c(4, 4, 4, 1)) 

# Plot 1
plot(covbal, type = "panel", include.unrefined.panel = FALSE,
     ylim = c(-0.5, 0.5), main = "Mahalanobis (maha)", 
     legend.position = "topleft", cex.legend = 0.1) # Tiny legend

# Plot 2
plot(covbal, type = "panel", include.unrefined.panel = FALSE,
     ylim = c(-0.5, 0.5), main = "Propensity Score (ps)", 
     legend.position = "topleft", cex.legend = 0.1)

# Plot 3 (The real legend)
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", 
       legend = c("sndem_mean", "log_pop"),
       col = c("black", "red"), 
       lty = 1, pch = 19, ncol = 2, cex = 1, bty = "n")



### Estimates
sn.PM.PE <- PanelEstimate(sn.PM, panel.data = dem.panel, pooled = FALSE)
plot(sn.PM.PE, main = "SN Dem (PM)", ylab = "PDET on SN Dem")

## EMEL scores ----
emel.panel <- PanelData(panel.data = df.pm,
                       unit.id = "MPIO_CDPMP",
                       time.id = "year",
                       treatment = "pdet_treatment",
                       outcome = "emel_score")

emel.PM <- PanelMatch(lag = 6, panel.data = emel.panel, match.missing = TRUE,
                    refinement.method = "mahalanobis", 
                    covs.formula = ~ I(lag(emel_score, 1:6)) + 
                      I(lag(log_pop, 1:6)) + 
                      I(lag(altura, 1:6)) + 
                      I(lag(MortInfantil1, 1:6)) + 
                      I(lag(Enroll5_16, 1:6)),
                    qoi = "att", 
                    lead = 0:6,
                    use.diagonal.variance.matrix = TRUE)
em.PM.PE <- PanelEstimate(emel.PM, panel.data = emel.panel, pooled = FALSE)
plot(em.PM.PE, main = "Electoral fairness (PM)", ylab = "PDET on EMEL")

## CSCW scores ----
cscw.panel <- PanelData(panel.data = df.pm,
                       unit.id = "MPIO_CDPMP",
                       time.id = "year",
                       treatment = "pdet_treatment",
                       outcome = "cscw_score")

cscw.PM <- PanelMatch(lag = 6, panel.data = cscw.panel, match.missing = TRUE,
                    refinement.method = "mahalanobis", 
                    covs.formula = ~ I(lag(cscw_score, 1:6)) + 
                      I(lag(log_pop, 1:6)) + 
                      I(lag(altura, 1:6)) + 
                      I(lag(MortInfantil1, 1:6)) + 
                      I(lag(Enroll5_16, 1:6)),
                    qoi = "att", 
                    lead = 0:6,
                    use.diagonal.variance.matrix = TRUE)
cs.PM.PE <- PanelEstimate(cscw.PM, panel.data = cscw.panel, pooled = FALSE)
plot(cs.PM.PE, main = "Civil Liberties strength (PM)", ylab = "PDET on CSCW")

# 5. Compare FEct with PM ----

# Evaluate balance on the covariates: small pre-treatments

## step a) compute correlation matrix
cor(df.pm[,c("POBLACION_5_16", "discapital", "deficit", "DESERCION")], use = "pairwise.complete.obs")
## step b) https://github.com/insongkim/PanelMatch 
get_covariate_balance()


# Dynamic Treatment Effects with placebo
## For pre-treatment dynamic effects? 2017 should not effect dropouts in 2016
e1.PM.placebo <- PanelMatch(lag = 5, 
                            time.id = "Year", unit.id = "CODIGODANE", 
                            treatment = "PDET", 
                            refinement.method = "mahalanobis", 
                            data = df.pm, match.missing = TRUE,
                            covs.formula = ~ I(lag(POBLACION_5_16, 1:5)) + 
                              I(lag(discapital, 1:5)) + I(lag(deficit, 1:5)) +
                              I(lag(ValueAdded, 1:5)),
                            qoi = "att", 
                            lead = 0:6, 
                            outcome.var = "DESERCION",
                            placebo.test = FALSE)

## Revise...

PM.results.placebo <- PanelMatch(lag = 5, 
                                 refinement.method = "none", 
                                 panel.data = df.pm, 
                                 qoi = "att", 
                                 lead = c(0:6), 
                                 match.missing = TRUE,
                                 placebo.test = TRUE)


# ATT
PE.results.pool <- PanelEstimate(PM.results, panel.data = df.pm, pooled = TRUE)
summary(PE.results.pool)

# Plot
plot(PM.results)

# Dynamic Treatment Effects
PE.results <- PanelEstimate(PM.results, panel.data = df.pm)
PE.results.placebo <- placebo_test(PM.results.placebo, panel.data = df.pm, plot = F)

# obtain lead and lag (placebo) estimates
est_lead <- as.vector(PE.results$estimate)
est_lag <- as.vector(PE.results.placebo$estimates)
sd_lead <- apply(PE.results$bootstrapped.estimates,2,sd)
sd_lag <- apply(PE.results.placebo$bootstrapped.estimates,2,sd)
coef <- c(est_lag, 0, est_lead)
sd <- c(sd_lag, 0, sd_lead)
pm.output <- cbind.data.frame(ATT=coef, se=sd, t=c(-2:4))

# plot
p.pm <- esplot(data = pm.output,Period = 't',
               Estimate = 'ATT',SE = 'se')
p.pm
