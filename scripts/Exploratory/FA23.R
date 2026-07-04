# Factor analysis of snvdem data
# Which criteria are most influential in the snvdem index?

library(dplyr)
library(tidyverse)
library(psych)
library(lavaan)
library(plm)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

# A. FA with all variables ----
## 1. Load and Prepare Data ----
# this is an expanded df with other covariates. see 01Expand/snvdem_expand.R.
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/01Expand/snvdem2.rds")


## 2. EMEL Analysis (Electoral) ----
emel_vars <- c("rural_urban", "econ_dev", "dist_cap", "north", "south", 
               "west", "east", "conflict", "pop_density", "remoteness", 
               "indig_pop", "ruling_party")

snvdem_emel <- snvdem %>%
  rename(
    rural_urban = emel0_1, econ_dev = emel2_3, dist_cap = emel4_5,
    north = emel6, south = emel7, west = emel8, east = emel9,
    conflict = emel10_11, pop_density = emel12, remoteness = emel13,
    indig_pop = emel14, ruling_party = emel15_16
  )

# Universal EFA for EMEL
efa_emel <- fa(snvdem_emel[, emel_vars], nfactors = 3, rotate = "oblimin")
print(efa_emel$loadings, cutoff = 0.3, sort = TRUE)

# CFA Model for EMEL
final_model <- '
  Geography =~ north + south + west + east + dist_cap + remoteness
  Development =~ econ_dev + pop_density + rural_urban
  Conflict_Social =~ conflict + indig_pop + ruling_party
  SN_Democracy =~ Geography + Development + Conflict_Social
'
fit_sn <- cfa(final_model, data = snvdem_emel, std.lv = TRUE)

## 3. CSCW Analysis (Civil Liberties) ----
cscw_vars <- emel_vars # using same variable names after renaming

snvdem_cscw <- snvdem %>%
  rename(
    rural_urban = cscw0_1, econ_dev = cscw2_3, dist_cap = cscw4_5,
    north = cscw6, south = cscw7, west = cscw8, east = cscw9,
    conflict = cscw10_11, pop_density = cscw12, remoteness = cscw13,
    indig_pop = cscw14, ruling_party = cscw15_16
  )

# Universal EFA for CSCW
efa_cscw <- fa(snvdem_cscw[, cscw_vars], nfactors = 3, rotate = "oblimin")
print(efa_cscw$loadings, cutoff = 0.3, sort = TRUE)

## 4. Congruence check ----
congruence <- factor.congruence(efa_emel, efa_cscw)
print(congruence)

# Max Congruence (0.87): "Periphery" factor (Ind. Pop/Distance) is very consistent across both dimensions.
# Lowest Congruence (0.19): The variables that define "Electoral Fairness" in the capital do not translate to how "Civil Liberties" function in those same areas.
# Overall Verdict: Since no score hit >0.90, you should treat EMEL and CSCW as related but distinct pillars.
# Top 3 most influential variables based on their strongest factor loadings:
## Electoral Fairness (EMEL): Remoteness (0.765), Econ. Dev't (0.695), Pop. Density (0.642)
## Civil Liberties (CSCW) Econ. Dev't (0.837), Remoteness (0.723), West (Inverse) (-0.653)

## 5. Visualize ----

# Extract Full Loading Matrices
extract_full_matrix <- function(efa_obj, prefix) {
  loadings <- as.data.frame(unclass(efa_obj$loadings))
  colnames(loadings) <- paste0(prefix, "_", colnames(loadings))
  loadings$Variable <- rownames(loadings)
  return(loadings)
}

emel_full <- extract_full_matrix(efa_emel, "EMEL")
cscw_full <- extract_full_matrix(efa_cscw, "CSCW")

# Join Matrices 
full_comparison <- emel_full %>%
  full_join(cscw_full, by = "Variable") %>%
  mutate(across(starts_with("EMEL") | starts_with("CSCW"), ~replace_na(., 0)))

# 1. Create the Variance Summary Rows manually from your output
variance_rows <- data.frame(
  Variable = c("SS Loadings", "Proportion Var", "Cumulative Var"),
  EMEL_MR1 = c(2.580, 0.215, 0.215),
  EMEL_MR2 = c(1.571, 0.131, 0.499), # Reordering to match table columns MR1, MR2, MR3
  EMEL_MR3 = c(1.836, 0.153, 0.368),
  CSCW_MR1 = c(1.716, 0.143, 0.143),
  CSCW_MR2 = c(1.642, 0.137, 0.280),
  CSCW_MR3 = c(1.547, 0.129, 0.409)
)

# 2. Combine with your original full_comparison data
# Note: select(Variable, ...) ensures we keep the order of factors consistent
full_display <- full_comparison %>%
  select(Variable, EMEL_MR1, EMEL_MR2, EMEL_MR3, CSCW_MR1, CSCW_MR2, CSCW_MR3) %>%
  bind_rows(variance_rows)

# 3. Create a numeric matrix for colors (excluding the Variance rows from color scaling)
# This prevents the 'SS Loadings' (high numbers) from washing out the actual factor loadings
color_data <- full_display %>%
  filter(!Variable %in% c("SS Loadings", "Proportion Var", "Cumulative Var")) %>%
  select(-Variable) %>%
  as.matrix()

# 4. Render the Final Table
tbl <- full_display %>%
  kable(caption = "Three-Dimensional Structural Matrix with Variance Explained", 
        digits = 3, booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  add_header_above(c(" " = 1, "Electoral Pillar (EMEL)" = 3, "Civil Liberties Pillar (CSCW)" = 3)) %>%
  # Styling the background blocks
  column_spec(2:4, background = "#f0f7ff") %>% 
  column_spec(5:7, background = "#fffcf0") %>%
  # Formatting the Variance rows to stand out
  row_spec((nrow(full_display)-2):nrow(full_display), bold = T, background = "#eeeeee")

# 5. Apply heatmap colors only to the variable rows (1 through 12)
for (j in 2:7) {
  tbl <- tbl %>%
    column_spec(j, color = spec_color(full_display[[j]][1:12], option = "D", end = 0.8))
}

tbl
tbl %>%
  save_kable(file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/02FA/democracy_gap_table.html", self_contained = TRUE)


# Map the Factors based on your Congruence results
# EMEL MR1 -> CSCW MR2 (0.75: Development/Demographics)
# EMEL MR2 -> CSCW MR3 (0.87: Conflict/Social)
# EMEL MR3 -> CSCW MR1 (0.48: Geographic/Regional - Weak/Inverse)


# Diagnose Skew and Kurtosis ----
diag_stats <- describe(snvdem_emel[, emel_vars]) 

diag_df <- as.data.frame(diag_stats) %>%
  mutate(variable = rownames(.))

# Identify problematic variables (Skew > |2| or Kurtosis > |7|)
problematic_vars <- diag_df %>%
  filter(abs(as.numeric(skew)) > 2 | abs(as.numeric(kurtosis)) > 7) %>%
  select(variable, n, mean, skew, kurtosis)

print(problematic_vars)

# Run these on the EMEL variables
kmo_result <- KMO(snvdem_emel[, emel_vars])
bartlett_result <- cortest.bartlett(snvdem_emel[, emel_vars])

print(kmo_result)
print(bartlett_result$p.value)

# "The suitability of the data for factor analysis was confirmed via the Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy and Bartlett’s test of sphericity. The overall KMO was 0.76, exceeding the recommended threshold of 0.60, and Bartlett’s test was highly significant ($p < .001$), indicating that the correlation matrix was not an identity matrix and possessed sufficient common variance for factor extraction."