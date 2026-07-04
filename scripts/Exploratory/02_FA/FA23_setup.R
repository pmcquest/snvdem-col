# Shared setup for FA23.R (console/kable output) and FA23_word_export.R
# (Word/.docx output). Loads the data, reconstructs per-pillar weighted
# components, fits the Baseline (12-var) and No-Geography (8-var, aka
# "Trimmed") factor models, and builds the final_df table structure (loadings
# + SS Loadings/Var Explained/Model Fit rows) that both renderers consume.
#
# Source this rather than duplicating -- the two renderers should only differ
# in presentation, not in the underlying model.

library(dplyr)
library(tidyverse)
library(psych)

options(width = 200) # keep plain-text table prints from wrapping across blocks

# 1. Load Data ----
# snvdem_col_weighted.rds is the current production output of
# 07_weighting/01_weight_predictors.R (normalized weighted-average method).
# It stores the hi/lo-split predictor averages (avg2t3hi/lo, etc.) and the
# el_*/cl_* boundary-condition weights separately, rather than the final
# per-component weighted values, so we recombine them below using the exact
# formula from that script (verified to reproduce snelect/sncivlib to
# floating-point precision).
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_weighted.rds")

# 2. Variable Definitions & Reconstruction ----
# North/South/West/East are kept as four separate directional exposures
# (rather than combined into axis_ns/axis_we), matching how they enter the
# snelect/sncivlib formula in 01_weight_predictors.R as four independent terms.
emel_vars <- c("urban", "econ_dev", "prox_cap", "north", "south", "west", "east",
              "nonviolent", "pop_density", "nonremote",
               "nonindig", "compete")

trimmed_vars <- c("urban", "econ_dev", "prox_cap", "nonviolent",
                  "pop_density", "nonremote", "nonindig", "compete")

snvdem_emel <- snvdem %>%
  mutate(
    urban       = avg0t1hi * el_Urban + avg0t1lo * el_Rural,
    econ_dev    = avg2t3hi * el_More_development + avg2t3lo * el_Less_development,
    prox_cap    = avg4t5hi * (1 - el_Inside_capital) + avg4t5lo * (1 - el_Outside_capital),
    north       = avg6 * el_North,
    south       = avg7 * el_South,
    west        = avg8 * el_West,
    east        = avg9 * el_East,
    nonviolent  = avg10t11 * wt_el_1011,
    pop_density = avg12 * el_Sparse_population,
    nonremote   = avg13 * el_Remote,
    nonindig    = avg14 * (1 - el_Indigenous),
    compete     = avg15t16hi * (1 - el_Ruling_party_strong) + avg15t16lo * (1 - el_Ruling_party_weak)
  )

snvdem_cscw <- snvdem %>%
  mutate(
    urban       = avg0t1hi * cl_Urban + avg0t1lo * cl_Rural,
    econ_dev    = avg2t3hi * cl_More_development + avg2t3lo * cl_Less_development,
    prox_cap    = avg4t5hi * (1 - cl_Inside_capital) + avg4t5lo * (1 - cl_Outside_capital),
    north       = avg6 * cl_North,
    south       = avg7 * cl_South,
    west        = avg8 * cl_West,
    east        = avg9 * cl_East,
    nonviolent  = avg10t11 * wt_cl_1011,
    pop_density = avg12 * cl_Sparse_population,
    nonremote   = avg13 * cl_Remote,
    nonindig    = avg14 * (1 - cl_Indigenous),
    compete     = avg15t16hi * (1 - cl_Ruling_party_strong) + avg15t16lo * (1 - cl_Ruling_party_weak)
  )

# 3. Helper: Normality and Suitability Diagnostics ----
get_diagnostics <- function(vars, label) {
  # Skew/Kurtosis
  diag <- describe(snvdem_emel[, vars]) %>%
    as.data.frame() %>%
    mutate(Variable = rownames(.), Model = label) %>%
    select(Model, Variable, skew, kurtosis)
  # KMO/Bartlett
  kmo <- KMO(snvdem_emel[, vars])
  bart <- cortest.bartlett(snvdem_emel[, vars])
  return(list(diag = diag, kmo = kmo$MSA, bart_p = bart$p.value))
}

# 4. Fit the Baseline (12-var) and No-Geography (8-var) Models ----
efa_emel_base <- fa(snvdem_emel[, emel_vars], nfactors = 3, rotate = "oblimin")
efa_cscw_base <- fa(snvdem_cscw[, emel_vars], nfactors = 3, rotate = "oblimin")
base_diag <- get_diagnostics(emel_vars, "Baseline")

efa_emel_trim <- fa(snvdem_emel[, trimmed_vars], nfactors = 3, rotate = "oblimin")
efa_cscw_trim <- fa(snvdem_cscw[, trimmed_vars], nfactors = 3, rotate = "oblimin")
trim_diag <- get_diagnostics(trimmed_vars, "Trimmed")

# 5. Build the final_df Table Structure (shared by kable and flextable renderers) ----
# One row per variable (loadings), plus SS Loadings/Proportion Var/Cumulative
# Var/Model Fit summary rows -- consumed as-is by render_fa_table() in FA23.R
# and make_word_table() in FA23_word_export.R.
build_final_df <- function(fa_emel_obj, fa_cscw_obj) {
  get_fa_parts <- function(fa_obj, prefix) {
    loadings <- as.data.frame(unclass(fa_obj$loadings))
    colnames(loadings) <- paste0(prefix, "_", colnames(loadings))
    loadings$Variable <- rownames(loadings)
    var_stats <- as.data.frame(fa_obj$Vaccounted)[1:3, ]
    rownames(var_stats) <- c("SS Loadings", "Proportion Var", "Cumulative Var")
    colnames(var_stats) <- paste0(prefix, "_", colnames(var_stats))
    var_stats$Variable <- rownames(var_stats)
    return(list(loadings = loadings, stats = var_stats))
  }

  d_emel <- get_fa_parts(fa_emel_obj, "EMEL")
  d_cscw <- get_fa_parts(fa_cscw_obj, "CSCW")

  loadings_stats <- bind_rows(
    full_join(d_emel$loadings, d_cscw$loadings, by = "Variable"),
    full_join(d_emel$stats, d_cscw$stats, by = "Variable")
  ) %>%
    mutate(across(where(is.numeric), ~sprintf("%.3f", .))) %>%
    relocate(Variable)

  fit_row <- data.frame(
    Variable = "Model Fit (TLI / RMSEA)",
    EMEL_MR1 = paste0(round(fa_emel_obj$TLI, 2), " / ", round(fa_emel_obj$RMSEA[1], 3)),
    EMEL_MR2 = "", EMEL_MR3 = "",
    CSCW_MR1 = paste0(round(fa_cscw_obj$TLI, 2), " / ", round(fa_cscw_obj$RMSEA[1], 3)),
    CSCW_MR2 = "", CSCW_MR3 = ""
  )

  bind_rows(loadings_stats, fit_row)
}

df1 <- build_final_df(efa_emel_base, efa_cscw_base)
df2 <- build_final_df(efa_emel_trim, efa_cscw_trim)
