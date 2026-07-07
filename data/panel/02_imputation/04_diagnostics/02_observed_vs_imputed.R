#---- Observed vs. imputed diagnostics, one panel per imputation script ----
# Written 2026-07-06 in response to a direct question: imp01_RuralMI.R's own bundled diagnostic
# plot showed only "Observed" in its legend. Root cause: its imputed_flag was built from a
# hardcoded list of municipality codes (problem_codes <- c("23685", "27086", "27415", "99572",
# "99760")) that were a one-time inspection snapshot, never tied to actual NA status -- and those
# specific codes are the legacy DIVIPOLA codes dropped by the 2026-07-03 empirical-merge rewrite,
# so none of them exist in the data anymore. The flag was never "imputed == true missingness";
# it silently returned FALSE for every row.
#
# Auditing every other imputation script found the same class of problem is common here: only
# imp1011_CrimeMI-PMM_v3.R computes a correct is.na()-based observed/imputed flag; several older
# (v2/) scripts did too but that got lost in the "v3" rewrites. None of the current canonical
# scripts ever call ggsave() on these diagnostic plots, so even the correct ones only render to
# whatever the default graphics device is in a batch run (a generic Rplots.pdf), not a persisted,
# labeled file.
#
# This script is the fix: for every imputed variable, join the PRE-imputation value (from
# df_col_clean.rds, before that variable's script ran) against the POST-imputation value (from
# the script's saved .rds) on (MPIO_CDPMP, year), flag was_missing = is.na(pre-value), and save a
# correctly-labeled density comparison. Run this after 03_merge_imputed/01_merge_imputed.R.

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

out_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/04_diagnostics/01_criteria-png/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
imp_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/"

pre <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds") %>%
  select(MPIO_CDPMP, year, everything())

# ---- Helper: one observed-vs-imputed density panel for a set of variables from one script ----
# `pre_df`/`post_df` are joined on (MPIO_CDPMP, year); `vars` names columns present in both
# (post-imputation column name is used for both sides unless `pre_vars` is supplied for
# differently-named/derived pre-imputation columns, e.g. proportions built from raw counts).
plot_observed_vs_imputed <- function(post_df, vars, pre_vars = vars, title, subtitle, file,
                                      pre_df = pre) {
  joined <- inner_join(
    pre_df %>% select(MPIO_CDPMP, year, all_of(unique(pre_vars))),
    post_df %>% select(MPIO_CDPMP, year, all_of(vars)),
    by = c("MPIO_CDPMP", "year"),
    suffix = c("_pre", "_post")
  )

  long_data <- purrr::map2_dfr(vars, pre_vars, function(v, pv) {
    pre_col <- if (pv == v) paste0(pv, "_pre") else pv
    post_col <- if (v == pv) paste0(v, "_post") else v
    tibble(
      variable = v,
      was_missing = is.na(joined[[pre_col]]),
      value = joined[[post_col]]
    )
  }) %>%
    mutate(Status = if_else(was_missing, "Imputed", "Observed")) %>%
    filter(!is.na(value))

  n_summary <- long_data %>%
    count(variable, Status) %>%
    pivot_wider(names_from = Status, values_from = n, values_fill = 0)
  print(n_summary)

  p <- ggplot(long_data, aes(x = value, fill = Status)) +
    geom_density(alpha = 0.45, color = NA) +
    facet_wrap(~variable, scales = "free", ncol = min(3, length(vars))) +
    scale_fill_manual(values = c("Imputed" = "#E41A1C", "Observed" = "#377EB8")) +
    theme_minimal() +
    theme(legend.position = "top", strip.text = element_text(face = "bold")) +
    labs(title = title, subtitle = subtitle, x = "Value", y = "Density", fill = "Data status")

  ggsave(paste0(out_dir, file), p, width = 4 * min(3, length(vars)), height = 4, dpi = 300, bg = "white")
  invisible(p)
}

# ---- 1. impStatic_LOCF.R -- static geography ----
impStatic <- read_rds(paste0(imp_dir, "impStatic.rds"))
plot_observed_vs_imputed(
  impStatic,
  vars = c("DisBog_4t5", "north6", "south7", "west8", "east9", "axis_ns", "axis_we", "disp_ns", "disp_we"),
  title = "impStatic_LOCF.R: static geography",
  subtitle = "LOCF (downup) fill of time-invariant geographic variables",
  file = "diag_01_impStatic.png"
)

# ---- 2. imp01_RuralMI.R -- rurality index ----
imp01 <- read_rds(paste0(imp_dir, "imp01.rds"))
plot_observed_vs_imputed(
  imp01,
  vars = "IndRur_0t1",
  title = "imp01_RuralMI.R: rurality index",
  subtitle = "MICE (PMM, group-mean predictor) + LOCF finishing pass",
  file = "diag_02_imp01_rurality.png"
)

# ---- 3. imp23_EconGrowth_v3b.R -- municipal GDP ----
imp23 <- read_rds(paste0(imp_dir, "imp23.rds"))
plot_observed_vs_imputed(
  imp23,
  vars = "PIB_2t3",
  title = "imp23_EconGrowth_v3b.R: municipal GDP",
  subtitle = "Own/department growth chain blended 50/50 with random forest",
  file = "diag_03_imp23_gdp.png"
)

# ---- 4. imp23_FiscalCART_v3.R -- fiscal performance ----
# NOTE 2026-07-06: renamed from imp23_FiscalPMM_v3.R -- filename/header used to say "PMM", but
# the method has actually been CART since at least 2026-07-03 (see memo).
imp23b <- read_rds(paste0(imp_dir, "imp23b.rds"))
plot_observed_vs_imputed(
  imp23b,
  vars = "IDF_2t3",
  title = "imp23_FiscalCART_v3.R: fiscal performance index",
  subtitle = "MICE (CART, group-mean predictor)",
  file = "diag_04_imp23b_fiscal.png"
)

# ---- 5. imp1011_CrimeMI-PMM_v3.R -- crime/violence inputs ----
imp1011 <- read_rds(paste0(imp_dir, "imp1011.rds"))
plot_observed_vs_imputed(
  imp1011,
  vars = c("Desp_1011", "VDays_1011", "HHomix_1011"),
  title = "imp1011_CrimeMI-PMM_v3.R: crime/violence inputs",
  subtitle = "MICE (PMM, group-mean predictor)",
  file = "diag_05_imp1011_crime.png"
)

# ---- 6. imp1011_FAviolence_pmq.R -- violence factor score ----
# ViolInd_1011 is fully derived (no pre-imputation counterpart of its own), so "observed vs.
# imputed" doesn't apply the same way. Instead: flag each row by whether ANY of its three inputs
# was itself imputed upstream -- this is the meaningful version of the same question here (does
# the factor score for originally-incomplete rows look like the score for fully-observed rows?).
imp1011FA <- read_rds(paste0(imp_dir, "imp1011FA.rds"))
fa_joined <- inner_join(
  pre %>% select(MPIO_CDPMP, year, Desp_1011, VDays_1011, HHomix_1011),
  imp1011FA %>% select(MPIO_CDPMP, year, ViolInd_1011),
  by = c("MPIO_CDPMP", "year"), suffix = c("_pre", "")
) %>%
  mutate(Status = if_else(
    is.na(Desp_1011) | is.na(VDays_1011) | is.na(HHomix_1011),
    "Uses ≥ 1 imputed input", "All inputs observed"
  ))
print(count(fa_joined, Status))
p_fa <- ggplot(fa_joined, aes(x = ViolInd_1011, fill = Status)) +
  geom_density(alpha = 0.45, color = NA) +
  scale_fill_manual(values = c("Uses ≥ 1 imputed input" = "#E41A1C", "All inputs observed" = "#377EB8")) +
  theme_minimal() + theme(legend.position = "top") +
  labs(title = "imp1011_FAviolence_pmq.R: violence factor score",
       subtitle = "ViolInd_1011 is derived, not itself imputed -- split by whether any input was",
       x = "ViolInd_1011 (factor score)", y = "Density", fill = NULL)
ggsave(paste0(out_dir, "diag_06_imp1011FA_violence.png"), p_fa, width = 6, height = 4, dpi = 300, bg = "white")

# ---- 7. imp1214_PopGBIv2.R -- indigenous/ethnic population shares ----
# Post-imputation columns are proportions (Pob*_14 / PobTot_12); build matching pre-imputation
# proportions from the raw counts so both sides are on the same scale.
imp1214 <- read_rds(paste0(imp_dir, "imp1214.rds"))
pre_props <- pre %>%
  mutate(PropInd_14 = PobInd_14 / PobTot_12, PropEtn_14 = PobEtn_14 / PobTot_12) %>%
  select(MPIO_CDPMP, year, PropInd_14, PropEtn_14, PobInd_14, PobEtn_14)
# was_missing must reflect the raw count's missingness, not the derived proportion's, since the
# proportion is NA whenever the count is NA anyway -- using the count directly as pre_vars would
# compare counts to proportions, so flag manually instead.
plot_observed_vs_imputed(
  imp1214,
  vars = c("PropInd_14", "PropEtn_14"),
  pre_df = pre_props %>% mutate(PropInd_14 = if_else(is.na(PobInd_14), NA_real_, PropInd_14),
                                 PropEtn_14 = if_else(is.na(PobEtn_14), NA_real_, PropEtn_14)),
  title = "imp1214_PopGBIv2.R: indigenous/ethnic population share",
  subtitle = "Growth interpolation (census anchors) -> constant -> yearly median fallback",
  file = "diag_07_imp1214_population.png"
)

# ---- 8. imp13_RoadsLOCF.R -- distance to market ----
imp13 <- read_rds(paste0(imp_dir, "imp13.rds"))
plot_observed_vs_imputed(
  imp13,
  vars = "DisMer_13",
  title = "imp13_RoadsLOCF.R: distance to market",
  subtitle = "LOCF (downup); road-count/length variables are fully observed, not shown",
  file = "diag_08_imp13_roads.png"
)

# ---- 9. imp1516_ElectionsLOCF.R -- ruling party ----
imp1516 <- read_rds(paste0(imp_dir, "imp1516.rds"))
plot_observed_vs_imputed(
  imp1516,
  vars = c("RulPar_15t16", "RulParD_15t16"),
  title = "imp1516_ElectionsLOCF.R: ruling party",
  subtitle = "LOCF, forward-only (.direction = \"down\") -- residual NAs are pre-first-election years, left unfilled deliberately",
  file = "diag_09_imp1516_elections.png"
)

cat("\nAll observed-vs-imputed diagnostic plots written to", out_dir, "\n")
