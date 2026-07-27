# ====================================================================== #
# Full period-split x specification battery for the ratchet ("hardliner",
# loss-only) TMF DV, mirroring re_yearonly_test.R's existing structure for
# the bidirectional TMF DV exactly, so the two are directly comparable.
# Also reproduces GFC's own quadratic results for reference, since GFC
# (not ratchet-TMF) is what v1 of the memo actually used.
# ====================================================================== #

suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
analysis_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
turning_point_bc <- function(b1, b2) unname(-b1 / (2 * b2))

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

run_three_specs <- function(df, label, dv, lag_control) {
  cat(sprintf("\n\n========== %s (n = %d) ==========\n", label, nrow(df)))
  f_two  <- as.formula(sprintf("%s ~ snelect_norm + I(snelect_norm^2) + %s | MPIO_CDPMP + year", dv, lag_control))
  f_year <- as.formula(sprintf("%s ~ snelect_norm + I(snelect_norm^2) + %s | year", dv, lag_control))
  f_re   <- as.formula(sprintf("%s ~ snelect_norm + I(snelect_norm^2) + %s + factor(year)", dv, lag_control))

  m_twoway <- feols(f_two, data = df, cluster = ~MPIO_CDPMP)
  cf <- coef(m_twoway); s <- se(m_twoway)
  cat(sprintf("Two-way FE:      b1=%.4f (se %.4f)  b2=%.4f (t=%.2f)  TP=%.3f  [%s]\n",
              cf[1], s[1], cf[2], cf[2]/s[2], turning_point_bc(cf[1], cf[2]),
              if (cf[2] > 0) "U-shape" else "inverted-U"))

  m_yearonly <- feols(f_year, data = df, cluster = ~MPIO_CDPMP)
  cf <- coef(m_yearonly); s <- se(m_yearonly)
  cat(sprintf("Year-only FE:    b1=%.4f (se %.4f)  b2=%.4f (t=%.2f)  TP=%.3f  [%s]\n",
              cf[1], s[1], cf[2], cf[2]/s[2], turning_point_bc(cf[1], cf[2]),
              if (cf[2] > 0) "U-shape" else "inverted-U"))

  pdf <- pdata.frame(df, index = c("MPIO_CDPMP", "year"))
  m_re <- plm(f_re, data = pdf, model = "random")
  cf <- coef(m_re); s <- sqrt(diag(vcovHC(m_re, cluster = "group")))
  cat(sprintf("Random effects:  b1=%.4f (se %.4f)  b2=%.4f (t=%.2f)  TP=%.3f  [%s]\n",
              cf["snelect_norm"], s["snelect_norm"],
              cf["I(snelect_norm^2)"], cf["I(snelect_norm^2)"]/s["I(snelect_norm^2)"],
              turning_point_bc(cf["snelect_norm"], cf["I(snelect_norm^2)"]),
              if (cf["I(snelect_norm^2)"] > 0) "U-shape" else "inverted-U"))

  invisible(list(twoway = m_twoway, yearonly = m_yearonly, re = m_re))
}

# ---- A. Ratchet (hardliner) TMF -----------------------------------------

tmf_ratchet <- readRDS(file.path(analysis_dir, "tmf_muni_ratchet.rds")) %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct_ratchet), !is.na(forest_pct_ratchet_lag1), !is.na(snelect_norm))

cat("\n\n#####################################################\n")
cat("### RATCHET (hardliner, loss-only) TMF DV ###\n")
cat("#####################################################\n")
run_three_specs(tmf_ratchet, "FULL PANEL", "d_forest_pct_ratchet", "forest_pct_ratchet_lag1")
run_three_specs(filter(tmf_ratchet, year <= 2015), "PRE-2016 (2001-2015)", "d_forest_pct_ratchet", "forest_pct_ratchet_lag1")
run_three_specs(filter(tmf_ratchet, year >= 2016), "POST-2016 (2016-2023)", "d_forest_pct_ratchet", "forest_pct_ratchet_lag1")

# ---- B. Bidirectional (softliner) TMF, SAME sample as ratchet -----------

cat("\n\n#####################################################\n")
cat("### BIDIRECTIONAL (softliner) TMF DV -- same restricted sample ###\n")
cat("#####################################################\n")
run_three_specs(tmf_ratchet, "FULL PANEL", "d_forest_pct", "forest_pct_lag1")
run_three_specs(filter(tmf_ratchet, year <= 2015), "PRE-2016 (2001-2015)", "d_forest_pct", "forest_pct_lag1")
run_three_specs(filter(tmf_ratchet, year >= 2016), "POST-2016 (2016-2023)", "d_forest_pct", "forest_pct_lag1")

# ---- C. GFC (what v1 of the memo actually used) --------------------------

gfc <- readRDS(file.path(out_dir, "gfc_muni_cover.rds")) %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm))

cat("\n\n#####################################################\n")
cat("### GFC (Hansen lossyear-based) -- v1's actual data source ###\n")
cat("#####################################################\n")
run_three_specs(gfc, "FULL PANEL", "d_forest_pct", "forest_pct_lag1")
run_three_specs(filter(gfc, year <= 2015), "PRE-2016", "d_forest_pct", "forest_pct_lag1")
run_three_specs(filter(gfc, year >= 2016), "POST-2016", "d_forest_pct", "forest_pct_lag1")
