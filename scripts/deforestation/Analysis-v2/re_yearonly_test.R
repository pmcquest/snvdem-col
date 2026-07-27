suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

tmf_cover <- readRDS(file.path(out_dir, "tmf_muni_cover.rds"))
dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

analysis_tmf <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm))

turning_point_bc <- function(b1, b2) unname(-b1 / (2 * b2))

run_three_specs <- function(df, label) {
  cat(sprintf("\n\n========== %s (n = %d) ==========\n", label, nrow(df)))

  # Model 7/8-style: two-way FE (municipality + year), what the v3 script already does
  m_twoway <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                     data = df, cluster = ~MPIO_CDPMP)
  cf <- coef(m_twoway); s <- se(m_twoway)
  cat(sprintf("Two-way FE (muni+year), current approach:  b1=%.4f (se %.4f)  b2=%.4f (se %.4f)  TP=%.3f  [%s]\n",
              cf[1], s[1], cf[2], s[2], turning_point_bc(cf[1], cf[2]),
              if (cf[2] > 0) "U-shape" else "inverted-U"))

  # Model 9-style: year-only FE (drops municipality FE, keeps between-municipality variation)
  m_yearonly <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | year,
                       data = df, cluster = ~MPIO_CDPMP)
  cf <- coef(m_yearonly); s <- se(m_yearonly)
  cat(sprintf("Year-only FE (BnB Model 9 analogue):        b1=%.4f (se %.4f)  b2=%.4f (se %.4f)  TP=%.3f  [%s]\n",
              cf[1], s[1], cf[2], s[2], turning_point_bc(cf[1], cf[2]),
              if (cf[2] > 0) "U-shape" else "inverted-U"))

  # Model 10-style: random effects (Swamy-Arora), municipality as the panel unit, year as a
  # regressor (matching BnB's own year FE structure inside a RE model) since plm's RE
  # estimator needs a declared panel id/time pair but year dummies enter as covariates here,
  # same as BnB's `i.year` inside `xtreg ..., re`.
  pdf <- pdata.frame(df, index = c("MPIO_CDPMP", "year"))
  m_re <- plm(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 + factor(year),
              data = pdf, model = "random")
  cf <- coef(m_re)
  s  <- sqrt(diag(vcovHC(m_re, cluster = "group")))
  cat(sprintf("Random effects (BnB Model 10 analogue):     b1=%.4f (se %.4f)  b2=%.4f (se %.4f)  TP=%.3f  [%s]\n",
              cf["snelect_norm"], s["snelect_norm"],
              cf["I(snelect_norm^2)"], s["I(snelect_norm^2)"],
              turning_point_bc(cf["snelect_norm"], cf["I(snelect_norm^2)"]),
              if (cf["I(snelect_norm^2)"] > 0) "U-shape" else "inverted-U"))

  invisible(list(twoway = m_twoway, yearonly = m_yearonly, re = m_re))
}

run_three_specs(analysis_tmf, "FULL PANEL 2000-2023")
run_three_specs(filter(analysis_tmf, year < 2016), "PRE-2016 (2000-2015)")
run_three_specs(filter(analysis_tmf, year >= 2016), "POST-2016 (2016-2023)")
