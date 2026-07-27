suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
  library(sf)
})
options(width = 200)

# Consolidated battery for memo v2 Section 4 expansion (2026-07-17 session):
#   - sncivlib/sndem alongside snelect (currently only snelect is narrated in 4.1,
#     though all three already exist in fe_regression_table_tmf*.tex)
#   - year-only-FE and RE specs alongside two-way FE, for all three measures
#     (re_yearonly_test.R already computes this but only for snelect, console-only)
#   - department-level (n=33) aggregation, all three measures (department_level_test.R
#     already computes this but only for snelect, console-only, linear+quad but not
#     cleanly exported)
#   - municipality- vs department-clustered SEs, all three measures (spatial_robustness.R
#     already computes this but only for snelect, console-only)
# Full panel (2000-2023) only -- pre/post-2016 split for snelect already lives in
# Appendix A / turning_point_prepost.R and is not re-run here.

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
turning_point <- function(b1, b2) unname(-b1 / (2 * b2))

tmf_cover <- readRDS(file.path(out_dir, "tmf_muni_cover.rds"))
dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

muni_dept <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP), DPTO_CCDGO = as.character(as.numeric(DPTO_CCDGO))) %>%
  st_drop_geometry() %>% select(MPIO_CDPMP, DPTO_CCDGO)

analysis <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  left_join(muni_dept, by = "MPIO_CDPMP") %>%
  mutate(
    snelect_norm  = norm01(snelect),
    sncivlib_norm = norm01(sncivlib),
    sndem_norm    = norm01(sndem)
  ) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1))

cat("Municipality-level analysis panel:", nrow(analysis), "rows |",
    n_distinct(analysis$MPIO_CDPMP), "municipalities |", n_distinct(analysis$DPTO_CCDGO), "departments\n\n")

measures <- c("snelect_norm", "sncivlib_norm", "sndem_norm")
measure_labels <- c(snelect_norm = "snelect", sncivlib_norm = "sncivlib", sndem_norm = "sndem")

# ============================================================================
# PART 1: Municipality-level, three specs x three measures, linear + quadratic,
#         with BOTH municipality- and department-clustered SEs
# ============================================================================
cat("################ PART 1: MUNICIPALITY-LEVEL, 3 SPECS x 3 MEASURES ################\n\n")

part1_rows <- list()

for (dv in measures) {
  lbl <- measure_labels[dv]
  rhs_lin  <- paste(dv, "+ forest_pct_lag1")
  rhs_quad <- paste(dv, "+ I(", dv, "^2) + forest_pct_lag1")

  # ---- Two-way FE (muni + year) ----
  m_lin_tw  <- feols(as.formula(paste("d_forest_pct ~", rhs_lin,  "| MPIO_CDPMP + year")), data = analysis)
  m_quad_tw <- feols(as.formula(paste("d_forest_pct ~", rhs_quad, "| MPIO_CDPMP + year")), data = analysis)

  # ---- Year-only FE (drops muni FE, keeps between-municipality variation) ----
  m_lin_yo  <- feols(as.formula(paste("d_forest_pct ~", rhs_lin,  "| year")), data = analysis)
  m_quad_yo <- feols(as.formula(paste("d_forest_pct ~", rhs_quad, "| year")), data = analysis)

  # ---- Random effects (Swamy-Arora), year entered as factor covariate ----
  pdf <- pdata.frame(analysis, index = c("MPIO_CDPMP", "year"))
  m_lin_re  <- plm(as.formula(paste("d_forest_pct ~", dv, "+ forest_pct_lag1 + factor(year)")), data = pdf, model = "random")
  m_quad_re <- plm(as.formula(paste("d_forest_pct ~", dv, "+ I(", dv, "^2) + forest_pct_lag1 + factor(year)")), data = pdf, model = "random")

  extract_row <- function(m, spec, quad, is_plm = FALSE) {
    if (is_plm) {
      cf <- coef(m)
      se_r <- sqrt(diag(vcovHC(m, cluster = "group")))
      b1 <- unname(cf[dv]); se1_muni <- unname(se_r[dv]); se1_dept <- NA
      if (quad) { b2 <- unname(cf[paste0(dv, "^2)")]); se2_muni <- unname(se_r[paste0(dv, "^2)")]); se2_dept <- NA
      } else { b2 <- NA; se2_muni <- NA; se2_dept <- NA }
    } else {
      cf <- coef(m)
      se_muni <- se(m, cluster = ~MPIO_CDPMP)
      se_dept <- se(m, cluster = ~DPTO_CCDGO)
      b1 <- unname(cf[1]); se1_muni <- unname(se_muni[1]); se1_dept <- unname(se_dept[1])
      if (quad) { b2 <- unname(cf[2]); se2_muni <- unname(se_muni[2]); se2_dept <- unname(se_dept[2])
      } else { b2 <- NA; se2_muni <- NA; se2_dept <- NA }
    }
    data.frame(measure = lbl, spec = spec, quad = quad,
               b1 = b1, se1_muni = se1_muni, t1_muni = b1/se1_muni, se1_dept = se1_dept, t1_dept = b1/se1_dept,
               b2 = b2, se2_muni = se2_muni, t2_muni = b2/se2_muni, se2_dept = se2_dept, t2_dept = b2/se2_dept,
               tp = if (quad) turning_point(b1, b2) else NA)
  }

  part1_rows[[length(part1_rows)+1]] <- extract_row(m_lin_tw,  "twoway",   FALSE)
  part1_rows[[length(part1_rows)+1]] <- extract_row(m_quad_tw, "twoway",   TRUE)
  part1_rows[[length(part1_rows)+1]] <- extract_row(m_lin_yo,  "yearonly", FALSE)
  part1_rows[[length(part1_rows)+1]] <- extract_row(m_quad_yo, "yearonly", TRUE)
  part1_rows[[length(part1_rows)+1]] <- extract_row(m_lin_re,  "re",       FALSE, is_plm = TRUE)
  part1_rows[[length(part1_rows)+1]] <- extract_row(m_quad_re, "re",       TRUE,  is_plm = TRUE)

  cat("Done:", lbl, "\n")
}

part1 <- bind_rows(part1_rows)
write_csv(part1, file.path(out_dir, "section4_muni_battery.csv"))
cat("\n")
print(as_tibble(part1), n = 100, width = 220)

# ============================================================================
# PART 2: Department-level (n=33) aggregation, three measures, linear only,
#         three specs (matches department_level_test.R but exported + 3 measures)
# ============================================================================
cat("\n\n################ PART 2: DEPARTMENT-LEVEL (n=33) AGGREGATION ################\n\n")

dept_level <- analysis %>%
  group_by(DPTO_CCDGO, year) %>%
  summarise(
    snelect_wtd  = weighted.mean(snelect, w = area_ha, na.rm = TRUE),
    sncivlib_wtd = weighted.mean(sncivlib, w = area_ha, na.rm = TRUE),
    sndem_wtd    = weighted.mean(sndem, w = area_ha, na.rm = TRUE),
    forest_ha    = sum(forest_ha, na.rm = TRUE),
    area_ha      = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(forest_pct = forest_ha / area_ha * 100) %>%
  arrange(DPTO_CCDGO, year) %>%
  group_by(DPTO_CCDGO) %>%
  mutate(forest_pct_lag1 = dplyr::lag(forest_pct), d_forest_pct = forest_pct - forest_pct_lag1) %>%
  ungroup() %>%
  filter(!is.na(d_forest_pct)) %>%
  mutate(
    snelect_norm  = norm01(snelect_wtd),
    sncivlib_norm = norm01(sncivlib_wtd),
    sndem_norm    = norm01(sndem_wtd)
  )

cat("Department-level panel:", nrow(dept_level), "rows |", n_distinct(dept_level$DPTO_CCDGO), "departments\n\n")

part2_rows <- list()
for (dv in measures) {
  lbl <- measure_labels[dv]
  rhs <- paste(dv, "+ forest_pct_lag1")

  m_tw <- feols(as.formula(paste("d_forest_pct ~", rhs, "| DPTO_CCDGO + year")), data = dept_level, cluster = ~DPTO_CCDGO)
  m_yo <- feols(as.formula(paste("d_forest_pct ~", rhs, "| year")), data = dept_level, cluster = ~DPTO_CCDGO)
  pdf_d <- pdata.frame(dept_level, index = c("DPTO_CCDGO", "year"))
  m_re <- tryCatch(plm(as.formula(paste("d_forest_pct ~", dv, "+ forest_pct_lag1 + factor(year)")), data = pdf_d, model = "random"),
                    error = function(e) NULL)

  row_tw <- data.frame(measure = lbl, spec = "twoway", n = nrow(dept_level),
                        b1 = unname(coef(m_tw)[1]), se1 = unname(se(m_tw)[1]))
  row_yo <- data.frame(measure = lbl, spec = "yearonly", n = nrow(dept_level),
                        b1 = unname(coef(m_yo)[1]), se1 = unname(se(m_yo)[1]))
  if (!is.null(m_re)) {
    cf <- coef(m_re); s <- sqrt(diag(vcovHC(m_re, cluster = "group")))
    row_re <- data.frame(measure = lbl, spec = "re", n = nrow(dept_level),
                          b1 = unname(cf[dv]), se1 = unname(s[dv]))
  } else {
    row_re <- data.frame(measure = lbl, spec = "re", n = nrow(dept_level), b1 = NA, se1 = NA)
  }
  part2_rows[[length(part2_rows)+1]] <- bind_rows(row_tw, row_yo, row_re) %>% mutate(t1 = b1/se1)
}

part2 <- bind_rows(part2_rows)
write_csv(part2, file.path(out_dir, "section4_dept_battery.csv"))
cat("\n")
print(as_tibble(part2), n = 100, width = 220)

cat("\n\nDONE.\n")
