suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
  library(sf)
})
options(width = 200)

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

muni_level <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  left_join(muni_dept, by = "MPIO_CDPMP") %>%
  filter(!is.na(forest_ha), !is.na(area_ha), !is.na(snelect))

cat("Municipality-level rows feeding department aggregation:", nrow(muni_level), "\n")
cat("Departments:", n_distinct(muni_level$DPTO_CCDGO), "\n\n")

# ---- Aggregate municipality -> department, area-weighted throughout (same
# weighting logic used for raster -> municipality in the original extraction) ----
dept_level <- muni_level %>%
  group_by(DPTO_CCDGO, year) %>%
  summarise(
    snelect_wtd  = weighted.mean(snelect, w = area_ha, na.rm = TRUE),
    sncivlib_wtd = weighted.mean(sncivlib, w = area_ha, na.rm = TRUE),
    sndem_wtd    = weighted.mean(sndem, w = area_ha, na.rm = TRUE),
    n_munis      = n(),
    forest_ha    = sum(forest_ha, na.rm = TRUE),
    area_ha      = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(forest_pct = forest_ha / area_ha * 100) %>%
  arrange(DPTO_CCDGO, year) %>%
  group_by(DPTO_CCDGO) %>%
  mutate(
    forest_pct_lag1 = dplyr::lag(forest_pct),
    d_forest_pct    = forest_pct - forest_pct_lag1
  ) %>%
  ungroup() %>%
  filter(!is.na(d_forest_pct)) %>%
  mutate(
    snelect_norm  = norm01(snelect_wtd),
    sncivlib_norm = norm01(sncivlib_wtd),
    sndem_norm    = norm01(sndem_wtd)
  )

cat("Department-level panel:", nrow(dept_level), "rows |", n_distinct(dept_level$DPTO_CCDGO),
    "departments |", n_distinct(dept_level$year), "years\n")
cat("Munis per department (min/median/max):", min(dept_level$n_munis), "/",
    median(dept_level$n_munis), "/", max(dept_level$n_munis), "\n\n")

# ===================== Parametric: quadratic + linear, 3 specs, 3 periods =====================
cat("################ DEPARTMENT-LEVEL (n=33) QUADRATIC & LINEAR MODELS ################\n\n")

run_dept_spec <- function(df, spec_name, quad = TRUE) {
  if (nrow(df) < 30 || n_distinct(df$year) < 2) return(NULL)
  rhs <- if (quad) "snelect_norm + I(snelect_norm^2) + forest_pct_lag1" else "snelect_norm + forest_pct_lag1"
  if (spec_name == "twoway") {
    m <- feols(as.formula(paste("d_forest_pct ~", rhs, "| DPTO_CCDGO + year")), data = df, cluster = ~DPTO_CCDGO)
  } else if (spec_name == "yearonly") {
    m <- feols(as.formula(paste("d_forest_pct ~", rhs, "| year")), data = df, cluster = ~DPTO_CCDGO)
  } else {
    pdf <- pdata.frame(df, index = c("DPTO_CCDGO", "year"))
    frhs <- if (quad) "snelect_norm + I(snelect_norm^2) + forest_pct_lag1 + factor(year)" else "snelect_norm + forest_pct_lag1 + factor(year)"
    m <- tryCatch(plm(as.formula(paste("d_forest_pct ~", frhs)), data = pdf, model = "random"), error = function(e) NULL)
    if (is.null(m)) return(NULL)
  }
  cf <- coef(m)
  if (spec_name == "re") {
    s <- sqrt(diag(vcovHC(m, cluster = "group")))
    b1 <- cf["snelect_norm"]; se1 <- s["snelect_norm"]
    if (quad) { b2 <- cf["I(snelect_norm^2)"]; se2 <- s["I(snelect_norm^2)"] }
  } else {
    s <- se(m)
    b1 <- cf[1]; se1 <- s[1]
    if (quad) { b2 <- cf[2]; se2 <- s[2] }
  }
  if (quad) {
    data.frame(spec = spec_name, n = nrow(df), b1 = unname(b1), se1 = unname(se1),
               b2 = unname(b2), se2 = unname(se2), tp = turning_point(b1, b2),
               shape = if (b2 > 0) "U" else "inverted-U", b2_tstat = unname(b2 / se2))
  } else {
    data.frame(spec = spec_name, n = nrow(df), b1 = unname(b1), se1 = unname(se1), b1_tstat = unname(b1 / se1))
  }
}

periods <- list(FULL = dept_level, `PRE-2016` = filter(dept_level, year < 2016), `POST-2016` = filter(dept_level, year >= 2016))

cat("-- QUADRATIC --\n")
for (pname in names(periods)) {
  cat("\n", pname, "(n =", nrow(periods[[pname]]), "):\n")
  res <- map_dfr(c("twoway", "yearonly", "re"), ~ run_dept_spec(periods[[pname]], .x, quad = TRUE))
  print(res)
}

cat("\n\n-- LINEAR --\n")
for (pname in names(periods)) {
  cat("\n", pname, "(n =", nrow(periods[[pname]]), "):\n")
  res <- map_dfr(c("twoway", "yearonly", "re"), ~ run_dept_spec(periods[[pname]], .x, quad = FALSE))
  print(res)
}

# ===================== Non-parametric: quartile breakdown =====================
cat("\n\n################ NON-PARAMETRIC: MEAN d_forest_pct BY DEPARTMENT SNELECT QUARTILE ################\n\n")
for (pname in names(periods)) {
  cat("\n--", pname, "--\n")
  qtab <- periods[[pname]] %>%
    mutate(q = ntile(snelect_norm, 4)) %>%
    group_by(q) %>%
    summarise(snelect_mean = mean(snelect_norm), d_forest_pct_mean = mean(d_forest_pct, na.rm = TRUE),
              d_forest_pct_med = median(d_forest_pct, na.rm = TRUE), n = n(), .groups = "drop")
  print(qtab)
}

write_csv(dept_level, file.path(out_dir, "tmf_department_level_panel.csv"))
cat("\nDONE.\n")
