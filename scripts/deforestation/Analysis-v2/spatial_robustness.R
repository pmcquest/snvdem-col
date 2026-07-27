suppressMessages({
  library(tidyverse)
  library(fixest)
  library(sf)
  library(spdep)
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

analysis_tmf <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm)) %>%
  left_join(muni_dept, by = "MPIO_CDPMP")

cat("Departments in panel:", n_distinct(analysis_tmf$DPTO_CCDGO), "\n\n")

periods <- list(FULL = analysis_tmf, `PRE-2016` = filter(analysis_tmf, year < 2016), `POST-2016` = filter(analysis_tmf, year >= 2016))

# ===================== PART 1: department-clustered SEs =====================
cat("################ PART 1: MUNI- vs DEPARTMENT-CLUSTERED SEs ################\n\n")

report_both_clusters <- function(m, label) {
  se_muni <- se(m, cluster = ~MPIO_CDPMP)
  se_dept <- se(m, cluster = ~DPTO_CCDGO)
  cf <- coef(m)
  for (i in seq_along(cf)) {
    cat(sprintf("%-28s %-18s b=%9.4f  se(muni)=%.4f t=%6.2f | se(dept)=%.4f t=%6.2f\n",
                label, names(cf)[i], cf[i], se_muni[i], cf[i] / se_muni[i], se_dept[i], cf[i] / se_dept[i]))
  }
}

for (pname in names(periods)) {
  df <- periods[[pname]]
  m_lin_tw <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data = df)
  report_both_clusters(m_lin_tw, paste(pname, "LINEAR twoway"))
  m_quad_tw <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data = df)
  report_both_clusters(m_quad_tw, paste(pname, "QUADRATIC twoway"))
  m_lin_yo <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | year, data = df)
  report_both_clusters(m_lin_yo, paste(pname, "LINEAR yearonly"))
  m_quad_yo <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | year, data = df)
  report_both_clusters(m_quad_yo, paste(pname, "QUADRATIC yearonly"))
  cat("\n")
}

# ===================== PART 2: Moran's I on period-specific residuals =====================
cat("\n################ PART 2: MORAN'S I ON RESIDUALS, BY PERIOD ################\n\n")

muni_sf <- st_read(shp_path, quiet = TRUE) %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

moran_check <- function(df, label) {
  m <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data = df)
  df2 <- df %>% mutate(resid_val = resid(m))
  resid_by_muni <- df2 %>% group_by(MPIO_CDPMP) %>% summarise(resid_val = mean(resid_val), .groups = "drop") %>% arrange(MPIO_CDPMP)
  sf_sub <- muni_sf %>% filter(MPIO_CDPMP %in% resid_by_muni$MPIO_CDPMP) %>% arrange(MPIO_CDPMP)
  stopifnot(identical(sf_sub$MPIO_CDPMP, resid_by_muni$MPIO_CDPMP))
  nb <- poly2nb(sf_sub, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  mt <- moran.test(resid_by_muni$resid_val, lw, zero.policy = TRUE)
  cat(sprintf("%-12s Moran's I = %.4f   p-value = %s   (n munis = %d)\n",
              label, mt$estimate[1], format(mt$p.value, scientific = TRUE, digits = 3), nrow(resid_by_muni)))
}

for (pname in names(periods)) moran_check(periods[[pname]], pname)

# ===================== PART 3: does department FE absorb the curvature? =====================
cat("\n\n################ PART 3: DOES DEPARTMENT FE ABSORB THE CURVATURE? ################\n\n")
cat("(Municipality FE already nests department FE, so this test uses year-only-FE\n")
cat(" as the baseline and asks whether swapping in department+year FE shrinks b2)\n\n")

dept_fe_test <- function(df, label) {
  m_yearonly <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | year, data = df, cluster = ~DPTO_CCDGO)
  m_deptyear <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | DPTO_CCDGO + year, data = df, cluster = ~DPTO_CCDGO)
  cf1 <- coef(m_yearonly); s1 <- se(m_yearonly)
  cf2 <- coef(m_deptyear); s2 <- se(m_deptyear)
  cat(sprintf("%-12s NO dept FE  (year-only):    b1=%8.4f b2=%8.4f (se %.4f, t=%6.2f) TP=%7.3f\n",
              label, cf1[1], cf1[2], s1[2], cf1[2] / s1[2], turning_point(cf1[1], cf1[2])))
  cat(sprintf("%-12s WITH dept FE (dept+year):   b1=%8.4f b2=%8.4f (se %.4f, t=%6.2f) TP=%7.3f\n",
              label, cf2[1], cf2[2], s2[2], cf2[2] / s2[2], turning_point(cf2[1], cf2[2])))
  cat(sprintf("%-12s -> b2 changes from %.4f to %.4f (%.1f%% of original magnitude remains)\n\n",
              label, cf1[2], cf2[2], 100 * cf2[2] / cf1[2]))
}

for (pname in names(periods)) dept_fe_test(periods[[pname]], pname)

cat("DONE.\n")
