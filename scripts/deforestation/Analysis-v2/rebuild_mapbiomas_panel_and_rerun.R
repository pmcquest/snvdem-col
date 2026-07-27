suppressMessages({ library(readr); library(dplyr); library(sf); library(stringr); library(tidyr); library(fixest) })

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
analysis_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
norm_str <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x); x <- gsub("[^a-z0-9 ]", " ", x); gsub("\\s+", " ", trimws(x))
}

diag <- readRDS(file.path(analysis_dir, "mb_crosswalk_diagnostics.rds"))
alias_fixed <- diag$alias_fixed
eti_match   <- diag$eti_match
direct      <- diag$direct

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))

# Combine: direct matches + alias/ETI-recovered matches
recovered <- alias_fixed %>%
  left_join(eti_match, by = c("municipio", "DPTO_CCDGO")) %>%
  mutate(MPIO_CDPMP = coalesce(MPIO_CDPMP, matched_MPIO_CDPMP)) %>%
  filter(!is.na(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, area_ha)

direct_matched <- direct %>% filter(!is.na(MPIO_CDPMP)) %>% select(MPIO_CDPMP, year, area_ha)

mb_corrected_raw <- bind_rows(direct_matched, recovered) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(forest_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  arrange(MPIO_CDPMP, year)

cat("Corrected panel: ", nrow(mb_corrected_raw), "muni-years |",
    n_distinct(mb_corrected_raw$MPIO_CDPMP), "municipalities\n")
cat("National forest_ha, 2000 (corrected):", format(sum(mb_corrected_raw$forest_ha[mb_corrected_raw$year==2000]), big.mark=","), "\n")
cat("National forest_ha, 2000 (OLD, uncorrected mb_muni_cover.rds):",
    format(sum(readRDS(file.path(out_dir,"mb_muni_cover.rds"))$forest_ha[readRDS(file.path(out_dir,"mb_muni_cover.rds"))$year==2000]), big.mark=","), "\n\n")

add_forest_change_vars <- function(df, area_df) {
  df %>% left_join(area_df, by = "MPIO_CDPMP") %>%
    mutate(forest_pct = forest_ha / area_ha * 100) %>%
    arrange(MPIO_CDPMP, year) %>% group_by(MPIO_CDPMP) %>%
    mutate(forest_pct_lag1 = lag(forest_pct), d_forest_pct = forest_pct - forest_pct_lag1) %>%
    ungroup()
}
exclude_never_forested <- function(df) {
  keep <- df %>% filter(year == min(year)) %>% filter(forest_pct > 0) %>% pull(MPIO_CDPMP)
  filter(df, MPIO_CDPMP %in% keep)
}

mb_corrected <- mb_corrected_raw %>% add_forest_change_vars(muni_areas) %>% exclude_never_forested()
saveRDS(mb_corrected, file.path(analysis_dir, "mb_muni_cover_corrected.rds"))

cat("Final corrected cover panel:", nrow(mb_corrected), "rows |",
    n_distinct(mb_corrected$MPIO_CDPMP), "municipalities\n\n")

# ---- Re-run linear + quadratic, two-way FE, compare old vs corrected ----
dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect) %>% distinct(MPIO_CDPMP, year, .keep_all = TRUE)

turning_point_bc <- function(b1, b2) unname(-b1 / (2 * b2))

analysis_mb_new <- mb_corrected %>% filter(year >= 2001) %>%
  inner_join(dem_df, by = c("MPIO_CDPMP","year")) %>%
  mutate(snelect_norm = norm01(snelect))

cat("=== CORRECTED MapBiomas, two-way FE, full panel ===\n")
m_lin_new <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data = analysis_mb_new, cluster = ~MPIO_CDPMP)
cat("Linear: b=", round(coef(m_lin_new)[1],4), " t=", round(coef(m_lin_new)[1]/se(m_lin_new)[1],3), "\n")

m_q_new <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data = analysis_mb_new, cluster = ~MPIO_CDPMP)
cf <- coef(m_q_new); s <- se(m_q_new)
cat("Quadratic: b1=", round(cf[1],4), " t1=", round(cf[1]/s[1],3),
    " | b2=", round(cf[2],4), " t2=", round(cf[2]/s[2],3),
    " | TP=", round(turning_point_bc(cf[1],cf[2]),3), "\n")

cat("\n=== OLD (uncorrected) MapBiomas, two-way FE, full panel, for comparison ===\n")
mb_old <- readRDS(file.path(out_dir, "mb_muni_cover.rds")) %>% filter(year >= 2001) %>%
  inner_join(dem_df, by = c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect))
m_lin_old <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data = mb_old, cluster = ~MPIO_CDPMP)
cat("Linear: b=", round(coef(m_lin_old)[1],4), " t=", round(coef(m_lin_old)[1]/se(m_lin_old)[1],3), "\n")
m_q_old <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data = mb_old, cluster = ~MPIO_CDPMP)
cf <- coef(m_q_old); s <- se(m_q_old)
cat("Quadratic: b1=", round(cf[1],4), " t1=", round(cf[1]/s[1],3),
    " | b2=", round(cf[2],4), " t2=", round(cf[2]/s[2],3),
    " | TP=", round(turning_point_bc(cf[1],cf[2]),3), "\n")

cat("\nShare of corrected-panel obs below new TP:",
    round(mean(analysis_mb_new$snelect_norm <= turning_point_bc(coef(m_q_new)[1],coef(m_q_new)[2]), na.rm=TRUE)*100,1), "%\n")
