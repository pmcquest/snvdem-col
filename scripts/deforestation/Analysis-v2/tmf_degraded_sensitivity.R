# ====================================================================== #
# Sensitivity test: does the TMF-based finding depend on counting
# "degraded" pixels as forest? National composition (2000-2023):
#   undisturbed: 65.2M ha (95.1% of forest) -> 57.1M ha (82.1%), -12.5%
#   degraded:     3.1M ha (4.5%)            ->  6.4M ha (9.3%),  +109%
#   regrowth:     0.3M ha (0.4%)            ->  6.0M ha (8.7%),  +2130%
# The current DV (forest = undisturbed+degraded+regrowth) is blind to the
# undisturbed -> degraded transition, since both count as "forest." This
# script builds two stricter alternative DVs and reruns the panel.
# ====================================================================== #

suppressMessages({ library(jsonlite); library(dplyr); library(fixest) })

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
analysis_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2"
px_area_ha <- 0.09
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
turning_point_bc <- function(b1, b2) unname(-b1 / (2 * b2))

raw <- fromJSON(file.path(out_dir, "tmf_muni_classcounts.json")) %>% as_tibble()

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))

build_panel <- function(forest_px_expr) {
  df <- raw %>%
    mutate(total_px = c0+c1+c2+c3+c4+c5+c6,
           forest_px = !!rlang::parse_expr(forest_px_expr),
           area_ha_px = total_px * px_area_ha,
           forest_ha = forest_px * px_area_ha) %>%
    select(MPIO_CDPMP, year, forest_ha) %>%
    left_join(muni_areas, by = "MPIO_CDPMP") %>%
    mutate(forest_pct = forest_ha / area_ha * 100) %>%
    arrange(MPIO_CDPMP, year) %>% group_by(MPIO_CDPMP) %>%
    mutate(forest_pct_lag1 = lag(forest_pct), d_forest_pct = forest_pct - forest_pct_lag1) %>%
    ungroup() %>% filter(year >= 2000)
  keep <- df %>% group_by(MPIO_CDPMP) %>% summarise(never = all(forest_pct==0, na.rm=TRUE)) %>%
    filter(!never) %>% pull(MPIO_CDPMP)
  filter(df, MPIO_CDPMP %in% keep)
}

# Current headline DV: undisturbed + degraded + regrowth
panel_current <- build_panel("c1 + c2 + c4")
# Strict: undisturbed + regrowth only (degraded treated as NOT forest)
panel_strict  <- build_panel("c1 + c4")
# Pristine: undisturbed only (most conservative -- degraded AND regrowth excluded)
panel_pristine <- build_panel("c1")

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect) %>% distinct(MPIO_CDPMP, year, .keep_all = TRUE)

run_both <- function(panel, label) {
  a <- panel %>% inner_join(dem_df, by=c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect)) %>%
    filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1))
  cat("\n===", label, "=== n =", nrow(a), "\n")
  cat("National forest stock 2000 (M ha):", round(sum(panel$forest_ha[panel$year==2000])/1e6,2),
      "| 2023 (M ha):", round(sum(panel$forest_ha[panel$year==2023])/1e6,2),
      "| pct change:", round((sum(panel$forest_ha[panel$year==2023])/sum(panel$forest_ha[panel$year==2000])-1)*100,2), "%\n")

  m_lin <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data=a, cluster=~MPIO_CDPMP)
  cat("Linear:    b=", round(coef(m_lin)[1],4), " t=", round(coef(m_lin)[1]/se(m_lin)[1],3), "\n")

  m_q <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data=a, cluster=~MPIO_CDPMP)
  cf <- coef(m_q); s <- se(m_q)
  tp <- turning_point_bc(cf[1],cf[2])
  cat("Quadratic: b1=", round(cf[1],4), " t1=", round(cf[1]/s[1],3),
      " | b2=", round(cf[2],4), " t2=", round(cf[2]/s[2],3), " | TP=", round(tp,3),
      " | share below TP:", round(mean(a$snelect_norm <= tp, na.rm=TRUE)*100,1), "%\n")
  invisible(list(lin=m_lin, q=m_q))
}

run_both(panel_current, "CURRENT DV (undisturbed+degraded+regrowth)")
run_both(panel_strict, "STRICT DV (undisturbed+regrowth, degraded excluded)")
run_both(panel_pristine, "PRISTINE DV (undisturbed ONLY)")

# ---- Does degradation growth itself correlate with democracy? ----
cat("\n\n=== Does municipal 'degraded' share GROWTH correlate with snelect? ===\n")
deg_panel <- raw %>%
  mutate(total_px = c0+c1+c2+c3+c4+c5+c6, degraded_ha = c2*px_area_ha, area_ha_px = total_px*px_area_ha,
         degraded_pct = degraded_ha/area_ha_px*100) %>%
  select(MPIO_CDPMP, year, degraded_pct) %>%
  arrange(MPIO_CDPMP, year) %>% group_by(MPIO_CDPMP) %>%
  mutate(d_degraded_pct = degraded_pct - lag(degraded_pct)) %>% ungroup() %>%
  filter(year >= 2000) %>%
  inner_join(dem_df, by=c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_degraded_pct))

m_deg <- feols(d_degraded_pct ~ snelect_norm | MPIO_CDPMP + year, data=deg_panel, cluster=~MPIO_CDPMP)
cat("d_degraded_pct ~ snelect_norm: b=", round(coef(m_deg)[1],4), " t=", round(coef(m_deg)[1]/se(m_deg)[1],3), "\n")
cat("(negative b = higher democracy associated with SLOWER growth in degraded-forest share)\n")
