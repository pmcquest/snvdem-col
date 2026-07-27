suppressMessages({
  library(tidyverse)
  library(fixest)
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
  mutate(snelect_norm = norm01(snelect))

cat("=== 1. National totals: is forest stock trending up or down, 2000-2023? ===\n")
national_trend <- analysis_tmf %>%
  group_by(year) %>%
  summarise(
    total_forest_ha  = sum(forest_ha, na.rm = TRUE),
    mean_d_forest_pct = mean(d_forest_pct, na.rm = TRUE),
    mean_snelect_norm = mean(snelect_norm, na.rm = TRUE),
    share_gain        = mean(d_forest_pct > 0, na.rm = TRUE),
    share_loss        = mean(d_forest_pct < 0, na.rm = TRUE),
    .groups = "drop"
  )
print(national_trend, n = 30)
cat("\nNet change in total forest stock, 2000 -> 2023 (ha):",
    national_trend$total_forest_ha[national_trend$year == 2023] -
    national_trend$total_forest_ha[national_trend$year == 2000], "\n")
cat("As % of 2000 stock:",
    100 * (national_trend$total_forest_ha[national_trend$year == 2023] -
           national_trend$total_forest_ha[national_trend$year == 2000]) /
      national_trend$total_forest_ha[national_trend$year == 2000], "%\n")

cat("\n=== 2. Share of muni-years with net GAIN vs. LOSS, by snelect decile ===\n")
analysis_tmf %>%
  mutate(decile = ntile(snelect_norm, 10)) %>%
  group_by(decile) %>%
  summarise(
    snelect_mean = mean(snelect_norm, na.rm = TRUE),
    share_gain   = mean(d_forest_pct > 0, na.rm = TRUE),
    share_loss   = mean(d_forest_pct < 0, na.rm = TRUE),
    share_zero   = mean(d_forest_pct == 0, na.rm = TRUE),
    mean_d       = mean(d_forest_pct, na.rm = TRUE),
    n            = n(),
    .groups = "drop"
  ) %>%
  print(n = 10)

cat("\n=== 3. Turning point by time period -- is it drifting? ===\n")
turning_point <- function(model) {
  b <- coef(model)[1]; c <- coef(model)[2]
  unname(-b / (2 * c))
}

periods <- list(
  "2000-2005" = 2000:2005,
  "2006-2011" = 2006:2011,
  "2012-2017" = 2012:2017,
  "2018-2023" = 2018:2023
)

period_results <- map_dfr(names(periods), function(pname) {
  yrs <- periods[[pname]]
  sub <- analysis_tmf %>% filter(year %in% yrs)
  # re-normalize snelect WITHIN this period's own observed range, so the
  # turning point is interpretable on the same 0-1 scale used throughout
  # the main script (0 = least democratic *in this period*, 1 = most) --
  # NOT the full-panel normalization, which would make period comparisons
  # about the wrong thing (whether that period's low end approached the
  # full panel's historical low, not the within-period curve shape).
  sub <- sub %>% mutate(snelect_norm_p = norm01(snelect))
  m <- feols(d_forest_pct ~ snelect_norm_p + I(snelect_norm_p^2) + forest_pct_lag1 | MPIO_CDPMP + year,
             data = sub, cluster = ~MPIO_CDPMP)
  tp <- turning_point(m)
  cf <- coef(m)
  data.frame(
    period = pname,
    n_years = length(yrs),
    mean_snelect_full_scale = mean(sub$snelect, na.rm = TRUE),
    b1_linear = unname(cf[1]),
    b2_quadratic = unname(cf[2]),
    turning_point_within_period = tp,
    n_obs = nrow(sub)
  )
})
print(period_results, digits = 4)

cat("\n=== 3b. Same, but using the FULL-PANEL (2000-2023) snelect normalization ===\n")
cat("(so turning points are directly comparable across periods on ONE fixed scale,\n")
cat(" complementing 3's within-period-relative version above)\n\n")

period_results_fixedscale <- map_dfr(names(periods), function(pname) {
  yrs <- periods[[pname]]
  sub <- analysis_tmf %>% filter(year %in% yrs)   # snelect_norm already full-panel-normalized
  m <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
             data = sub, cluster = ~MPIO_CDPMP)
  tp <- turning_point(m)
  cf <- coef(m)
  data.frame(
    period = pname,
    b1_linear = unname(cf[1]),
    b2_quadratic = unname(cf[2]),
    turning_point_full_scale = tp,
    n_obs = nrow(sub)
  )
})
print(period_results_fixedscale, digits = 4)

saveRDS(list(national_trend = national_trend,
             period_results = period_results,
             period_results_fixedscale = period_results_fixedscale),
        file.path(out_dir, "tmf_turning_point_drift_check.rds"))
