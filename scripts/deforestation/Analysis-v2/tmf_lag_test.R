suppressMessages({
  library(tidyverse)
})

# Tests the "construction lag" hypothesis from tmf_gfc_divergence_test.R: does TMF's
# "new Class 3 (Deforested land)" entry for a real clearing event at true year t only
# get confirmed and counted in TMF's series at t+L (L ~ 2-2.5 years), because JRC
# requires ~2.5 years of persistent disruption before promoting degraded -> deforested?
# If so, shifting TMF's series backward by L years (i.e., reading TMF's year-Y value as
# reflecting a true clearing year of Y-L) should track GFC's contemporaneous lossyear
# much more closely than the unshifted (L=0) comparison, especially during the
# 2016-2020 acceleration period -- while the 2021-2023 edge-of-observation shortfall
# (H2, already confirmed broad/universal) should persist regardless of L, since no
# amount of re-dating fixes "not enough time has passed yet to confirm."

tmf <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_lossonly.rds") %>%
  rename(tmf_ha = new_def_ha)
gfc <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/gfc_muni_loss.rds") %>%
  rename(gfc_ha = loss_ha)

lags <- 0:4

cat("=== National annual totals: TMF shifted back by L years, vs. GFC same true-year ===\n\n")

tmf_national <- tmf %>% group_by(year) %>% summarise(tmf_total = sum(tmf_ha), .groups="drop")
gfc_national <- gfc %>% group_by(year) %>% summarise(gfc_total = sum(gfc_ha), .groups="drop")

national_results <- map_dfr(lags, function(L) {
  shifted <- tmf_national %>% mutate(true_year = year - L) %>% select(true_year, tmf_total)
  merged <- inner_join(shifted, gfc_national, by = c("true_year" = "year"))
  merged_all <- merged
  merged_accel <- merged %>% filter(true_year >= 2016, true_year <= 2020)
  tibble(
    lag = L,
    n_years_all = nrow(merged_all),
    r_all = cor(merged_all$tmf_total, merged_all$gfc_total),
    mape_all = mean(abs(merged_all$tmf_total - merged_all$gfc_total) / merged_all$gfc_total) * 100,
    n_years_accel = nrow(merged_accel),
    r_accel = suppressWarnings(cor(merged_accel$tmf_total, merged_accel$gfc_total)),
    mape_accel = mean(abs(merged_accel$tmf_total - merged_accel$gfc_total) / merged_accel$gfc_total) * 100,
    ratio_accel = sum(merged_accel$tmf_total) / sum(merged_accel$gfc_total)
  )
})
print(national_results, width = Inf)

cat("\n\n=== Municipality-year panel: Pearson r between shifted TMF and GFC, by lag ===\n\n")

panel_results <- map_dfr(lags, function(L) {
  shifted <- tmf %>% mutate(true_year = year - L) %>% select(MPIO_CDPMP, true_year, tmf_ha)
  merged <- inner_join(shifted, gfc, by = c("MPIO_CDPMP", "true_year" = "year"))
  merged_accel <- merged %>% filter(true_year >= 2016, true_year <= 2020)
  tibble(
    lag = L,
    n_muniyears_all = nrow(merged),
    r_all = cor(merged$tmf_ha, merged$gfc_ha, use = "complete.obs"),
    n_muniyears_accel = nrow(merged_accel),
    r_accel = suppressWarnings(cor(merged_accel$tmf_ha, merged_accel$gfc_ha, use = "complete.obs"))
  )
})
print(panel_results, width = Inf)

cat("\n\n=== National annual totals table, true-year 2010-2023, for the best lag by r_accel ===\n")
best_L <- panel_results$lag[which.max(panel_results$r_accel)]
cat("Best lag by muni-year r_accel:", best_L, "\n\n")

shifted_best <- tmf_national %>% mutate(true_year = year - best_L) %>% select(true_year, tmf_total)
compare_best <- inner_join(shifted_best, gfc_national, by = c("true_year" = "year")) %>%
  filter(true_year >= 2010) %>%
  mutate(ratio = tmf_total / gfc_total) %>%
  arrange(true_year)
print(compare_best, n = 20)

cat("\n\n=== Same table at lag=0 for direct comparison ===\n")
compare_zero <- tmf_national %>%
  rename(true_year = year) %>%
  inner_join(gfc_national, by = c("true_year" = "year")) %>%
  filter(true_year >= 2010) %>%
  mutate(ratio = tmf_total / gfc_total) %>%
  arrange(true_year)
print(compare_zero, n = 20)
