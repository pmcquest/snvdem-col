suppressMessages({
  library(tidyverse)
})

# Tests two competing (non-exclusive) explanations for the national TMF-vs-GFC
# loss-only divergence found in build_tmf_lossonly.R:
#   (H1) 2016-2017 gap is a GFC-side fire/burn-misclassification artifact
#        (Vancutsem et al. 2021's own explanation) -> should be spatially
#        CONCENTRATED in specific fire-prone departments, not uniform.
#   (H2) 2021-2023 gap is a TMF-side observation-edge effect (insufficient
#        persistence time to confirm recent disturbances) -> should be
#        spatially DIFFUSE/uniform across nearly all departments, since it is
#        a structural property of the classifier, not a localized event.

tmf <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_lossonly.rds")
gfc <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/gfc_muni_loss.rds")

cat("tmf cols:", paste(names(tmf), collapse=", "), "\n")
cat("gfc cols:", paste(names(gfc), collapse=", "), "\n")

panel <- inner_join(
  tmf %>% rename(tmf_ha = new_def_ha),
  gfc %>% rename(gfc_ha = loss_ha),
  by = c("MPIO_CDPMP", "year")
) %>%
  mutate(
    dpto = substr(MPIO_CDPMP, 1, 2),
    gap = gfc_ha - tmf_ha,          # positive = GFC exceeds TMF
    ratio = tmf_ha / gfc_ha
  )

cat("\nMerged panel rows:", nrow(panel), "| years:", paste(range(panel$year), collapse="-"), "\n")

# --- H1: is the 2016-2017 GFC-excess concentrated in specific departments? ---
cat("\n\n=== H1: 2016-2017 GFC-excess (gap = GFC - TMF), department concentration ===\n")

conc_check <- function(df, yrs, label) {
  sub <- df %>% filter(year %in% yrs, gap > 0)
  by_dpto <- sub %>%
    group_by(dpto) %>%
    summarise(dpto_gap = sum(gap), .groups = "drop") %>%
    arrange(desc(dpto_gap)) %>%
    mutate(cum_share = cumsum(dpto_gap) / sum(dpto_gap))
  total_gap <- sum(by_dpto$dpto_gap)
  top5_share <- sum(by_dpto$dpto_gap[1:5]) / total_gap
  n_dptos <- nrow(by_dpto)
  n_for_80pct <- which(by_dpto$cum_share >= 0.8)[1]
  cat(sprintf("\n[%s] total positive gap: %s ha across %d departments\n",
              label, format(round(total_gap), big.mark=","), n_dptos))
  cat(sprintf("  Top-5 departments' share of total gap: %.1f%%\n", top5_share*100))
  cat(sprintf("  Departments needed to reach 80%% of gap: %d (of %d)\n", n_for_80pct, n_dptos))
  cat("  Top 8 departments by gap (DIVIPOLA dpto code):\n")
  print(head(by_dpto, 8))
  invisible(by_dpto)
}

top_2016_17 <- conc_check(panel, 2016:2017, "2016-2017")

# Baseline comparison: a "normal" two-year window not flagged by either hypothesis
top_baseline <- conc_check(panel, 2012:2013, "2012-2013 baseline")

# --- H2: is the 2021-2023 TMF-shortfall diffuse across departments? ---
cat("\n\n=== H2: 2021-2023 TMF-shortfall, breadth across departments/municipalities ===\n")

diffuse_check <- function(df, yrs, label) {
  sub <- df %>% filter(year %in% yrs, gfc_ha > 5)  # drop near-zero-GFC muni-years, ratio unstable
  by_dpto <- sub %>%
    group_by(dpto) %>%
    summarise(tmf_total = sum(tmf_ha), gfc_total = sum(gfc_ha), .groups = "drop") %>%
    mutate(dpto_ratio = tmf_total / gfc_total)
  n_dptos <- nrow(by_dpto)
  n_below_half <- sum(by_dpto$dpto_ratio < 0.5, na.rm = TRUE)
  n_below_08 <- sum(by_dpto$dpto_ratio < 0.8, na.rm = TRUE)
  cat(sprintf("\n[%s] %d departments with usable data\n", label, n_dptos))
  cat(sprintf("  Departments with TMF/GFC ratio < 0.5: %d (%.0f%%)\n", n_below_half, 100*n_below_half/n_dptos))
  cat(sprintf("  Departments with TMF/GFC ratio < 0.8: %d (%.0f%%)\n", n_below_08, 100*n_below_08/n_dptos))
  cat(sprintf("  Median department ratio: %.2f | IQR: [%.2f, %.2f]\n",
              median(by_dpto$dpto_ratio, na.rm=TRUE),
              quantile(by_dpto$dpto_ratio, 0.25, na.rm=TRUE),
              quantile(by_dpto$dpto_ratio, 0.75, na.rm=TRUE)))
  invisible(by_dpto)
}

diffuse_2021_23 <- diffuse_check(panel, 2021:2023, "2021-2023")
diffuse_baseline <- diffuse_check(panel, 2012:2013, "2012-2013 baseline")
diffuse_2016_17  <- diffuse_check(panel, 2016:2017, "2016-2017 (for contrast)")

cat("\n\nSaving full department-year panel for inspection.\n")
saveRDS(panel, "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_gfc_divergence_panel.rds")
