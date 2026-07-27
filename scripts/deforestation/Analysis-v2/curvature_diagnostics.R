# ====================================================================== #
# Two follow-up tests on why GFC/MapBiomas show centrally-located
# curvature while TMF does not (Section 4.2/4.3):
#
# Test A: is GFC's/MapBiomas's two-way-FE quadratic term driven by a
#   small number of influential/outlier municipality-years, vs TMF's?
#   (Two-way FE already nets out cross-sectional department sorting --
#   the department-FE-absorption test in Appendix A was only valid for
#   the year-only-FE model, which lacks municipality FE. For a two-way
#   FE result, an outlier/leverage check is the right analogous test.)
#
# Test B: does TMF's temporal-persistence classification actually
#   produce a smoother (lower year-to-year variance) d_forest_pct series
#   than GFC/MapBiomas, consistent with it filtering transient noise?
# ====================================================================== #

suppressMessages({ library(dplyr); library(fixest) })

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
analysis_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
turning_point_bc <- function(b1, b2) unname(-b1 / (2 * b2))

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect) %>% distinct(MPIO_CDPMP, year, .keep_all = TRUE)

tmf <- readRDS(file.path(out_dir, "tmf_muni_cover.rds")) %>%
  inner_join(dem_df, by=c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect))
gfc <- readRDS(file.path(out_dir, "gfc_muni_cover.rds")) %>%
  inner_join(dem_df, by=c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect))
mb  <- readRDS(file.path(analysis_dir, "mb_muni_cover_corrected.rds")) %>% filter(year >= 2001) %>%
  inner_join(dem_df, by=c("MPIO_CDPMP","year")) %>% mutate(snelect_norm = norm01(snelect))

run_q <- function(df, label, winsorize = NULL, drop_pct = NULL) {
  d <- df %>% filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1))
  if (!is.null(winsorize)) {
    lo <- quantile(d$d_forest_pct, winsorize, na.rm=TRUE); hi <- quantile(d$d_forest_pct, 1-winsorize, na.rm=TRUE)
    d$d_forest_pct <- pmin(pmax(d$d_forest_pct, lo), hi)
  }
  if (!is.null(drop_pct)) {
    lo <- quantile(d$d_forest_pct, drop_pct, na.rm=TRUE); hi <- quantile(d$d_forest_pct, 1-drop_pct, na.rm=TRUE)
    d <- d %>% filter(d_forest_pct >= lo, d_forest_pct <= hi)
  }
  m <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data=d, cluster=~MPIO_CDPMP)
  cf <- coef(m); s <- se(m)
  cat(sprintf("%-40s n=%6d  b2=%7.4f  t2=%6.2f  TP=%7.3f\n", label, nrow(d), cf[2], cf[2]/s[2], turning_point_bc(cf[1],cf[2])))
  invisible(m)
}

cat("=== TEST A: robustness of quadratic term to influential observations ===\n\n")
cat("--- TMF ---\n")
run_q(tmf, "TMF, raw")
run_q(tmf, "TMF, Winsorized at 1%/99%", winsorize=0.01)
run_q(tmf, "TMF, drop top/bottom 1%", drop_pct=0.01)

cat("\n--- GFC ---\n")
run_q(gfc, "GFC, raw")
run_q(gfc, "GFC, Winsorized at 1%/99%", winsorize=0.01)
run_q(gfc, "GFC, drop top/bottom 1%", drop_pct=0.01)

cat("\n--- MapBiomas (corrected) ---\n")
run_q(mb, "MapBiomas, raw")
run_q(mb, "MapBiomas, Winsorized at 1%/99%", winsorize=0.01)
run_q(mb, "MapBiomas, drop top/bottom 1%", drop_pct=0.01)

cat("\n\n=== TEST B: how much year-to-year noise does each source have? ===\n")
noise_summary <- function(df, label) {
  s <- df %>% filter(!is.na(d_forest_pct)) %>% group_by(MPIO_CDPMP) %>%
    summarise(sd_d = sd(d_forest_pct, na.rm=TRUE), .groups="drop")
  cat(sprintf("%-15s median muni-level SD(d_forest_pct) = %.3f | mean = %.3f\n",
              label, median(s$sd_d, na.rm=TRUE), mean(s$sd_d, na.rm=TRUE)))
}
noise_summary(tmf, "TMF")
noise_summary(gfc, "GFC")
noise_summary(mb, "MapBiomas")

cat("\nNational annual d_forest_pct range (max - min across years), each source:\n")
nat_range <- function(df, label) {
  nat <- df %>% group_by(year) %>% summarise(mean_d = mean(d_forest_pct, na.rm=TRUE), .groups="drop")
  cat(sprintf("%-15s range = %.3f (min %.3f, max %.3f)\n", label,
              max(nat$mean_d,na.rm=TRUE)-min(nat$mean_d,na.rm=TRUE), min(nat$mean_d,na.rm=TRUE), max(nat$mean_d,na.rm=TRUE)))
}
nat_range(tmf, "TMF")
nat_range(gfc, "GFC")
nat_range(mb, "MapBiomas")
