suppressMessages({
  library(tidyverse)
  library(fixest)
  library(plm)
})
options(width = 200)

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

run_linear <- function(df, spec, label) {
  if (spec == "twoway") {
    m <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data = df, cluster = ~MPIO_CDPMP)
    b <- coef(m)[1]; s <- se(m)[1]
  } else if (spec == "yearonly") {
    m <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | year, data = df, cluster = ~MPIO_CDPMP)
    b <- coef(m)[1]; s <- se(m)[1]
  } else {
    pdf <- pdata.frame(df, index = c("MPIO_CDPMP", "year"))
    m <- plm(d_forest_pct ~ snelect_norm + forest_pct_lag1 + factor(year), data = pdf, model = "random")
    cf <- coef(m)
    b <- cf["snelect_norm"]
    s <- sqrt(diag(vcovHC(m, cluster = "group")))["snelect_norm"]
  }
  cat(sprintf("%-10s %-9s b=%8.4f  se=%.4f  t=%6.2f  n=%d\n", label, spec, b, s, b / s, nrow(df)))
}

cat("Plain LINEAR specification (no quadratic term): snelect_norm coefficient by period x spec\n\n")
for (spec in c("twoway", "yearonly", "re")) run_linear(analysis_tmf, spec, "FULL")
cat("\n")
for (spec in c("twoway", "yearonly", "re")) run_linear(filter(analysis_tmf, year < 2016), spec, "PRE-2016")
cat("\n")
for (spec in c("twoway", "yearonly", "re")) run_linear(filter(analysis_tmf, year >= 2016), spec, "POST-2016")

cat("\n-- Same, but also for GFC and MapBiomas full panels, for comparison (if files exist) --\n")
for (fname in c("gfc_muni_cover.rds", "mb_muni_cover.rds")) {
  fpath <- file.path(out_dir, fname)
  if (!file.exists(fpath)) { cat(fname, "not found, skipping\n"); next }
  cov <- readRDS(fpath)
  a <- cov %>% inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
    mutate(snelect_norm = norm01(snelect)) %>%
    filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm))
  cat("\n", fname, "\n")
  for (spec in c("twoway", "yearonly", "re")) run_linear(a, spec, "FULL")
}
