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

turning_point <- function(model) {
  b <- coef(model)[1]; c <- coef(model)[2]
  unname(-b / (2 * c))
}

for (label in c("Pre-2016 (2000-2015)", "Post-2016 (2016-2023)")) {
  sub <- if (label == "Pre-2016 (2000-2015)") filter(analysis_tmf, year < 2016) else filter(analysis_tmf, year >= 2016)
  m_full  <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                    data = sub, cluster = ~MPIO_CDPMP)
  cf <- coef(m_full); se <- se(m_full)
  cat(sprintf("\n=== %s (n=%d) ===\n", label, nrow(sub)))
  cat(sprintf("b1 (linear)    = %.4f  (se %.4f)\n", cf[1], se[1]))
  cat(sprintf("b2 (quadratic) = %.4f  (se %.4f)\n", cf[2], se[2]))
  cat(sprintf("Shape: %s\n", if (cf[2] > 0) "U-shape (upward-opening, real minimum)" else "Inverted-U (downward-opening, this is a MAXIMUM not a minimum)"))
  cat(sprintf("Turning point (full-panel snelect scale) = %.3f\n", turning_point(m_full)))
}
