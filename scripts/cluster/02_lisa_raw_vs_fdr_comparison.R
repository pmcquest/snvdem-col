# ============================================================================
# Raw p<0.05 vs. FDR-corrected LISA classifications, by year
#
# Goal: quantify how much the Benjamini-Hochberg (BH) false-discovery-rate
# correction (used in 01_lisa_year_screen.R and the final map) shrinks the
# set of "significant" clusters relative to just using the local Moran's I
# p-value at face value. Testing 1,122 municipios at once means an uncorrected
# p<0.05 threshold will flag a meaningful number of clusters by chance alone
# (~56 municipios, in expectation, if sndem had no real spatial pattern at
# all) -- this script makes that cost visible year by year so we can decide
# how to justify the choice in the paper.
#
# Reuses the weights matrix (lw/nb) and shapefile/panel data (shp/dat) saved
# by 01_lisa_year_screen.R rather than rebuilding them.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(dplyr)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; dat <- obj$dat; lw <- obj$lw; nb <- obj$nb

# Same High-High/Low-Low/High-Low/Low-High logic as 01_lisa_year_screen.R,
# but pulled into a function since we need to apply it twice per year (once
# with raw p-values, once with FDR-corrected p-values).
classify <- function(p, z_x, z_lag, alpha) {
  cl <- rep("Not significant", length(p))
  sig <- !is.na(p) & p < alpha
  cl[sig & z_x > 0 & z_lag > 0] <- "High-High"
  cl[sig & z_x < 0 & z_lag < 0] <- "Low-Low"
  cl[sig & z_x > 0 & z_lag < 0] <- "High-Low"
  cl[sig & z_x < 0 & z_lag > 0] <- "Low-High"
  cl
}

years <- sort(unique(dat$year))
rows <- list()

for (yr in years) {
  d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
  merged <- shp %>% st_drop_geometry() %>% left_join(d_yr, by = "MPIO_CDPMP")
  merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
  x <- merged$sndem

  lmres <- localmoran(x, lw, zero.policy = TRUE, na.action = na.exclude)
  p_raw <- lmres[, "Pr(z != E(Ii))"]
  p_fdr <- p.adjust(p_raw, method = "BH")

  z_x <- as.numeric(scale(x))
  z_lag <- lag.listw(lw, z_x, zero.policy = TRUE, NAOK = TRUE)
  z_lag[card(nb) == 0] <- NA

  # Classify the SAME underlying pattern (same z_x, z_lag) twice, differing
  # only in which p-value threshold decides "significant."
  cl_raw <- classify(p_raw, z_x, z_lag, 0.05); cl_raw[card(nb) == 0] <- "No neighbors"
  cl_fdr <- classify(p_fdr, z_x, z_lag, 0.05); cl_fdr[card(nb) == 0] <- "No neighbors"

  lv <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors")
  tab_raw <- table(factor(cl_raw, levels = lv))
  tab_fdr <- table(factor(cl_fdr, levels = lv))

  rows[[as.character(yr)]] <- data.frame(
    year = yr,
    raw_sig = sum(tab_raw[1:4]), raw_HH = tab_raw["High-High"], raw_LL = tab_raw["Low-Low"],
    raw_HL = tab_raw["High-Low"], raw_LH = tab_raw["Low-High"],
    fdr_sig = sum(tab_fdr[1:4]), fdr_HH = tab_fdr["High-High"], fdr_LL = tab_fdr["Low-Low"],
    fdr_HL = tab_fdr["High-Low"], fdr_LH = tab_fdr["Low-High"]
  )
}

cmp <- do.call(rbind, rows)
rownames(cmp) <- NULL
cmp$pct_of_1122_raw <- round(100 * cmp$raw_sig / 1122, 1)
cmp$pct_of_1122_fdr <- round(100 * cmp$fdr_sig / 1122, 1)

write.csv(cmp, file.path(out_dir, "lisa_raw_vs_fdr_comparison.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "lisa_raw_vs_fdr_comparison.csv"), "\n")
print(cmp)
