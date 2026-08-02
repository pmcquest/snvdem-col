# ============================================================================
# Full LISA panel, 2000-2023 -- exploratory pass
#
# The cluster-size time series (07) shows the HH/LL bands moving roughly in
# lockstep -- informative, but not a striking finding on its own, and Patrick
# flagged that lay readers and Colombianists alike may find it underwhelming.
# Before concluding there's nothing more interesting in the LISA panel than
# an aggregate count, this script computes the full per-municipio-year
# classification (not just the year-by-year tallies from 01) and asks two
# questions that a tally can't answer:
#   1. Are there specific municipios that are persistently HL/LH outliers
#      (an anomaly against its neighbors, not just for one year but for
#      years running)? A persistent outlier is a much stronger "worth a
#      closer look" claim than a one-year blip.
#   2. Which municipios switch LISA category most often across 2000-2023?
#      High switch-frequency municipios are the ones the *classification
#      itself is unstable for* -- via boundary/threshold effects, real
#      volatility, or both -- and that's a genuinely different claim than
#      "the two big clusters grow and shrink together."
# Purely exploratory: this does not build a case-selection pipeline, just
# summarizes what's in the panel so we can decide whether there's a stronger
# finding than the aggregate time series before writing the section.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(dplyr)
  library(tidyr)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; dat <- obj$dat; lw <- obj$lw; nb <- obj$nb

years <- sort(unique(dat$year))
rows <- list()

for (yr in years) {
  d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
  merged <- shp %>% st_drop_geometry() %>% left_join(d_yr, by = "MPIO_CDPMP")
  merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
  x <- merged$sndem

  lmres <- localmoran(x, lw, zero.policy = TRUE, na.action = na.exclude)
  p_fdr <- p.adjust(lmres[, "Pr(z != E(Ii))"], method = "BH")

  z_x <- as.numeric(scale(x))
  z_lag <- lag.listw(lw, z_x, zero.policy = TRUE, NAOK = TRUE)

  cluster <- rep("Not significant", length(x))
  sig <- !is.na(p_fdr) & p_fdr < 0.05
  cluster[sig & z_x > 0 & z_lag > 0] <- "High-High"
  cluster[sig & z_x < 0 & z_lag < 0] <- "Low-Low"
  cluster[sig & z_x > 0 & z_lag < 0] <- "High-Low"
  cluster[sig & z_x < 0 & z_lag > 0] <- "Low-High"
  cluster[card(nb) == 0] <- "No neighbors"

  rows[[as.character(yr)]] <- data.frame(
    MPIO_CDPMP = shp$MPIO_CDPMP, DPTO_CCDGO = shp$DPTO_CCDGO, MPIO_CNMBR = shp$MPIO_CNMBR,
    year = yr, sndem = x, local_I = lmres[, "Ii"], cluster = cluster
  )
}

panel <- do.call(rbind, rows)
rownames(panel) <- NULL
write.csv(panel, file.path(out_dir, "lisa_full_panel.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "lisa_full_panel.csv"), "--", nrow(panel), "rows\n\n")

# --- Question 1: persistent HL/LH outliers -----------------------------------
outlier_counts <- panel %>%
  filter(cluster %in% c("High-Low", "Low-High")) %>%
  count(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO, cluster, name = "n_years") %>%
  arrange(desc(n_years))

cat("=== Municipios most often classified as an HL/LH outlier (2000-2023, max 24) ===\n")
print(head(outlier_counts, 20))

# --- Question 2: switch frequency ---------------------------------------------
# "Switches" = number of times a municipio's category differs from the
# previous year in the panel (0-23 possible). High switch count = unstable
# classification for that municipio, whatever the underlying cause.
switch_counts <- panel %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO) %>%
  summarise(
    n_switches = sum(cluster != lag(cluster), na.rm = TRUE),
    categories_seen = n_distinct(cluster),
    modal_category = names(sort(table(cluster), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(n_switches))

cat("\n=== Municipios with the most LISA category switches, 2000-2023 (max 23) ===\n")
print(head(switch_counts, 20))

cat("\n=== Distribution of switch counts across all municipios ===\n")
print(table(switch_counts$n_switches))

# --- Cross-check: does Low-Low persistence track known conflict departments --
# Rough sanity check, not a rigorous test: departments historically associated
# with sustained armed-group presence (Choco 27, Cauca 19, Narino 52, Norte de
# Santander/Catatumbo 54, Meta 50, Arauca 81, Caqueta 18, Putumayo 86) vs.
# everywhere else, compared on the share of years spent Low-Low.
conflict_depts <- c("27", "19", "52", "54", "50", "81", "18", "86")
ll_share <- panel %>%
  filter(cluster != "No neighbors") %>%
  group_by(MPIO_CDPMP, DPTO_CCDGO) %>%
  summarise(share_ll = mean(cluster == "Low-Low"), .groups = "drop") %>%
  mutate(conflict_dept = DPTO_CCDGO %in% conflict_depts)

cat("\n=== Mean share of years classified Low-Low: conflict-associated depts vs. rest ===\n")
print(ll_share %>% group_by(conflict_dept) %>% summarise(mean_share_ll = mean(share_ll), n = n()))
