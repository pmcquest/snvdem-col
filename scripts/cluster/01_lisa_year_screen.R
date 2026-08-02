# ============================================================================
# LISA year screen
#
# Goal: for each year 2000-2023, run a Local Indicators of Spatial Association
# (LISA) analysis on the municipal democracy index (sndem) and tally how many
# municipios fall into each of the four cluster/outlier types. We use this to
# pick a single illustrative year for the paper's LISA map (Kelly asked for a
# year with several different cluster types, or failing that, one with a
# cluster around Bogota).
#
# LISA (Anselin 1995) decomposes the global Moran's I spatial-autocorrelation
# statistic into a value for every individual unit, so instead of asking
# "is democracy spatially clustered in Colombia overall?" we can ask
# "is THIS municipio part of a cluster, and if so, what kind?" for each of
# the 1,122 municipios separately.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)      # reads/handles the municipio polygon shapefile
  library(spdep)   # spatial weights + local Moran's I (spatial dependence stats)
  library(dplyr)   # data wrangling
})

data_dir <- "G:/Shared drives/snvdem/snvdem-col/data"
out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

shp <- st_read(file.path(data_dir, "geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"), quiet = TRUE)
dat <- readRDS(file.path(data_dir, "panel/05_weighting/03_output/snvdem_col_weighted.rds"))

# Keep only the join key + labels we need from the shapefile's ~90 census
# columns, and sort both the polygons and the panel data into the same
# municipio order so that row i of shp always corresponds to row i of any
# per-year vector we build below (localmoran() takes a plain numeric vector,
# not a data frame, so this alignment has to be enforced manually).
shp <- shp[, c("MPIO_CDPMP", "DPTO_CCDGO", "MPIO_CNMBR")]
shp <- shp[order(shp$MPIO_CDPMP), ]
stopifnot(all(shp$MPIO_CDPMP %in% dat$MPIO_CDPMP), all(dat$MPIO_CDPMP %in% shp$MPIO_CDPMP))

# --- Spatial weights matrix (W) ---------------------------------------------
# poly2nb() finds, for every municipio, which other municipios share a
# boundary or corner ("queen" contiguity -- a shared corner counts, unlike
# "rook" contiguity which requires a shared edge). This defines what "nearby"
# means for the whole analysis; it's built once because the geometry doesn't
# change across years, only the sndem values do.
#
# San Andres (88001) and Providencia (88564) are islands with no land
# neighbors, so poly2nb() gives them an empty neighbor set. zero.policy=TRUE
# tells spdep to allow that (rather than error out) -- those two municipios
# simply can't be tested for local clustering and will show up as NA / "No
# neighbors" downstream.
#
# nb2listw() converts the raw neighbor lists into weights. style="W" row-
# standardizes them, i.e. each municipio's neighbor values are averaged
# (weights sum to 1 per row) rather than summed. This is the standard choice
# for LISA because it keeps the "spatial lag" (see below) on the same scale
# as the original variable regardless of how many neighbors a municipio has.
nb <- poly2nb(shp, queen = TRUE, row.names = shp$MPIO_CDPMP)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
saveRDS(list(shp = shp, dat = dat, nb = nb, lw = lw), file.path(out_dir, "lisa_weights.rds"))

bogota_code <- "11001"
years <- sort(unique(dat$year))
rows <- list()

for (yr in years) {
  # Pull this year's sndem values in the same municipio order as shp/lw.
  d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
  merged <- shp %>% st_drop_geometry() %>% left_join(d_yr, by = "MPIO_CDPMP")
  merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
  x <- merged$sndem

  # --- Local Moran's I ---------------------------------------------------
  # localmoran() returns one Ii statistic per municipio: roughly, "how much
  # does this municipio's value covary with its neighbors' values, relative
  # to what we'd expect if sndem were randomly scattered across the map?"
  # A large positive Ii means the municipio and its neighbors are unusually
  # similar (both high or both low); Ii near zero means no local pattern;
  # a large negative Ii means the municipio is unusually different from its
  # neighbors. Pr(z != E(Ii)) is the p-value for that departure from
  # spatial randomness (via a permutation-based test under the hood).
  lmres <- localmoran(x, lw, zero.policy = TRUE, na.action = na.exclude)

  # --- Multiple-testing correction ----------------------------------------
  # We are running 1,122 significance tests simultaneously (one per
  # municipio). At an uncorrected p<0.05 threshold we'd expect ~56 "false
  # positive" clusters by chance alone, which is exactly the "discarding
  # data" concern Michael raised about LISA maps. Benjamini-Hochberg (BH)
  # controls the false discovery rate across all 1,122 tests at once, so
  # what survives is a more defensible (if visually sparser) set of
  # clusters. See 02_lisa_raw_vs_fdr_comparison.R for how much this changes
  # the picture relative to using raw p-values.
  p_fdr <- p.adjust(lmres[, "Pr(z != E(Ii))"], method = "BH")

  # --- Classifying each municipio into a cluster/outlier type -------------
  # The four LISA categories are defined by two things: (1) is this
  # municipio's OWN value above or below the national average, and (2) is
  # the AVERAGE of its neighbors' values ("spatial lag") above or below the
  # national average. We standardize both to z-scores (mean 0) purely so
  # "above/below average" is just "positive/negative" -- the z-scoring
  # doesn't change which municipios end up in which category, it just makes
  # the sign comparison below read cleanly.
  #   High-High: municipio is high, and so are its neighbors  -> a cluster
  #              of relatively democratic municipios (a "hot spot")
  #   Low-Low:   municipio is low, and so are its neighbors    -> a cluster
  #              of relatively undemocratic municipios (a "cold spot")
  #   High-Low:  municipio is high, but its neighbors are low  -> a
  #              democratic outlier surrounded by less democratic neighbors
  #   Low-High:  municipio is low, but its neighbors are high  -> an
  #              undemocratic outlier surrounded by more democratic neighbors
  # A municipio only gets one of these labels if its local Moran's I was
  # ALSO statistically significant (sig, from the FDR-corrected p-value
  # above) -- otherwise it's "Not significant," meaning we can't
  # distinguish its neighborhood pattern from spatial randomness.
  z_x <- as.numeric(scale(x))
  z_lag <- lag.listw(lw, z_x, zero.policy = TRUE, NAOK = TRUE)

  cluster <- rep("Not significant", length(x))
  sig <- !is.na(p_fdr) & p_fdr < 0.05
  cluster[sig & z_x > 0 & z_lag > 0] <- "High-High"
  cluster[sig & z_x < 0 & z_lag < 0] <- "Low-Low"
  cluster[sig & z_x > 0 & z_lag < 0] <- "High-Low"
  cluster[sig & z_x < 0 & z_lag > 0] <- "Low-High"
  cluster[card(nb) == 0] <- "No neighbors"  # San Andres / Providencia override

  # Tally this year's results: how many of each cluster type, and whether
  # Bogota itself landed in a cluster (it's High-High in every year we
  # checked, which is worth noting in the paper regardless of which single
  # year we illustrate).
  tab <- table(factor(cluster, levels = c(
    "High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors"
  )))
  n_types <- sum(tab[c("High-High", "Low-Low", "High-Low", "Low-High")] > 0)
  bog_cluster <- cluster[which(shp$MPIO_CDPMP == bogota_code)]

  rows[[as.character(yr)]] <- data.frame(
    year = yr, n_types = n_types, n_sig_total = sum(tab[1:4]),
    HH = tab["High-High"], LL = tab["Low-Low"], HL = tab["High-Low"], LH = tab["Low-High"],
    bogota_cluster = bog_cluster
  )
}

summary_df <- do.call(rbind, rows)
rownames(summary_df) <- NULL
write.csv(summary_df, file.path(out_dir, "lisa_year_screen_summary.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "lisa_year_screen_summary.csv"), "\n")
# Rank years by cluster-type diversity, then by total significant municipios,
# to surface the best illustrative-year candidates first.
print(summary_df[order(-summary_df$n_types, -summary_df$n_sig_total), ])
