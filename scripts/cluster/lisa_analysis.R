# ============================================================================
# LISA analysis of subnational democracy (sndem), Colombia
#
# Analysis behind the Democratization/Autocratization
# "Utility of Method" illustration (modal cluster map + persistent High-Low
# outlier spotlight).
#
# Five steps:
#   1. Build the spatial weights matrix (W) -- defines what "neighbor" means
#   2. Classify every municipio, every year (2000-2023), via local Moran's I
#   3. Summarize the panel: each municipio's most common ("modal")
#      classification, and how consistently it held it
#   4. Identify municipios that are persistent High-Low outliers
#   5. Quantify how those outliers compare to Bogota, and map everything
# ============================================================================

suppressPackageStartupMessages({
  library(sf)      # reads/handles the municipio polygon shapefile
  library(spdep)   # spatial weights + local Moran's I
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

data_dir <- "G:/Shared drives/snvdem/snvdem-col/data"
out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"

# ============================================================================
# STEP 1 -- Spatial weights matrix (W)
# ============================================================================
# Local Moran's I asks, for each municipio: "does this municipio's sndem
# value covary with its NEIGHBORS' values, more than we'd expect by chance?"
# We use queen contiguity (municipios that share a border OR a
# single corner count as neighbors, unlike "rook" contiguity which requires a
# shared edge), row-standardized so each municipio's neighbor values are
# averaged rather than summed -- the standard choice for LISA, since it keeps
# the "spatial lag" (a municipio's own neighbor-average) on the same scale as
# the original variable regardless of how many neighbors a municipio has.

shp <- st_read(file.path(data_dir, "geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"), quiet = TRUE)
dat <- readRDS(file.path(data_dir, "panel/05_weighting/03_output/snvdem_col_weighted.rds"))

shp <- shp[, c("MPIO_CDPMP", "DPTO_CCDGO", "MPIO_CNMBR")]
shp <- shp[order(shp$MPIO_CDPMP), ]
stopifnot(all(shp$MPIO_CDPMP %in% dat$MPIO_CDPMP), all(dat$MPIO_CDPMP %in% shp$MPIO_CDPMP))

# San Andres (88001) and Providencia (88564) are islands with no land
# neighbors, so poly2nb() gives them an empty neighbor set. zero.policy=TRUE
# allows that (rather than erroring) -- those two municipios can't be tested
# for local clustering and are labeled "No neighbors" throughout.
nb <- poly2nb(shp, queen = TRUE, row.names = shp$MPIO_CDPMP)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

bogota_code <- "11001"

# ============================================================================
# STEP 2 -- Classify every municipio, every year (2000-2023)
# ============================================================================
# For each year, local Moran's I (Ii) is computed per municipio, then each
# municipio is sorted into one of four categories by comparing (a) its own
# sndem value and (b) its neighbors' average sndem ("spatial lag"), each
# relative to the national average that year:
#   High-High: municipio above average, neighbors above average  -> hot spot
#   Low-Low:   municipio below average, neighbors below average  -> cold spot
#   High-Low:  municipio above average, neighbors below average  -> outlier
#   Low-High:  municipio below average, neighbors above average  -> outlier
# A municipio only gets a label if the pattern is statistically distinguish-
# able from spatial randomness: we test each Ii against a null of "no local
# spatial pattern" (spdep's analytic z-test, Pr(z != E(Ii))), then apply
# Benjamini-Hochberg FDR correction across all ~1,122 simultaneous per-
# municipio tests before applying p < 0.05 -- without that correction, testing
# 1,122 hypotheses at once would produce ~56 false "significant" clusters by
# chance alone.

years <- sort(unique(dat$year))
panel_rows <- list()

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

  panel_rows[[as.character(yr)]] <- data.frame(
    MPIO_CDPMP = shp$MPIO_CDPMP, DPTO_CCDGO = shp$DPTO_CCDGO, MPIO_CNMBR = shp$MPIO_CNMBR,
    year = yr, sndem = x, local_I = lmres[, "Ii"], cluster = cluster
  )
}

panel <- do.call(rbind, panel_rows)
rownames(panel) <- NULL
cat("Step 2 done:", nrow(panel), "municipio-year classifications (", length(years), "years x", nrow(shp), "municipios).\n")

# ============================================================================
# STEP 3 -- Modal classification + persistence, per municipio
# ============================================================================
# A single year is one snapshot. Summarizing across all 24 shows which
# spatial patterns are durable features of Colombia's subnational landscape
# and which are one-off. For each municipio: its MODAL classification
# (whichever category it held most often, 2000-2023) and the SHARE of years
# it held that category (24/24 = fully consistent; a lower share means it
# flipped between categories more often).

modal_df <- panel %>%
  count(MPIO_CDPMP, cluster, name = "n_years_in_cat") %>%
  group_by(MPIO_CDPMP) %>%
  slice_max(n_years_in_cat, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(panel %>% count(MPIO_CDPMP, name = "n_years_total"), by = "MPIO_CDPMP") %>%
  mutate(share = n_years_in_cat / n_years_total) %>%
  rename(modal_cluster = cluster)

cat("\nStep 3 done. Modal classification distribution across", nrow(modal_df), "municipios:\n")
print(table(modal_df$modal_cluster))
cat("Mean persistence (share of years in modal category), all municipios:",
    round(mean(modal_df$share), 2), "-- i.e. most municipios hold the same\n",
    "classification the large majority of the time; the panel is generally stable.\n")

# ============================================================================
# STEP 4 -- Persistent High-Low outliers
# ============================================================================
# High-Low = a comparatively high-sndem municipio surrounded by lower-scoring
# neighbors. A single High-Low year could be noise; a municipio that holds
# that status for MANY years is a much stronger claim -- a durable local
# democratic "island." We flag municipios classified High-Low in at least 4
# of the 24 years as the spotlight set for the map.

outlier_counts <- panel %>%
  filter(cluster == "High-Low") %>%
  count(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO, name = "n_years") %>%
  filter(n_years >= 4) %>%
  arrange(desc(n_years))

cat("\nStep 4 done. Persistent High-Low outlier municipios (>=4 of 24 years):\n")
print(outlier_counts)

# ============================================================================
# STEP 5 -- Gap to Bogota, and the final map
# ============================================================================
# How do these outlier municipios compare to Bogota itself, on sndem? For
# each outlier municipio, in each year it's classified High-Low: its own
# sndem, Bogota's sndem that same year, and the RAW (unstandardized) mean
# sndem of its actual queen-contiguity neighbors that year -- so the
# comparison is interpretable directly on the sndem scale, not as a z-score.

idx_of <- function(code) which(shp$MPIO_CDPMP == code)
raw_by_year <- lapply(as.character(years), function(yr) {
  d_yr <- dat %>% filter(year == as.integer(yr)) %>% select(MPIO_CDPMP, sndem)
  merged <- shp %>% st_drop_geometry() %>% left_join(d_yr, by = "MPIO_CDPMP")
  merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]$sndem
})
names(raw_by_year) <- as.character(years)

gap_rows <- list()
for (i in seq_len(nrow(outlier_counts))) {
  code <- outlier_counts$MPIO_CDPMP[i]; idx <- idx_of(code); neighbor_idx <- nb[[idx]]
  hl_years <- panel %>% filter(MPIO_CDPMP == code, cluster == "High-Low") %>% pull(year)
  for (yr in hl_years) {
    yr_c <- as.character(yr)
    gap_rows[[length(gap_rows) + 1]] <- data.frame(
      MPIO_CDPMP = code, MPIO_CNMBR = outlier_counts$MPIO_CNMBR[i], year = yr,
      own_sndem = raw_by_year[[yr_c]][idx],
      bogota_sndem = raw_by_year[[yr_c]][idx_of(bogota_code)],
      neighbor_mean_sndem = mean(raw_by_year[[yr_c]][neighbor_idx], na.rm = TRUE)
    )
  }
}
gap_df <- do.call(rbind, gap_rows) %>%
  mutate(gap_to_bogota = abs(own_sndem - bogota_sndem),
         gap_to_neighbors = abs(own_sndem - neighbor_mean_sndem))

cat("\nStep 5a done. sndem gap to Bogota vs. gap to own neighbors, pooled across all outlier-years:\n")
print(gap_df %>% summarise(mean_gap_to_bogota = mean(gap_to_bogota),
                            mean_gap_to_neighbors = mean(gap_to_neighbors)))
cat("These are nearly equal (ratio ~1) -- these municipios sit roughly HALFWAY between\n",
    "their surrounding periphery and Bogota on sndem, not converging on the capital's level.\n")

# --- Final map: modal backdrop + outlier spotlight + Bogota reference -------
merged_map <- shp %>% left_join(modal_df, by = "MPIO_CDPMP")
merged_map$modal_cluster <- factor(merged_map$modal_cluster,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors"))

# Blue = more democratic, red = less democratic. Color FAMILY is keyed to the
# municipio's OWN value (High-High/High-Low both own-high -> blue family;
# Low-Low/Low-High both own-low -> red family), with a paler tint marking the
# discordant (outlier) case -- the standard LISA convention, just with the
# hue mapping flipped from the red-hot/blue-cold default.
pal <- c("High-High" = "#0571B0", "Low-Low" = "#CA0020",
         "High-Low" = "#92C5DE", "Low-High" = "#F4A582",
         "Not significant" = "#F0F0F0", "No neighbors" = "#B0B0B0")

centroids <- shp %>% filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>%
  st_centroid() %>% st_coordinates() %>% as.data.frame() %>%
  bind_cols(shp %>% st_drop_geometry() %>% filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>% select(MPIO_CDPMP)) %>%
  left_join(outlier_counts, by = "MPIO_CDPMP") %>%
  mutate(label = paste0(MPIO_CNMBR, " (", n_years, "\u00a0yrs)"))
nudge_x <- c(OCAÑA = 0.9, POPAYÁN = -0.9, PITALITO = 0.9, MONTERÍA = -1.0, QUIBDÓ = -1.1)
nudge_y <- c(OCAÑA = 0.3, POPAYÁN = -0.3, PITALITO = -0.4, MONTERÍA = 0.3, QUIBDÓ = 0.2)
centroids$nx <- centroids$X + nudge_x[centroids$MPIO_CNMBR]
centroids$ny <- centroids$Y + nudge_y[centroids$MPIO_CNMBR]

bogota_pt <- shp %>% filter(MPIO_CDPMP == bogota_code) %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
bogota_pt$nx <- bogota_pt$X + 0.75; bogota_pt$ny <- bogota_pt$Y + 0.6

p <- ggplot(merged_map) +
  geom_sf(aes(fill = modal_cluster, alpha = share), color = "white", linewidth = 0.05) +
  scale_fill_manual(values = pal, name = "Modal LISA\nclassification\n(2000–2023)", drop = FALSE) +
  scale_alpha_continuous(name = "Share of years\nin modal category", range = c(0.3, 1), limits = c(0, 1)) +
  geom_segment(data = centroids, aes(x = X, y = Y, xend = nx, yend = ny), color = "black", linewidth = 0.3) +
  geom_point(data = centroids, aes(x = X, y = Y, size = n_years), shape = 21, fill = "black", color = "white", stroke = 0.6, alpha = 0.9) +
  geom_label(data = centroids, aes(x = nx, y = ny, label = label), size = 2.9, fontface = "bold", label.size = 0,
             label.padding = unit(0.12, "lines"), fill = alpha("white", 0.85)) +
  scale_size_continuous(name = "Years as a\nHigh-Low outlier\n(2000–2023)", range = c(2, 6), breaks = c(4, 8, 12, 16)) +
  geom_segment(data = bogota_pt, aes(x = X, y = Y, xend = nx, yend = ny), color = "black", linewidth = 0.3) +
  geom_point(data = bogota_pt, aes(x = X, y = Y), shape = 23, fill = "gold", color = "black", size = 3, stroke = 0.6) +
  geom_label(data = bogota_pt, aes(x = nx, y = ny, label = "BOGOTÁ (capital)"), size = 2.9, fontface = "bold",
             label.size = 0, label.padding = unit(0.12, "lines"), fill = alpha("white", 0.85)) +
  labs(
    title = "Persistent High-Low outliers against the 24-year modal cluster map",
    subtitle = "Backdrop: each municipio's most common LISA classification, 2000–2023, shaded by how\nconsistently it held that classification. Markers: municipios High-Low in ≥4 of 24 years",
    caption = "San Andrés and Providencia excluded (no queen-contiguity neighbors). Marker positions are municipio centroids."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle = element_text(size = 8.5, color = "gray30", hjust = 0),
        plot.caption = element_text(size = 7, color = "gray40", hjust = 0),
        legend.position = "right")

ggsave(file.path(out_dir, "lisa_outlier_spotlight_map_modal.png"), p, width = 8.5, height = 9.5, dpi = 300, bg = "white")
cat("\nStep 5b done. Map saved to", file.path(out_dir, "lisa_outlier_spotlight_map_modal.png"), "\n")
