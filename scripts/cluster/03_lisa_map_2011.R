# ============================================================================
# Final LISA cluster map: Colombia, sndem, 2011
#
# Reruns the same classification logic as 01_lisa_year_screen.R for the
# single year selected from that screen (2011 -- the year with the most
# total significant municipios among years showing all four cluster types;
# see output/lisa_year_screen_summary.csv), then renders it as a map and
# writes the per-municipio classification to CSV for writing up the "brief
# description of clusters" text in the paper.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(dplyr)
  library(ggplot2)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; dat <- obj$dat; lw <- obj$lw; nb <- obj$nb

yr <- 2011
d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
merged <- shp %>% left_join(d_yr, by = "MPIO_CDPMP")
merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
x <- merged$sndem

# See 01_lisa_year_screen.R for the full explanation of each step below --
# local Moran's I, FDR correction, and the four-category classification are
# identical here, just run for one year instead of looped over all 24.
lmres <- localmoran(x, lw, zero.policy = TRUE, na.action = na.exclude)
p_fdr <- p.adjust(lmres[, "Pr(z != E(Ii))"], method = "BH")

z_x <- as.numeric(scale(x))
z_lag <- lag.listw(lw, z_x, zero.policy = TRUE, NAOK = TRUE)
z_lag[card(nb) == 0] <- NA

cluster <- rep("Not significant", length(x))
sig <- !is.na(p_fdr) & p_fdr < 0.05
cluster[sig & z_x > 0 & z_lag > 0] <- "High-High"
cluster[sig & z_x < 0 & z_lag < 0] <- "Low-Low"
cluster[sig & z_x > 0 & z_lag < 0] <- "High-Low"
cluster[sig & z_x < 0 & z_lag > 0] <- "Low-High"
cluster[card(nb) == 0] <- "No neighbors"

merged$cluster <- factor(cluster,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors"))
merged$local_I <- lmres[, "Ii"]  # each municipio's own local Moran's I value
merged$p_fdr <- p_fdr            # ...and its FDR-corrected significance

# Per-municipio classification table (drop the polygon geometry -- we just
# want the attributes here), useful for writing the cluster-description text
# and for 04_lisa_adjacency_check.R.
write.csv(st_drop_geometry(merged)[, c("MPIO_CDPMP", "DPTO_CCDGO", "MPIO_CNMBR", "sndem", "local_I", "p_fdr", "cluster")],
          file.path(out_dir, "lisa_clusters_2011.csv"), row.names = FALSE)

# --- Map colors --------------------------------------------------------
# ColorBrewer's 4-class RdBu is documented colorblind-safe (colorbrewer2.org)
# and matches the conventional GeoDa LISA color scheme, which readers
# familiar with spatial statistics will recognize on sight: red family = high
# value, blue family = low value; dark/saturated = concordant cluster (value
# and neighbors agree), light/pale = discordant outlier (value and neighbors
# disagree). Gray tones are reserved for "not part of a cluster" states so
# they read as recessive background rather than competing with the four
# substantive categories.
pal <- c(
  "High-High"       = "#CA0020",
  "Low-Low"         = "#0571B0",
  "High-Low"        = "#F4A582",
  "Low-High"        = "#92C5DE",
  "Not significant" = "#F0F0F0",
  "No neighbors"    = "#B0B0B0"
)

p <- ggplot(merged) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.05) +
  scale_fill_manual(values = pal, name = "Cluster type", drop = FALSE) +
  labs(
    title = "LISA Cluster Map: Subnational Democracy Index (sndem), Colombia 2011",
    subtitle = "Local Moran's I, queen contiguity weights, Benjamini-Hochberg FDR-corrected p < 0.05",
    caption = "San Andr\u00e9s and Providencia (island municipios) have no queen-contiguity neighbors and are excluded from the LISA test."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 7, color = "gray40", hjust = 0),
    legend.position = "right"
  )

out_path <- file.path(out_dir, "lisa_map_colombia_2011.png")
ggsave(out_path, p, width = 7.5, height = 8.5, dpi = 300, bg = "white")
cat("Saved", out_path, "\n")
