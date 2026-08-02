# ============================================================================
# Persistent High-Low outlier spotlight map
#
# 08_lisa_full_panel_explore.R found that a handful of specific municipios --
# Ocana, Popayan, Pitalito, Monteria, Quibdo -- are classified as a High-Low
# LISA outlier (a high-sndem municipio surrounded by lower-scoring neighbors)
# in most years of the 2000-2023 panel, not just one. That's a stronger,
# more geographically specific finding than the aggregate cluster-size time
# series (07): these are regional urban centers -- several of them literal
# department capitals -- persisting as democratic "islands" inside some of
# Colombia's most conflict-affected departments (Catatumbo, Cauca, Choco).
#
# This script puts that finding on the map readers already recognize: the
# standard 2011 LISA cluster map (same classification as 03_lisa_map_2011.R),
# with the persistent-outlier cities spotlighted on top, sized by how many of
# the 24 years (2000-2023) each one spent classified High-Low. The backdrop
# is a single year; the spotlighted markers summarize the full panel -- the
# caption says so explicitly to avoid implying all 24 years looked like 2011.
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

# --- Backdrop: 2011 LISA cluster map (identical logic to 03_lisa_map_2011.R) -
yr <- 2011
d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
merged <- shp %>% left_join(d_yr, by = "MPIO_CDPMP")
merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
x <- merged$sndem

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

pal <- c(
  "High-High"       = "#CA0020",
  "Low-Low"         = "#0571B0",
  "High-Low"        = "#F4A582",
  "Low-High"        = "#92C5DE",
  "Not significant" = "#F0F0F0",
  "No neighbors"    = "#B0B0B0"
)

# --- Spotlight: persistent High-Low outliers across the full 2000-2023 panel -
panel <- read.csv(file.path(out_dir, "lisa_full_panel.csv"), colClasses = c(MPIO_CDPMP = "character"))

outlier_counts <- panel %>%
  filter(cluster == "High-Low") %>%
  count(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO, name = "n_years") %>%
  filter(n_years >= 4) %>%
  arrange(desc(n_years))

cat("Spotlighting", nrow(outlier_counts), "persistent High-Low outlier municipios:\n")
print(outlier_counts)

centroids <- shp %>%
  filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(shp %>% st_drop_geometry() %>% filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>% select(MPIO_CDPMP)) %>%
  left_join(outlier_counts, by = "MPIO_CDPMP") %>%
  mutate(label = paste0(MPIO_CNMBR, " (", n_years, "\u00a0yrs)"))

# Manual label nudges so the five labels don't overlap the point or each other
# (only 5 points -- not worth a dependency on ggrepel for this).
nudge_x <- c(OCAÑA = 0.9, POPAYÁN = -0.9, PITALITO = 0.9, MONTERÍA = -1.0, QUIBDÓ = -1.1)
nudge_y <- c(OCAÑA = 0.3, POPAYÁN = -0.3, PITALITO = -0.4, MONTERÍA = 0.3, QUIBDÓ = 0.2)
centroids$nx <- centroids$X + nudge_x[centroids$MPIO_CNMBR]
centroids$ny <- centroids$Y + nudge_y[centroids$MPIO_CNMBR]

p <- ggplot(merged) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.05) +
  scale_fill_manual(values = pal, name = "2011 LISA\ncluster type", drop = FALSE) +
  geom_segment(data = centroids, aes(x = X, y = Y, xend = nx, yend = ny),
               color = "black", linewidth = 0.3) +
  geom_point(data = centroids, aes(x = X, y = Y, size = n_years),
             shape = 21, fill = "black", color = "white", stroke = 0.6, alpha = 0.9) +
  geom_label(data = centroids, aes(x = nx, y = ny, label = label),
             size = 2.9, fontface = "bold", label.size = 0, label.padding = unit(0.12, "lines"),
             fill = alpha("white", 0.85)) +
  scale_size_continuous(name = "Years as a\nHigh-Low outlier\n(2000–2023)", range = c(2, 6),
                         breaks = c(4, 8, 12, 16)) +
  labs(
    title = "Persistent High-Low outliers against the 2011 LISA cluster map",
    subtitle = "Backdrop: single-year LISA classification, 2011. Markers: municipios classified High-Low\nin ≥4 of 24 years (2000–2023) — sustained democratic outliers within lower-scoring surroundings",
    caption = "San Andrés and Providencia excluded (no queen-contiguity neighbors). Marker positions are municipio centroids."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 8.5, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 7, color = "gray40", hjust = 0),
    legend.position = "right"
  )

out_path <- file.path(out_dir, "lisa_outlier_spotlight_map.png")
ggsave(out_path, p, width = 8.5, height = 9, dpi = 300, bg = "white")
cat("Saved", out_path, "\n")
