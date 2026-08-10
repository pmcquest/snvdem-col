# ============================================================================
# Persistent High-Low outlier spotlight, modal-classification backdrop
#
# 09_lisa_outlier_spotlight_map.R used the single-year 2011 LISA map as a
# backdrop for the persistent-outlier markers -- serviceable, but an
# arbitrary cross-section that doesn't represent the full 2000-2023 panel
# any better than any other year would.
#
# This version replaces that backdrop with a genuine 24-year summary: each
# municipio is shaded by its MODAL LISA classification (whichever of the six
# categories it held most often across 2000-2023), with shading intensity
# (alpha) scaled to how consistently it held that category -- 24/24 years
# renders fully saturated, a municipio that flipped every other year renders
# washed out. This is preferred over averaging the raw local Moran's I
# statistic because Ii isn't on a comparable scale across years (it depends
# on that year's variance structure) and doesn't by itself recover the
# HH/LL/HL/LH label without re-pairing it against the sign of z_x each time --
# taking the modal categorical label sidesteps both problems.
#
# The persistent High-Low outlier spotlight (Ocana, Popayan, Pitalito,
# Monteria, Quibdo) is unchanged from 09 -- it was already a full-panel
# statistic, just previously shown against a single-year backdrop.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp

panel <- read.csv(file.path(out_dir, "lisa_full_panel.csv"), colClasses = c(MPIO_CDPMP = "character"))

# --- Modal classification + persistence share, per municipio -----------------
modal_df <- panel %>%
  count(MPIO_CDPMP, cluster, name = "n_years_in_cat") %>%
  group_by(MPIO_CDPMP) %>%
  slice_max(n_years_in_cat, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(panel %>% count(MPIO_CDPMP, name = "n_years_total"), by = "MPIO_CDPMP") %>%
  mutate(share = n_years_in_cat / n_years_total) %>%
  rename(modal_cluster = cluster)

cat("Modal classification, distribution across 1,122 municipios:\n")
print(table(modal_df$modal_cluster))
cat("\nMean persistence (share of years in modal category):", round(mean(modal_df$share), 2), "\n")

merged <- shp %>% left_join(modal_df, by = "MPIO_CDPMP")
merged$modal_cluster <- factor(merged$modal_cluster,
  levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant", "No neighbors"))

# Blue = more democratic, red = less democratic (color family keyed to the
# municipio's OWN value, matching the standard LISA convention of pairing a
# saturated color for the concordant case with a paler tint of the same
# family for the discordant case): High-High/High-Low both own-high -> blue
# family; Low-Low/Low-High both own-low -> red family.
pal <- c(
  "High-High"       = "#0571B0",
  "Low-Low"         = "#CA0020",
  "High-Low"        = "#92C5DE",
  "Low-High"        = "#F4A582",
  "Not significant" = "#F0F0F0",
  "No neighbors"    = "#B0B0B0"
)

# --- Spotlight markers: same persistent High-Low outliers as 09 --------------
outlier_counts <- panel %>%
  filter(cluster == "High-Low") %>%
  count(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO, name = "n_years") %>%
  filter(n_years >= 4) %>%
  arrange(desc(n_years))

centroids <- shp %>%
  filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(shp %>% st_drop_geometry() %>% filter(MPIO_CDPMP %in% outlier_counts$MPIO_CDPMP) %>% select(MPIO_CDPMP)) %>%
  left_join(outlier_counts, by = "MPIO_CDPMP") %>%
  mutate(label = paste0(MPIO_CNMBR, " (", n_years, "\u00a0yrs)"))

nudge_x <- c(OCAÑA = 0.9, POPAYÁN = -0.9, PITALITO = 0.9, MONTERÍA = -1.0, QUIBDÓ = -1.1)
nudge_y <- c(OCAÑA = -0.5, POPAYÁN = -0.3, PITALITO = -0.4, MONTERÍA = 0.5, QUIBDÓ = 0.2)
centroids$nx <- centroids$X + nudge_x[centroids$MPIO_CNMBR]
centroids$ny <- centroids$Y + nudge_y[centroids$MPIO_CNMBR]

# --- Reference labels: Bogota + the two High-High clusters west of it --------
# Bogota (single municipio), Medellin/Antioquia (north cluster), and Pereira/
# Eje Cafetero -- Quindio/Risaralda/Caldas dept/N. Valle (south cluster) --
# identified from modal_df as the three contiguous blocks of modal-High-High
# municipios. Each is anchored on a named city's own centroid and reports that
# city's modal persistence (years in High-High out of 24), same convention as
# the outlier spotlight markers above; labels are pushed off the polygon into
# open ("Not significant") space with a pointer line so the cluster shapes
# stay visible underneath.
ref_points <- data.frame(
  MPIO_CDPMP = c("11001", "05001", "66001"),
  ref_label  = c("BOGOTÁ (capital)", "MEDELLÍN", "EJE CAFETERO")
)
ref_years <- modal_df %>%
  filter(MPIO_CDPMP %in% ref_points$MPIO_CDPMP, modal_cluster == "High-High") %>%
  select(MPIO_CDPMP, n_years_in_cat, n_years_total)

ref <- shp %>%
  filter(MPIO_CDPMP %in% ref_points$MPIO_CDPMP) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(shp %>% st_drop_geometry() %>% filter(MPIO_CDPMP %in% ref_points$MPIO_CDPMP) %>% select(MPIO_CDPMP)) %>%
  left_join(ref_points, by = "MPIO_CDPMP") %>%
  left_join(ref_years, by = "MPIO_CDPMP") %>%
  mutate(label = paste0(ref_label))

ref_nudge_x <- c("11001" = 1.7, "05001" = 1.3, "66001" = 0)
ref_nudge_y <- c("11001" = 0.4, "05001" = 0.3, "66001" = -1)
ref$nx <- ref$X + ref_nudge_x[ref$MPIO_CDPMP]
ref$ny <- ref$Y + ref_nudge_y[ref$MPIO_CDPMP]

# --- Bajo Cauca-Sur de Bolivar corridor: the red Low-Low band running -------
# east-west across N. Antioquia (Turbo-Mutata-Ituango-Taraza-Caucasia) into
# S. Bolivar (Monteoristo-Tiquisio-Achi-Rio Viejo-San Martin de Loba-Barranco
# de Loba-Pinillos). Anchored near Caucasia, roughly the corridor's midpoint.
corridor <- shp %>%
  filter(MPIO_CDPMP == "05154") %>%  # Caucasia
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(label = "BAJO CAUCA–SUR DE BOLÍVAR\nCORRIDOR", nx = X + 0.3, ny = Y + 2.3)

p <- ggplot(merged) +
  geom_sf(aes(fill = modal_cluster, alpha = share), color = "white", linewidth = 0.05) +
  scale_fill_manual(values = pal, name = "Modal LISA\nclassification\n(2000–2023)", drop = FALSE) +
  scale_alpha_continuous(name = "Share of years\nin modal category", range = c(0.3, 1), limits = c(0, 1)) +
  geom_segment(data = centroids, aes(x = X, y = Y, xend = nx, yend = ny),
               color = "black", linewidth = 0.3) +
  geom_point(data = centroids, aes(x = X, y = Y, size = n_years),
             shape = 21, fill = "black", color = "white", stroke = 0.6, alpha = 0.9) +
  geom_label(data = centroids, aes(x = nx, y = ny, label = label),
             size = 2.9, fontface = "bold", label.size = 0, label.padding = unit(0.12, "lines"),
             fill = alpha("white", 0.85)) +
  scale_size_continuous(name = "Years as a\nHigh-Low outlier\n(2000–2023)", range = c(2, 6),
                         breaks = c(4, 8, 12, 16)) +
  geom_segment(data = ref, aes(x = X, y = Y, xend = nx, yend = ny),
               color = "black", linewidth = 0.3) +
  geom_point(data = ref %>% filter(MPIO_CDPMP == "11001"), aes(x = X, y = Y),
             shape = 23, fill = "gold", color = "black", size = 3, stroke = 0.6) +
  geom_point(data = ref %>% filter(MPIO_CDPMP != "11001"), aes(x = X, y = Y),
             shape = 24, fill = "#0571B0", color = "white", size = 2.6, stroke = 0.6) +
  geom_label(data = ref, aes(x = nx, y = ny, label = label),
             size = 2.9, fontface = "bold", label.size = 0, label.padding = unit(0.12, "lines"),
             fill = alpha("white", 0.85)) +
  geom_segment(data = corridor, aes(x = X, y = Y, xend = nx, yend = ny),
               color = "gray25", linewidth = 0.35, linetype = "22") +
  geom_label(data = corridor, aes(x = nx, y = ny, label = label),
             size = 2.9, fontface = "bold", color = "gray20", label.size = 0,
             label.padding = unit(0.15, "lines"), lineheight = 0.85,
             fill = alpha("white", 0.8)) +
  labs(
    title = "Persistent High-Low outliers against the 24-year modal cluster map",
    subtitle = "Backdrop: each municipio's most common LISA classification, 2000–2023, shaded by how\nconsistently it held that classification. Markers: municipios High-Low in ≥4 of 24 years",
    caption = "San Andrés and Providencia excluded (no queen-contiguity neighbors). Marker positions are municipio centroids."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 8.5, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 7, color = "gray40", hjust = 0),
    legend.position = "right"
  )

out_path <- file.path(out_dir, "lisa_outlier_spotlight_map_modal.png")
ggsave(out_path, p, width = 8.5, height = 9.5, dpi = 300, bg = "white")
cat("Saved", out_path, "\n")
