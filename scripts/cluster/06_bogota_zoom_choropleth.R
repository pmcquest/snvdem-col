# ============================================================================
# Zoomed choropleth around Bogota: the raw, continuous surface instead of
# discrete LISA categories
#
# LISA collapses each municipio's value into one of five buckets (a limitation). 
# This script shows the same neighborhood without that collapse: municipios within
# 6 contiguity-steps of Bogota (see 05_bogota_ring_analysis.R for how "steps"
# is defined), colored by their raw sndem value on a continuous scale. This
# is the local, cropped equivalent of the facet-map choropleth
# already in the paper, and it answers the question
# "how steep is the actual gradient as you move away from the capital." 
# A single panel does this for 2011; a 3-panel version repeats it
# for 2000/2011/2023 to show whether that local gradient has changed shape
# over the two decades.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(dplyr)
  library(ggplot2)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; dat <- obj$dat; nb <- obj$nb

bogota_idx <- which(shp$MPIO_CDPMP == "11001")

bfs_ring_dist <- function(nb, source_idx) {
  n <- length(nb)
  dist <- rep(NA_integer_, n)
  dist[source_idx] <- 0L
  frontier <- source_idx
  d <- 0L
  while (length(frontier) > 0) {
    d <- d + 1L
    nxt <- integer(0)
    for (i in frontier) {
      nbrs <- nb[[i]]; nbrs <- nbrs[nbrs != 0]
      new <- nbrs[is.na(dist[nbrs])]
      dist[new] <- d
      nxt <- c(nxt, new)
    }
    frontier <- unique(nxt)
  }
  dist
}
ring <- bfs_ring_dist(nb, bogota_idx)

# Crop to everything within 6 contiguity-steps of Bogota (~330 municipios,
# roughly greater Cundinamarca/Boyaca/Tolima/Meta) -- wide enough to see the
# surrounding gradient, not so wide the map stops being a "zoom."
zoom_ids <- shp$MPIO_CDPMP[!is.na(ring) & ring <= 6]
shp_zoom <- shp %>% filter(MPIO_CDPMP %in% zoom_ids)
bogota_geom <- shp %>% filter(MPIO_CDPMP == "11001")

# viridis is used here (rather than a custom brand ramp) because it is a
# peer-reviewed, colorblind-safe, perceptually-uniform sequential palette
# that is the de facto standard for continuous choropleth maps in academic
# geography/political science journals -- readers will recognize it as a
# magnitude scale on sight.
make_zoom_map <- function(yr, shp_zoom, dat, bogota_geom, show_legend = TRUE) {
  d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
  merged <- shp_zoom %>% left_join(d_yr, by = "MPIO_CDPMP")

  ggplot(merged) +
    geom_sf(aes(fill = sndem), color = "white", linewidth = 0.1) +
    geom_sf(data = bogota_geom, fill = NA, color = "black", linewidth = 0.8) +
    scale_fill_viridis_c(name = "sndem", option = "viridis", limits = c(0.1, 0.6)) +
    labs(title = as.character(yr)) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      legend.position = if (show_legend) "right" else "none"
    )
}

# --- Single-year zoom, 2011 (matches the main LISA map's illustrative year) -
p_2011 <- make_zoom_map(2011, shp_zoom, dat, bogota_geom) +
  labs(
    title = "Subnational democracy index (sndem) around Bogota, 2011",
    subtitle = "Municipios within 6 contiguity-steps of Bogota (black outline); continuous scale, not LISA categories"
  ) +
  theme(plot.title = element_text(hjust = 0), plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0))

ggsave(file.path(out_dir, "bogota_zoom_choropleth_2011.png"), p_2011, width = 7.5, height = 7, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "bogota_zoom_choropleth_2011.png"), "\n")

# --- Three-panel version across time, same crop and same color scale -------
# Using one shared fill scale (limits = c(0.1, 0.6), set above) across all
# three panels is what makes them comparable -- if each panel auto-scaled to
# its own year's range, color differences between panels could just be an
# artifact of rescaling rather than a real change in the gradient.
library(patchwork)
p_2000 <- make_zoom_map(2000, shp_zoom, dat, bogota_geom, show_legend = FALSE)
p_2011b <- make_zoom_map(2011, shp_zoom, dat, bogota_geom, show_legend = FALSE)
p_2023 <- make_zoom_map(2023, shp_zoom, dat, bogota_geom, show_legend = TRUE)

p_panel <- p_2000 + p_2011b + p_2023 +
  plot_layout(nrow = 1) +
  plot_annotation(
    title = "Subnational democracy index (sndem) around Bogota, 2000 / 2011 / 2023",
    subtitle = "Same crop (\u2264 6 contiguity-steps of Bogota) and same color scale in all three panels"
  )

ggsave(file.path(out_dir, "bogota_zoom_choropleth_timepanel.png"), p_panel, width = 12, height = 5.5, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "bogota_zoom_choropleth_timepanel.png"), "\n")
