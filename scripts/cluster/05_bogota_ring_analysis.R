# ============================================================================
# Distance-decay from Bogota: does democracy "radiate outward" from the
# capital, and has that changed over time?
#
# The LISA map (03_lisa_map_2011.R) only tells us whether Bogota's IMMEDIATE
# neighbors are part of its cluster -- it doesn't say anything about what
# happens two, three, or ten municipios further out, or whether that pattern
# has been stable since 2000. This script measures each municipio's "ring distance" from Bogota (how many queen-contiguity
# (how many queen-contiguity
# steps away it is -- 0 = Bogota itself, 1 = touches Bogota, 2 = touches a
# municipio that touches Bogota, etc.) and then averaging sndem within each
# ring, for every year. If democracy really does radiate outward from the
# capital, we'd expect mean sndem to fall off smoothly as ring distance
# increases; if the "buffer" we found in 04_lisa_adjacency_check.R reflects a
# gradual transition rather than a sharp edge, the decay should look
# continuous rather than a cliff at ring 1-2.
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

# --- Ring distance via breadth-first search on the contiguity graph --------
# nb is an adjacency list (nb[[i]] = indices of municipios touching municipio
# i). Starting from Bogota, we expand outward one contiguity-step at a time,
# recording the step number at which each municipio is first reached -- that
# step number is its "ring order." This is exactly the graph-theoretic
# analogue of "how many municipios removed from Bogota."
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
# San Andres/Providencia (islands, no land neighbors) are unreachable by
# definition and stay NA -- they're excluded from this analysis, same as
# from the LISA test.
cat("Ring distances from Bogota range 0 to", max(ring, na.rm = TRUE),
    "; unreachable (islands):", sum(is.na(ring)), "\n")

ring_df <- data.frame(MPIO_CDPMP = shp$MPIO_CDPMP, ring = ring)

# --- Ring-level summary, every year -----------------------------------------
# For each year and each ring order, summarize sndem across all municipios
# at that ring order: mean (the decay curve itself), n (how many municipios
# support that mean -- rings get thin at high orders, so treat those with
# caution), and sd (spread within the ring -- a high sd means the ring isn't
# a uniform "shell," i.e. ring distance from Bogota alone doesn't fully
# explain a municipio's score at that order).
panel <- dat %>%
  select(MPIO_CDPMP, year, sndem) %>%
  left_join(ring_df, by = "MPIO_CDPMP") %>%
  filter(!is.na(ring))

ring_year_summary <- panel %>%
  group_by(year, ring) %>%
  summarise(mean_sndem = mean(sndem), sd_sndem = sd(sndem), n = n(), .groups = "drop")

write.csv(ring_year_summary, file.path(out_dir, "bogota_ring_year_summary.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "bogota_ring_year_summary.csv"), "\n")

# --- Distance-decay plot for a few illustrative years -----------------------
# Restrict to rings with enough municipios (n >= 10) that the mean is not
# just one or two idiosyncratic border towns; this caps the x-axis around
# ring 15 rather than the full 21, where remaining rings are down to a
# handful of far-flung municipios (see the BFS ring-size table).
sel_years <- c(2000, 2005, 2011, 2017, 2023)
decay_plot_df <- ring_year_summary %>% filter(year %in% sel_years, n >= 10)

p_decay <- ggplot(decay_plot_df, aes(x = ring, y = mean_sndem, color = factor(year))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_color_viridis_d(name = "Year", option = "viridis") +
  labs(
    title = "Distance decay from Bogota, subnational democracy index (sndem)",
    subtitle = "Mean sndem by number of queen-contiguity steps from Bogota, selected years",
    x = "Ring distance from Bogota (contiguity steps)",
    y = "Mean sndem"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "bogota_distance_decay.png"), p_decay, width = 8, height = 5, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "bogota_distance_decay.png"), "\n")

# --- Ring x year heatmap: the whole 2000-2023 time series in one figure ----
# Same underlying numbers as the decay plot, but showing every year at once
# (not just five) so any drift in the decay profile over time -- e.g. the
# high-value zone around Bogota widening or narrowing -- is visible directly
# as a shift in the heatmap's color band rather than something we'd have to
# infer from a handful of selected lines.
heat_df <- ring_year_summary %>% filter(ring <= 15, n >= 10)

p_heat <- ggplot(heat_df, aes(x = year, y = ring, fill = mean_sndem)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Mean\nsndem", option = "viridis") +
  scale_y_reverse(breaks = 0:15) +
  labs(
    title = "Clustering around Bogota over time",
    subtitle = "Mean sndem by ring distance from Bogota (rows) and year (columns)",
    x = "Year", y = "Ring distance from Bogota (0 = Bogota itself)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(out_dir, "bogota_ring_year_heatmap.png"), p_heat, width = 9, height = 6, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "bogota_ring_year_heatmap.png"), "\n")
