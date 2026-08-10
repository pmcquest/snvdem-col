# ======================================================================
# Facet Maps: Subnational Election Quality (snelect) — All Years
#
# Variable: snelect = "Elections Free & Fair" component of SNVDEM
# Data:     SN_Index_tentative.rds (MC subfolder)
# Output:   snelect_faceted_allyears.png
# ======================================================================

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)

# ---- 1. LOAD DATA -------------------------------------------------------

master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# ---- 2. PREPARE snelect DATA --------------------------------------------

master_clean <- master_df %>%
  filter(!is.na(snelect)) %>%
  mutate(
    MPIO_CDPMP = clean_mpio(MPIO_CDPMP),
    # Normalize globally across all years so color scale is consistent
    snelect_norm = (snelect - min(snelect, na.rm = TRUE)) /
                   (max(snelect, na.rm = TRUE) - min(snelect, na.rm = TRUE))
  )

years_available <- sort(unique(master_clean$year))
cat("Years with snelect data:", paste(years_available, collapse = ", "), "\n")
cat("N municipalities per year (median):",
    median(table(master_clean$year)), "\n")

# ---- 3. JOIN DATA WITH MUNICIPALITY GEOMETRIES --------------------------

map_data <- muni_geo %>%
  left_join(master_clean, by = "MPIO_CDPMP")

# Neighbor countries for context inset
neighbors <- ne_countries(
  country     = c("Venezuela", "Brazil", "Ecuador", "Peru", "Panama"),
  scale       = "medium",
  returnclass = "sf"
)

# ---- 4. FULL FACET MAP (all years) --------------------------------------

ncols      <- 6
nrows      <- ceiling(length(years_available) / ncols)
plot_ht    <- nrows * 3.2 + 1.5   # inches

facet_plot <- ggplot(data = map_data %>% filter(!is.na(year))) +
  geom_sf(data = neighbors, fill = "#f0f4f0", color = "grey85", linewidth = 0.2) +
  geom_sf(aes(fill = snelect_norm), color = NA) +
  scale_fill_viridis_c(
    option   = "magma",
    name     = "Score\n(0 = low\n1 = high)",
    na.value = "grey80",
    limits   = c(0, 1),
    guide    = guide_colorbar(
      barheight  = unit(4, "cm"),
      ticks      = FALSE,
      title.hjust = 0.5
    )
  ) +
  coord_sf(xlim = c(-79.5, -66.5), ylim = c(-4.5, 12.5), expand = FALSE) +
  facet_wrap(~year, ncol = ncols) +
  labs(
    title    = "Subnational Election Quality in Colombia",
    subtitle = "snelect: Elections Free & Fair Index  |  0 = low  →  1 = high",
    caption  = "Source: SN-VDem Project. Grey = missing data."
  ) +
  theme_void() +
  theme(
    strip.text       = element_text(size = 10, face = "bold", margin = margin(b = 3)),
    legend.position  = "right",
    plot.title       = element_text(size = 16, face = "bold",  hjust = 0.5,
                                    margin = margin(t = 8, b = 4)),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 9, hjust = 1, color = "grey50"),
    plot.margin      = margin(10, 10, 10, 10),
    plot.background  = element_rect(fill = "white", color = NA)
  )

out_path <- "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/snelect_faceted_allyears.png"

ggsave(
  filename = out_path,
  plot     = facet_plot,
  width    = 18,
  height   = plot_ht,
  dpi      = 300,
  bg       = "white"
)
cat("Saved:", out_path, "\n")


# ---- 5. OPTIONAL: STYLED PANEL (selected milestone years + inset map) ---
# Mirrors the paper's Figure 4 style (map-legend.R) but for snelect.
# Change years_to_map to match what the paper uses.

years_to_map <- c(2000, 2006, 2012, 2018, 2023)
years_to_map <- years_to_map[years_to_map %in% years_available]

map_data_sel <- muni_geo %>%
  left_join(master_clean %>% filter(year %in% years_to_map), by = "MPIO_CDPMP")

# Context (inset) map
context_map <- ggplot() +
  geom_rect(aes(xmin = -82, xmax = -66, ymin = -6, ymax = 14), fill = "#e0f3ff") +
  geom_sf(data = ne_countries(continent = "South America", scale = "medium", returnclass = "sf"),
          fill = "#d5e8d4", color = "grey70") +
  geom_sf(data = ne_countries(country = "Colombia", scale = "medium", returnclass = "sf"),
          fill = "white", color = "grey40", linewidth = 0.4) +
  coord_sf(xlim = c(-82, -66), ylim = c(-6, 14), expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.5))

# Individual year panels
year_panels <- lapply(years_to_map, function(y) {
  ggplot(data = map_data_sel %>% filter(year == y)) +
    geom_rect(aes(xmin = -79.5, xmax = -66.5, ymin = -4.5, ymax = 12.5), fill = "#f0f8ff") +
    geom_sf(data = neighbors, fill = "#f2f9f2", color = "grey90", linewidth = 0.05) +
    geom_sf(aes(fill = snelect_norm), color = NA) +
    scale_fill_viridis_c(
      option   = "magma",
      name     = "snelect\nScore",
      na.value = "grey80",
      limits   = c(0, 1),
      guide    = guide_colorbar(barheight = unit(3, "cm"), ticks = FALSE)
    ) +
    coord_sf(xlim = c(-79.5, -66.5), ylim = c(-4.5, 12.5), expand = FALSE) +
    labs(title = as.character(y)) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 12, face = "bold", hjust = 0.5,
                                     margin = margin(t = 5, b = 5)),
      legend.position = "none"
    )
})

# Assemble with patchwork (context map + year panels)
n_panels <- length(years_to_map)
rows_needed <- ceiling(n_panels / 3)

area_list    <- c(
  list(area(1, 1)),
  lapply(seq_along(years_to_map), function(i) {
    r  <- ceiling(i / 3)
    cc <- ((i - 1) %% 3) + 2
    area(r, cc)
  })
)
layout_areas <- do.call(c, area_list)

panel_list <- c(list(context_map), year_panels)
names(panel_list) <- c("context", paste0("y", years_to_map))

styled_plot <- Reduce(`+`, panel_list) +
  plot_layout(design = layout_areas, guides = "collect") +
  plot_annotation(
    title    = "Spatial Evolution of Subnational Election Quality in Colombia",
    subtitle = "snelect: Elections Free & Fair Index (0 = Low, 1 = High)",
    caption  = "Source: SN-VDem Project."
  ) & theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey30", size = 12),
    legend.position  = "right",
    plot.background  = element_rect(fill = "white", color = NA)
  )

out_path2 <- "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/snelect_milestone_years.png"
ggsave(out_path2, styled_plot, width = 14, height = 8, dpi = 300, bg = "white")
cat("Saved:", out_path2, "\n")


# ---- 6. PRESIDENTIAL ELECTION YEARS ------------------------------------
# One panel per election; strip label shows year + winning candidate.

pres_elections <- tribble(
  ~year, ~winner,
  2002,  "Álvaro Uribe",
  2006,  "Álvaro Uribe (re-election)",
  2010,  "Juan Manuel Santos",
  2014,  "Juan Manuel Santos (re-election)",
  2018,  "Iván Duque",
  2022,  "Gustavo Petro"
) %>%
  mutate(
    strip_label = factor(
      paste0(year, "  —  ", winner),
      levels = paste0(year, "  —  ", winner)   # preserve chronological order
    )
  )

map_data_pres <- muni_geo %>%
  left_join(
    master_clean %>%
      filter(year %in% pres_elections$year) %>%
      left_join(pres_elections, by = "year"),
    by = "MPIO_CDPMP"
  )

pres_plot <- ggplot(data = map_data_pres %>% filter(!is.na(year))) +
  geom_sf(aes(fill = snelect_norm), color = NA) +
  scale_fill_viridis_c(
    option   = "magma",
    name     = "snelect\n(0 = low\n1 = high)",
    na.value = "grey80",
    limits   = c(0, 1),
    guide    = guide_colorbar(
      barheight   = unit(5, "cm"),
      ticks       = FALSE,
      title.hjust = 0.5
    )
  ) +
  coord_sf(xlim = c(-79.5, -66.5), ylim = c(-4.5, 12.5), expand = FALSE) +
  facet_wrap(~strip_label, ncol = 3) +
  labs(
    title    = "Subnational Election Quality at Presidential Election Years",
    subtitle = "Colombia municipal snelect scores in years of presidential elections (2002–2022)",
    caption  = "Sources: SN-VDem Project; Registraduría Nacional del Estado Civil."
  ) +
  theme_void() +
  theme(
    strip.text      = element_text(size = 10, face = "bold",
                                   margin = margin(t = 6, b = 4)),
    legend.position = "right",
    plot.title      = element_text(size = 15, face = "bold",  hjust = 0.5,
                                   margin = margin(t = 8, b = 4)),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey40",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(size = 9,  hjust = 1,   color = "grey50"),
    plot.background = element_rect(fill = "white", color = NA)
  )

out_path3 <- "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/snelect_pres_elections.png"
ggsave(out_path3, pres_plot, width = 14, height = 10, dpi = 300, bg = "white")
cat("Saved:", out_path3, "\n")
