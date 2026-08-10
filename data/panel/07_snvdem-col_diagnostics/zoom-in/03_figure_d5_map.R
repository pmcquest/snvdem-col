#---- Zoom-in Step 3: Appendix D Figure D.5 -- map of case municipalities (2023) ----

# PURPOSE: LASA26/PoP draft's Appendix D, Figure D.5 shows a small full-Colombia context map
# (with a dashed box marking a zoomed region) beside a larger zoomed choropleth of normalized
# sndem (2023), with the three focal case municipalities pinned and labeled. The original
# figure (scripts/visualization/ZoomIn/SNDEM_three_muni.png, embedded in the current appendix
# docx as Figure D.5) has no surviving generating script in the repo -- rebuilt from scratch
# here, reproducing its visual design, for the NEW case municipalities and CURRENT pipeline
# data (see 01_identify_case_municipalities.R).
#
# GEOGRAPHIC NOTE (confirmed 2026-08-08): the old three cases (Cocorna/Antioquia, Timbiqui/
# Cauca, Puerto Lopez/Meta) were clustered within ~5 degrees of latitude, so a single tight
# zoom box worked. The new three are NOT similarly clustered -- Uribia (La Guajira) sits at
# 12.00 N, on the Guajira peninsula, ~650km north of Nobsa (5.78 N) and Quinchia (5.32 N).
# Per PM: keep the same 2-panel (context + single zoom) structure, but widen the zoom box's
# latitude range to include Uribia while still cropping the empty deep-Amazon/Orinoquia
# south-east (none of the 3 cases are there). This is a real, larger zoom box than the old
# figure's, not a bug -- it reflects the new cases' actual geographic spread.

library(tidyverse)
library(sf)
library(ggrepel)
library(rnaturalearth)
library(patchwork)

## ---- Config ----
panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
zoom_dir  <- file.path(panel_dir, "07_snvdem-col_diagnostics", "zoom-in")
SNAP_YEAR <- 2023

case_codes <- c(Nobsa = "15491", Uribia = "44847", Quinchía = "66594")
case_labels <- c(Nobsa = "NOBSA (High)", Uribia = "URIBIA (Low)", Quinchía = "QUINCHÍA (Average)")

# Zoom box: covers Uribia (far north, La Guajira) down through the central Andean cases,
# while still cropping the empty deep-south (Amazonas/Putumayo/Caqueta) and far-east
# (Vichada/Guainia/Vaupes) lowlands -- none of the 3 focal municipalities are there.
zoom_xlim <- c(-78, -67)
zoom_ylim <- c(3, 13.5)

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
rescale01  <- function(x, ref) (x - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))

## ---- Load sndem (current pipeline, normalized -- matches Table 3/D.1 convention) ----
weighted <- read_rds(file.path(panel_dir, "05_weighting", "03_output", "snvdem_col_weighted.rds"))
snap <- weighted %>%
  filter(year == SNAP_YEAR) %>%
  mutate(sndem_norm = rescale01(sndem, weighted$sndem)) %>%
  select(MPIO_CDPMP, sndem_norm)

## ---- Load municipal boundaries ----
muni_geo <- st_read(file.path("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer",
                               "MGN_ANM_MPIOS.shp"), quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_data <- muni_geo %>% left_join(snap, by = "MPIO_CDPMP")

## ---- Focal points (centroids + labels) ----
focal_geo <- map_data %>%
  filter(MPIO_CDPMP %in% case_codes) %>%
  st_centroid() %>%
  mutate(
    municipio  = names(case_codes)[match(MPIO_CDPMP, case_codes)],
    case_label = case_labels[municipio],
    label_text = paste0(case_label, "\nsndem = ", sprintf("%.2f", sndem_norm))
  )

## ---- Neighboring countries for context ----
neighbors <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "South America" | name == "Panama")

zoom_box <- tibble(xmin = zoom_xlim[1], xmax = zoom_xlim[2],
                    ymin = zoom_ylim[1], ymax = zoom_ylim[2])

## ---- Context map (full country, small inset, dashed box marks the zoom region) ----
context_map <- ggplot() +
  geom_rect(aes(xmin = -82, xmax = -66, ymin = -5, ymax = 14), fill = "#d6eaf8") +
  geom_sf(data = neighbors, fill = "#f0f0f0", color = "grey70", linewidth = 0.2) +
  geom_sf(data = map_data, aes(fill = sndem_norm), color = NA) +
  scale_fill_viridis_c(option = "magma", limits = c(0, 1), na.value = "grey75", guide = "none") +
  geom_rect(data = zoom_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "white", linewidth = 1.2) +
  geom_rect(data = zoom_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "grey15", linewidth = 0.5, linetype = "dashed") +
  coord_sf(xlim = c(-82, -66), ylim = c(-5, 14), expand = FALSE) +
  theme_void(base_size = 11) +
  theme(panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
        plot.margin  = margin(4, 0, 4, 4))

## ---- Main zoomed map ----
zoom_map_main <- ggplot(data = map_data) +
  geom_sf(data = neighbors, fill = "#f5f5f5", color = "grey80", linewidth = 0.2) +
  geom_sf(aes(fill = sndem_norm), color = NA) +
  geom_sf(data = focal_geo, shape = 21, size = 4, fill = "white", color = "black", stroke = 1.4) +
  geom_label_repel(
    data = focal_geo,
    aes(label = label_text, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5, fontface = "bold",
    box.padding = 0.8, segment.color = "grey30", segment.size = 0.4,
    min.segment.length = 0, fill = alpha("white", 0.90),
    label.size = 0.25, label.padding = unit(0.20, "lines")
  ) +
  scale_fill_viridis_c(
    option = "magma", name = "sndem\n(2023)", limits = c(0, 1), na.value = "grey75",
    guide = guide_colorbar(barheight = unit(3, "cm"), barwidth = unit(0.4, "cm"),
                            ticks = FALSE, title.theme = element_text(size = 9, face = "bold"))
  ) +
  coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
  labs(title = "Focal municipalities") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 6)),
    legend.position     = "right",
    legend.box.spacing  = unit(0.15, "cm"),
    legend.margin       = margin(l = 0),
    plot.margin = margin(4, 4, 4, 0)
  )

## ---- Combine: small context inset (left) + main zoomed map (right) ----
final_map <- context_map + zoom_map_main +
  plot_layout(widths = c(1.4, 3))

print(final_map)

out_path <- file.path(zoom_dir, "figure_d5_focal_municipalities.png")
ggsave(out_path, final_map, width = 9.5, height = 6, dpi = 300, bg = "white")
cat(sprintf("Wrote %s\n", out_path))
