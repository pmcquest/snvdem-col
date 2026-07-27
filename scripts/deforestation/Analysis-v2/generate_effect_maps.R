suppressMessages({
  library(tidyverse)
  library(sf)
  library(fixest)
  library(patchwork)
})

# Standalone reproduction of colombia_deforestation_v3.R's Section 5.2
# map-generation block, to verify the 2026-07-17 linear-panel addition
# without re-running the full ~1600-line pipeline (raster extraction etc.
# is already cached in tmf_muni_cover.rds; this script only needs that
# output plus the democracy panel and municipal shapefile).

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")
norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

tmf_cover <- readRDS(file.path(out_dir, "tmf_muni_cover.rds"))
dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

analysis_tmf <- tmf_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct), !is.na(forest_pct_lag1), !is.na(snelect_norm))

fe_snelect_tmf   <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year, data = analysis_tmf)
fe_snelect_tmf_q <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year, data = analysis_tmf)

cat("Linear model coef (should match memo's 1.086):\n"); print(coef(fe_snelect_tmf))
cat("\nQuadratic model coef (should match memo's -0.339, 1.389):\n"); print(coef(fe_snelect_tmf_q))

muni_shp <- st_read(shp_path, quiet = TRUE) %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

muni_means <- analysis_tmf %>%
  group_by(MPIO_CDPMP) %>%
  summarise(mean_snelect_norm = mean(snelect_norm, na.rm = TRUE),
            mean_d_forest_pct = mean(d_forest_pct, na.rm = TRUE), .groups = "drop")

map_df <- muni_shp %>% left_join(muni_means, by = "MPIO_CDPMP")

map_democracy <- ggplot(map_df) +
  geom_sf(aes(fill = mean_snelect_norm), color = NA) +
  scale_fill_distiller(palette = "PuBu", direction = 1, na.value = "grey85",
                        name = "Mean snelect\n(normalized 0-1)") +
  labs(title = "Municipal Democracy (snelect)", subtitle = "Mean 2000-2023") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

map_forestchange <- ggplot(map_df) +
  geom_sf(aes(fill = mean_d_forest_pct), color = NA) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, na.value = "grey85",
                        name = "Mean annual\nforest-cover\nchange (pp)") +
  labs(title = "Forest-Cover Change (JRC TMF, primary)", subtitle = "Mean 2000-2023, red = loss") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

cf_lin <- coef(fe_snelect_tmf)
b1_lin <- cf_lin[1]

cf_q <- coef(fe_snelect_tmf_q)
b1_q <- cf_q[1]
b2_q <- cf_q[2]

muni_means_lin <- muni_means %>% mutate(pred_linear = b1_lin * mean_snelect_norm)
muni_means_q   <- muni_means %>% mutate(pred_quadratic = b1_q * mean_snelect_norm + b2_q * mean_snelect_norm^2)

map_df_lin <- muni_shp %>% left_join(muni_means_lin %>% select(MPIO_CDPMP, pred_linear), by = "MPIO_CDPMP")
map_df_q   <- muni_shp %>% left_join(muni_means_q %>% select(MPIO_CDPMP, pred_quadratic), by = "MPIO_CDPMP")

map_linear <- ggplot(map_df_lin) +
  geom_sf(aes(fill = pred_linear), color = NA) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, na.value = "grey85",
                        name = "Predicted Delta\nforest cover, pp\n(democracy term only)",
                        limits = c(-1, 1) * max(abs(muni_means_lin$pred_linear), na.rm = TRUE)) +
  labs(title = "Predicted Effect of Democracy (JRC TMF, primary)",
       subtitle = "At each municipality's own mean snelect -- isolates the linear pattern") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

map_quadratic <- ggplot(map_df_q) +
  geom_sf(aes(fill = pred_quadratic), color = NA) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, na.value = "grey85",
                        name = "Predicted Delta\nforest cover, pp\n(democracy^2 term only)",
                        limits = c(-1, 1) * max(abs(muni_means_q$pred_quadratic), na.rm = TRUE)) +
  labs(title = "Predicted Effect of Democracy^2 (JRC TMF, primary)",
       subtitle = "At each municipality's own mean snelect -- isolates the curvilinear pattern") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

ggsave(file.path(out_dir, "map_linear_effect_only.png"), map_linear, width = 6, height = 8, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "map_quadratic_effect_only.png"), map_quadratic, width = 6, height = 8, dpi = 300, bg = "white")

combined_map <- map_democracy + map_forestchange + map_linear + map_quadratic +
  plot_layout(nrow = 1) +
  plot_annotation(
    title    = "Where Democracy Is Weak/Strong, Where Forest Loss Is Worst/Best, and the Linear vs. Curvilinear Effect Alone",
    subtitle = "Panel 3 isolates the predicted linear (democracy) effect, Panel 4 the quadratic (democracy^2) effect, both from noise/confounds -- see Section 4.1-4.2 for the formal test",
    caption  = "Sources: SN-VDem (pre-benchmark); JRC Tropical Moist Forest v1_2025"
  )

ggsave(file.path(out_dir, "map_democracy_vs_forestchange.png"), combined_map, width = 22, height = 8, dpi = 300, bg = "white")

cat("\nDONE. Wrote map_linear_effect_only.png, map_quadratic_effect_only.png, map_democracy_vs_forestchange.png\n")
