# ====================================================================== #
# Colombia Municipal-Level Deforestation and Subnational Democracy
# 
# Author: Patrick McQuestion, with editing help from Claude Code Sonnet 4.6
# Version: June 25, 2026
#
# Research question:
#   Does subnational election quality (snelect) correlate with municipal
#   forest cover loss rates in Colombia, 2001-2023?
#
# Primary data:
#   - Hansen/GFW Global Forest Change 2023 v1.11 (lossyear raster)
#     → gfc_muni_loss.rds  (municipality × year × loss_ha)
#   - SN_Index_tentative.rds (snelect panel, MPIO_CDPMP × year)
#
# Validation:
#   - MapBiomas Colombia Collection 3.0 (pre-computed CSV, Formacion Boscosa)
#     → mb_muni_loss.rds  (municipality × year × loss_ha)
#     Pearson r = 0.771, Spearman ρ = 0.625 vs. Hansen GFC
#
# Merge key:  DIVIPOLA 5-digit code (MPIO_CDPMP)
# Outputs:    scripts/output/*.rds and scripts/output/*.png
# 
# ====================================================================== #

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(viridis)

clean_mpio <- function(x) {
  str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
}

shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp"
out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"


# ====================================================================== #
# ==== SECTION 1: FOREST LOSS DATA  (Hansen GFC rasters → per-municipality) ====
# ====================================================================== #
# Run once to produce gfc_muni_loss.rds; skip to Section 2 thereafter.
# Tiles cover Colombia (~1.5 GB total); download takes ~20-40 min once.

# ---- 1.1  Download GFC tiles (one-time) --------------------------------

gfc_base <- "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11"
tiles    <- c("00N_070W", "00N_080W", "10N_070W", "10N_080W")
tile_dir <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/GFC-2023"

if (!dir.exists(tile_dir)) dir.create(tile_dir, recursive = TRUE)

for (tile in tiles) {
  fname <- file.path(tile_dir, paste0("Hansen_GFC-2023-v1.11_lossyear_", tile, ".tif"))
  if (!file.exists(fname))
    download.file(paste0(gfc_base, "/Hansen_GFC-2023-v1.11_lossyear_", tile, ".tif"),
                  destfile = fname, mode = "wb")
}

# ---- 1.2  Mosaic, clip, extract per municipality -----------------------

tile_files <- list.files(tile_dir, pattern = "lossyear.*\\.tif$", full.names = TRUE)
lossyear   <- mosaic(sprc(lapply(tile_files, rast)))

muni_sf <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  st_transform(crs = crs(lossyear))

loss_col <- crop(lossyear, ext(vect(muni_sf)))

# 23 passes (one per loss year); exact_extract handles memory internally
px_area_ha <- 0.09   # 30m pixel ≈ 0.09 ha

year_list <- lapply(1:23, function(yr) {
  data.frame(
    MPIO_CDPMP    = muni_sf$MPIO_CDPMP,
    year          = yr + 2000L,
    n_loss_pixels = exact_extract(loss_col == yr, muni_sf, fun = "sum")
  )
})

gfc_muni <- bind_rows(year_list) %>%
  filter(n_loss_pixels > 0) %>%
  mutate(loss_ha = n_loss_pixels * px_area_ha) %>%
  select(MPIO_CDPMP, year, loss_ha)

cat("Years:", paste(range(gfc_muni$year), collapse = "-"),
    "| Municipalities with loss:", n_distinct(gfc_muni$MPIO_CDPMP), "\n")

saveRDS(gfc_muni, file.path(out_dir, "gfc_muni_loss.rds"))


# ---- 1.3  Visualize tiles -----------------------

tiles <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)

# Visualize one tile at a time
for (f in tiles) {
  r <- rast(f)
  r_loss <- classify(r, cbind(0, NA))  # mask no-loss pixels
  plot(r_loss, main = basename(f),
       col = hcl.colors(23, "YlOrRd"), range = c(1, 23))
}


# ====================================================================== #
# ==== SECTION 2: ANALYSIS — snelect × forest loss (Hansen GFC) ====
# ====================================================================== #

forest_df  <- readRDS(file.path(out_dir, "gfc_muni_loss.rds"))

snelect_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds") %>%
  filter(!is.na(snelect)) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

muni_areas <- st_read(shp_path, quiet = TRUE) %>%
  mutate(
    MPIO_CDPMP = clean_mpio(MPIO_CDPMP),
    area_ha    = as.numeric(st_area(.)) / 10000
  ) %>%
  st_drop_geometry() %>%
  select(MPIO_CDPMP, area_ha)

analysis_df <- snelect_df %>%
  left_join(forest_df, by = c("MPIO_CDPMP", "year")) %>%
  left_join(muni_areas, by = "MPIO_CDPMP") %>%
  mutate(
    loss_ha       = replace_na(loss_ha, 0),
    loss_rate_pct = (loss_ha / area_ha) * 100,
    snelect_norm  = (snelect - min(snelect, na.rm = TRUE)) /
                    (max(snelect, na.rm = TRUE) - min(snelect, na.rm = TRUE))
  )

cat("Panel:", nrow(analysis_df), "rows |",
    n_distinct(analysis_df$MPIO_CDPMP), "municipalities |",
    n_distinct(analysis_df$year), "years\n")

# ---- 2.1  Annual summary table -----------------------------------------

analysis_df %>%
  group_by(year) %>%
  summarise(
    total_loss_ha  = sum(loss_ha,       na.rm = TRUE),
    mean_loss_rate = mean(loss_rate_pct, na.rm = TRUE),
    mean_snelect   = mean(snelect_norm,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 30)

# ---- 2.2  Scatter: snelect vs. forest loss rate ------------------------

ggplot(analysis_df %>% filter(loss_rate_pct > 0),
       aes(x = snelect_norm, y = loss_rate_pct)) +
  geom_point(alpha = 0.08, color = "steelblue", size = 0.8) +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  scale_y_log10(labels = scales::label_number(suffix = "%")) +
  labs(
    title    = "Subnational Election Quality vs. Forest Loss Rate",
    subtitle = "Colombia municipalities, 2001-2023  |  snelect normalized 0-1",
    x        = "snelect score (0 = low quality, 1 = high quality)",
    y        = "Annual forest cover loss (% of municipal area, log scale)",
    caption  = "Sources: SN-VDem; Hansen/GFW Global Forest Change 2023 v1.11"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "scatter_snelect_forestloss.png"),
       width = 9, height = 6, dpi = 300, bg = "white")

# ---- 2.3  Scatter by period (pre/post 2016 Peace Accord) ---------------

analysis_df %>%
  filter(loss_rate_pct > 0) %>%
  mutate(period = factor(
    if_else(year < 2016, "Pre-Accord (2001-2015)", "Post-Accord (2016-2023)"),
    levels = c("Pre-Accord (2001-2015)", "Post-Accord (2016-2023)")
  )) %>%
  ggplot(aes(x = snelect_norm, y = loss_rate_pct)) +
  geom_point(alpha = 0.06, color = "steelblue", size = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  scale_y_log10(labels = scales::label_number(suffix = "%")) +
  facet_wrap(~period) +
  labs(
    title   = "Election Quality vs. Forest Loss: Before and After the 2016 Peace Accord",
    x       = "snelect score (0 = low, 1 = high)",
    y       = "Annual forest cover loss (% area, log scale)",
    caption = "Sources: SN-VDem; Hansen/GFW GFC 2023 v1.11"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(out_dir, "scatter_snelect_forestloss_period.png"),
       width = 11, height = 6, dpi = 300, bg = "white")

# ---- 2.4  Time-series: national forest loss and mean snelect -----------

analysis_df %>%
  filter(year >= 2001) %>%
  group_by(year) %>%
  summarise(
    `Forest loss (000 ha)` = sum(loss_ha, na.rm = TRUE) / 1000,
    `Mean snelect (0-1)`   = mean(snelect_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  facet_wrap(~series, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Forest loss (000 ha)" = "#2d7a2d",
                                "Mean snelect (0-1)"   = "#1e66b0")) +
  labs(
    title   = "National Trends: Forest Loss and Subnational Election Quality",
    x       = "Year",
    caption = "Sources: SN-VDem; Hansen/GFW GFC 2023 v1.11"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave(file.path(out_dir, "timeseries_snelect_forestloss.png"),
       width = 9, height = 7, dpi = 300, bg = "white")

# ---- 2.5  Correlation: snelect vs. forest loss rate --------------------

cor_main <- cor.test(analysis_df$snelect_norm, analysis_df$loss_rate_pct,
                     use = "complete.obs", method = "pearson")
cat("\nPearson r (snelect vs. GFC loss rate):", round(cor_main$estimate, 3),
    " p =", format(cor_main$p.value, scientific = TRUE), "\n")

# ---- 2.6  Map: mean annual forest loss by municipality -----------------

analysis_df %>%
  group_by(MPIO_CDPMP) %>%
  summarise(mean_loss_ha = mean(loss_ha, na.rm = TRUE), .groups = "drop") %>%
  { left_join(st_read(shp_path, quiet = TRUE) %>%
                mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)), ., by = "MPIO_CDPMP") } %>%
  ggplot() +
  geom_sf(aes(fill = mean_loss_ha), color = NA) +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    name     = "Mean annual\nforest loss (ha)",
    na.value = "grey85",
    trans    = "log1p",
    labels   = scales::comma
  ) +
  labs(
    title    = "Mean Annual Forest Cover Loss by Municipality",
    subtitle = "Colombia, 2001-2023",
    caption  = "Source: Hansen/GFW Global Forest Change 2023 v1.11"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(out_dir, "map_mean_forestloss.png"),
       width = 9, height = 11, dpi = 300, bg = "white")


# ====================================================================== #
# ==== SECTION 3: VALIDATION — MapBiomas Colombia Col. 3.0 vs. Hansen GFC ====
# ====================================================================== #
# Source: MapBiomas Colombia → Estadísticas → Cobertura
#         Nivel territorio: Municipios | Clase: Formacion Boscosa
#         Exported CSV saved as: output/mapbiomas_col_muni_stats.csv
#
# CSV format: wide (years 1985-2024 as columns), municipality names only.
# DIVIPOLA codes are recovered by name-matching against the project shapefile.
# Indigenous territories (resguardos) appear in the CSV but have no DIVIPOLA
# code and are dropped -- this affects <5% of forest area.
#
# Validation result: Pearson r = 0.771, Spearman ρ = 0.625 (p ≈ 0, n ≈ 25k)
# The two sources diverge in 2002-2003 (MapBiomas higher, reflecting Plan
# Colombia coca eradication and conflict displacement not captured by GFC).
# They converge from 2005 onward and both show the post-2016 surge.

# ---- 3.1  Parse MapBiomas CSV and recover DIVIPOLA codes ---------------

norm_str <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

dept_codes <- tibble::tribble(
  ~dpto_norm,                               ~DPTO_CCDGO,
  "amazonas",                               "91",
  "antioquia",                              "05",
  "arauca",                                 "81",
  "atlantico",                              "08",
  "bolivar",                                "13",
  "boyaca",                                 "15",
  "caldas",                                 "17",
  "caqueta",                                "18",
  "casanare",                               "85",
  "cauca",                                  "19",
  "cesar",                                  "20",
  "choco",                                  "27",
  "cordoba",                                "23",
  "cundinamarca",                           "25",
  "guainia",                                "94",
  "guaviare",                               "95",
  "huila",                                  "41",
  "la guajira",                             "44",
  "magdalena",                              "47",
  "meta",                                   "50",
  "narino",                                 "52",
  "norte de santander",                     "54",
  "putumayo",                               "86",
  "quindio",                                "63",
  "risaralda",                              "66",
  "san andres providencia y santa catalina","88",
  "santander",                              "68",
  "sucre",                                  "70",
  "tolima",                                 "73",
  "valle del cauca",                        "76",
  "vaupes",                                 "97",
  "vichada",                                "99",
  "bogota d c",                             "11",
  "bogota",                                 "11"
)

muni_lookup <- st_read(shp_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(
    MPIO_CDPMP = clean_mpio(MPIO_CDPMP),
    DPTO_CCDGO = as.character(DPTO_CCDGO),
    mpio_norm  = norm_str(MPIO_CNMBR)
  ) %>%
  select(MPIO_CDPMP, DPTO_CCDGO, mpio_norm)

mb_loss <- readr::read_csv(file.path(out_dir, "mapbiomas_col_muni_stats.csv"),
                           show_col_types = FALSE,
                           locale = readr::locale(encoding = "UTF-8")) %>%
  filter(grepl("Formacion Boscosa|Bosque", class_level_1, ignore.case = TRUE)) %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "area_ha") %>%
  mutate(year = as.integer(year), area_ha = as.numeric(area_ha)) %>%
  filter(year >= 2000, year <= 2023) %>%
  mutate(mpio_norm = norm_str(municipio), dpto_norm = norm_str(departamento)) %>%
  left_join(dept_codes,  by = "dpto_norm") %>%
  left_join(muni_lookup, by = c("mpio_norm", "DPTO_CCDGO")) %>%
  filter(!is.na(MPIO_CDPMP)) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(mb_loss_ha = pmax(0, lag(area_ha) - area_ha)) %>%
  filter(!is.na(mb_loss_ha), mb_loss_ha > 0) %>%
  select(MPIO_CDPMP, year, mb_loss_ha)

cat("MapBiomas municipality-years with forest loss:", nrow(mb_loss), "\n")
saveRDS(mb_loss, file.path(out_dir, "mb_muni_loss.rds"))


# Visualize MapBiomas data

mb_raw <- read_csv("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/mapbiomas_col_muni_stats.csv")

# 1. National forest area over time
mb_raw %>%
  filter(grepl("Formacion Boscosa|Bosque", class_level_1, ignore.case = TRUE)) %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "area_ha") %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarise(total_ha = sum(area_ha, na.rm = TRUE) / 1e6) %>%
  ggplot(aes(x = year, y = total_ha)) +
  geom_line() + geom_point() +
  labs(y = "Forest area (million ha)", title = "MapBiomas: Colombia Forest Cover 1985-2023")

# 2. Choropleth of total forest loss 2001-2023 per municipality
# (requires mb_muni_loss.rds and the shapefile already loaded)
mb_loss <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/mb_muni_loss.rds")
shp <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp", quiet=TRUE) %>%
  mutate(MPIO_CDPMP = str_pad(as.character(as.numeric(MPIO_CDPMP)), 5, "left", "0"))

mb_loss %>%
  group_by(MPIO_CDPMP) %>%
  summarise(total_loss_ha = sum(mb_loss_ha, na.rm=TRUE)) %>%
  right_join(shp, by="MPIO_CDPMP") %>% st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = total_loss_ha), color=NA) +
  scale_fill_distiller(palette="YlOrRd", direction=1, trans="log1p",
                       na.value="grey85", labels=scales::comma,
                       name="Total loss (ha)") +
  labs(title="MapBiomas: Total Forest Loss 2001-2023") +
  theme_void()


# ---- 3.2  snelect × MapBiomas forest loss (robustness check) -----------
# Mirrors Section 2 but uses MapBiomas as the deforestation measure.
# Strong agreement with GFC results would confirm the findings are not
# sensitive to the choice of remote sensing product.

mb_loss <- readRDS(file.path(out_dir, "mb_muni_loss.rds"))

analysis_mb <- snelect_df %>%
  left_join(mb_loss, by = c("MPIO_CDPMP", "year")) %>%
  left_join(muni_areas, by = "MPIO_CDPMP") %>%
  mutate(
    mb_loss_ha    = replace_na(mb_loss_ha, 0),
    loss_rate_pct = (mb_loss_ha / area_ha) * 100,
    snelect_norm  = (snelect - min(snelect, na.rm = TRUE)) /
                    (max(snelect, na.rm = TRUE) - min(snelect, na.rm = TRUE))
  )

cor_mb <- cor.test(analysis_mb$snelect_norm, analysis_mb$loss_rate_pct,
                   use = "complete.obs", method = "pearson")
cat("\nPearson r (snelect vs. MapBiomas loss rate):", round(cor_mb$estimate, 3),
    " p =", format(cor_mb$p.value, scientific = TRUE), "\n")

ggplot(analysis_mb %>% filter(loss_rate_pct > 0),
       aes(x = snelect_norm, y = loss_rate_pct)) +
  geom_point(alpha = 0.08, color = "#2d6e2d", size = 0.8) +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  scale_y_log10(labels = scales::label_number(suffix = "%")) +
  labs(
    title    = "Subnational Election Quality vs. Forest Loss Rate (MapBiomas)",
    subtitle = "Colombia municipalities, 2001-2023  |  snelect normalized 0-1",
    x        = "snelect score (0 = low quality, 1 = high quality)",
    y        = "Annual forest cover loss (% of municipal area, log scale)",
    caption  = "Sources: SN-VDem; MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "scatter_snelect_forestloss_mb.png"),
       width = 9, height = 6, dpi = 300, bg = "white")

# ---- 3.3  Cross-source validation scatter ------------------------------

gfc_muni   <- readRDS(file.path(out_dir, "gfc_muni_loss.rds"))
compare_df <- gfc_muni %>%
  rename(gfc_loss_ha = loss_ha) %>%
  inner_join(mb_loss, by = c("MPIO_CDPMP", "year"))

cat("Joined municipality-year observations:", nrow(compare_df), "\n")

cor_p <- cor.test(compare_df$gfc_loss_ha, compare_df$mb_loss_ha, method = "pearson")
cor_s <- cor.test(compare_df$gfc_loss_ha, compare_df$mb_loss_ha, method = "spearman", exact = FALSE)
cat(sprintf("Pearson  r   = %.3f  (p = %.2e)\n", cor_p$estimate, cor_p$p.value))
cat(sprintf("Spearman rho = %.3f  (p = %.2e)\n", cor_s$estimate, cor_s$p.value))

ggplot(compare_df, aes(x = gfc_loss_ha, y = mb_loss_ha)) +
  geom_point(alpha = 0.07, color = "steelblue", size = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title    = "Hansen GFC vs. MapBiomas Colombia: Annual Forest Loss by Municipality",
    subtitle = sprintf("Pearson r = %.2f  |  Spearman ρ = %.2f  |  dashed = perfect agreement",
                       cor_p$estimate, cor_s$estimate),
    x        = "Hansen GFC loss (ha, log scale)",
    y        = "MapBiomas loss (ha, log scale)",
    caption  = "Sources: Hansen/GFW GFC 2023 v1.11; MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "validation_scatter_gfc_vs_mb.png"),
       width = 8, height = 6, dpi = 300, bg = "white")

# ---- 3.4  Annual national totals ---------------------------------------

compare_df %>%
  group_by(year) %>%
  summarise(
    `Hansen GFC`         = sum(gfc_loss_ha, na.rm = TRUE),
    `MapBiomas Col. 3.0` = sum(mb_loss_ha,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "source", values_to = "loss_ha") %>%
  ggplot(aes(x = year, y = loss_ha / 1e3, color = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Hansen GFC" = "steelblue",
                                "MapBiomas Col. 3.0" = "darkgreen")) +
  labs(
    title    = "Annual Deforestation: Hansen GFC vs. MapBiomas Colombia",
    subtitle = "National totals 2001-2023",
    x        = NULL,
    y        = "Forest loss (thousand ha)",
    color    = NULL,
    caption  = "Sources: Hansen/GFW GFC 2023 v1.11; MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "validation_annual_trends.png"),
       width = 9, height = 5, dpi = 300, bg = "white")


# ====================================================================== #
# ==== SECTION 4: DEEPER ANALYSIS — snelect × forest loss ====
# ====================================================================== #
# Extends Section 2.5 (GFC) and Section 3.2 (MapBiomas) with:
#   4.1  Nonzero-loss observations only
#   4.2  Log-transformed loss rate
#   4.3  Pre / post 2016 Peace Accord split
#   4.4  Decile breakdown (mean loss rate by snelect decile)
#   4.5  Municipality-level averages (collapse across years)
#   4.6  Annual Pearson r (year-by-year)
#
# Requires: analysis_df (GFC) and analysis_mb (MapBiomas) from Sections 2-3.
# If running standalone, rebuild them first:
#   source the file up through Section 3.2 (line ~357).

# ---- 4.1  Nonzero-loss observations only --------------------------------

cat("\n=== 4.1  Nonzero-loss observations only ===\n")

nz_gfc <- analysis_df %>% filter(loss_rate_pct > 0)
nz_mb  <- analysis_mb %>% filter(loss_rate_pct > 0)

cat(sprintf("GFC  nonzero obs: %d  (%.1f%% of full panel)\n",
            nrow(nz_gfc), 100 * nrow(nz_gfc) / nrow(analysis_df)))
cat(sprintf("MB   nonzero obs: %d  (%.1f%% of full panel)\n",
            nrow(nz_mb),  100 * nrow(nz_mb)  / nrow(analysis_mb)))

cor_gfc_nz   <- cor.test(nz_gfc$snelect_norm, nz_gfc$loss_rate_pct,
                         use = "complete.obs")
cor_mb_nz    <- cor.test(nz_mb$snelect_norm,  nz_mb$loss_rate_pct,
                         use = "complete.obs")
cor_gfc_nz_s <- cor.test(nz_gfc$snelect_norm, nz_gfc$loss_rate_pct,
                         use = "complete.obs", method = "spearman", exact = FALSE)
cor_mb_nz_s  <- cor.test(nz_mb$snelect_norm,  nz_mb$loss_rate_pct,
                         use = "complete.obs", method = "spearman", exact = FALSE)

cat(sprintf("GFC  Pearson r = %.4f  Spearman rho = %.4f\n",
            cor_gfc_nz$estimate, cor_gfc_nz_s$estimate))
cat(sprintf("MB   Pearson r = %.4f  Spearman rho = %.4f\n",
            cor_mb_nz$estimate,  cor_mb_nz_s$estimate))

# ---- 4.2  Log-transformed loss rate (nonzero obs) -----------------------

cat("\n=== 4.2  Log-transformed loss rate (nonzero obs) ===\n")

nz_gfc <- nz_gfc %>% mutate(log_loss = log(loss_rate_pct))
nz_mb  <- nz_mb  %>% mutate(log_loss = log(loss_rate_pct))

cor_gfc_log <- cor.test(nz_gfc$snelect_norm, nz_gfc$log_loss, use = "complete.obs")
cor_mb_log  <- cor.test(nz_mb$snelect_norm,  nz_mb$log_loss,  use = "complete.obs")

cat(sprintf("GFC  log Pearson r = %.4f  (p = %s)\n",
            cor_gfc_log$estimate, format(cor_gfc_log$p.value, scientific = TRUE, digits = 3)))
cat(sprintf("MB   log Pearson r = %.4f  (p = %s)\n",
            cor_mb_log$estimate,  format(cor_mb_log$p.value,  scientific = TRUE, digits = 3)))

# ---- 4.3  Pre / post 2016 Peace Accord ----------------------------------

cat("\n=== 4.3  Pre / post 2016 Peace Accord ===\n")

for (src in c("GFC", "MB")) {
  df <- if (src == "GFC") analysis_df else analysis_mb
  for (period in c("Pre-2016", "Post-2016")) {
    sub <- if (period == "Pre-2016") filter(df, year < 2016) else filter(df, year >= 2016)
    cr  <- cor.test(sub$snelect_norm, sub$loss_rate_pct, use = "complete.obs")
    cat(sprintf("%s  %s: r = %.4f  (n = %d)\n",
                src, period, cr$estimate,
                sum(complete.cases(sub[c("snelect_norm", "loss_rate_pct")]))))
  }
}

# ---- 4.4  Decile breakdown: mean loss rate by snelect decile ------------

cat("\n=== 4.4  Mean loss rate by snelect decile (GFC, full panel) ===\n")

analysis_df %>%
  mutate(decile = ntile(snelect_norm, 10)) %>%
  group_by(decile) %>%
  summarise(
    snelect_mean   = mean(snelect_norm,   na.rm = TRUE),
    loss_rate_mean = mean(loss_rate_pct,  na.rm = TRUE),
    loss_rate_med  = median(loss_rate_pct, na.rm = TRUE),
    n              = n(),
    .groups        = "drop"
  ) %>%
  print(n = 10)

# ---- 4.5  Municipality-level averages (collapse across years) -----------

cat("\n=== 4.5  Municipality-level averages ===\n")

muni_gfc <- analysis_df %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    snelect_mean   = mean(snelect_norm,   na.rm = TRUE),
    loss_rate_mean = mean(loss_rate_pct,  na.rm = TRUE),
    .groups        = "drop"
  )

muni_mb <- analysis_mb %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    snelect_mean   = mean(snelect_norm,   na.rm = TRUE),
    loss_rate_mean = mean(loss_rate_pct,  na.rm = TRUE),
    .groups        = "drop"
  )

cor_m_gfc <- cor.test(muni_gfc$snelect_mean, muni_gfc$loss_rate_mean, use = "complete.obs")
cor_m_mb  <- cor.test(muni_mb$snelect_mean,  muni_mb$loss_rate_mean,  use = "complete.obs")

cat(sprintf("GFC  municipality-mean Pearson r = %.4f  (n = %d)\n",
            cor_m_gfc$estimate, nrow(muni_gfc)))
cat(sprintf("MB   municipality-mean Pearson r = %.4f  (n = %d)\n",
            cor_m_mb$estimate,  nrow(muni_mb)))

# ---- 4.6  Annual Pearson r (year-by-year, GFC) --------------------------

cat("\n=== 4.6  Annual Pearson r, GFC ===\n")

analysis_df %>%
  group_by(year) %>%
  summarise(
    r = cor(snelect_norm, loss_rate_pct, use = "complete.obs"),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(r = round(r, 4)) %>%
  print(n = 30)


# ====================================================================== #
# ==== APPENDIX A: ColOpenData / IDEAM climate data (not pursued) ====
# ====================================================================== #
# Explored as a possible control variable (precipitation). Blocked by
# Notre Dame IT: port 9000 on tracelac.uniandes.edu.co is filtered.
# ColOpenData provides weather station data only -- no forest/land cover.

if (FALSE) {
  library(ColOpenData)
  library(lubridate)

  server_ok <- tryCatch({
    httr::GET("https://tracelac.uniandes.edu.co:9000", httr::timeout(10))$status_code == 200
  }, error = function(e) FALSE)

  if (!server_ok) stop("Cannot reach ColOpenData server (port 9000 blocked).")

  safe_dl <- function(code) {
    tryCatch(
      download_climate(code = code, start_date = "2000-01-01", end_date = "2023-12-31",
                       tag = "PTPM_CON") %>%
        aggregate_climate("year") %>%
        mutate(MPIO_CDPMP = code),
      error = function(e) NULL
    )
  }

  mpio_codes  <- sort(unique(snelect_df$MPIO_CDPMP))
  precip_list <- map(mpio_codes, safe_dl, .progress = TRUE)
  precip_raw  <- bind_rows(compact(precip_list))
  saveRDS(precip_raw, file.path(out_dir, "ideam_precip_raw.rds"))
}


# ====================================================================== #
# ==== APPENDIX B: GFW Data API approach (alternative to raster, not pursued) ====
# ====================================================================== #
# The GFW REST API returns pre-aggregated loss-ha by GADM admin-2 unit.
# Requires free account + API key. Abandoned in favour of the direct
# raster approach (Section 1) because the GADM → DIVIPOLA crosswalk
# introduced matching uncertainty.

if (FALSE) {
  library(httr2)
  library(geodata)

  gfw_key <- Sys.getenv("GFW_API_TOKEN")

  gfw_raw <- request("https://data-api.globalforestwatch.org") %>%
    req_url_path_append("/dataset/umd_tree_cover_loss/latest/query") %>%
    req_headers(`x-api-key` = gfw_key) %>%
    req_url_query(sql = paste(
      "SELECT iso, adm1, adm2, umd_tree_cover_loss__year,",
      "  SUM(umd_tree_cover_loss__ha) AS loss_ha",
      "FROM data WHERE iso = 'COL'",
      "  AND umd_tree_cover_density_threshold__pct = 30",
      "GROUP BY iso, adm1, adm2, umd_tree_cover_loss__year",
      "ORDER BY adm1, adm2, umd_tree_cover_loss__year"
    )) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    .[["data"]]

  muni_geo  <- st_read(shp_path, quiet = TRUE) %>%
    mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
    select(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO)
  gadm_col  <- gadm("COL", level = 2, path = tempdir()) %>%
    st_as_sf() %>% select(GID_2, NAME_1, NAME_2)
  crosswalk <- st_join(st_centroid(muni_geo),
                       st_transform(gadm_col, st_crs(muni_geo)),
                       join = st_within) %>%
    st_drop_geometry() %>% select(MPIO_CDPMP, GID_2)

  gfw_divipola <- gfw_raw %>%
    mutate(GID_2   = paste0("COL.", adm1, ".", adm2, "_1"),
           year    = as.integer(umd_tree_cover_loss__year),
           loss_ha = as.numeric(loss_ha)) %>%
    select(GID_2, year, loss_ha) %>%
    left_join(crosswalk, by = "GID_2")

  saveRDS(gfw_divipola, file.path(out_dir, "gfw_muni_loss.rds"))
}


# ====================================================================== #
# ==== APPENDIX C: MapBiomas raster extraction (alternative to CSV, Section 3) ====
# ====================================================================== #
# The pre-computed CSV (Section 3.1) is preferred. Raster extraction stalled
# on this machine: parallel workers blocked by ND IT (localhost sockets
# refused); sequential 30m extraction runs ~20 min/year × 24 years.
#
# Two fallbacks are preserved:
#   C1. National totals only -- terra::global, no polygon loop (~5 min total)
#   C2. Municipal-level, 6 years, downsampled 30m → 510m (~30-60 min)

if (FALSE) {
  FOREST_CLASS      <- 2L
  px_area_ha_mb     <- 0.09
  mapbiomas_src     <- "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/vEnv/MapBiomas"
  mapbiomas_local   <- "C:/Users/patri/AppData/Local/Temp/mapbiomas_local"

  dir.create(mapbiomas_local, showWarnings = FALSE, recursive = TRUE)
  mb_src <- sort(list.files(mapbiomas_src, "\\.tif$", full.names = TRUE))
  if (length(list.files(mapbiomas_local, "\\.tif$")) < length(mb_src))
    file.copy(mb_src, file.path(mapbiomas_local, basename(mb_src)), overwrite = FALSE)

  mb_files <- sort(list.files(mapbiomas_local, "\\.tif$", full.names = TRUE))
  mb_years <- as.integer(substr(basename(mb_files), 1, 4))

  muni_sf_mb <- st_read(shp_path, quiet = TRUE) %>%
    mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
    st_transform(4326)
  col_ext <- ext(vect(muni_sf_mb))

  # C1. National totals
  mb_nat <- lapply(seq_along(mb_years), function(i) {
    cat(sprintf("[%d/%d] %d ...", i, length(mb_years), mb_years[i]))
    r   <- crop(rast(mb_files[i]), col_ext)
    cnt <- global(r == FOREST_CLASS, "sum", na.rm = TRUE)[[1]]
    cat(sprintf(" %.0f px\n", cnt))
    rm(r); gc(verbose = FALSE)
    data.frame(year = mb_years[i], mb_forest_ha = cnt * px_area_ha_mb)
  })
  mb_national <- bind_rows(mb_nat) %>%
    arrange(year) %>%
    mutate(mb_loss_ha = pmax(0, lag(mb_forest_ha) - mb_forest_ha))
  saveRDS(mb_national, file.path(out_dir, "mb_national_loss.rds"))

  # C2. Municipal, 6 years, downsampled
  val_years         <- c(2001, 2005, 2010, 2015, 2020, 2023)
  all_needed        <- sort(unique(c(val_years, val_years - 1L)))
  all_idx           <- which(mb_years %in% all_needed)
  agg_fact          <- 17L
  px_area_ha_coarse <- (30 * agg_fact)^2 / 10000

  fyl <- vector("list", length(all_idx))
  for (k in seq_along(all_idx)) {
    i <- all_idx[k]; yr <- mb_years[i]
    cat(sprintf("[%d/%d] Year %d ...\n", k, length(all_idx), yr))
    r_agg  <- aggregate(ifel(crop(rast(mb_files[i]), col_ext) == FOREST_CLASS, 1L, 0L),
                        fact = agg_fact, fun = "mean", na.rm = TRUE)
    px     <- exact_extract(r_agg, muni_sf_mb, fun = "sum", progress = FALSE)
    fyl[[k]] <- data.frame(MPIO_CDPMP = muni_sf_mb$MPIO_CDPMP,
                           year = yr, forest_ha = px * px_area_ha_coarse)
    rm(r_agg); gc(verbose = FALSE)
  }
  mb_loss_raster <- bind_rows(fyl) %>%
    arrange(MPIO_CDPMP, year) %>%
    group_by(MPIO_CDPMP) %>%
    mutate(mb_loss_ha = pmax(0, lag(forest_ha) - forest_ha)) %>%
    filter(year %in% val_years, !is.na(mb_loss_ha), mb_loss_ha > 0) %>%
    select(MPIO_CDPMP, year, mb_loss_ha)
  saveRDS(mb_loss_raster, file.path(out_dir, "mb_muni_loss.rds"))
}
