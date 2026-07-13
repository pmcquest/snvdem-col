# ====================================================================== #
# Colombia Municipal-Level Deforestation and Subnational Democracy
#
# Author: Patrick McQuestion, with editing help from Claude Code Sonnet 5
# Version: July 10, 2026 (v2 -- aligned to Sanford (2023) methodology)
#
# Research question:
#   Does subnational democracy (snelect / sncivlib / sndem) correlate with
#   municipal forest-cover change in Colombia, 2001-2023?
#
# Changes from v1 (June 25, 2026) -- see deforestation_memo.md for the full
# rationale:
#   - DV: percentage-point change in forest cover, Delta forest_pct(t) =
#     forest_pct(t) - forest_pct(t-1), NOT loss_ha / municipal_area. This
#     matches Sanford (2023)'s first-differenced DV, which sidesteps the
#     unit root in cover *levels* -- and is also the reason it no longer
#     needs a log transform (the DV can be negative/zero/positive).
#   - Control: forest cover level at t-1 (Sanford's "amount of forest
#     remaining ... at the start of the year"), reconstructed from Hansen's
#     treecover2000 baseline band minus cumulative masked lossyear.
#   - Sample: municipalities with zero forest cover at the 2000 baseline are
#     dropped (never eligible to lose forest), per Sanford.
#   - Model: adds a two-way (municipality + year) fixed-effects panel
#     regression, clustered SEs by municipality, alongside the existing
#     correlation/scatter outputs.
#   - Democracy measure: snelect (elections), sncivlib (civil liberties),
#     and sndem (full index) are all tested as alternative main IVs --
#     pre-benchmark values from
#     data/panel/06_benchmark/03_output/snvdem_col_benchmarked.rds.
#   - No additional controls (agriculture/GDP): already embedded in how the
#     SN-VDem index itself is constructed -- decided 2026-07-09, see memo.
#   - Spatial dependence: Moran's I on regression residuals (per year, queen
#     contiguity) and a department-clustered-SE robustness check, addressing whether municipality-
#     year observations can be treated as independent given spatial
#     clustering. See memo Section on spatial dependence.
#   - Added a B&B-style curvilinear marginal-effect plot (6-panel grid) and
#     a combined choropleth (mean democracy vs. mean forest-cover change).
#
# Primary data:
#   - Hansen/GFW Global Forest Change 2023 v1.11: lossyear + treecover2000
#     -> gfc_muni_cover.rds (municipality x year: forest_ha, forest_pct,
#        forest_pct_lag1, d_forest_pct)
#   - snvdem_col_benchmarked.rds: snelect / sncivlib / sndem panel
#
# Validation:
#   - MapBiomas Colombia Collection 3.0 (pre-computed CSV, Formacion
#     Boscosa) -> mb_muni_cover.rds, same DV/control construction
#
# Merge key:  DIVIPOLA 5-digit code (MPIO_CDPMP)
# Outputs:    scripts/deforestation/output/*.rds, *.png, *.tex
#
# ====================================================================== #

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(viridis)
library(fixest)
library(knitr)

# % canopy cover (Hansen treecover2000) required to call a pixel "forest".
# 30% is the standard convention in the GFC-derived deforestation-rate
# literature. Applied to BOTH the 2000 baseline and each year's loss mask,
# so cumulative loss can never exceed the baseline stock.
FOREST_THRESHOLD <- 30

clean_mpio <- function(x) {
  str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
}

# Builds forest_pct, its 1-year lag, and the percentage-point change (the
# DV) from a municipality x year forest_ha series. `df` must already
# contain every year from the baseline year onward (a balanced panel per
# municipality) so lag() steps one calendar year, not one row.
add_forest_change_vars <- function(df, area_df) {
  df %>%
    left_join(area_df, by = "MPIO_CDPMP") %>%
    mutate(forest_pct = forest_ha / area_ha * 100) %>%
    arrange(MPIO_CDPMP, year) %>%
    group_by(MPIO_CDPMP) %>%
    mutate(
      forest_pct_lag1 = lag(forest_pct),
      d_forest_pct    = forest_pct - forest_pct_lag1
    ) %>%
    ungroup()
}

# Sanford excludes cells that never have forest cover (never eligible to
# lose forest). Our cover series has no regrowth term (see Section 1.3
# note), so it is monotonically non-increasing -- "never forested" reduces
# to "zero cover at the baseline year".
exclude_never_forested <- function(df) {
  keep <- df %>%
    filter(year == min(year)) %>%
    filter(forest_pct > 0) %>%
    pull(MPIO_CDPMP)
  filter(df, MPIO_CDPMP %in% keep)
}

shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"


# ====================================================================== #
# ==== SECTION 1: FOREST DATA  (Hansen GFC rasters -> per-municipality) ====
# ====================================================================== #
# Run once to produce gfc_muni_loss.rds, gfc_muni_baseline2000.rds,
# gfc_muni_cover.rds, and muni_areas.rds; skip to Section 2 thereafter.
# Tiles cover Colombia (~1.5 GB lossyear, already on disk from v1, plus
# ~0.8 GB new treecover2000); the treecover2000 download takes ~20-40 min
# once.

if (!file.exists(file.path(out_dir, "gfc_muni_cover.rds"))) {

# ---- 1.1  Download GFC tiles (one-time) --------------------------------

gfc_base <- "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11"
tiles    <- c("00N_070W", "00N_080W", "10N_070W", "10N_080W")
layers   <- c("lossyear", "treecover2000")
tile_dir <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/GFC-2023"

if (!dir.exists(tile_dir)) dir.create(tile_dir, recursive = TRUE)

for (layer in layers) {
  for (tile in tiles) {
    fname <- file.path(tile_dir, paste0("Hansen_GFC-2023-v1.11_", layer, "_", tile, ".tif"))
    if (!file.exists(fname))
      download.file(paste0(gfc_base, "/Hansen_GFC-2023-v1.11_", layer, "_", tile, ".tif"),
                    destfile = fname, mode = "wb")
  }
}

# ---- 1.2  Mosaic, clip, extract per municipality -----------------------

lossyear_files <- list.files(tile_dir, pattern = "lossyear.*\\.tif$", full.names = TRUE)
cover_files    <- list.files(tile_dir, pattern = "treecover2000.*\\.tif$", full.names = TRUE)

lossyear <- mosaic(sprc(lapply(lossyear_files, rast)))
treecov  <- mosaic(sprc(lapply(cover_files, rast)))

muni_sf <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  st_transform(crs = crs(lossyear))

loss_col  <- crop(lossyear, ext(vect(muni_sf)))
cover_col <- crop(treecov, ext(vect(muni_sf)))
forest_mask_2000 <- cover_col >= FOREST_THRESHOLD

muni_areas <- muni_sf %>%
  mutate(area_ha = as.numeric(st_area(.)) / 10000) %>%
  st_drop_geometry() %>%
  select(MPIO_CDPMP, area_ha)

saveRDS(muni_areas, file.path(out_dir, "muni_areas.rds"))

px_area_ha <- 0.09   # 30m pixel ~ 0.09 ha

# Annual loss, masked to pixels that were forest (>= FOREST_THRESHOLD% canopy)
# at the 2000 baseline -- keeps the running cover reconstruction in 1.3
# internally consistent (loss can never exceed remaining stock). This
# differs slightly from v1, which counted all lossyear pixels regardless of
# baseline canopy density.
year_list <- lapply(1:23, function(yr) {
  data.frame(
    MPIO_CDPMP    = muni_sf$MPIO_CDPMP,
    year          = yr + 2000L,
    n_loss_pixels = exact_extract(forest_mask_2000 & (loss_col == yr), muni_sf, fun = "sum")
  )
})

gfc_muni <- bind_rows(year_list) %>%
  filter(n_loss_pixels > 0) %>%
  mutate(loss_ha = n_loss_pixels * px_area_ha) %>%
  select(MPIO_CDPMP, year, loss_ha)

cat("Years:", paste(range(gfc_muni$year), collapse = "-"),
    "| Municipalities with loss:", n_distinct(gfc_muni$MPIO_CDPMP), "\n")

saveRDS(gfc_muni, file.path(out_dir, "gfc_muni_loss.rds"))

# 2000 forest baseline
n_forest_pixels_2000 <- exact_extract(forest_mask_2000, muni_sf, fun = "sum")

gfc_baseline <- data.frame(
  MPIO_CDPMP     = muni_sf$MPIO_CDPMP,
  forest_ha_2000 = n_forest_pixels_2000 * px_area_ha
)

cat("Baseline:", sum(gfc_baseline$forest_ha_2000 > 0), "of", nrow(gfc_baseline),
    "municipalities have forest cover in 2000\n")

saveRDS(gfc_baseline, file.path(out_dir, "gfc_muni_baseline2000.rds"))


# ---- 1.3  Reconstruct annual forest-cover level series (2000-2023) -----
# forest_ha(t) = forest_ha(2000) - cumulative masked loss through year t.
#
# Simplification: Hansen's `gain` layer (regrowth) is not modeled here,
# This makes the reconstructed series monotonically
# non-increasing by construction. Sanford makes the same simplifying call:
# gains are hard to
# attribute to a political event because different tree species take many
# years to register as canopy in the data. 

gfc_baseline <- readRDS(file.path(out_dir, "gfc_muni_baseline2000.rds"))
gfc_loss     <- readRDS(file.path(out_dir, "gfc_muni_loss.rds"))

gfc_cover <- expand_grid(MPIO_CDPMP = gfc_baseline$MPIO_CDPMP, year = 2000:2023) %>%
  left_join(gfc_loss, by = c("MPIO_CDPMP", "year")) %>%
  left_join(gfc_baseline, by = "MPIO_CDPMP") %>%
  mutate(loss_ha = replace_na(loss_ha, 0)) %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    cum_loss_ha = cumsum(loss_ha),
    forest_ha   = pmax(0, forest_ha_2000 - cum_loss_ha)
  ) %>%
  ungroup() %>%
  select(MPIO_CDPMP, year, forest_ha) %>%
  add_forest_change_vars(muni_areas) %>%
  exclude_never_forested()

cat("GFC cover panel:", nrow(gfc_cover), "rows |",
    n_distinct(gfc_cover$MPIO_CDPMP), "municipalities (after excluding never-forested)\n")

saveRDS(gfc_cover, file.path(out_dir, "gfc_muni_cover.rds"))

} else {
  cat("Section 1 (Hansen GFC extraction) skipped -- gfc_muni_cover.rds already exists on disk.\n")
}


# ====================================================================== #
# ==== SECTION 2: ANALYSIS -- democracy x forest-cover change (Hansen GFC) ====
# ====================================================================== #

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))
gfc_cover  <- readRDS(file.path(out_dir, "gfc_muni_cover.rds")) %>%
  filter(year >= 2001)   # 2000 row exists only to seed the year-1 lag

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

norm01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

analysis_df <- gfc_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    snelect_norm  = norm01(snelect),
    sncivlib_norm = norm01(sncivlib),
    sndem_norm    = norm01(sndem)
  )

# merge drops any municipality-year rows that don't have a match
cat("Panel:", nrow(analysis_df), "rows |",
    n_distinct(analysis_df$MPIO_CDPMP), "municipalities |",
    n_distinct(analysis_df$year), "years\n")

# ---- 2.1  Annual summary table -----------------------------------------

analysis_df %>%
  group_by(year) %>%
  summarise(
    mean_d_forest_pct = mean(d_forest_pct, na.rm = TRUE),
    mean_snelect      = mean(snelect_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 30)

# ---- 2.2  Scatter: snelect vs. forest-cover change ----------------------
# No log transform. d_forest_pct can
# be negative (loss), zero, or positive (apparent gain); a horizontal
# reference line at 0 marks no net change.

ggplot(analysis_df, aes(x = snelect_norm, y = d_forest_pct)) +
  geom_point(alpha = 0.06, color = "steelblue", size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  labs(
    title    = "Subnational Election Quality vs. Forest-Cover Change",
    subtitle = "Colombia municipalities, 2001-2023  |  snelect normalized 0-1",
    x        = "snelect score (0 = low quality, 1 = high quality)",
    y        = "Change in forest cover (percentage points, year over year)",
    caption  = "Sources: SN-VDem (pre-benchmark); Hansen/GFW Global Forest Change 2023 v1.11"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "scatter_snelect_forestchange.png"),
       width = 9, height = 6, dpi = 300, bg = "white")

# ---- 2.3  Scatter by period (pre/post 2016 Peace Accord) ---------------

analysis_df %>%
  mutate(period = factor(
    if_else(year < 2016, "Pre-Accord (2001-2015)", "Post-Accord (2016-2023)"),
    levels = c("Pre-Accord (2001-2015)", "Post-Accord (2016-2023)")
  )) %>%
  ggplot(aes(x = snelect_norm, y = d_forest_pct)) +
  geom_point(alpha = 0.05, color = "steelblue", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  facet_wrap(~period) +
  labs(
    title   = "Election Quality vs. Forest-Cover Change: Before and After the 2016 Peace Accord",
    x       = "snelect score (0 = low, 1 = high)",
    y       = "Change in forest cover (percentage points)",
    caption = "Sources: SN-VDem (pre-benchmark); Hansen/GFW GFC 2023 v1.11"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(out_dir, "scatter_snelect_forestchange_period.png"),
       width = 11, height = 6, dpi = 300, bg = "white")

# ---- 2.4  Time-series: national forest stock and mean snelect ----------

analysis_df %>%
  group_by(year) %>%
  summarise(
    `Forest stock (000 ha)`       = sum(forest_ha, na.rm = TRUE) / 1000,
    `Mean forest change (pp)`     = mean(d_forest_pct, na.rm = TRUE),
    `Mean snelect (0-1)`          = mean(snelect_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  facet_wrap(~series, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Forest stock (000 ha)"   = "#2d7a2d",
                                "Mean forest change (pp)" = "#b0521e",
                                "Mean snelect (0-1)"      = "#1e66b0")) +
  labs(
    title   = "National Trends: Forest Stock, Forest-Cover Change, and Election Quality",
    x       = "Year",
    caption = "Sources: SN-VDem (pre-benchmark); Hansen/GFW GFC 2023 v1.11"
  ) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave(file.path(out_dir, "timeseries_forestchange.png"),
       width = 9, height = 9, dpi = 300, bg = "white")

# ---- 2.5  Correlations: each democracy measure vs. forest-cover change --

for (v in c("snelect_norm", "sncivlib_norm", "sndem_norm")) {
  cr <- cor.test(analysis_df[[v]], analysis_df$d_forest_pct, use = "complete.obs")
  cat(sprintf("Pearson r (%s vs. Delta forest_pct): %.4f  p = %s\n",
              v, cr$estimate, format(cr$p.value, scientific = TRUE, digits = 3)))
}

# ---- 2.6  Map: mean annual forest-cover change by municipality ---------
# Diverging scale centered at 0 (red = net loss, green = net gain) --
# replaces v1's log1p sequential scale, which assumed a positive-only
# quantity.

map_summary_gfc <- analysis_df %>%
  group_by(MPIO_CDPMP) %>%
  summarise(mean_d_forest_pct = mean(d_forest_pct, na.rm = TRUE), .groups = "drop")

map_summary_gfc %>%
  { left_join(st_read(shp_path, quiet = TRUE) %>%
                mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)), ., by = "MPIO_CDPMP") } %>%
  ggplot() +
  geom_sf(aes(fill = mean_d_forest_pct), color = NA) +
  scale_fill_distiller(
    palette   = "RdYlGn",
    direction = 1,
    name      = "Mean annual\nforest-cover\nchange (pp)",
    na.value  = "grey85",
    limits    = c(-1, 1) * max(abs(map_summary_gfc$mean_d_forest_pct), na.rm = TRUE),
    labels    = scales::comma
  ) +
  labs(
    title    = "Mean Annual Forest-Cover Change by Municipality",
    subtitle = "Colombia, 2001-2023  |  red = net loss, green = net gain",
    caption  = "Source: Hansen/GFW Global Forest Change 2023 v1.11"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(out_dir, "map_mean_forestchange.png"),
       width = 9, height = 11, dpi = 300, bg = "white")

# ---- 2.7  Fixed-effects panel regression (Sanford / Boehmelt & Bernauer spec) ----
# Delta forest_pct_it ~ democracy_it + forest_pct_(i,t-1) | muni + year
# One control only (lagged forest-cover level) -- 
# agriculture/GDP controls are already embedded in how the
# SN-VDem index is generated, so adding them again would double-count the
# same underlying data. Clustered SEs by municipality. Three parallel
# specifications (not one joint model) because sndem is a composite of
# snelect and sncivlib -- including all three together would be mechanical
# multicollinearity. Democracy measures enter
# on their normalized 0-1 scale (see Section 2) so coefficients are
# comparable across the three measures and the quadratic turning point
# below is interpretable within-sample.

fe_snelect  <- feols(d_forest_pct ~ snelect_norm  + forest_pct_lag1 | MPIO_CDPMP + year,
                      data = analysis_df, cluster = ~MPIO_CDPMP)
fe_sncivlib <- feols(d_forest_pct ~ sncivlib_norm + forest_pct_lag1 | MPIO_CDPMP + year,
                      data = analysis_df, cluster = ~MPIO_CDPMP)
fe_sndem    <- feols(d_forest_pct ~ sndem_norm    + forest_pct_lag1 | MPIO_CDPMP + year,
                      data = analysis_df, cluster = ~MPIO_CDPMP)

fe_table <- etable(fe_snelect, fe_sncivlib, fe_sndem,
                    headers = c("Elections\n(snelect)", "Civil liberties\n(sncivlib)", "Full index\n(sndem)"),
                    title   = "Democracy and Forest-Cover Change (GFC), Linear Specification, 2001-2023")
print(fe_table)

etable(fe_snelect, fe_sncivlib, fe_sndem,
       headers = c("Elections (snelect)", "Civil liberties (sncivlib)", "Full index (sndem)"),
       title   = "Democracy and Forest-Cover Change (GFC), Linear Specification, 2001-2023",
       tex     = TRUE, file = file.path(out_dir, "fe_regression_table_gfc.tex"), replace = TRUE)

# ---- 2.7b  Curvilinear specification (Boehmelt & Bernauer 2025) --------
# B&B extend Sanford's linear spec with a squared democracy term and find
# a U-shaped relationship: forest-cover growth is lowest (deforestation
# highest) at intermediate democracy, and better at both extremes. Testing
# the same functional form here.

fe_snelect_q  <- feols(d_forest_pct ~ snelect_norm  + I(snelect_norm^2)  + forest_pct_lag1 | MPIO_CDPMP + year,
                        data = analysis_df, cluster = ~MPIO_CDPMP)
fe_sncivlib_q <- feols(d_forest_pct ~ sncivlib_norm + I(sncivlib_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                        data = analysis_df, cluster = ~MPIO_CDPMP)
fe_sndem_q    <- feols(d_forest_pct ~ sndem_norm    + I(sndem_norm^2)    + forest_pct_lag1 | MPIO_CDPMP + year,
                        data = analysis_df, cluster = ~MPIO_CDPMP)

# fixest 0.14.0 double-wraps I(x^2) terms internally -- names(coef(fe_snelect_q))
# actually returns "I(I(snelect_norm^2))", not "I(snelect_norm^2)" as written
# in the formula. Two
# workarounds follow: turning_point() below uses positional indexing rather
# than name-based lookup (a name-based lookup on "I(x^2)" would silently
# return NA), and these dicts rename the doubled label to something clean
# for every etable() call that includes a quadratic model.
sq_dict_txt <- c(
  "I(I(snelect_norm^2))"  = "snelect_norm^2",
  "I(I(sncivlib_norm^2))" = "sncivlib_norm^2",
  "I(I(sndem_norm^2))"    = "sndem_norm^2"
)
sq_dict_tex <- c(
  "I(I(snelect_norm^2))"  = "snelect\\_norm\\textsuperscript{2}",
  "I(I(sncivlib_norm^2))" = "sncivlib\\_norm\\textsuperscript{2}",
  "I(I(sndem_norm^2))"    = "sndem\\_norm\\textsuperscript{2}"
)

fe_table_q <- etable(fe_snelect_q, fe_sncivlib_q, fe_sndem_q,
                      headers = c("Elections\n(snelect)", "Civil liberties\n(sncivlib)", "Full index\n(sndem)"),
                      title   = "Democracy and Forest-Cover Change (GFC), Curvilinear Specification, 2001-2023",
                      dict    = sq_dict_txt)
print(fe_table_q)

etable(fe_snelect_q, fe_sncivlib_q, fe_sndem_q,
       headers = c("Elections (snelect)", "Civil liberties (sncivlib)", "Full index (sndem)"),
       title   = "Democracy and Forest-Cover Change (GFC), Curvilinear Specification, 2001-2023",
       dict    = sq_dict_tex,
       tex     = TRUE, file = file.path(out_dir, "fe_regression_table_gfc_quadratic.tex"), replace = TRUE)

# Turning point of y = a + b*x + c*x^2 is x* = -b / (2c), on the normalized
# 0-1 scale (0 = least democratic municipality observed, 1 = most). Positional
# indexing, not name-based -- see the double-wrapping note above.
turning_point <- function(model) {
  b <- coef(model)[1]
  c <- coef(model)[2]
  unname(-b / (2 * c))
}

cat("\n=== Turning points (normalized 0-1 scale), GFC ===\n")
cat(sprintf("snelect:  %.3f\n", turning_point(fe_snelect_q)))
cat(sprintf("sncivlib: %.3f\n", turning_point(fe_sncivlib_q)))
cat(sprintf("sndem:    %.3f\n", turning_point(fe_sndem_q)))

# ---- 2.8  Spatial dependence diagnostic: Moran's I on residuals --------
# Michael's question (2026-07-12): are municipality-year residuals spatially
# correlated? Municipality FE removes each muni's own time-invariant
# geography; year FE removes national annual shocks; muni-clustered SEs
# handle serial correlation *within* a municipality over time. None of that
# addresses correlated shocks *across* neighboring municipalities within the
# same year (e.g. a regional conflict spike). Moran's I, computed separately
# for each year on that year's residuals using queen contiguity between
# municipalities, tests exactly that.

library(spdep)

resid_muni_ids <- sort(unique(analysis_df$MPIO_CDPMP))

muni_sf_nb <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(MPIO_CDPMP %in% resid_muni_ids) %>%
  arrange(MPIO_CDPMP)

# Shapefile data used; ~1050 polygons at full cadastral resolution (expect delay)
nb <- poly2nb(muni_sf_nb, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

analysis_df$resid_snelect <- resid(fe_snelect)

moran_by_year <- lapply(sort(unique(analysis_df$year)), function(yr) {
  # Sort to match muni_sf_nb's row order -- moran.test has no ID column,
  # it matches vector position i to listw unit i positionally.
  yr_df <- analysis_df %>% filter(year == yr) %>% arrange(MPIO_CDPMP)
  stopifnot(identical(yr_df$MPIO_CDPMP, muni_sf_nb$MPIO_CDPMP))
  # One Moran's I per year: are this year's residuals spatially clustered
  # under the queen-contiguity weights built above?
  mt <- moran.test(yr_df$resid_snelect, lw, zero.policy = TRUE)
  data.frame(year = yr,
             moran_i = unname(mt$estimate["Moran I statistic"]),
             p_value = mt$p.value)
}) %>% bind_rows()

cat("\n=== Moran's I on GFC snelect-model residuals, by year (queen contiguity) ===\n")
print(moran_by_year)
cat(sprintf("\nYears with significant (p<0.05) positive spatial autocorrelation: %d of %d\n",
            sum(moran_by_year$p_value < 0.05 & moran_by_year$moran_i > 0), nrow(moran_by_year)))

saveRDS(moran_by_year, file.path(out_dir, "moran_by_year_gfc.rds"))

# ---- 2.9  Robustness check: cluster SEs by department instead of municipality
# Colombia has 33 departments (fewer, larger clusters than 1,050
# municipalities). If results survive department-clustering, that's
# reassuring against the same spatial-dependence concern Moran's I tests
# directly -- though note the standard few-clusters caveat (Cameron and Miller 2015):
# with only 33 clusters, cluster-robust SEs themselves become less reliable,
# so this is a check, not a replacement for municipality-clustering.

muni_dept <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP),
         DPTO_CCDGO = as.character(DPTO_CCDGO)) %>%
  st_drop_geometry() %>%
  select(MPIO_CDPMP, DPTO_CCDGO)

analysis_df <- analysis_df %>% left_join(muni_dept, by = "MPIO_CDPMP")

cat("\nNumber of departments in GFC panel:", n_distinct(analysis_df$DPTO_CCDGO), "\n")

fe_snelect_dept    <- feols(d_forest_pct ~ snelect_norm  + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)
fe_sncivlib_dept   <- feols(d_forest_pct ~ sncivlib_norm + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)
fe_sndem_dept      <- feols(d_forest_pct ~ sndem_norm    + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)
fe_snelect_q_dept  <- feols(d_forest_pct ~ snelect_norm  + I(snelect_norm^2)  + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)
fe_sncivlib_q_dept <- feols(d_forest_pct ~ sncivlib_norm + I(sncivlib_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)
fe_sndem_q_dept    <- feols(d_forest_pct ~ sndem_norm    + I(sndem_norm^2)    + forest_pct_lag1 | MPIO_CDPMP + year,
                             data = analysis_df, cluster = ~DPTO_CCDGO)

cat("\n=== Municipality-clustered vs. department-clustered SEs, GFC (linear) ===\n")
print(etable(fe_snelect, fe_snelect_dept, fe_sncivlib, fe_sncivlib_dept, fe_sndem, fe_sndem_dept,
             headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)")))

cat("\n=== Municipality-clustered vs. department-clustered SEs, GFC (quadratic) ===\n")
print(etable(fe_snelect_q, fe_snelect_q_dept, fe_sncivlib_q, fe_sncivlib_q_dept, fe_sndem_q, fe_sndem_q_dept,
             headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)"),
             dict = sq_dict_txt))

etable(fe_snelect, fe_snelect_dept, fe_sncivlib, fe_sncivlib_dept, fe_sndem, fe_sndem_dept,
       headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)"),
       tex = TRUE, file = file.path(out_dir, "fe_regression_table_gfc_dept_cluster.tex"), replace = TRUE)


# ====================================================================== #
# ==== SECTION 3: VALIDATION -- MapBiomas Colombia Col. 3.0 vs. Hansen GFC ====
# ====================================================================== #
# Source: MapBiomas Colombia -> Estadisticas -> Cobertura
#         Nivel territorio: Municipios | Clase: Formacion Boscosa
#         Exported CSV saved as: output/mapbiomas_col_muni_stats.csv
#
# CSV format: wide (years 1985-2024 as columns), municipality names only.
# DIVIPOLA codes are recovered by name-matching against the project
# shapefile. Indigenous territories (resguardos) appear in the CSV but have
# no DIVIPOLA code and are dropped -- this affects <5% of forest area.
#
# Unlike Hansen GFC (Section 1), MapBiomas already reports forest area
# directly for every year -- no baseline+cumulative-loss reconstruction is
# needed, only the same DV/control/exclusion treatment as Section 1.3-2.

# ---- 3.1  Parse MapBiomas CSV and recover DIVIPOLA codes ---------------

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))

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

mb_cover_raw <- readr::read_csv(file.path(out_dir, "mapbiomas_col_muni_stats.csv"),
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
  summarise(forest_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  arrange(MPIO_CDPMP, year)

cat("MapBiomas municipality-years parsed:", nrow(mb_cover_raw), "\n")

mb_cover <- mb_cover_raw %>%
  add_forest_change_vars(muni_areas) %>%
  exclude_never_forested()

cat("MapBiomas cover panel:", nrow(mb_cover), "rows |",
    n_distinct(mb_cover$MPIO_CDPMP), "municipalities (after excluding never-forested)\n")

saveRDS(mb_cover, file.path(out_dir, "mb_muni_cover.rds"))


# ---- 3.2  snelect / sncivlib / sndem x MapBiomas forest-cover change ----
# Mirrors Section 2. Strong agreement with GFC results would confirm the
# findings are not sensitive to the choice of remote sensing product.

mb_cover <- readRDS(file.path(out_dir, "mb_muni_cover.rds")) %>%
  filter(year >= 2001)

analysis_mb <- mb_cover %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    snelect_norm  = norm01(snelect),
    sncivlib_norm = norm01(sncivlib),
    sndem_norm    = norm01(sndem)
  )

for (v in c("snelect_norm", "sncivlib_norm", "sndem_norm")) {
  cr <- cor.test(analysis_mb[[v]], analysis_mb$d_forest_pct, use = "complete.obs")
  cat(sprintf("Pearson r (%s vs. Delta forest_pct, MapBiomas): %.4f  p = %s\n",
              v, cr$estimate, format(cr$p.value, scientific = TRUE, digits = 3)))
}

ggplot(analysis_mb, aes(x = snelect_norm, y = d_forest_pct)) +
  geom_point(alpha = 0.06, color = "#2d6e2d", size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  labs(
    title    = "Subnational Election Quality vs. Forest-Cover Change (MapBiomas)",
    subtitle = "Colombia municipalities, 2001-2023  |  snelect normalized 0-1",
    x        = "snelect score (0 = low quality, 1 = high quality)",
    y        = "Change in forest cover (percentage points)",
    caption  = "Sources: SN-VDem (pre-benchmark); MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "scatter_snelect_forestchange_mb.png"),
       width = 9, height = 6, dpi = 300, bg = "white")

fe_snelect_mb  <- feols(d_forest_pct ~ snelect_norm  + forest_pct_lag1 | MPIO_CDPMP + year,
                         data = analysis_mb, cluster = ~MPIO_CDPMP)
fe_sncivlib_mb <- feols(d_forest_pct ~ sncivlib_norm + forest_pct_lag1 | MPIO_CDPMP + year,
                         data = analysis_mb, cluster = ~MPIO_CDPMP)
fe_sndem_mb    <- feols(d_forest_pct ~ sndem_norm    + forest_pct_lag1 | MPIO_CDPMP + year,
                         data = analysis_mb, cluster = ~MPIO_CDPMP)

fe_table_mb <- etable(fe_snelect_mb, fe_sncivlib_mb, fe_sndem_mb,
                       headers = c("Elections\n(snelect)", "Civil liberties\n(sncivlib)", "Full index\n(sndem)"),
                       title   = "Democracy and Forest-Cover Change (MapBiomas), Linear Specification, 2001-2023")
print(fe_table_mb)

etable(fe_snelect_mb, fe_sncivlib_mb, fe_sndem_mb,
       headers = c("Elections (snelect)", "Civil liberties (sncivlib)", "Full index (sndem)"),
       title   = "Democracy and Forest-Cover Change (MapBiomas), Linear Specification, 2001-2023",
       tex     = TRUE, file = file.path(out_dir, "fe_regression_table_mb.tex"), replace = TRUE)

# ---- 3.2b  Curvilinear specification (Boehmelt & Bernauer 2025), MapBiomas

fe_snelect_mb_q  <- feols(d_forest_pct ~ snelect_norm  + I(snelect_norm^2)  + forest_pct_lag1 | MPIO_CDPMP + year,
                           data = analysis_mb, cluster = ~MPIO_CDPMP)
fe_sncivlib_mb_q <- feols(d_forest_pct ~ sncivlib_norm + I(sncivlib_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                           data = analysis_mb, cluster = ~MPIO_CDPMP)
fe_sndem_mb_q    <- feols(d_forest_pct ~ sndem_norm    + I(sndem_norm^2)    + forest_pct_lag1 | MPIO_CDPMP + year,
                           data = analysis_mb, cluster = ~MPIO_CDPMP)

fe_table_mb_q <- etable(fe_snelect_mb_q, fe_sncivlib_mb_q, fe_sndem_mb_q,
                         headers = c("Elections\n(snelect)", "Civil liberties\n(sncivlib)", "Full index\n(sndem)"),
                         title   = "Democracy and Forest-Cover Change (MapBiomas), Curvilinear Specification, 2001-2023",
                         dict    = sq_dict_txt)
print(fe_table_mb_q)

etable(fe_snelect_mb_q, fe_sncivlib_mb_q, fe_sndem_mb_q,
       headers = c("Elections (snelect)", "Civil liberties (sncivlib)", "Full index (sndem)"),
       title   = "Democracy and Forest-Cover Change (MapBiomas), Curvilinear Specification, 2001-2023",
       dict    = sq_dict_tex,
       tex     = TRUE, file = file.path(out_dir, "fe_regression_table_mb_quadratic.tex"), replace = TRUE)

cat("\n=== Turning points (normalized 0-1 scale), MapBiomas ===\n")
cat(sprintf("snelect:  %.3f\n", turning_point(fe_snelect_mb_q)))
cat(sprintf("sncivlib: %.3f\n", turning_point(fe_sncivlib_mb_q)))
cat(sprintf("sndem:    %.3f\n", turning_point(fe_sndem_mb_q)))

# ---- 3.2b  Spatial diagnostics + department-clustering robustness (MapBiomas)
# Same two checks as Section 2.8-2.9, applied to the MapBiomas panel.

resid_muni_ids_mb <- sort(unique(analysis_mb$MPIO_CDPMP))

muni_sf_nb_mb <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(MPIO_CDPMP %in% resid_muni_ids_mb) %>%
  arrange(MPIO_CDPMP)

nb_mb <- poly2nb(muni_sf_nb_mb, queen = TRUE)
lw_mb <- nb2listw(nb_mb, style = "W", zero.policy = TRUE)

analysis_mb$resid_snelect <- resid(fe_snelect_mb)

moran_by_year_mb <- lapply(sort(unique(analysis_mb$year)), function(yr) {
  yr_df <- analysis_mb %>% filter(year == yr) %>% arrange(MPIO_CDPMP)
  stopifnot(identical(yr_df$MPIO_CDPMP, muni_sf_nb_mb$MPIO_CDPMP))
  mt <- moran.test(yr_df$resid_snelect, lw_mb, zero.policy = TRUE)
  data.frame(year = yr,
             moran_i = unname(mt$estimate["Moran I statistic"]),
             p_value = mt$p.value)
}) %>% bind_rows()

cat("\n=== Moran's I on MapBiomas snelect-model residuals, by year (queen contiguity) ===\n")
print(moran_by_year_mb)
cat(sprintf("\nYears with significant (p<0.05) positive spatial autocorrelation: %d of %d\n",
            sum(moran_by_year_mb$p_value < 0.05 & moran_by_year_mb$moran_i > 0), nrow(moran_by_year_mb)))

saveRDS(moran_by_year_mb, file.path(out_dir, "moran_by_year_mb.rds"))

analysis_mb <- analysis_mb %>% left_join(muni_dept, by = "MPIO_CDPMP")

fe_snelect_mb_dept    <- feols(d_forest_pct ~ snelect_norm  + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)
fe_sncivlib_mb_dept   <- feols(d_forest_pct ~ sncivlib_norm + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)
fe_sndem_mb_dept      <- feols(d_forest_pct ~ sndem_norm    + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)
fe_snelect_mb_q_dept  <- feols(d_forest_pct ~ snelect_norm  + I(snelect_norm^2)  + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)
fe_sncivlib_mb_q_dept <- feols(d_forest_pct ~ sncivlib_norm + I(sncivlib_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)
fe_sndem_mb_q_dept    <- feols(d_forest_pct ~ sndem_norm    + I(sndem_norm^2)    + forest_pct_lag1 | MPIO_CDPMP + year,
                                data = analysis_mb, cluster = ~DPTO_CCDGO)

cat("\n=== Municipality-clustered vs. department-clustered SEs, MapBiomas (linear) ===\n")
print(etable(fe_snelect_mb, fe_snelect_mb_dept, fe_sncivlib_mb, fe_sncivlib_mb_dept, fe_sndem_mb, fe_sndem_mb_dept,
             headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)")))

cat("\n=== Municipality-clustered vs. department-clustered SEs, MapBiomas (quadratic) ===\n")
print(etable(fe_snelect_mb_q, fe_snelect_mb_q_dept, fe_sncivlib_mb_q, fe_sncivlib_mb_q_dept, fe_sndem_mb_q, fe_sndem_mb_q_dept,
             headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)"),
             dict = sq_dict_txt))

etable(fe_snelect_mb, fe_snelect_mb_dept, fe_sncivlib_mb, fe_sncivlib_mb_dept, fe_sndem_mb, fe_sndem_mb_dept,
       headers = c("snelect (muni)", "snelect (dept)", "sncivlib (muni)", "sncivlib (dept)", "sndem (muni)", "sndem (dept)"),
       tex = TRUE, file = file.path(out_dir, "fe_regression_table_mb_dept_cluster.tex"), replace = TRUE)


# ---- 3.3  Cross-source validation: forest-cover change, GFC vs. MapBiomas

compare_df <- gfc_cover %>%
  filter(year >= 2001) %>%
  select(MPIO_CDPMP, year, gfc_d_forest_pct = d_forest_pct) %>%
  inner_join(mb_cover %>% select(MPIO_CDPMP, year, mb_d_forest_pct = d_forest_pct),
             by = c("MPIO_CDPMP", "year"))

cat("Joined municipality-year observations:", nrow(compare_df), "\n")

cor_p <- cor.test(compare_df$gfc_d_forest_pct, compare_df$mb_d_forest_pct, method = "pearson")
cor_s <- cor.test(compare_df$gfc_d_forest_pct, compare_df$mb_d_forest_pct, method = "spearman", exact = FALSE)
cat(sprintf("Pearson  r   = %.3f  (p = %.2e)\n", cor_p$estimate, cor_p$p.value))
cat(sprintf("Spearman rho = %.3f  (p = %.2e)\n", cor_s$estimate, cor_s$p.value))

# Expect this to be modest (r/rho ~ 0.2):
#  - GFC is <= 0 by construction (no regrowth term); MapBiomas
#    reports actual annual class area, so it can and does show gains (up to
#    +9pp here).
#  - The two products measure different constructs: GFC flags pixel-level
#    canopy disturbance against a hard 30% threshold, while MapBiomas assigns
#    an annual land-cover class via classifier -- year-to-year class flips
#    (including spurious ones, near the forest/non-forest boundary) show up
#    as MapBiomas "change" with no GFC counterpart, which is most of why the
#    scatter fans out so widely right at gfc_d_forest_pct == 0.
#  - Both series are first differences, not levels -- differencing amplifies
#    each product's own measurement noise relative to signal, so even two
#    reasonably-agreeing sources will correlate much more weakly in Delta
#    than they would in forest_pct levels.

ggplot(compare_df, aes(x = gfc_d_forest_pct, y = mb_d_forest_pct)) +
  geom_point(alpha = 0.07, color = "steelblue", size = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_vline(xintercept = 0, color = "grey70") +
  labs(
    title    = "Hansen GFC vs. MapBiomas Colombia: Forest-Cover Change by Municipality-Year",
    subtitle = sprintf("Pearson r = %.2f  |  Spearman rho = %.2f  |  dashed = perfect agreement",
                       cor_p$estimate, cor_s$estimate),
    x        = "Hansen GFC: Delta forest cover (pp)",
    y        = "MapBiomas: Delta forest cover (pp)",
    caption  = "Sources: Hansen/GFW GFC 2023 v1.11; MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "validation_scatter_gfc_vs_mb.png"),
       width = 8, height = 6, dpi = 300, bg = "white")

# ---- 3.4  Annual national totals (forest stock, both sources) ----------

bind_rows(
  gfc_cover %>% filter(year >= 2001) %>% mutate(source = "Hansen GFC"),
  mb_cover  %>% mutate(source = "MapBiomas Col. 3.0")
) %>%
  group_by(year, source) %>%
  summarise(total_forest_ha = sum(forest_ha, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_forest_ha / 1e6, color = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Hansen GFC" = "steelblue",
                                "MapBiomas Col. 3.0" = "darkgreen")) +
  labs(
    title    = "National Forest Stock: Hansen GFC vs. MapBiomas Colombia",
    subtitle = "2001-2023",
    x        = NULL,
    y        = "Forest area (million ha)",
    color    = NULL,
    caption  = "Sources: Hansen/GFW GFC 2023 v1.11; MapBiomas Colombia Collection 3.0"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "validation_annual_trends.png"),
       width = 9, height = 5, dpi = 300, bg = "white")


# ====================================================================== #
# ==== SECTION 4: DEEPER ANALYSIS -- snelect x forest-cover change ====
# ====================================================================== #
# Extends Section 2.5 (GFC) and Section 3.2 (MapBiomas) with:
#   4.1  Pre / post 2016 Peace Accord split
#   4.2  Decile breakdown (mean Delta forest_pct by snelect decile)
#   4.3  Municipality-level averages (collapse across years)
#   4.4  Annual Pearson r (year-by-year)
#
# v1's "nonzero-loss only" and "log-transformed loss rate" subsections are
# dropped here -- they existed specifically to work around the log(DV)
# problem Kelly flagged (2026-07-02), which the percentage-point DV
# resolves directly. Keeping them would reintroduce the same distortion
# this whole revision is meant to fix.
#
# Requires: analysis_df (GFC) and analysis_mb (MapBiomas) from Sections 2-3.

# ---- 4.1  Pre / post 2016 Peace Accord ----------------------------------

cat("\n=== 4.1  Pre / post 2016 Peace Accord ===\n")

for (src in c("GFC", "MB")) {
  df <- if (src == "GFC") analysis_df else analysis_mb
  for (period in c("Pre-2016", "Post-2016")) {
    sub <- if (period == "Pre-2016") filter(df, year < 2016) else filter(df, year >= 2016)
    cr  <- cor.test(sub$snelect_norm, sub$d_forest_pct, use = "complete.obs")
    cat(sprintf("%s  %s: r = %.4f  (n = %d)\n",
                src, period, cr$estimate,
                sum(complete.cases(sub[c("snelect_norm", "d_forest_pct")]))))
  }
}
# Result (memo Sec. 3): the snelect-d_forest_pct correlation roughly
# quadruples after the Accord in both sources (GFC 0.052 -> 0.189; MapBiomas
# 0.018 -> 0.159). Simple pre/post correlations, not FE-adjusted -- but
# directionally consistent with the 2017 loss-rate spike in Section 2.4 and
# the documented land-use scramble in former FARC territory post-Accord.

# ---- 4.2  Decile breakdown: mean forest-cover change by snelect decile ----

cat("\n=== 4.2  Mean Delta forest_pct by snelect decile (GFC, full panel) ===\n")

analysis_df %>%
  mutate(decile = ntile(snelect_norm, 10)) %>%
  group_by(decile) %>%
  summarise(
    snelect_mean      = mean(snelect_norm, na.rm = TRUE),
    d_forest_pct_mean = mean(d_forest_pct, na.rm = TRUE),
    d_forest_pct_med  = median(d_forest_pct, na.rm = TRUE),
    n                 = n(),
    .groups = "drop"
  ) %>%
  print(n = 10)
# This is the curvilinear (U-shaped) pattern from Section 2.7b's regression,
# but read directly off raw group means -- no FE, no controls, no quadratic
# term fit. If the worst d_forest_pct_mean decile sits in the middle
# (roughly decile 4-6) rather than at either end, that's a useful sanity
# check: the U-shape is visible in the unmodeled data, not an artifact of the
# functional form imposed on it.

# ---- 4.3  Municipality-level averages (collapse across years) -----------

cat("\n=== 4.3  Municipality-level averages ===\n")

muni_gfc <- analysis_df %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    snelect_mean      = mean(snelect_norm, na.rm = TRUE),
    d_forest_pct_mean = mean(d_forest_pct, na.rm = TRUE),
    .groups = "drop"
  )

muni_mb <- analysis_mb %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    snelect_mean      = mean(snelect_norm, na.rm = TRUE),
    d_forest_pct_mean = mean(d_forest_pct, na.rm = TRUE),
    .groups = "drop"
  )

cor_m_gfc <- cor.test(muni_gfc$snelect_mean, muni_gfc$d_forest_pct_mean, use = "complete.obs")
cor_m_mb  <- cor.test(muni_mb$snelect_mean,  muni_mb$d_forest_pct_mean,  use = "complete.obs")

cat(sprintf("GFC  municipality-mean Pearson r = %.4f  (n = %d)\n",
            cor_m_gfc$estimate, nrow(muni_gfc)))
cat(sprintf("MB   municipality-mean Pearson r = %.4f  (n = %d)\n",
            cor_m_mb$estimate,  nrow(muni_mb)))
# Result (memo Sec. 3): collapsing to one row per municipality strengthens
# the relationship, most visibly for MapBiomas (r = 0.292 here vs. 0.064
# pooled across municipality-years). Reading alongside 4.1/4.4: a
# municipality's *average* democracy score over the panel tracks its
# *average* forest trend more cleanly than year-to-year snelect fluctuations
# track that same year's forest change -- consistent with this being a
# slower-moving structural relationship, not one democracy shifts should be
# expected to move within a single year.

# ---- 4.4  Annual Pearson r (year-by-year, GFC) --------------------------

cat("\n=== 4.4  Annual Pearson r, GFC ===\n")

analysis_df %>%
  group_by(year) %>%
  summarise(
    r = cor(snelect_norm, d_forest_pct, use = "complete.obs"),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(r = round(r, 4)) %>%
  print(n = 30)
# This is the disaggregated view behind 4.1 and 4.3's summary numbers --
# expect r to bounce around (some years near zero or slightly negative)
# rather than tracking smoothly. That year-to-year noise is exactly why
# 4.1's pre/post split and 4.3's municipality-mean collapse both show a
# cleaner relationship than any single year here does on its own.


# ====================================================================== #
# ==== SECTION 5: CURVILINEAR VISUALIZATION AND SPATIAL PATTERN MAP ====
# ====================================================================== #
# 5.1 mirrors Boehmelt & Bernauer's Figure 2 (predicted forest-cover-change
# curve across the democracy range, with a confidence band) for all three
# democracy measures x both forest-data sources.
# 5.2 maps mean democracy and mean forest-cover change side by side, so a
# reader can visually compare where governance is weak/strong against where
# deforestation is worst/best -- NOTE: because the underlying relationship
# is curvilinear (Section 2.7b/3.2b), NOT monotonic, this map will not show
# a simple "low democracy = high loss" gradient -- the worst loss clusters
# at INTERMEDIATE democracy (see the decile breakdown, Section 4.2). The
# map lets the geographic pattern speak for itself, not to imply a
# monotonic story the regression already rules out.

library(patchwork)

# ---- 5.1  Curvilinear marginal-effect plots -----------------------------
# Predicted curve f(x) = b1*x + b2*x^2, relative to x=0 (the least
# democratic municipality-year observed in the sample), with a 95% CI band
# from the delta method using the model's (cluster-robust) covariance
# matrix. FE intercepts are absorbed by the model and don't have a single
# meaningful "baseline level" to add back, so the plot shows the *relative*
# predicted change as democracy increases from its observed minimum.

curve_data <- function(model) {
  cf  <- coef(model)
  vc  <- vcov(model)
  b1  <- cf[1]; b2 <- cf[2]
  v11 <- vc[1, 1]; v22 <- vc[2, 2]; v12 <- vc[1, 2]

  x   <- seq(0, 1, by = 0.01)
  fit <- b1 * x + b2 * x^2
  se  <- sqrt(v11 * x^2 + v22 * x^4 + 2 * v12 * x^3)
  data.frame(x = x, fit = fit, lo = fit - 1.96 * se, hi = fit + 1.96 * se)
}

plot_curve <- function(model, title) {
  ggplot(curve_data(model), aes(x = x, y = fit)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.25) +
    geom_line(color = "darkred", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 9)
}

curve_panels <- list(
  plot_curve(fe_snelect_q,     "snelect (GFC)")      + labs(y = "Delta forest cover, pp\n(rel. to least democratic)"),
  plot_curve(fe_snelect_mb_q,  "snelect (MapBiomas)"),
  plot_curve(fe_sncivlib_q,    "sncivlib (GFC)")      + labs(y = "Delta forest cover, pp\n(rel. to least democratic)"),
  plot_curve(fe_sncivlib_mb_q, "sncivlib (MapBiomas)"),
  plot_curve(fe_sndem_q,       "sndem (GFC)")         + labs(x = "Democracy score (normalized 0-1)", y = "Delta forest cover, pp\n(rel. to least democratic)"),
  plot_curve(fe_sndem_mb_q,    "sndem (MapBiomas)")   + labs(x = "Democracy score (normalized 0-1)")
)

combined_curves <- wrap_plots(curve_panels, ncol = 2) +
  plot_annotation(
    title    = "Predicted Forest-Cover Change Across the Democracy Range",
    subtitle = "Curvilinear (quadratic) fixed-effects models, 95% CI (cluster-robust by municipality)",
    caption  = "Sources: SN-VDem (pre-benchmark); Hansen/GFW GFC 2023 v1.11; MapBiomas Colombia Collection 3.0"
  )

ggsave(file.path(out_dir, "curvilinear_effects_grid.png"), combined_curves,
       width = 9, height = 10, dpi = 300, bg = "white")

# ---- 5.2  Combined choropleth: mean democracy vs. mean forest-cover change

muni_shp <- st_read(shp_path, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

muni_means <- analysis_df %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    mean_snelect_norm = mean(snelect_norm, na.rm = TRUE),
    mean_d_forest_pct = mean(d_forest_pct, na.rm = TRUE),
    .groups = "drop"
  )

map_df <- muni_shp %>% left_join(muni_means, by = "MPIO_CDPMP")

map_democracy <- ggplot(map_df) +
  geom_sf(aes(fill = mean_snelect_norm), color = NA) +
  scale_fill_distiller(palette = "PuBu", direction = 1, na.value = "grey85",
                        name = "Mean snelect\n(normalized 0-1)") +
  labs(title = "Municipal Democracy (snelect)", subtitle = "Mean 2001-2023") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

map_forestchange <- ggplot(map_df) +
  geom_sf(aes(fill = mean_d_forest_pct), color = NA) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, na.value = "grey85",
                        name = "Mean annual\nforest-cover\nchange (pp)") +
  labs(title = "Forest-Cover Change (GFC)", subtitle = "Mean 2001-2023, red = loss") +
  theme_void(base_size = 9) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40"))

combined_map <- map_democracy + map_forestchange +
  plot_annotation(
    title    = "Where Democracy Is Weak/Strong vs. Where Forest Loss Is Worst/Best",
    subtitle = "The relationship is curvilinear, not a simple gradient -- see Section 2.7b/3.2b for the formal test",
    caption  = "Sources: SN-VDem (pre-benchmark); Hansen/GFW Global Forest Change 2023 v1.11"
  )

ggsave(file.path(out_dir, "map_democracy_vs_forestchange.png"), combined_map,
       width = 12, height = 8, dpi = 300, bg = "white")

# ---- 5.2b  Appendix 2 tables: municipality deforestation vs. mean snelect ----
# Numeric complement to the map above: the map is
# persuasive but qualitative, so these give the same comparison as numbers.
# Two views, both from muni_gfc (Section 4.3, municipality-level means):
#  (a) tercile table -- bins municipalities by their own mean d_forest_pct
#      (the inverse of Section 4.2, which binned by snelect instead) and
#      reports mean snelect per bin.
#  (b) named leaderboard -- the 10 highest-loss and 10 lowest-loss/highest-
#      gain municipalities with their snelect score, so the map's pattern is
#      checkable against real places, not just color shades.

muni_terciles_gfc <- muni_gfc %>%
  mutate(
    loss_tercile = ntile(d_forest_pct_mean, 3),
    loss_tercile = factor(loss_tercile, levels = 3:1,
                           labels = c("Highest loss", "Middle", "Lowest loss / gain"))
  ) %>%
  group_by(loss_tercile) %>%
  summarise(
    `Mean snelect`        = mean(snelect_mean, na.rm = TRUE),
    `Mean d_forest_pct`   = mean(d_forest_pct_mean, na.rm = TRUE),
    `N municipalities`    = n(),
    .groups = "drop"
  ) %>%
  arrange(loss_tercile) %>%
  rename(`Forest-cover-change tercile` = loss_tercile)

cat("\n=== Appendix 2: forest-cover-change tercile x mean snelect (GFC) ===\n")
print(muni_terciles_gfc)

muni_names <- st_read(shp_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, MPIO_CNMBR)

muni_leaderboard <- muni_gfc %>%
  left_join(muni_names, by = "MPIO_CDPMP") %>%
  arrange(d_forest_pct_mean) %>%
  transmute(
    Municipality      = MPIO_CNMBR,
    DIVIPOLA          = MPIO_CDPMP,
    `Mean snelect`     = snelect_mean,
    `Mean d_forest_pct` = d_forest_pct_mean
  )

leaderboard_top10    <- head(muni_leaderboard, 10)   # highest loss (most negative)
leaderboard_bottom10 <- tail(muni_leaderboard, 10) %>% arrange(desc(`Mean d_forest_pct`))

cat("\n=== Appendix 2: 10 highest-loss municipalities (GFC) ===\n")
print(leaderboard_top10, n = 10)
cat("\n=== Appendix 2: 10 lowest-loss / highest-gain municipalities (GFC) ===\n")
print(leaderboard_bottom10, n = 10)

save_kable_tex <- function(df, file, caption) {
  writeLines(as.character(kable(df, format = "latex", booktabs = TRUE,
                                 digits = 3, caption = caption)),
             file)
}

save_kable_tex(muni_terciles_gfc,
                file.path(out_dir, "appendix2_tercile_table.tex"),
                "Mean snelect by forest-cover-change tercile (GFC, municipality-level means)")
save_kable_tex(leaderboard_top10,
                file.path(out_dir, "appendix2_leaderboard_top10.tex"),
                "10 highest-loss municipalities (GFC, municipality-level means)")
save_kable_tex(leaderboard_bottom10,
                file.path(out_dir, "appendix2_leaderboard_bottom10.tex"),
                "10 lowest-loss / highest-gain municipalities (GFC, municipality-level means)")

# Same two tables, MapBiomas source (muni_mb from Section 4.3) -- mirrors
# the GFC pair above so the memo can show both sources side by side, same
# caveat from Section 3.3/memo Open Question 6 applies: MapBiomas measures
# land-cover-class transitions, not canopy disturbance, so don't read
# agreement/disagreement between the two leaderboards as one confirming or
# contradicting the other.

muni_terciles_mb <- muni_mb %>%
  mutate(
    loss_tercile = ntile(d_forest_pct_mean, 3),
    loss_tercile = factor(loss_tercile, levels = 3:1,
                           labels = c("Highest loss", "Middle", "Lowest loss / gain"))
  ) %>%
  group_by(loss_tercile) %>%
  summarise(
    `Mean snelect`        = mean(snelect_mean, na.rm = TRUE),
    `Mean d_forest_pct`   = mean(d_forest_pct_mean, na.rm = TRUE),
    `N municipalities`    = n(),
    .groups = "drop"
  ) %>%
  arrange(loss_tercile) %>%
  rename(`Forest-cover-change tercile` = loss_tercile)

cat("\n=== Appendix 2: forest-cover-change tercile x mean snelect (MapBiomas) ===\n")
print(muni_terciles_mb)

muni_leaderboard_mb <- muni_mb %>%
  left_join(muni_names, by = "MPIO_CDPMP") %>%
  arrange(d_forest_pct_mean) %>%
  transmute(
    Municipality      = MPIO_CNMBR,
    DIVIPOLA          = MPIO_CDPMP,
    `Mean snelect`     = snelect_mean,
    `Mean d_forest_pct` = d_forest_pct_mean
  )

leaderboard_top10_mb    <- head(muni_leaderboard_mb, 10)
leaderboard_bottom10_mb <- tail(muni_leaderboard_mb, 10) %>% arrange(desc(`Mean d_forest_pct`))

cat("\n=== Appendix 2: 10 highest-loss municipalities (MapBiomas) ===\n")
print(leaderboard_top10_mb, n = 10)
cat("\n=== Appendix 2: 10 lowest-loss / highest-gain municipalities (MapBiomas) ===\n")
print(leaderboard_bottom10_mb, n = 10)

save_kable_tex(muni_terciles_mb,
                file.path(out_dir, "appendix2_tercile_table_mb.tex"),
                "Mean snelect by forest-cover-change tercile (MapBiomas, municipality-level means)")
save_kable_tex(leaderboard_top10_mb,
                file.path(out_dir, "appendix2_leaderboard_top10_mb.tex"),
                "10 highest-loss municipalities (MapBiomas, municipality-level means)")
save_kable_tex(leaderboard_bottom10_mb,
                file.path(out_dir, "appendix2_leaderboard_bottom10_mb.tex"),
                "10 lowest-loss / highest-gain municipalities (MapBiomas, municipality-level means)")


# ====================================================================== #
# ==== APPENDIX A: ColOpenData / IDEAM climate data (not pursued) ====
# ====================================================================== #
# Explored as a possible control variable (precipitation). Blocked by
# Notre Dame IT: port 9000 on tracelac.uniandes.edu.co is filtered. Also
# moot given the 2026-07-09 decision not to add controls beyond the
# lagged forest-cover level (Section 2.7).
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

  mpio_codes  <- sort(unique(dem_df$MPIO_CDPMP))
  precip_list <- map(mpio_codes, safe_dl, .progress = TRUE)
  precip_raw  <- bind_rows(compact(precip_list))
  saveRDS(precip_raw, file.path(out_dir, "ideam_precip_raw.rds"))
}


# ====================================================================== #
# ==== APPENDIX B: GFW Data API approach (alternative to raster, not pursued) ====
# ====================================================================== #
# The GFW REST API returns pre-aggregated loss-ha by GADM admin-2 unit.
# Requires free account + API key. Abandoned in favour of the direct
# raster approach (Section 1) because the GADM -> DIVIPOLA crosswalk
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
# refused); sequential 30m extraction runs ~20 min/year x 24 years.
#
# Two fallbacks are preserved:
#   C1. National totals only -- terra::global, no polygon loop (~5 min total)
#   C2. Municipal-level, 6 years, downsampled 30m -> 510m (~30-60 min)

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
