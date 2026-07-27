# ====================================================================== #
# "Hardliner" robustness test: a Hansen/GFW-style ratchet (monotonic,
# loss-only) version of the TMF forest measure, cross-validated against
# GFC and MapBiomas's own primary-forest-adjacent class, then re-run
# through the same panel specifications as the bidirectional ("softliner")
# TMF DV to test whether the curvilinear result is specific to loss-only
# constructions.
#
# Ratchet construction: pixel-level "sticky" ratchet logic (once a pixel is
# flagged lost, it never comes back) can't be reconstructed from TMF's
# municipality-year class *counts* alone -- those tell us how many pixels
# are in each class each year, not which pixels. Instead we apply a
# municipality-level running minimum (cummin) to the existing bidirectional
# forest_pct series. This produces a monotonically non-increasing series by
# construction -- the same aggregate behavior a cumulative clearing mask
# produces -- while requiring no new Earth Engine extraction.
# ====================================================================== #

suppressMessages({
  library(tidyverse)
  library(sf)
  library(fixest)
})

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
norm01     <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
norm_str <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# ---- 1. Build the ratchet TMF series ------------------------------------

tmf <- readRDS(file.path(out_dir, "tmf_muni_cover.rds"))

tmf_ratchet <- tmf %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    forest_pct_ratchet = cummin(forest_pct),
    forest_pct_ratchet_lag1 = lag(forest_pct_ratchet),
    d_forest_pct_ratchet = forest_pct_ratchet - forest_pct_ratchet_lag1
  ) %>%
  ungroup()

cat("=== Ratchet-TMF sanity check ===\n")
cat("Share of d_forest_pct_ratchet == 0 (no new cumulative low reached):",
    round(mean(tmf_ratchet$d_forest_pct_ratchet == 0, na.rm = TRUE), 3), "\n")
cat("Share > 0 (should be exactly 0, ratchet cannot recover):",
    round(mean(tmf_ratchet$d_forest_pct_ratchet > 0, na.rm = TRUE), 3), "\n")
cat("Share < 0:", round(mean(tmf_ratchet$d_forest_pct_ratchet < 0, na.rm = TRUE), 3), "\n\n")

analysis_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2"
saveRDS(tmf_ratchet, file.path(analysis_dir, "tmf_muni_ratchet.rds"))

# ---- 2. Correlate ratchet-TMF against GFC (both loss-only, muni-year) ---

gfc <- readRDS(file.path(out_dir, "gfc_muni_cover.rds")) %>%
  select(MPIO_CDPMP, year, d_forest_pct_gfc = d_forest_pct)

cmp_gfc <- tmf_ratchet %>%
  select(MPIO_CDPMP, year, d_forest_pct_ratchet, d_forest_pct) %>%
  inner_join(gfc, by = c("MPIO_CDPMP", "year")) %>%
  filter(!is.na(d_forest_pct_ratchet), !is.na(d_forest_pct_gfc))

cat("=== Ratchet-TMF vs GFC (muni-year, n =", nrow(cmp_gfc), ") ===\n")
cat("Pearson r  (ratchet-TMF vs GFC):   ",
    round(cor(cmp_gfc$d_forest_pct_ratchet, cmp_gfc$d_forest_pct_gfc, method = "pearson"), 3), "\n")
cat("Spearman rho:                       ",
    round(cor(cmp_gfc$d_forest_pct_ratchet, cmp_gfc$d_forest_pct_gfc, method = "spearman"), 3), "\n")
cat("(for comparison) bidirectional-TMF vs GFC, Pearson r:",
    round(cor(cmp_gfc$d_forest_pct, cmp_gfc$d_forest_pct_gfc, method = "pearson"), 3), "\n\n")

# ---- 3. Pull MapBiomas "1.1. Bosque" (forest-formation-only, closest thing
#         to a "primary/native forest" proxy this collection has) and
#         "1.2. Manglar" (mangrove, the class the memo already flagged as
#         growing) SEPARATELY -- mb_muni_cover.rds sums ALL of class_level_1
#         "1. Formacion Boscosa" (Bosque + Manglar + Bosque inundable +
#         Vegetacion lenosa sobre arena), which is too coarse for this check.

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))

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

build_mb_class <- function(class2_pattern) {
  readr::read_csv(file.path(out_dir, "mapbiomas_col_muni_stats.csv"),
                  show_col_types = FALSE,
                  locale = readr::locale(encoding = "UTF-8")) %>%
    filter(grepl(class2_pattern, class_level_2, ignore.case = TRUE)) %>%
    pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "area_ha") %>%
    mutate(year = as.integer(year), area_ha = as.numeric(area_ha)) %>%
    filter(year >= 2000, year <= 2023) %>%
    mutate(mpio_norm = norm_str(municipio), dpto_norm = norm_str(departamento)) %>%
    left_join(dept_codes,  by = "dpto_norm") %>%
    left_join(muni_lookup, by = c("mpio_norm", "DPTO_CCDGO")) %>%
    filter(!is.na(MPIO_CDPMP)) %>%
    group_by(MPIO_CDPMP, year) %>%
    summarise(forest_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    arrange(MPIO_CDPMP, year) %>%
    left_join(muni_areas, by = "MPIO_CDPMP") %>%
    mutate(forest_pct = forest_ha / area_ha * 100) %>%
    group_by(MPIO_CDPMP) %>%
    mutate(d_forest_pct = forest_pct - lag(forest_pct)) %>%
    ungroup()
}

mb_bosque  <- build_mb_class("^1\\.1\\.")   # 1.1. Bosque
mb_manglar <- build_mb_class("^1\\.2\\.")   # 1.2. Manglar

cat("MapBiomas 'Bosque'  (1.1) muni-years:", nrow(mb_bosque), "\n")
cat("MapBiomas 'Manglar' (1.2) muni-years:", nrow(mb_manglar), "\n\n")

cmp_bosque <- tmf_ratchet %>%
  select(MPIO_CDPMP, year, d_forest_pct_ratchet, d_forest_pct) %>%
  inner_join(mb_bosque %>% select(MPIO_CDPMP, year, d_bosque = d_forest_pct),
             by = c("MPIO_CDPMP", "year")) %>%
  filter(!is.na(d_forest_pct_ratchet), !is.na(d_bosque))

cat("=== Ratchet-TMF vs MapBiomas 'Bosque' (1.1) only, n =", nrow(cmp_bosque), "===\n")
cat("Pearson r:  ", round(cor(cmp_bosque$d_forest_pct_ratchet, cmp_bosque$d_bosque), 3), "\n")
cat("Spearman rho:", round(cor(cmp_bosque$d_forest_pct_ratchet, cmp_bosque$d_bosque, method = "spearman"), 3), "\n")
cat("(for comparison) bidirectional-TMF vs 'Bosque', Pearson r:",
    round(cor(cmp_bosque$d_forest_pct, cmp_bosque$d_bosque), 3), "\n\n")

cmp_manglar <- tmf_ratchet %>%
  select(MPIO_CDPMP, year, d_forest_pct_ratchet, d_forest_pct) %>%
  inner_join(mb_manglar %>% select(MPIO_CDPMP, year, d_manglar = d_forest_pct),
             by = c("MPIO_CDPMP", "year")) %>%
  filter(!is.na(d_forest_pct_ratchet), !is.na(d_manglar))

cat("=== Bidirectional-TMF vs MapBiomas 'Manglar' (1.2) only, n =", nrow(cmp_manglar), "===\n")
cat("Pearson r:  ", round(cor(cmp_manglar$d_forest_pct, cmp_manglar$d_manglar), 3), "\n")
cat("Mean d_manglar (national gain check):", round(mean(cmp_manglar$d_manglar, na.rm = TRUE), 4), "\n")
cat("Mean d_bosque  (national loss check):", round(mean(cmp_bosque$d_bosque, na.rm = TRUE), 4), "\n\n")

# ---- 4. Re-run the panel: ratchet (loss-only, "hardliner") DV vs the
#         existing bidirectional ("softliner") DV, same specifications ----

dem_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  select(MPIO_CDPMP, year, snelect, sncivlib, sndem) %>%
  distinct(MPIO_CDPMP, year, .keep_all = TRUE)

turning_point <- function(model) {
  b <- coef(model)[1]; c <- coef(model)[2]
  unname(-b / (2 * c))
}

analysis_ratchet <- tmf_ratchet %>%
  inner_join(dem_df, by = c("MPIO_CDPMP", "year")) %>%
  mutate(snelect_norm = norm01(snelect)) %>%
  filter(!is.na(d_forest_pct_ratchet), !is.na(forest_pct_ratchet_lag1))

cat("Ratchet panel:", nrow(analysis_ratchet), "rows |",
    n_distinct(analysis_ratchet$MPIO_CDPMP), "municipalities\n\n")

fe_ratchet_lin <- feols(d_forest_pct_ratchet ~ snelect_norm + forest_pct_ratchet_lag1 | MPIO_CDPMP + year,
                        data = analysis_ratchet, cluster = ~MPIO_CDPMP)
fe_ratchet_q   <- feols(d_forest_pct_ratchet ~ snelect_norm + I(snelect_norm^2) + forest_pct_ratchet_lag1 | MPIO_CDPMP + year,
                        data = analysis_ratchet, cluster = ~MPIO_CDPMP)

fe_bidir_lin <- feols(d_forest_pct ~ snelect_norm + forest_pct_lag1 | MPIO_CDPMP + year,
                      data = analysis_ratchet, cluster = ~MPIO_CDPMP)
fe_bidir_q   <- feols(d_forest_pct ~ snelect_norm + I(snelect_norm^2) + forest_pct_lag1 | MPIO_CDPMP + year,
                      data = analysis_ratchet, cluster = ~MPIO_CDPMP)

cat("=== HARDLINER (ratchet, loss-only) DV, full panel, two-way FE ===\n")
cat("-- Linear --\n"); print(summary(fe_ratchet_lin))
cat("-- Quadratic --\n"); print(summary(fe_ratchet_q))
cat("Turning point:", round(turning_point(fe_ratchet_q), 3),
    "| beta2 sign:", ifelse(coef(fe_ratchet_q)[2] > 0, "positive (U-shape)", "negative (inverted-U)"), "\n\n")

cat("=== SOFTLINER (bidirectional) DV, SAME sample as ratchet, two-way FE ===\n")
cat("-- Linear --\n"); print(summary(fe_bidir_lin))
cat("-- Quadratic --\n"); print(summary(fe_bidir_q))
cat("Turning point:", round(turning_point(fe_bidir_q), 3),
    "| beta2 sign:", ifelse(coef(fe_bidir_q)[2] > 0, "positive (U-shape)", "negative (inverted-U)"), "\n\n")

cat("=== Summary comparison ===\n")
cat(sprintf("%-30s %10s %10s\n", "", "linear b1", "quad b2"))
cat(sprintf("%-30s %10.3f %10.3f\n", "Hardliner (ratchet)", coef(fe_ratchet_lin)[1], coef(fe_ratchet_q)[2]))
cat(sprintf("%-30s %10.3f %10.3f\n", "Softliner (bidirectional)", coef(fe_bidir_lin)[1], coef(fe_bidir_q)[2]))

saveRDS(list(fe_ratchet_lin = fe_ratchet_lin, fe_ratchet_q = fe_ratchet_q,
             fe_bidir_lin = fe_bidir_lin, fe_bidir_q = fe_bidir_q,
             cmp_gfc = cmp_gfc, cmp_bosque = cmp_bosque, cmp_manglar = cmp_manglar),
        file.path(analysis_dir, "hardliner_ratchet_results.rds"))
cat("\nSaved results to hardliner_ratchet_results.rds\n")

