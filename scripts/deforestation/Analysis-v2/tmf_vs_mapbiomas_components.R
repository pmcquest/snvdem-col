suppressMessages({
  library(tidyverse)
  library(sf)
})
options(width = 200)

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) stringr::str_pad(as.character(as.numeric(x)), 5, "left", "0")

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
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP), DPTO_CCDGO = as.character(DPTO_CCDGO),
         mpio_norm  = norm_str(MPIO_CNMBR)) %>%
  select(MPIO_CDPMP, DPTO_CCDGO, mpio_norm)

muni_areas <- readRDS(file.path(out_dir, "muni_areas.rds"))

# ---- Parse the FULL MapBiomas class hierarchy (not just Formacion Boscosa) ----
mb_raw <- readr::read_csv(file.path(out_dir, "mapbiomas_col_muni_stats.csv"),
                           show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "area_ha") %>%
  mutate(year = as.integer(year), area_ha = as.numeric(area_ha)) %>%
  filter(year >= 2000, year <= 2023) %>%
  mutate(mpio_norm = norm_str(municipio), dpto_norm = norm_str(departamento)) %>%
  left_join(dept_codes,  by = "dpto_norm") %>%
  left_join(muni_lookup, by = c("mpio_norm", "DPTO_CCDGO")) %>%
  filter(!is.na(MPIO_CDPMP)) %>%
  group_by(MPIO_CDPMP, year, class_level_0, class_level_1, class_level_2) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

cat("MapBiomas class-level rows parsed:", nrow(mb_raw), "\n\n")

# ===================== PART 1: national trend by forest subtype =====================
cat("################ PART 1: NATIONAL TREND, EACH FOREST SUBTYPE (class_level_2 under 1. Formacion Boscosa) ################\n\n")

forest_subtypes <- mb_raw %>% filter(class_level_1 == "1. Formacion Boscosa")
cat("Forest subtypes present:", paste(unique(forest_subtypes$class_level_2), collapse = " | "), "\n\n")

national_by_subtype <- forest_subtypes %>%
  group_by(class_level_2, year) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

for (cl in unique(national_by_subtype$class_level_2)) {
  sub <- national_by_subtype %>% filter(class_level_2 == cl) %>% arrange(year)
  v0 <- sub$area_ha[sub$year == 2000]; v1 <- sub$area_ha[sub$year == 2023]
  cat(sprintf("%-45s 2000: %12.0f ha | 2023: %12.0f ha | net change: %+.0f ha (%+.2f%%)\n",
              cl, v0, v1, v1 - v0, 100 * (v1 - v0) / v0))
}

# ===================== PART 2: national trend, key anthropic "destination" classes =====================
cat("\n\n################ PART 2: NATIONAL TREND, KEY ANTHROPIC CLASSES (loss destinations) ################\n\n")

anthropic_of_interest <- c("3.1. Silvicultura", "3.2. Palma aceitera", "3.3. Platano y banano (beta)",
                            "3.4. Mosaico de agricultura o pasto", "4.2. Infraestructura urbana", "4.3. Mineria")
anthropic_trend <- mb_raw %>% filter(class_level_2 %in% anthropic_of_interest) %>%
  group_by(class_level_2, year) %>% summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

for (cl in unique(anthropic_trend$class_level_2)) {
  sub <- anthropic_trend %>% filter(class_level_2 == cl) %>% arrange(year)
  if (nrow(sub) == 0) next
  v0 <- sub$area_ha[sub$year == 2000]; v1 <- sub$area_ha[sub$year == 2023]
  cat(sprintf("%-45s 2000: %12.0f ha | 2023: %12.0f ha | net change: %+.0f ha (%+.2f%%)\n",
              cl, v0, v1, v1 - v0, 100 * (v1 - v0) / v0))
}

# ===================== PART 3: municipality-year panel per class, pct of muni area, joined to TMF =====================
cat("\n\n################ PART 3: TMF vs. EACH MAPBIOMAS CLASS, MUNICIPALITY-YEAR CORRELATION ################\n\n")

classes_to_check <- mb_raw %>% filter(class_level_1 == "1. Formacion Boscosa" | class_level_2 %in% anthropic_of_interest) %>%
  pull(class_level_2) %>% unique()

mb_class_panel <- mb_raw %>%
  filter(class_level_2 %in% classes_to_check) %>%
  rename(class_area_ha = area_ha) %>%
  left_join(muni_areas, by = "MPIO_CDPMP") %>%
  rename(muni_area_ha = area_ha) %>%
  mutate(class_pct = class_area_ha / muni_area_ha * 100) %>%
  arrange(class_level_2, MPIO_CDPMP, year) %>%
  group_by(class_level_2, MPIO_CDPMP) %>%
  mutate(class_pct_lag1 = lag(class_pct), d_class_pct = class_pct - class_pct_lag1) %>%
  ungroup() %>%
  filter(!is.na(d_class_pct))

tmf_cover <- readRDS(file.path(out_dir, "tmf_muni_cover.rds")) %>%
  select(MPIO_CDPMP, year, d_forest_pct)

merged <- mb_class_panel %>% inner_join(tmf_cover, by = c("MPIO_CDPMP", "year"))

cat("-- Pearson/Spearman r between TMF d_forest_pct and each MapBiomas class's own d_class_pct --\n")
cat("(Forest subtypes: EXPECT positive r, since both should rise/fall together.\n")
cat(" Anthropic 'destination' subtypes: EXPECT negative r, since TMF loss should coincide with anthropic-class gain.)\n\n")

class_cor <- merged %>%
  group_by(class_level_2) %>%
  summarise(
    n = n(),
    pearson_r  = cor(d_forest_pct, d_class_pct, use = "complete.obs", method = "pearson"),
    spearman_r = cor(d_forest_pct, d_class_pct, use = "complete.obs", method = "spearman"),
    .groups = "drop"
  ) %>%
  arrange(desc(pearson_r))
print(class_cor, n = 30)

# ===================== PART 4: period split (pre/post 2016) for the aggregate forest measure =====================
cat("\n\n################ PART 4: TMF vs. MAPBIOMAS FOREST AGGREGATE, PRE/POST 2016 ################\n\n")

mb_forest_agg <- forest_subtypes %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(forest_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  left_join(muni_areas, by = "MPIO_CDPMP") %>%
  mutate(forest_pct = forest_ha / area_ha * 100) %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(forest_pct_lag1 = lag(forest_pct), d_forest_pct_mb = forest_pct - forest_pct_lag1) %>%
  ungroup() %>%
  filter(!is.na(d_forest_pct_mb))

merged_agg <- mb_forest_agg %>% select(MPIO_CDPMP, year, d_forest_pct_mb) %>%
  inner_join(tmf_cover, by = c("MPIO_CDPMP", "year"))

for (label in c("FULL", "PRE-2016", "POST-2016")) {
  sub <- switch(label, FULL = merged_agg, `PRE-2016` = filter(merged_agg, year < 2016), `POST-2016` = filter(merged_agg, year >= 2016))
  ct <- cor.test(sub$d_forest_pct, sub$d_forest_pct_mb, method = "pearson")
  cs <- cor.test(sub$d_forest_pct, sub$d_forest_pct_mb, method = "spearman", exact = FALSE)
  cat(sprintf("%-10s Pearson r = %.4f (p=%.2e)  Spearman rho = %.4f (p=%.2e)  n=%d\n",
              label, ct$estimate, ct$p.value, cs$estimate, cs$p.value, nrow(sub)))
}

# ===================== PART 5: department-level agreement =====================
cat("\n\n################ PART 5: DEPARTMENT-LEVEL TMF-MAPBIOMAS AGREEMENT (where do they agree most/least?) ################\n\n")

muni_dept <- muni_lookup %>% select(MPIO_CDPMP, DPTO_CCDGO) %>% distinct()
merged_dept <- merged_agg %>% left_join(muni_dept, by = "MPIO_CDPMP")

dept_cor <- merged_dept %>%
  group_by(DPTO_CCDGO) %>%
  filter(n() >= 30) %>%
  summarise(n = n(), r = cor(d_forest_pct, d_forest_pct_mb, use = "complete.obs"), .groups = "drop") %>%
  arrange(desc(r))
cat("-- Best-agreeing departments --\n"); print(head(dept_cor, 8))
cat("\n-- Worst-agreeing (or negatively-correlated) departments --\n"); print(tail(dept_cor, 8))

# ===================== PART 6: "destination of loss" diagnostic =====================
cat("\n\n################ PART 6: WHEN TMF SHOWS ITS BIGGEST LOSSES, WHAT DOES MAPBIOMAS SAY GREW? ################\n\n")

worst_tmf_loss <- tmf_cover %>% arrange(d_forest_pct) %>% head(500) %>% select(MPIO_CDPMP, year)

destination <- mb_class_panel %>%
  inner_join(worst_tmf_loss, by = c("MPIO_CDPMP", "year")) %>%
  filter(class_level_2 %in% anthropic_of_interest | class_level_1 == "1. Formacion Boscosa") %>%
  group_by(class_level_2) %>%
  summarise(mean_d_class_pct = mean(d_class_pct, na.rm = TRUE), median_d_class_pct = median(d_class_pct, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(desc(mean_d_class_pct))
cat("Among the 500 muni-years with the largest TMF-detected forest LOSS, mean/median change in each MapBiomas class's own area share:\n")
print(destination, n = 20)

write_csv(class_cor, file.path(out_dir, "tmf_vs_mapbiomas_class_correlations.csv"))
write_csv(dept_cor, file.path(out_dir, "tmf_vs_mapbiomas_department_agreement.csv"))
cat("\nDONE.\n")
