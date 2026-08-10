#---- Zoom-in Step 2: Appendix D Table D.1 -- Census profile of case municipalities ----

# PURPOSE: LASA26/PoP draft's Appendix D, Table D.1 reports period-average Census covariates
# (water/aqueduct coverage, infant mortality, school enrollment, SABER11 scores, population)
# for the three focal case municipalities, as a check that case selection isn't trivially
# capturing socioeconomic status. Previously built by scripts/legacy/snvdem_expand.R +
# scripts/Exploratory/Outcomes/outcome_wrangle.R + scripts/visualization/ZoomIn/
# ZoomIn_final2.Rmd's `covsprof` chunk -- all three predate the 2026-07 pipeline reorg and
# depend on stale paths / a since-superseded SNDEM formula (MC/SNDEM_tentative.rds). This
# script replaces that chain with a single, self-contained script pointed at CURRENT sources:
#   - Two of its five raw inputs (TerriData Dim3 "servicios"/water, Dim5 "salud"/infant
#     mortality) were missing entirely from the shared drive and were re-sourced by PM
#     directly from terridata.dnp.gov.co on 2026-08-08 (this sandbox has no network route to
#     that host, so re-sourcing could not be automated here).
#   - The three case municipalities are Nobsa/Uribia/Quinchia (see
#     01_identify_case_municipalities.R), NOT the old draft's Cocorna/Timbiqui/Puerto Lopez.
#   - SNDEM figures come from the CURRENT canonical Step 5 output
#     (05_weighting/03_output/snvdem_col_weighted.rds), min-max normalized over the full
#     2000-2023 panel -- same convention as Table 3.
#   - Per PM (2026-08-08): Table D.1's original trajectory framing used Avg/2000/2023/Change.
#     The "Change" column is dropped, but 2000/2010/2023 snapshots are kept (plus the
#     2000-2023 average) so readers can see how stable each case's score is across the
#     period, without reintroducing the old improver/decliner SELECTION framing (selection is
#     still 2023-only, per 01_identify_case_municipalities.R -- these extra year columns are
#     descriptive, not part of how the three cases were chosen). The Census covariates remain
#     PERIOD AVERAGES (2000-2023), matching the original design and the note under the
#     original table ("Control variables are period averages") -- also necessary in practice,
#     since Dim3 only covers 2005-2022 and Dim5 only 1998-2021, so no single year (let alone
#     2023) has data for both.

library(tidyverse)
library(readxl)

## ---- Config ----
panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
src_dir   <- file.path(panel_dir, "01_empirical_data", "01_source_files", "source_files")
zoom_dir  <- file.path(panel_dir, "07_snvdem-col_diagnostics", "zoom-in")

case_codes <- c(Nobsa = "15491", Uribia = "44847", Quinchía = "66594")

pad5 <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

# DANE/TerriData "Dato Numerico" formatting is INCONSISTENT ACROSS DIMENSION FILES, confirmed
# 2026-08-08: Dim3 (servicios) natively reads as character, Spanish-formatted ("95,14" =
# 95.14%, comma decimal) -- same fix already used in df01/df05-clean.R. Dim5 (salud) natively
# reads as a clean double (readxl auto-detected it as numeric) -- forcing col_types="text" on
# it (an earlier version of this script did) makes readxl emit full floating-point precision
# strings ("79.29000000000000...") instead of "79.29", and the Spanish-format gsub then
# strips the "." as if it were a thousands separator, producing nonsense (79.29 -> ~7.9e15).
# Fix: read each column in its NATIVE type (no col_types override) and only apply the
# Spanish-format conversion when a comma is actually present in that specific value.
parse_terridata_num <- function(x) {
  x_chr <- trimws(as.character(x))
  ifelse(grepl(",", x_chr, fixed = TRUE),
         as.numeric(gsub(",", ".", gsub(".", "", x_chr, fixed = TRUE))),
         as.numeric(x_chr))
}

## ---- 1. Services (Dim3): Acueducto (water/aqueduct coverage) ----
d3 <- read_excel(file.path(src_dir, "Terridata", "TerriData_Dim3.xlsx")) %>%
  transmute(MPIO_CDPMP = pad5(`Código Entidad`), Indicador, year = as.numeric(Año),
            DATOn = parse_terridata_num(`Dato Numérico`))

serv_keep <- c("Cobertura de acueducto (Censo)", "Cobertura de acueducto (REC)")

acueducto <- d3 %>%
  filter(MPIO_CDPMP %in% case_codes, Indicador %in% serv_keep, year >= 2000, year <= 2023) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(Acueducto = mean(DATOn, na.rm = TRUE), .groups = "drop")

## ---- 2. Health (Dim5): infant mortality (<1 year, per 1,000 live births) ----
d5 <- read_excel(file.path(src_dir, "Terridata", "TerriData_Dim5.xlsx")) %>%
  transmute(MPIO_CDPMP = pad5(`Código Entidad`), Indicador, year = as.numeric(Año),
            DATOn = parse_terridata_num(`Dato Numérico`))

inf_mort_indicator <- "Tasa de mortalidad infantil en menores de 1 año (x cada 1.000 nacidos vivos)"

mort_infantil <- d5 %>%
  filter(MPIO_CDPMP %in% case_codes, Indicador == inf_mort_indicator, year >= 2000, year <= 2023) %>%
  transmute(MPIO_CDPMP, year, MortInfantil1 = DATOn)

## ---- 3. Education enrollment (MEN, net coverage) ----
# NOTE: sourced from a personal (non-shared-drive) path in the original script -- kept as-is
# per PM (file confirmed to still exist there 2026-08-08), but this remains a portability
# concern for anyone else trying to reproduce this appendix table.
men_path <- "G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/Education/MEN/MEN_ESTADISTICAS_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_POR_MUNICIPIO.xlsx"

net_cover <- read_excel(men_path) %>%
  rename(year = 1, MPIO_CDPMP = 2, NetCover = COBERTURA_NETA) %>%
  mutate(MPIO_CDPMP = pad5(MPIO_CDPMP), year = as.numeric(year)) %>%
  filter(MPIO_CDPMP %in% case_codes, year >= 2000, year <= 2023) %>%
  select(MPIO_CDPMP, year, NetCover)

## ---- 4. SABER11 scores (CEDE) ----
saber <- read_excel(file.path(src_dir, "a2-CEDE_PM", "2022", "Edu", "PANEL_DE_EDUCACION(2021).xlsx")) %>%
  rename(MPIO_CDPMP = 1, year = 2, AvgS11 = s11_total) %>%
  mutate(MPIO_CDPMP = pad5(MPIO_CDPMP)) %>%
  filter(MPIO_CDPMP %in% case_codes, year >= 2000, year <= 2023) %>%
  select(MPIO_CDPMP, year, AvgS11)

## ---- 5. Population (current, fixed pipeline output -- df05_clean.rds) ----
pob <- read_rds(file.path(panel_dir, "01_empirical_data", "03_clean_outputs", "df05_clean.rds")) %>%
  filter(MPIO_CDPMP %in% case_codes, year >= 2000, year <= 2023) %>%
  select(MPIO_CDPMP, year, PobTot_12)

## ---- 6. Period averages (2000-2023) per municipality ----
census_avg <- acueducto %>%
  full_join(mort_infantil, by = c("MPIO_CDPMP", "year")) %>%
  full_join(net_cover,     by = c("MPIO_CDPMP", "year")) %>%
  full_join(saber,         by = c("MPIO_CDPMP", "year")) %>%
  full_join(pob,           by = c("MPIO_CDPMP", "year")) %>%
  group_by(MPIO_CDPMP) %>%
  summarise(across(c(Acueducto, MortInfantil1, NetCover, AvgS11, PobTot_12),
                    ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

cat("=== Period-average (2000-2023) Census covariates, by municipality ===\n")
print(census_avg)

## ---- 7. SNDEM 2000/2010/2023 snapshots + full-period average (current pipeline, normalized
## -- matches Table 3's convention: min-max rescaled over the full 2000-2023 panel) ----
# Per PM (2026-08-08): show snapshots at three years plus the period average, rather than a
# single 2023 value or a 2023-minus-2000 change column, so readers can see how stable each
# case's score is across the period without reintroducing the old trajectory-selection framing.
weighted <- read_rds(file.path(panel_dir, "05_weighting", "03_output", "snvdem_col_weighted.rds"))
rescale01 <- function(x, ref) (x - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))

weighted_norm <- weighted %>% mutate(sndem_norm = rescale01(sndem, weighted$sndem))

sndem_snapshots <- weighted_norm %>%
  filter(MPIO_CDPMP %in% case_codes, year %in% c(2000, 2010, 2023)) %>%
  select(MPIO_CDPMP, year, sndem_norm) %>%
  pivot_wider(names_from = year, values_from = sndem_norm, names_prefix = "sndem_")

sndem_avg <- weighted_norm %>%
  filter(MPIO_CDPMP %in% case_codes) %>%
  group_by(MPIO_CDPMP) %>%
  summarise(sndem_avg = mean(sndem_norm, na.rm = TRUE), .groups = "drop")

sndem_wide <- sndem_snapshots %>% left_join(sndem_avg, by = "MPIO_CDPMP")

## ---- 8. Assemble Table D.1 ----
muni_meta <- tibble(MPIO_CDPMP = case_codes, municipio = names(case_codes))
departamentos <- read_rds(file.path(panel_dir, "01_empirical_data", "01_source_files", "MunYrs.rds")) %>%
  filter(MPIO_CDPMP %in% case_codes) %>%
  distinct(MPIO_CDPMP, departamento)

table_d1 <- muni_meta %>%
  left_join(departamentos, by = "MPIO_CDPMP") %>%
  left_join(sndem_wide,    by = "MPIO_CDPMP") %>%
  left_join(census_avg,    by = "MPIO_CDPMP") %>%
  transmute(
    Municipality = municipio,
    Department   = departamento,
    `2000`       = round(sndem_2000, 3),
    `2010`       = round(sndem_2010, 3),
    `2023`       = round(sndem_2023, 3),
    `Avg (2000-2023)` = round(sndem_avg, 3),
    `Water (%)`  = round(Acueducto, 2),
    `Inf. Mort.` = round(MortInfantil1, 2),
    `Enroll. (%)`= round(NetCover, 2),
    SABER        = round(AvgS11, 2),
    Pop.         = round(PobTot_12, 1)
  )

cat("\n=== Table D.1 (replacement) ===\n")
print(table_d1)

write_csv(table_d1, file.path(zoom_dir, "table_d1_census_profile.csv"))
cat(sprintf("\nWrote %s\n", file.path(zoom_dir, "table_d1_census_profile.csv")))
