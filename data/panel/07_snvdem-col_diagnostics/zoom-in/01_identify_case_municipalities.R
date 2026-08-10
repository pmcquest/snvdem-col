#---- Zoom-in Step 1: Identify high/average/low-democracy case municipalities (2023) ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation")
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Flip and multiply geopredictors by V-Dem coder weights; normalize by weight sum
#         (Folder "05_weighting") -- reads THAT step's output, below
# Step 6: Benchmark using national V-Dem data (Folder "06_benchmark")
# Step 7 (this script): Diagnostics/zoom-in analysis (Folder "07_snvdem-col_diagnostics")

# PURPOSE: LASA26 draft (McQuestion_etal-MeasuringSubnationalDemocracy_041426.pdf), Table 3
# and surrounding text (pgs. 21-22) report 2023 geopredictor + index values for three
# "illustrative" municipalities -- Cocorna (Antioquia, high-democracy), Puerto Lopez (Meta,
# average-democracy), and Timbiqui (Cauca, low-democracy). Those numbers were computed before
# the 2026-07-26 "July formula" reweighting update (see
# 05_weighting/04_memo/geopredictor_weighting_review_2026-07-26.md) -- a change that measurably
# reshuffled municipality rankings (2023 sndem rank-correlation with the prior formula was only
# 0.879 Spearman). This script re-identifies the three cases under the CURRENT pre-benchmarked
# data and produces the disaggregated geopredictor table (Table 3's replacement), so the draft
# can be updated with numbers that actually match the current pipeline.
#
# SELECTION METHOD (confirmed with PM 2026-08-08): 2023 cross-section only, matching Table 3's
# own caption -- NOT a full 2000-2023 trajectory measure (that richer "extreme improver/decliner/
# typical" logic exists in the older, superseded scripts/visualization/ZoomIn/ZoomIn_final2.Rmd,
# but the current draft's simpler "high/average/low" framing is explicitly a single-year snapshot).
#   - High-democracy    = max sndem, 2023
#   - Low-democracy     = min sndem, 2023
#   - Average-democracy = municipality whose sndem is closest to the 2023 median sndem
# Areas No Municipalizadas (ANM -- corregimientos departamentales with no elected municipal
# government, e.g. "La Guadalupe (ANM)") are excluded from the candidate pool: they are part of
# the 1,122-unit panel (see 01_empirical_data/01_source_files/Mun-Year.R), but calling one an
# illustrative "municipality" in the text would be substantively misleading. Toggle
# EXCLUDE_ANM <- FALSE below to include them.

library(tidyverse)

## ---- Config ----
panel_dir   <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
zoom_dir    <- file.path(panel_dir, "07_snvdem-col_diagnostics", "zoom-in")
SNAP_YEAR   <- 2023
EXCLUDE_ANM <- TRUE

# The three municipalities named in the current LASA26 draft's Table 3, for direct
# before/after comparison (kept here, not re-derived, so the diff is explicit either way).
draft_codes <- c(
  "05197" = "Cocorna (Antioquia) -- drafted as HIGH-democracy",
  "50573" = "Puerto Lopez (Meta) -- drafted as AVERAGE-democracy",
  "19809" = "Timbiqui (Cauca) -- drafted as LOW-democracy"
)

## ---- Load pre-benchmarked (Step 5) index + geopredictor data ----
# Table 3's values are on a 0-1 scale (snelect=0.93, sncivlib=0.89, sndem=0.91 for Cocorna) --
# that is Step 5's UNBENCHMARKED output, not Step 6's V-Dem-anchored sndem_final (which the
# paper's own text says ranges roughly -3.5 to 3.6 for elections, not 0-1).
weighted <- read_rds(file.path(panel_dir, "05_weighting", "03_output", "snvdem_col_weighted.rds"))

## ---- Full-panel min/max for index normalization ----
# snelect/sncivlib/sndem are WEIGHTED AVERAGES of several already-0-1 CDF-ranked
# geopredictors, so they never actually reach 0 or 1 -- averaging compresses the range
# (confirmed empirically, 05_weighting/04_memo/geopredictor_weighting_review_2026-07-26.md:
# current-formula range is ~0.08-0.62 for snelect, ~0.11-0.59 for sncivlib, full 2000-2023
# panel). The draft's original Table 3 values (e.g. Cocorna snelect=0.93) EXCEED even the
# max any municipality could achieve under either formula -- proof those numbers were
# min-max normalized before display, not raw. Reproducing that here so the new table is
# presented the same way. Confirmed with PM (2026-08-08): normalize over the FULL
# 2000-2023 panel (not just 2023), matching the older ZoomIn_final2.Rmd convention, so
# these numbers stay comparable to other in-paper figures (e.g. the benchmark boxplots)
# that also use the full panel as their reference distribution. Rank order within 2023
# is provably unaffected by this choice -- see the "wait, i'm confused..." discussion in
# session notes: min-max rescaling is a monotonic (a*x+b, a>0) transform, so it cannot
# change which municipality is max/min/closest-to-median.
rescale01 <- function(x, ref) (x - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))
panel_range <- list(snelect = weighted$snelect, sncivlib = weighted$sncivlib, sndem = weighted$sndem)

## ---- Load municipality names/departments ----
# MunYrs.rds has one row per (MPIO_CDPMP, year); a handful of codes carry more than one name
# variant across years (e.g. 94343: "Barrancominas (ANM)" / "Barrancominas" / "Barrancominas*").
# Take each code's latest-year name as canonical so the join below doesn't fan out rows.
muni_names <- read_rds(file.path(panel_dir, "01_empirical_data", "01_source_files", "MunYrs.rds")) %>%
  group_by(MPIO_CDPMP) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(MPIO_CDPMP, municipio, departamento)

## ---- Build the 2023 cross-section ----
snap <- weighted %>%
  filter(year == SNAP_YEAR) %>%
  left_join(muni_names, by = "MPIO_CDPMP") %>%
  mutate(
    is_anm = str_detect(municipio, fixed("(ANM)")),
    # Ruling_party is a genuinely nonlinear (inverted-V / "tent") criterion in the ACTUAL
    # Step 5 formula, not a simple linear flip of avg15t16 -- see
    # 01_weighting_geopredictors.R's avg15t16hi_flip/avg15t16lo construction and MC's
    # MemoCalculations.docx. avg15t16 alone (0=strong ruling party, 1=weak/opposition) is
    # misleading to show on its own: a near-50/50 election (avg15t16 close to 0.5 from
    # EITHER side) is the most democratic outcome and should score highest, not middling.
    # This reproduces the same tent used in Step 5's diagnostic maps
    # (2*pmin(avg15t16, 1-avg15t16)), confirmed 2026-08-08 against a concrete case (Nobsa:
    # raw ruling-party vote share 0.512, essentially a dead-even race -- linear avg15t16
    # shows 0.49 (middling), tent shows 0.976 (correctly near-best of the three cases)).
    rulparty_tent = 2 * pmin(avg15t16, 1 - avg15t16),
    # Display-only normalized versions of the three composite indices (full 2000-2023 panel
    # min/max as the reference) -- selection above/below still uses RAW sndem throughout,
    # since normalization can't change rank order (see note above).
    snelect_norm  = rescale01(snelect,  panel_range$snelect),
    sncivlib_norm = rescale01(sncivlib, panel_range$sncivlib),
    sndem_norm    = rescale01(sndem,    panel_range$sndem)
  )

# NOTE on cardinal-direction rows (avg6-9): raw north6/south7/west8/east9 (df03-6789-clean.R)
# are one-sided, zero-clipped measures, not symmetric distances -- 78% of all 1,122
# municipalities have a raw south7 of EXACTLY 0 (only genuine southern-frontier
# municipalities, e.g. Amazonas/Caqueta/Putumayo, score nonzero), and 83% have a raw east9
# of exactly 0. Any case municipalities drawn from Colombia's non-frontier majority will tie
# on these CDF ranks (confirmed 2026-08-08: Nobsa/Uribia/Quinchia all show south=0.39,
# the exact CDF rank of the 875-way zero tie) -- this is a real property of the underlying
# geospatial data (M. Sisk's COL_NSEW.csv), not a join or computation bug.

stopifnot(all(!is.na(snap$sndem)))  # confirms Step 5's 50%-weight-mass floor left no NAs here

candidates <- if (EXCLUDE_ANM) filter(snap, !is_anm) else snap
cat(sprintf(
  "2023 cross-section: %d municipalities total, %d eligible candidates (ANM excluded: %s)\n",
  nrow(snap), nrow(candidates), EXCLUDE_ANM
))

median_sndem <- median(candidates$sndem, na.rm = TRUE)
cat(sprintf("Median sndem (2023, eligible candidates): %.4f\n", median_sndem))

## ---- Identify the three cases under current data ----
high_row <- candidates %>% slice_max(sndem, n = 1, with_ties = FALSE) %>% mutate(case = "High-democracy")
low_row  <- candidates %>% slice_min(sndem, n = 1, with_ties = FALSE) %>% mutate(case = "Low-democracy")
avg_row  <- candidates %>%
  mutate(dist_to_median = abs(sndem - median_sndem)) %>%
  slice_min(dist_to_median, n = 1, with_ties = FALSE) %>%
  mutate(case = "Average-democracy")

new_cases <- bind_rows(high_row, low_row, avg_row) %>%
  select(case, MPIO_CDPMP, municipio, departamento, everything(), -is_anm)

cat("\n=== Newly identified case municipalities (2023, current data) ===\n")
cat("(raw = un-normalized Step 5 output; norm = min-max rescaled over the full 2000-2023 panel)\n")
print(new_cases %>% select(case, municipio, departamento, MPIO_CDPMP,
                            snelect, snelect_norm, sncivlib, sncivlib_norm, sndem, sndem_norm))

## ---- Compare against the draft's named municipalities ----
snap <- snap %>% mutate(rank_sndem_desc = rank(-sndem, ties.method = "min"))

draft_rows <- snap %>%
  filter(MPIO_CDPMP %in% names(draft_codes)) %>%
  mutate(draft_label = draft_codes[MPIO_CDPMP])

cat("\n=== Draft-named municipalities under CURRENT (post-July-formula) data ===\n")
print(draft_rows %>% select(draft_label, municipio, departamento,
                             snelect_norm, sncivlib_norm, sndem_norm, rank_sndem_desc))
cat(sprintf("(N eligible municipalities with a computed sndem in 2023: %d)\n", nrow(candidates)))

same_set <- setequal(new_cases$MPIO_CDPMP, names(draft_codes))
cat(sprintf(
  "\n>>> The three case municipalities %s changed under the current data. <<<\n",
  if (same_set) "have NOT" else "HAVE"
))

## ---- Disaggregated geopredictor table (Table 3 replacement) ----
# Maps the 12 Table-3 geopredictor rows onto Step 5's post-direction-flip "avg*" columns.
# Every column below is ALREADY oriented so that higher = more democratic, per Step 5's
# direction-flip block (01_weighting_geopredictors.R, "Load geocoded predictors" section) --
# see that script's inline comments for the flip/no-flip rationale on each.
geopred_map <- tribble(
  ~label,                                              ~col,
  "Rural population (0 = rural, 1 = urban)",            "avg0t1",
  "Economic development (0 = low, 1 = high)",           "avg2t3",
  "Proximity to capital (0 = far, 1 = near)",            "avg4t5",
  "Northern location",                                   "avg6",
  "Southern location",                                   "avg7",
  "Western location",                                    "avg8",
  "Eastern location",                                     "avg9",
  "Violence (0 = high, 1 = low)",                        "avg10t11",
  "Population density (0 = sparse, 1 = dense)",          "avg12",
  "Remoteness (0 = remote, 1 = accessible)",             "avg13",
  "Indigenous population (0 = high, 1 = low)",           "avg14",
  "Ruling party competitiveness (0 = dominant, 1 = most competitive)", "rulparty_tent"
)

build_table3 <- function(df, label_col) {
  ids <- df[[label_col]]
  geo <- df %>%
    select(all_of(geopred_map$col)) %>%
    t() %>%
    as.data.frame() %>%
    setNames(ids) %>%
    mutate(Geopredictors = geopred_map$label, .before = 1)

  idx <- df %>%
    select(snelect_norm, sncivlib_norm, sndem_norm) %>%
    t() %>%
    as.data.frame() %>%
    setNames(ids) %>%
    mutate(
      Geopredictors = c(
        "Electoral fairness (snelect)",
        "Civil liberties (sncivlib)",
        "Composite democracy (sndem)"
      ),
      .before = 1
    )

  bind_rows(geo, idx) %>%
    mutate(across(-Geopredictors, ~ round(.x, 3)))
}

table3_new   <- build_table3(new_cases  %>% mutate(col_id = paste0(municipio, " (", case, ")")), "col_id")
table3_draft <- build_table3(draft_rows %>% mutate(col_id = paste0(municipio, " (", str_extract(draft_label, "(?<=drafted as ).*"), ")")), "col_id")

cat("\n=== Table 3 replacement -- NEWLY IDENTIFIED cases (2023) ===\n")
print(table3_new)

cat("\n=== Table 3 as currently drafted, but with CURRENT data -- draft-named cases (2023) ===\n")
print(table3_draft)

## ---- Write outputs ----
write_csv(table3_new,   file.path(zoom_dir, "table3_new_cases_2023.csv"))
write_csv(table3_draft, file.path(zoom_dir, "table3_draft_municipalities_current_data_2023.csv"))
write_csv(new_cases %>% select(case, MPIO_CDPMP, municipio, departamento,
                                snelect, snelect_norm, sncivlib, sncivlib_norm, sndem, sndem_norm),
          file.path(zoom_dir, "case_municipality_ids_2023.csv"))

cat(sprintf("\nWrote outputs to %s\n", zoom_dir))
