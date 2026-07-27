suppressMessages({
  library(tidyverse)
  library(jsonlite)
})

px_area_ha <- 0.09  # 30m pixel, same convention as gfc_muni_cover.rds / mb_muni_cover.rds

raw <- fromJSON("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_classcounts.json") %>%
  as_tibble()

cat("raw rows:", nrow(raw), "| years:", paste(range(raw$year), collapse="-"),
    "| municipalities:", n_distinct(raw$MPIO_CDPMP), "\n")

# Forest = undisturbed (c1) + degraded (c2) + regrowth (c4), matching TMF's own
# "Undisturbed and Degraded TMF" grouping, extended to include regrowth since our
# DV is bidirectional. Not-forest = deforested (c3) + water (c5) + other (c6).
# c0 (small residual, unlabeled in JRC's legend) is included in the denominator
# but not counted as forest.
tmf_cover <- raw %>%
  mutate(
    total_px   = c0 + c1 + c2 + c3 + c4 + c5 + c6,
    forest_px  = c1 + c2 + c4,
    area_ha    = total_px * px_area_ha,
    forest_ha  = forest_px * px_area_ha,
    forest_pct = forest_ha / area_ha * 100
  ) %>%
  select(MPIO_CDPMP, year, area_ha, forest_ha, forest_pct) %>%
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    forest_pct_lag1 = lag(forest_pct),
    d_forest_pct    = forest_pct - forest_pct_lag1
  ) %>%
  ungroup() %>%
  filter(year >= 2000)   # 1999 was only needed to anchor year-2000's lag

cat("\nAfter dropping 1999 (lag-only):", nrow(tmf_cover), "rows |",
    "years:", paste(range(tmf_cover$year), collapse="-"), "\n")

cat("\nd_forest_pct sign check (should now show BOTH positive and negative, unlike GFC):\n")
cat("  share > 0:", mean(tmf_cover$d_forest_pct > 0, na.rm=TRUE), "\n")
cat("  share < 0:", mean(tmf_cover$d_forest_pct < 0, na.rm=TRUE), "\n")
cat("  share == 0:", mean(tmf_cover$d_forest_pct == 0, na.rm=TRUE), "\n")

cat("\nforest_pct level sanity (should be 0-100, no >100 bug this time):\n")
print(summary(tmf_cover$forest_pct))
cat("n forest_pct > 100:", sum(tmf_cover$forest_pct > 100, na.rm=TRUE), "\n")

# Sample-inclusion rule (per this session's decision): exclude a municipality only
# if it has ZERO forest cover in EVERY year 2000-2023 -- not "zero at baseline"
# (that GFC/Sanford convention assumed a loss-only DV, which no longer applies).
zero_every_year <- tmf_cover %>%
  group_by(MPIO_CDPMP) %>%
  summarise(always_zero = all(forest_pct == 0, na.rm = TRUE), .groups = "drop")

cat("\nMunicipalities with zero forest cover in every year (excluded):",
    sum(zero_every_year$always_zero), "of", nrow(zero_every_year), "\n")

keep_ids <- zero_every_year %>% filter(!always_zero) %>% pull(MPIO_CDPMP)
tmf_cover_final <- tmf_cover %>% filter(MPIO_CDPMP %in% keep_ids)

cat("\nFinal panel:", nrow(tmf_cover_final), "rows |",
    n_distinct(tmf_cover_final$MPIO_CDPMP), "municipalities\n")

out_path <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_cover.rds"
saveRDS(tmf_cover_final, out_path)
cat("\nSaved:", out_path, "\n")

cat("\n--- Bogota (11001) spot check, a few years ---\n")
print(tmf_cover_final %>% filter(MPIO_CDPMP == "11001", year %in% c(2000, 2010, 2016, 2020, 2023)) %>%
        select(year, forest_pct, forest_pct_lag1, d_forest_pct))

cat("\n--- Warnings detail ---\n")
print(warnings())
