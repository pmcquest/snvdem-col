suppressMessages({
  library(tidyverse)
  library(jsonlite)
})

# National loss-only TMF series (Open Question 9, memo v2 Section 2.3): pixel-level
# transitions into TMF Class 3 ("Deforested land"), matching the construction used in
# tmf_guaviare_validation.py to reproduce Katz-Asprilla et al. (2024)'s Guaviare total.
# This is NOT the same statistic as tmf_muni_cover.rds's bidirectional d_forest_pct --
# it is a loss-only annual flow, directly comparable to gfc_muni_loss.rds on equal terms.

px_area_ha <- 0.09

raw <- fromJSON("G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_lossonly.json") %>%
  as_tibble()

cat("raw rows:", nrow(raw), "| years:", paste(range(raw$year), collapse="-"),
    "| municipalities:", n_distinct(raw$MPIO_CDPMP), "\n")

tmf_lossonly <- raw %>%
  select(MPIO_CDPMP, year, new_def_ha) %>%
  arrange(MPIO_CDPMP, year)

out_path <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/tmf_muni_lossonly.rds"
saveRDS(tmf_lossonly, out_path)
cat("\nSaved:", out_path, "\n")

cat("\n--- National annual totals ---\n")
national <- tmf_lossonly %>%
  group_by(year) %>%
  summarise(total_ha = sum(new_def_ha, na.rm = TRUE), .groups = "drop")
print(national, n = 30)

cat("\nNational total", paste(range(tmf_lossonly$year), collapse="-"), ":",
    format(sum(tmf_lossonly$new_def_ha, na.rm = TRUE), big.mark=",", scientific=FALSE), "ha\n")
cat("National annual average:",
    format(mean(national$total_ha), big.mark=",", scientific=FALSE), "ha/yr\n")

cat("\n--- Compare to GFC lossyear (gfc_muni_loss.rds), same years where overlapping ---\n")
gfc_path <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output/gfc_muni_loss.rds"
if (file.exists(gfc_path)) {
  gfc <- readRDS(gfc_path)
  cat("gfc_muni_loss.rds columns:", paste(names(gfc), collapse=", "), "\n")
  loss_col <- intersect(c("loss_ha", "forest_loss_ha"), names(gfc))[1]
  if (!is.na(loss_col)) {
    gfc_national <- gfc %>%
      group_by(year) %>%
      summarise(gfc_total_ha = sum(.data[[loss_col]], na.rm = TRUE), .groups = "drop")
    compare <- national %>%
      rename(tmf_total_ha = total_ha) %>%
      inner_join(gfc_national, by = "year")
    print(compare, n = 30)
  }
} else {
  cat("gfc_muni_loss.rds not found at expected path -- skipping comparison\n")
}

cat("\n--- Guaviare re-check from the national run (should match tmf_guaviare_validation.py's totals) ---\n")
guaviare_check <- tmf_lossonly %>%
  filter(MPIO_CDPMP %in% c("95001", "95015", "95025", "95200"), year <= 2020) %>%
  group_by(year) %>%
  summarise(guaviare_ha = sum(new_def_ha), .groups = "drop")
print(guaviare_check, n = 30)
cat("Guaviare 2000-2020 total (from national run):",
    format(sum(guaviare_check$guaviare_ha), big.mark=",", scientific=FALSE), "ha\n")
