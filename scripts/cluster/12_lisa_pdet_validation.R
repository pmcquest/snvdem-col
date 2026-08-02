# ============================================================================
# Low-Low persistence vs. PDET status (official conflict/poverty designation)
#
# 08_lisa_full_panel_explore.R validated the Low-Low clustering against a
# self-constructed list of "conflict-associated departments" -- a reasonable
# first pass, but department-level, ad hoc, and reliant on citations (
# Defensoria del Pueblo 2018; Vasquez Delgado & Barrera Ramirez 2016) that
# describe armed-group activity during a period when measuring conflict
# geography is itself contested.
#
# PDET status is a cleaner alternative: it is an official, municipio-level
# (not department-level) designation created by the Colombian government
# under Decreto 893 de 2017 to identify the 170 municipios most affected by
# armed conflict, illicit economies, and state absence, for prioritized
# post-Accord territorial investment. It's a single administrative fact (is
# this municipio on the government's own list, yes/no), not something we
# had to classify ourselves, and it's already used elsewhere in this
# project (PDET.dta, from the PanelMatch/DiD exploratory scripts). Using it
# in place of the department list avoids relying on our own reading of the
# contested conflict-measurement literature to make the validation point.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(haven)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
panel <- read.csv(file.path(out_dir, "lisa_full_panel.csv"), colClasses = c(MPIO_CDPMP = "character"))

pdet <- read_dta("G:/My Drive/Academia/PhD/Coursework/Y2/FA23/POLS60885_CausalInference/Paper/data/PDET.dta") %>%
  transmute(MPIO_CDPMP = sprintf("%05d", MunCode), PDET = as.integer(PDET))

cat("PDET municipios in this file:", sum(pdet$PDET, na.rm = TRUE), "of", nrow(pdet), "\n\n")

# --- Low-Low persistence, PDET vs. non-PDET municipios -----------------------
ll_share <- panel %>%
  filter(cluster != "No neighbors") %>%
  group_by(MPIO_CDPMP) %>%
  summarise(share_ll = mean(cluster == "Low-Low"), .groups = "drop") %>%
  left_join(pdet, by = "MPIO_CDPMP")

cat("=== Mean share of years classified Low-Low: PDET vs. non-PDET municipios ===\n")
print(ll_share %>% filter(!is.na(PDET)) %>% group_by(PDET) %>%
        summarise(mean_share_ll = mean(share_ll), n = n(), .groups = "drop"))

# --- Are the five persistent High-Low outlier cities themselves PDET? --------
outlier_codes <- c("54498", "19001", "41551", "23001", "27001")
names(outlier_codes) <- c("OCAÑA", "POPAYÁN", "PITALITO", "MONTERÍA", "QUIBDÓ")
cat("\n=== PDET status of the five spotlighted outlier municipios ===\n")
print(pdet %>% filter(MPIO_CDPMP %in% outlier_codes) %>%
        mutate(name = names(outlier_codes)[match(MPIO_CDPMP, outlier_codes)]) %>%
        select(name, MPIO_CDPMP, PDET))

# --- Are the outliers' immediate NEIGHBORS disproportionately PDET? ----------
# A sharper version of the same check: even if the outlier city itself isn't
# PDET-designated, its surrounding municipios being PDET (vs. the national
# PDET rate) would still support the "urban outlier inside an officially
# recognized conflict/poverty zone" reading.
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; nb <- obj$nb
national_pdet_rate <- mean(pdet$PDET, na.rm = TRUE)
cat("\nNational PDET rate (share of all municipios):", round(national_pdet_rate, 3), "\n\n")

cat("=== Share of each outlier's immediate (queen-contiguity) neighbors that are PDET ===\n")
for (code in outlier_codes) {
  idx <- which(shp$MPIO_CDPMP == code)
  neighbor_codes <- shp$MPIO_CDPMP[nb[[idx]]]
  neighbor_pdet_rate <- mean(pdet$PDET[match(neighbor_codes, pdet$MPIO_CDPMP)], na.rm = TRUE)
  cat(sprintf("%-10s PDET-neighbor share: %.2f  (n neighbors = %d)\n",
              names(outlier_codes)[outlier_codes == code], neighbor_pdet_rate, length(neighbor_codes)))
}
