# ============================================================================
# How far are the persistent High-Low outliers from Bogota, on sndem itself?
#
# The spotlight map (09/10) shows these five municipios as geographic
# outliers -- high-sndem places embedded in lower-scoring departments -- but
# doesn't say how their actual sndem values compare to Bogota's, or to their
# own immediate neighbors'. This computes, for each outlier municipio and
# each year it was classified High-Low: its own sndem, Bogota's sndem that
# same year, and the (raw, unstandardized) mean sndem of its actual queen-
# contiguity neighbors that year -- then summarizes the average gap to each
# across all of that municipio's High-Low years.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
obj <- readRDS(file.path(out_dir, "lisa_weights.rds"))
shp <- obj$shp; dat <- obj$dat; nb <- obj$nb

panel <- read.csv(file.path(out_dir, "lisa_full_panel.csv"), colClasses = c(MPIO_CDPMP = "character"))

bogota_code <- "11001"
outlier_codes <- c("54498", "19001", "41551", "23001", "27001")  # Ocana, Popayan, Pitalito, Monteria, Quibdo
names(outlier_codes) <- c("OCAÑA", "POPAYÁN", "PITALITO", "MONTERÍA", "QUIBDÓ")

years <- sort(unique(dat$year))
idx_of <- function(code) which(shp$MPIO_CDPMP == code)

# --- Raw (unstandardized) spatial lag of sndem, every year -------------------
# Same neighbor structure as the LISA test, but averaging actual sndem values
# rather than z-scores, so the result is interpretable directly on the sndem
# scale ("what did this municipio's neighbors actually score that year").
lag_by_year <- list()
for (yr in years) {
  d_yr <- dat %>% filter(year == yr) %>% select(MPIO_CDPMP, sndem)
  merged <- shp %>% sf::st_drop_geometry() %>% left_join(d_yr, by = "MPIO_CDPMP")
  merged <- merged[match(shp$MPIO_CDPMP, merged$MPIO_CDPMP), ]
  lag_by_year[[as.character(yr)]] <- merged$sndem
}

rows <- list()
for (i in seq_along(outlier_codes)) {
  code <- outlier_codes[i]; nm <- names(outlier_codes)[i]
  idx <- idx_of(code)
  neighbor_idx <- nb[[idx]]

  hl_years <- panel %>% filter(MPIO_CDPMP == code, cluster == "High-Low") %>% pull(year)

  for (yr in hl_years) {
    own_sndem <- lag_by_year[[as.character(yr)]][idx]
    bogota_sndem <- lag_by_year[[as.character(yr)]][idx_of(bogota_code)]
    neighbor_mean <- mean(lag_by_year[[as.character(yr)]][neighbor_idx], na.rm = TRUE)
    rows[[length(rows) + 1]] <- data.frame(
      municipio = nm, year = yr, own_sndem = own_sndem,
      bogota_sndem = bogota_sndem, neighbor_mean_sndem = neighbor_mean
    )
  }
}

gap_df <- do.call(rbind, rows) %>%
  mutate(
    gap_to_bogota = abs(own_sndem - bogota_sndem),
    gap_to_neighbors = abs(own_sndem - neighbor_mean_sndem)
  )

write.csv(gap_df, file.path(out_dir, "lisa_outlier_bogota_gap_detail.csv"), row.names = FALSE)

summary_df <- gap_df %>%
  group_by(municipio) %>%
  summarise(
    n_hl_years = n(),
    mean_own_sndem = mean(own_sndem),
    mean_bogota_sndem = mean(bogota_sndem),
    mean_gap_to_bogota = mean(gap_to_bogota),
    mean_neighbor_sndem = mean(neighbor_mean_sndem),
    mean_gap_to_neighbors = mean(gap_to_neighbors),
    .groups = "drop"
  ) %>%
  arrange(desc(n_hl_years))

cat("=== sndem gap to Bogota vs. gap to own neighbors, across each municipio's High-Low years ===\n")
print(as.data.frame(summary_df), digits = 3)

overall <- gap_df %>% summarise(
  mean_gap_to_bogota = mean(gap_to_bogota),
  mean_gap_to_neighbors = mean(gap_to_neighbors)
)
cat("\n=== Pooled across all outlier-years ===\n")
print(overall, digits = 3)
cat("\nRatio (gap to neighbors / gap to Bogota):", round(overall$mean_gap_to_neighbors / overall$mean_gap_to_bogota, 2), "\n")

write.csv(summary_df, file.path(out_dir, "lisa_outlier_bogota_gap_summary.csv"), row.names = FALSE)
cat("\nWrote", file.path(out_dir, "lisa_outlier_bogota_gap_summary.csv"), "and _detail.csv\n")
