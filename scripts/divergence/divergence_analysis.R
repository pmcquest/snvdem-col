# ============================================================
# Divergence Analysis: Electoral vs. Civil Liberties
# snvdem Colombia 2000–2023
# ============================================================

library(tidyverse)
library(sf)
library(spdep)
library(ggrepel)
library(patchwork)
library(viridis)

# --- Paths ---
DATA_PATH  <- "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/01_Expand/snvdem2.rds"
SHP_PATH   <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp"
OUT_DIR    <- "G:/Shared drives/snvdem/snvdem-col/scripts/divergence/output/"

clean_mpio <- function(x) {
  str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
}

# ============================================================
# LOAD & NORMALIZE
# ============================================================
snvdem_raw <- readRDS(DATA_PATH)

master_clean <- snvdem_raw %>%
  filter(!is.na(snelect) & !is.na(sncivlib) & !is.na(sndem)) %>%
  mutate(
    snelect_norm  = (snelect  - min(snelect,  na.rm = TRUE)) / (max(snelect,  na.rm = TRUE) - min(snelect,  na.rm = TRUE)),
    sncivlib_norm = (sncivlib - min(sncivlib, na.rm = TRUE)) / (max(sncivlib, na.rm = TRUE) - min(sncivlib, na.rm = TRUE)),
    sndem_norm    = (sndem    - min(sndem,    na.rm = TRUE)) / (max(sndem,    na.rm = TRUE) - min(sndem,    na.rm = TRUE)),
    div_score     = snelect_norm - sncivlib_norm,
    raw_div       = snelect - sncivlib          # raw (unnormalized) divergence
  ) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# ============================================================
# STEP 0: DIAGNOSTICS
# ============================================================
cat("\n===== STEP 0: DIAGNOSTICS =====\n")

r_corr <- cor(master_clean$snelect_norm, master_clean$sncivlib_norm, use = "complete.obs")
cat(sprintf("Correlation (snelect_norm vs sncivlib_norm): r = %.4f\n", r_corr))

cat(sprintf("div_score — mean: %.4f | SD: %.4f | min: %.4f | max: %.4f\n",
    mean(master_clean$div_score, na.rm = TRUE),
    sd(master_clean$div_score,   na.rm = TRUE),
    min(master_clean$div_score,  na.rm = TRUE),
    max(master_clean$div_score,  na.rm = TRUE)))

pct_positive <- mean(master_clean$div_score > 0, na.rm = TRUE)
pct_negative <- mean(master_clean$div_score < 0, na.rm = TRUE)
cat(sprintf("Fraction elections-dominant (div>0): %.1f%%\n", 100 * pct_positive))
cat(sprintf("Fraction civlib-dominant   (div<0): %.1f%%\n", 100 * pct_negative))

if (r_corr > 0.90) {
  cat("Framing: correlation is high — emphasize notable exceptions.\n")
} else if (r_corr < 0.80) {
  cat("Framing: correlation < 0.80 — substantively large divergences, stronger framing justified.\n")
} else {
  cat("Framing: moderate correlation — divergences present but not dominant.\n")
}

# Raw (unnormalized) divergence comparison
cat("\n--- Raw vs. Normalized divergence ---\n")
r_corr_raw <- cor(master_clean$snelect, master_clean$sncivlib, use = "complete.obs")
cat(sprintf("Correlation (raw snelect vs sncivlib):    r = %.4f\n", r_corr_raw))
cat(sprintf("snelect raw range:  %.4f to %.4f\n",
    min(master_clean$snelect, na.rm=TRUE), max(master_clean$snelect, na.rm=TRUE)))
cat(sprintf("sncivlib raw range: %.4f to %.4f\n",
    min(master_clean$sncivlib, na.rm=TRUE), max(master_clean$sncivlib, na.rm=TRUE)))
cat(sprintf("raw_div — mean: %.4f | SD: %.4f | min: %.4f | max: %.4f\n",
    mean(master_clean$raw_div, na.rm=TRUE), sd(master_clean$raw_div, na.rm=TRUE),
    min(master_clean$raw_div, na.rm=TRUE), max(master_clean$raw_div, na.rm=TRUE)))
cat(sprintf("Correlation (raw_div vs norm div_score): r = %.4f\n",
    cor(master_clean$raw_div, master_clean$div_score, use="complete.obs")))
cat("Note: normalization re-scales each index to [0,1] using global min/max.\n")
cat("Raw scores are on an arbitrary latent scale (not bounded to [0,1]).\n")
cat("Ranking of municipalities by divergence is preserved under normalization.\n")

# Histogram of div_score
png(file.path(OUT_DIR, "divergence_histogram.png"), width = 1800, height = 1200, res = 300)
hist(master_clean$div_score, breaks = 60,
     main = "Distribution of Divergence Scores (Elec − CivLib)",
     xlab = "div_score (positive = elections lead, negative = civlib leads)",
     col = "steelblue3", border = "white")
abline(v = 0, col = "firebrick", lwd = 2, lty = 2)
dev.off()

# ============================================================
# STEP 1: PER-MUNICIPALITY AVERAGES + TYPOLOGY
# ============================================================
cat("\n===== STEP 1: MUNICIPALITY SUMMARIES =====\n")

muni_div <- master_clean %>%
  group_by(MPIO_CDPMP, municipio, depto) %>%
  summarise(
    mean_div    = mean(div_score,      na.rm = TRUE),
    mean_elec   = mean(snelect_norm,   na.rm = TRUE),
    mean_civlib = mean(sncivlib_norm,  na.rm = TRUE),
    mean_dem    = mean(sndem_norm,     na.rm = TRUE),
    n_years     = n(),
    .groups     = "drop"
  )

# Thresholds: top/bottom quartile of mean_div
q25 <- quantile(muni_div$mean_div, 0.25, na.rm = TRUE)
q75 <- quantile(muni_div$mean_div, 0.75, na.rm = TRUE)
cat(sprintf("Divergence quartiles: Q25 = %.4f | Q75 = %.4f\n", q25, q75))

muni_div <- muni_div %>%
  mutate(type = case_when(
    mean_div >  q75 ~ "Elections-dominant",
    mean_div <  q25 ~ "CivLib-dominant",
    TRUE            ~ "Convergent"
  ))

cat("\nTypology counts:\n")
print(table(muni_div$type))

# ============================================================
# STEP 2: CHOROPLETH MAP
# ============================================================
cat("\n===== STEP 2: DIVERGENCE MAP =====\n")

muni_geo <- st_read(SHP_PATH, quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_div <- muni_geo %>%
  left_join(muni_div, by = "MPIO_CDPMP")

div_map <- ggplot(map_div) +
  geom_sf(aes(fill = mean_div), color = NA) +
  scale_fill_gradient2(
    low      = "firebrick3",
    mid      = "white",
    high     = "steelblue3",
    midpoint = 0,
    name     = "Divergence\n(Elec − CivLib)",
    na.value = "grey80"
  ) +
  labs(
    title    = "Electoral vs. Civil Liberties Divergence",
    subtitle = "Average divergence score per municipality, 2000–2023",
    caption  = "Positive (blue) = elections outperform civil liberties. Negative (red) = civil liberties outperform elections."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(hjust = 0.5, size = 8, color = "grey50"),
    legend.position = "right"
  )

ggsave(file.path(OUT_DIR, "divergence_map.png"),
       plot = div_map, width = 10, height = 10, dpi = 300, bg = "white")
cat("Saved: divergence_map.png\n")

# ============================================================
# STEP 3: ILLUSTRATIVE MUNICIPALITIES
# ============================================================
cat("\n===== STEP 3: EXTREME MUNICIPALITIES =====\n")

top_elec <- muni_div %>%
  arrange(desc(mean_div)) %>%
  select(MPIO_CDPMP, municipio, depto, mean_div, mean_elec, mean_civlib, mean_dem, n_years) %>%
  head(10)

top_civlib <- muni_div %>%
  arrange(mean_div) %>%
  select(MPIO_CDPMP, municipio, depto, mean_div, mean_elec, mean_civlib, mean_dem, n_years) %>%
  head(10)

cat("\nTop 10 elections-dominant municipalities:\n")
print(as.data.frame(top_elec))

cat("\nTop 10 civlib-dominant municipalities:\n")
print(as.data.frame(top_civlib))

# Zooming In municipalities
zooming_in <- c("Cocorná", "Puerto López", "Timbiquí")
cat("\nZooming In municipalities divergence check:\n")
zi_check <- muni_div %>%
  filter(municipio %in% zooming_in) %>%
  select(municipio, depto, mean_div, mean_elec, mean_civlib, mean_dem, type)
print(as.data.frame(zi_check))

# Save top municipalities table
all_extreme <- bind_rows(
  top_elec %>% mutate(extreme_type = "elections-dominant"),
  top_civlib %>% mutate(extreme_type = "civlib-dominant")
)
write_csv(all_extreme, file.path(OUT_DIR, "divergence_top_munis.csv"))
cat("Saved: divergence_top_munis.csv\n")

# ============================================================
# STEP 4: TEMPORAL PATTERNS
# ============================================================
cat("\n===== STEP 4: TEMPORAL PATTERNS =====\n")

# 4a: National average divergence over time
annual_div <- master_clean %>%
  group_by(year) %>%
  summarise(
    mean_div    = mean(div_score,     na.rm = TRUE),
    sd_div      = sd(div_score,       na.rm = TRUE),
    mean_elec   = mean(snelect_norm,  na.rm = TRUE),
    mean_civlib = mean(sncivlib_norm, na.rm = TRUE),
    pct_elec_dom   = mean(div_score >  0.10, na.rm = TRUE),
    pct_civlib_dom = mean(div_score < -0.10, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nAnnual mean divergence (first and last 5 years):\n")
print(as.data.frame(head(annual_div, 5)))
print(as.data.frame(tail(annual_div, 5)))

# Key political shocks for annotation
shocks <- data.frame(
  year  = c(2005,  2008,  2016),
  label = c("AUC demobilization\n(Law 975)",
            "Commodity boom\npeak",
            "FARC peace\nagreement"),
  vjust = c(-0.4, -0.4, -0.4)
)

# Time series plot: national mean divergence + component trends
ts_plot <- ggplot(annual_div, aes(x = year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(data = shocks, aes(xintercept = year),
             linetype = "dotted", color = "grey40", linewidth = 0.7) +
  geom_text(data = shocks, aes(x = year, y = Inf, label = label, vjust = vjust),
            size = 2.6, color = "grey35", hjust = 0.5, lineheight = 0.9) +
  geom_ribbon(aes(ymin = mean_div - sd_div, ymax = mean_div + sd_div),
              fill = "steelblue", alpha = 0.15) +
  geom_line(aes(y = mean_div), color = "steelblue3", linewidth = 1.2) +
  geom_point(aes(y = mean_div), color = "steelblue3", size = 2) +
  scale_x_continuous(breaks = seq(2000, 2023, 3)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "National Average Divergence Over Time (Elec − CivLib)",
    subtitle = "Mean ± 1 SD across all municipalities. Dotted lines mark major political shocks.",
    x = "Year", y = "Divergence score"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(t = 30, r = 10, b = 5, l = 10)
  )

# Component trends
comp_long <- annual_div %>%
  select(year, mean_elec, mean_civlib) %>%
  pivot_longer(c(mean_elec, mean_civlib), names_to = "index", values_to = "value") %>%
  mutate(index = recode(index,
    mean_elec   = "Electoral (snelect_norm)",
    mean_civlib = "Civil Liberties (sncivlib_norm)"
  ))

comp_plot <- ggplot(comp_long, aes(x = year, y = value, color = index)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("Electoral (snelect_norm)" = "steelblue3",
                                "Civil Liberties (sncivlib_norm)" = "firebrick3")) +
  scale_x_continuous(breaks = seq(2000, 2023, 3)) +
  labs(
    title = "Component Sub-Index Trends Over Time",
    x = "Year", y = "Normalized score (0–1)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "bottom"
  )

temporal_panel <- ts_plot / comp_plot
ggsave(file.path(OUT_DIR, "divergence_temporal.png"),
       plot = temporal_panel, width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: divergence_temporal.png\n")

# 4b: Per-municipality stability
THRESH <- 0.10
muni_stability <- master_clean %>%
  group_by(MPIO_CDPMP, municipio, depto) %>%
  summarise(
    pct_elec_dom    = mean(div_score >  THRESH, na.rm = TRUE),
    pct_civlib_dom  = mean(div_score < -THRESH, na.rm = TRUE),
    pct_convergent  = mean(abs(div_score) <= THRESH, na.rm = TRUE),
    n_years         = n(),
    .groups         = "drop"
  ) %>%
  mutate(stability_type = case_when(
    pct_elec_dom   >= 0.80 ~ "Structurally elections-dominant",
    pct_civlib_dom >= 0.80 ~ "Structurally civlib-dominant",
    pct_convergent >= 0.80 ~ "Structurally convergent",
    TRUE                   ~ "Episodic / mixed"
  ))

cat("\nStability type counts:\n")
print(table(muni_stability$stability_type))

write_csv(muni_stability, file.path(OUT_DIR, "divergence_stability.csv"))
cat("Saved: divergence_stability.csv\n")

# 4c: Faceted maps for selected years
years_to_map <- c(2000, 2005, 2010, 2015, 2020, 2023)
map_temporal <- muni_geo %>%
  left_join(
    master_clean %>%
      filter(year %in% years_to_map) %>%
      select(MPIO_CDPMP, year, div_score),
    by = "MPIO_CDPMP"
  )

div_range <- range(master_clean$div_score, na.rm = TRUE)

facet_map <- ggplot(map_temporal) +
  geom_sf(aes(fill = div_score), color = NA) +
  scale_fill_gradient2(
    low = "firebrick3", mid = "white", high = "steelblue3",
    midpoint = 0, limits = div_range,
    name = "Divergence\n(Elec − CivLib)",
    na.value = "grey80"
  ) +
  facet_wrap(~year, ncol = 3) +
  labs(
    title    = "Electoral vs. Civil Liberties Divergence by Year",
    subtitle = "Blue = elections lead; Red = civil liberties lead"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey40"),
    strip.text      = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

ggsave(file.path(OUT_DIR, "divergence_facet_years.png"),
       plot = facet_map, width = 12, height = 10, dpi = 300, bg = "white")
cat("Saved: divergence_facet_years.png\n")

# ============================================================
# STEP 6: SHOCK-PERIOD FACETED MAPS
# ============================================================
cat("\n===== STEP 6: SHOCK-PERIOD FACETED MAPS =====\n")

# Year pairs bracketing each shock (before / after)
shock_pairs <- data.frame(
  year  = c(2003, 2006, 2007, 2009, 2012, 2016, 2017, 2020, 2023),
  label = c(
    "2003\n(pre-AUC demob.)",
    "2006\n(post-AUC demob.)",
    "2007\n(pre-commodity)",
    "2009\n(commodity peak)",
    "2012\n(FARC talks begin)",
    "2016\n(peace deal signed)",
    "2017\n(post-FARC demob.)",
    "2020\n(pandemic / dissidents)",
    "2023\n(recent)"
  )
)

# Only keep years actually in the data
shock_years_avail <- intersect(shock_pairs$year, unique(master_clean$year))
shock_labels      <- shock_pairs %>% filter(year %in% shock_years_avail)

map_shock <- muni_geo %>%
  left_join(
    master_clean %>%
      filter(year %in% shock_years_avail) %>%
      select(MPIO_CDPMP, year, div_score),
    by = "MPIO_CDPMP"
  ) %>%
  left_join(shock_labels, by = "year") %>%
  mutate(label = factor(label, levels = shock_labels$label))

shock_map <- ggplot(map_shock) +
  geom_sf(aes(fill = div_score), color = NA) +
  scale_fill_gradient2(
    low = "firebrick3", mid = "white", high = "steelblue3",
    midpoint = 0, limits = div_range,
    name = "Divergence\n(Elec − CivLib)",
    na.value = "grey80"
  ) +
  facet_wrap(~label, ncol = 3) +
  labs(
    title    = "Divergence Around Major Political Shocks",
    subtitle = "Blue = elections lead; Red = civil liberties lead. Fixed color scale across all panels."
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey40", size = 8),
    strip.text      = element_text(face = "bold", size = 8),
    legend.position = "bottom"
  )

ggsave(file.path(OUT_DIR, "divergence_shock_periods.png"),
       plot = shock_map, width = 14, height = 14, dpi = 300, bg = "white")
cat("Saved: divergence_shock_periods.png\n")

# ============================================================
# STEP 5: SPATIAL CLUSTERING (MORAN'S I)
# ============================================================
cat("\n===== STEP 5: SPATIAL CLUSTERING (MORAN'S I) =====\n")

map_div_valid <- map_div %>% filter(!is.na(mean_div))
coords <- st_centroid(map_div_valid) %>% st_coordinates()
nb <- knn2nb(knearneigh(coords, k = 5))
lw <- nb2listw(nb, style = "W")

moran_result <- moran.test(map_div_valid$mean_div, lw, na.action = na.exclude)
cat("\nMoran's I test for spatial autocorrelation of mean divergence:\n")
print(moran_result)

# ============================================================
# WRITE DIAGNOSTICS SUMMARY
# ============================================================
diag_path <- file.path(OUT_DIR, "divergence_diagnostics.txt")
sink(diag_path)
cat("===== DIVERGENCE DIAGNOSTICS SUMMARY =====\n")
cat(sprintf("N municipality-years: %d\n", nrow(master_clean)))
cat(sprintf("N municipalities:     %d\n", nrow(muni_div)))
cat(sprintf("Years covered:        %d–%d\n", min(master_clean$year), max(master_clean$year)))
cat(sprintf("\nCorrelation (snelect_norm, sncivlib_norm): r = %.4f\n", r_corr))
cat(sprintf("Divergence mean:  %.4f\n", mean(master_clean$div_score, na.rm = TRUE)))
cat(sprintf("Divergence SD:    %.4f\n", sd(master_clean$div_score,   na.rm = TRUE)))
cat(sprintf("Divergence range: %.4f to %.4f\n",
    min(master_clean$div_score, na.rm = TRUE),
    max(master_clean$div_score, na.rm = TRUE)))
cat(sprintf("Fraction elections-dominant (div>0): %.1f%%\n", 100 * pct_positive))
cat(sprintf("Fraction civlib-dominant   (div<0): %.1f%%\n", 100 * pct_negative))
cat(sprintf("\nTypology thresholds: Q25 = %.4f | Q75 = %.4f\n", q25, q75))
cat("\nTypology counts:\n")
print(table(muni_div$type))
cat("\nStability type counts (threshold = ±0.10):\n")
print(table(muni_stability$stability_type))
cat("\nMoran's I:\n")
print(moran_result)
cat("\nZooming In municipalities:\n")
print(as.data.frame(zi_check))
sink()
cat(sprintf("Saved: divergence_diagnostics.txt\n"))

cat("\n===== DONE =====\n")
cat(sprintf("All outputs saved to: %s\n", OUT_DIR))
