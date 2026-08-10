#---- Appendix D trend figures, part 2: calibration-check plots for EL_col_mt/CL_col_mt ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation")
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Weight Averaged CDF data by V-Dem data (Folder "05_weighting")
# Step 6: Combine subindices into sndem_final, compute EL_col_mt/CL_col_mt (Folder "06_benchmark",
#         "01_benchmark.R" -- this script's input)
# Step 7: Compare unbenchmarked/benchmarked data, run diagnostics (Folder "07_snvdem-col_diagnostics";
#         this script lives in the "trends" subfolder alongside 01_appendix_density_plots.R)

# PURPOSE: Appendix D companion for the _mt (V-Dem-native-scale) variables specifically -- not a
# repeat of the D.3b/D.4b dispersion story. Plots each dimension's V-Dem national anchor +/- the
# manuscript's Eq. 4/5 "maximum national range" as a band, with the actual empirical distribution of
# calibrated municipalities (EL_col_mt/CL_col_mt) overlaid as a boxplot per year -- a direct visual
# check of whether the calibrated municipalities fill the envelope V-Dem's own unevenness questions
# imply. Requested by PM (2026-08-09) after discussing what a _mt-specific graphic could add beyond
# what D.3b/D.4b (on the _gz scale) already show. Originally written to 06_benchmark/01_benchmark/;
# moved here same day so all Appendix D figure-generating scripts live together at the 07_ level --
# see 01_appendix_density_plots.R's header for the fuller rationale.

# EL_col_mt and CL_col_mt are on different native V-Dem scales (elections: unbounded latent,
# ~[-3.5,3.5]; civil liberties: bounded [0,1], but empirically compressed to ~[0.29,0.58] for
# Colombia) so, as with every other pre/post-benchmark comparison in this appendix, they get separate
# panels on their own native axes -- not one shared plot.

library(tidyverse)

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
trends_dir <- file.path(panel_dir, "07_snvdem-col_diagnostics", "trends")

#---- Load the canonical benchmarked output (Step 6) ----
snvdem_final <- read_rds(file.path(panel_dir, "06_benchmark", "03_output", "snvdem_col_benchmarked.rds"))

#---- Per-year national anchor + range (constant across municipalities within a year) ----
# Per the manuscript's "Calibrating Data for Cross-National Comparisons" section: the upper/lower
# bound of the national range around the subnational mean is anchor +/- Range (Eqs. 4-6). Here,
# Range = weighted_range (elections) / wtdCL_range (civil liberties), both already HPD x unevenness
# products computed in Step 4 and merged in by 01_benchmark.R.
band_by_year <- snvdem_final %>%
  group_by(year) %>%
  summarise(
    EL_anchor = first(v2elffelr),  EL_range = first(weighted_range),
    CL_anchor = first(CLSNmean),   CL_range = first(wtdCL_range),
    .groups = "drop"
  ) %>%
  mutate(
    EL_lo = EL_anchor - EL_range, EL_hi = EL_anchor + EL_range,
    CL_lo = CL_anchor - CL_range, CL_hi = CL_anchor + CL_range
  )

calibration_plot <- function(varname, band_lo, band_hi, anchor, ylab, title, fill_color) {
  ggplot() +
    geom_ribbon(data = band_by_year,
                aes(x = year, ymin = .data[[band_lo]], ymax = .data[[band_hi]]),
                fill = fill_color, alpha = 0.25) +
    geom_line(data = band_by_year, aes(x = year, y = .data[[anchor]]),
              color = fill_color, linewidth = 0.9) +
    geom_boxplot(data = snvdem_final, aes(x = year, y = .data[[varname]], group = year),
                 width = 0.5, outlier.size = 0.6, fill = "white", alpha = 0.7) +
    scale_x_continuous(breaks = seq(2000, 2023, 4)) +
    theme_bw() +
    labs(x = "Year", y = ylab, title = title)
}

#---- Elections: EL_col_mt vs. v2elffelr +/- weighted_range ----
figEL_mt <- calibration_plot("EL_col_mt", "EL_lo", "EL_hi", "EL_anchor",
                              "EL_col_mt (V-Dem latent scale)",
                              "Calibrated free & fair elections (EL_col_mt) vs. V-Dem's implied national range",
                              "salmon")
ggsave(figEL_mt, filename = file.path(trends_dir, "calibration_band_EL_col_mt.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

#---- Civil liberties: CL_col_mt vs. CLSNmean +/- wtdCL_range ----
figCL_mt <- calibration_plot("CL_col_mt", "CL_lo", "CL_hi", "CL_anchor",
                              "CL_col_mt (V-Dem 0-1 scale)",
                              "Calibrated civil liberties (CL_col_mt) vs. V-Dem's implied national range",
                              "skyblue")
ggsave(figCL_mt, filename = file.path(trends_dir, "calibration_band_CL_col_mt.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

cat(sprintf("Wrote %s\n", file.path(trends_dir, "calibration_band_EL_col_mt.png")))
cat(sprintf("Wrote %s\n", file.path(trends_dir, "calibration_band_CL_col_mt.png")))
