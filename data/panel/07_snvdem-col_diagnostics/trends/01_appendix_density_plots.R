#---- Appendix D trend figures, part 1: municipal density plots by year ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation")
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Weight Averaged CDF data by V-Dem data (Folder "05_weighting")
# Step 6: Combine subindices into sndem_final, compute EL_col_mt/CL_col_mt (Folder "06_benchmark",
#         "01_benchmark.R" -- this script's input)
# Step 7: Compare unbenchmarked/benchmarked data, run diagnostics (Folder "07_snvdem-col_diagnostics";
#         this script lives in the "trends" subfolder, alongside "zoom-in"'s case-study Appendix D
#         content -- "trends" holds by-year distributional figures, "zoom-in" holds the three focal
#         municipalities' cross-sectional profile)

# PURPOSE: per PM (2026-08-09), Appendix D's variable paragraph was expanded to describe both the
# 5 pre-benchmark and 5 benchmarked variables (see appendix_D_variables_paragraph.md in this same
# folder for the revised prose). D.3b/D.4b (EL_col_gz/CL_col_gz) were the first figures written here;
# D.3a/D.4a were added same session, after PM noticed only the "b" (benchmarked) versions existed --
# these are CURRENT-data regenerations of the existing appendix's Figures D.3/D.4 (snelect/sncivlib
# by-year density plots), not just a copy of the old docx images. Same rationale as the D.1/D.2
# refresh in 03_appendix_national_trends.R: worth checking whether the originals were also stale
# before assuming they're still accurate. Originally placed in 06_benchmark/01_benchmark/ alongside
# the Step 6 pipeline script itself; moved here 2026-08-09 so all Appendix D figure-generating
# scripts live together at the 07_ level, script+output co-located, matching
# zoom-in/02_appendix_census_profile.R's existing convention -- 06_benchmark/ stays reserved for
# pipeline computation and the LASA26 main-paper figures (02_benchmark_boxplots.R).

library(tidyverse)

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
trends_dir <- file.path(panel_dir, "07_snvdem-col_diagnostics", "trends")

#---- Load the canonical benchmarked output (Step 6) ----
snvdem_final <- read_rds(file.path(panel_dir, "06_benchmark", "03_output", "snvdem_col_benchmarked.rds"))

density_by_year <- function(varname, xlab, title) {
  ggplot(snvdem_final, aes(x = .data[[varname]], color = as.factor(year))) +
    geom_density(linewidth = 0.5) +
    theme_bw() +
    labs(x = xlab, y = "density", title = title, color = NULL)
}

#---- Figure D.3a: pre-benchmark elections (snelect), by year -- current-data version of existing D.3 ----
figD3a <- density_by_year("snelect", "snelect (0-1 CDF scale)",
                           "Density plot of subnational electoral freeness and fairness scores")
ggsave(figD3a, filename = file.path(trends_dir, "figD3a_snelect_byyear.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

#---- Figure D.4a: pre-benchmark civil liberties (sncivlib), by year -- current-data version of existing D.4 ----
figD4a <- density_by_year("sncivlib", "sncivlib (0-1 CDF scale)",
                           "Density plot of subnational civil liberty strength scores")
ggsave(figD4a, filename = file.path(trends_dir, "figD4a_sncivlib_byyear.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

#---- Figure D.3b: benchmarked elections (EL_col_gz), by year ----
figD3b <- density_by_year("EL_col_gz", "EL_col_gz (global SD units)",
                           "Density plot of benchmarked subnational electoral freeness and fairness scores")
ggsave(figD3b, filename = file.path(trends_dir, "figD3b_EL_col_gz_byyear.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

#---- Figure D.4b: benchmarked civil liberties (CL_col_gz), by year ----
figD4b <- density_by_year("CL_col_gz", "CL_col_gz (global SD units)",
                           "Density plot of benchmarked subnational civil liberty strength scores")
ggsave(figD4b, filename = file.path(trends_dir, "figD4b_CL_col_gz_byyear.png"),
       device = "png", height = 5.2, width = 8.5, units = "in", dpi = 300)

cat(sprintf("Wrote %s\n", file.path(trends_dir, "figD3a_snelect_byyear.png")))
cat(sprintf("Wrote %s\n", file.path(trends_dir, "figD4a_sncivlib_byyear.png")))
cat(sprintf("Wrote %s\n", file.path(trends_dir, "figD3b_EL_col_gz_byyear.png")))
cat(sprintf("Wrote %s\n", file.path(trends_dir, "figD4b_CL_col_gz_byyear.png")))
