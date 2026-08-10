#---- Step 6b: benchmarked municipal boxplots by year ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder "01_empirical_data")
# Step 2: Impute missing values and merge into one panel (Folder "02_imputation")
# Step 3: Calculate averages of Empirical CDF data (Folder "03_geocoded_panel")
# Step 4: Subset V-Dem data, calculate criteria weights, apply national range (Folder "04_vdem_data")
# Step 5: Weight Averaged CDF data by V-Dem data (Folder "05_weighting")
# Step 6: Combine subindices into sndem_final, compute EL_col_mt/CL_col_mt (Folder "06_benchmark",
#         "01_benchmark.R" -- this script's input)
# Step 6b (this script): Redraw the paper's Figure 4 (free & fair elections) and Figure 5
#         (civil liberties) -- municipal-level distributions by year, on the V-Dem scale.

# This script instead reads the canonical 03_output/snvdem_col_benchmarked.rds directly, so the
# figures always match whatever 01_benchmark.R most recently produced. MC's original script is
# left in place under 01_benchmark/MC/ as the historical record.

library(tidyverse)

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
bench_dir <- file.path(panel_dir, "06_benchmark")

#---- Load the canonical benchmarked output (Step 6) ----
snvdem_final <- read_rds(file.path(bench_dir, "03_output", "snvdem_col_benchmarked.rds"))

# Per PM (2026-08-08): plot the GLOBALLY-STANDARDIZED pair (CL_col_gz / EL_col_gz), not the raw
# CL_col_mt (0-1) / EL_col_mt (latent V-Dem scale) pair MC's original script used. qnorm() alone
# (CL_col_z) puts civil liberties on the same *kind* of latent scale as elections but not the same
# *magnitude* -- CL_col_z's spread (-0.55 to 0.21) is much narrower than EL_col_mt's (-0.69 to
# 2.28) because CL_col_mt itself never approaches the 0/1 boundary. CL_col_gz/EL_col_gz are what
# 01_benchmark.R's own further rebalancing step produces (divided by each dimension's FIXED global
# sd), and are literally the two inputs sndem_final averages (Eq. 1) -- so they're the pair
# actually designed to be comparable, and both boxplots now share one y-axis for direct comparison.
shared_ylim <- c(-1.2, 1.2)  # covers both variables' full range (EL_col_gz: -0.93 to 0.94; CL_col_gz: -0.51 to 0.53) with padding

# Per PM (2026-08-08): pages holding these figures were rotated to landscape, one figure per
# page. Target ~9x6in (fits an 11x8.5in landscape page with 1in margins, leaving ~0.5in below
# for the caption). Year labels abbreviated to 2 digits ("00".."23") since 24 full 4-digit labels
# crowd at 9in width; base_size bumped so text holds up printed at this physical size.
FIG_WIDTH  <- 9
FIG_HEIGHT <- 6
LINEWIDTH <- 0.7

#---- Figure 5: civil liberties (CL_col_gz, global-standardized) ----
snclplot <- ggplot(snvdem_final, aes(x = as.factor(year), y = CL_col_gz)) +
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = LINEWIDTH) +
  geom_boxplot(stat = "boxplot", fill = "skyblue", linewidth = LINEWIDTH) +
  scale_x_discrete(labels = function(x) sprintf("%02d", as.integer(x) %% 100)) +
  labs(title = "Distribution of Colombian municipalities on civil liberties",
       x = "Year (20xx)", y = "Municipal-level civil liberties index\n(global SD units)") +
  scale_y_continuous(limits = shared_ylim) +
  theme_light(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave(snclplot, filename = file.path(bench_dir, "03_output", "snclplot.png"),
       device = ragg::agg_png, height = FIG_HEIGHT, width = FIG_WIDTH, units = "in", dpi = 300)

#---- Figure 4: free & fair elections (EL_col_gz, global-standardized) ----
snelplot <- ggplot(snvdem_final, aes(x = as.factor(year), y = EL_col_gz)) +
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = LINEWIDTH) +
  geom_boxplot(stat = "boxplot", fill = "salmon", linewidth = LINEWIDTH) +
  scale_x_discrete(labels = function(x) sprintf("%02d", as.integer(x) %% 100)) +
  labs(title = "Distribution of Colombian municipalities on free and fair elections",
       x = "Year (20xx)", y = "Municipal-level free & fair elections index\n(global SD units)") +
  scale_y_continuous(limits = shared_ylim) +
  theme_light(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave(snelplot, filename = file.path(bench_dir, "03_output", "snelplot.png"),
       device = ragg::agg_png, height = FIG_HEIGHT, width = FIG_WIDTH, units = "in", dpi = 300)

cat(sprintf("Wrote %s\n", file.path(bench_dir, "03_output", "snclplot.png")))
cat(sprintf("Wrote %s\n", file.path(bench_dir, "03_output", "snelplot.png")))

#---- Combined figure: both series dodged side-by-side per year, one shared axis ----
# Makes the elections-vs-civil-liberties dispersion
# contrast directly visible (see memory: within-year IQR rose in most years under the July
# formula -- elections +8% on average, civil liberties +5% -- and elections' boxes are visibly
# taller than civil liberties' in every year here).
combined_long <- bind_rows(
  snvdem_final %>% transmute(year, series = "Free & fair elections", value = EL_col_gz),
  snvdem_final %>% transmute(year, series = "Civil liberties",       value = CL_col_gz)
) %>%
  mutate(series = factor(series, levels = c("Free & fair elections", "Civil liberties")))

combined_plot <- ggplot(combined_long, aes(x = as.factor(year), y = value, fill = series)) +
  stat_boxplot(geom = "errorbar", position = position_dodge(0.75), width = 0.4, linewidth = LINEWIDTH) +
  geom_boxplot(position = position_dodge(0.75), width = 0.65, outlier.size = 0.8, linewidth = LINEWIDTH) +
  scale_fill_manual(values = c("Free & fair elections" = "salmon", "Civil liberties" = "skyblue")) +
  scale_x_discrete(labels = function(x) sprintf("%02d", as.integer(x) %% 100)) +
  labs(title = "Distribution of Colombian municipalities: free & fair elections vs. civil liberties",
       x = "Year (20xx)", y = "Municipal-level index (global SD units)", fill = "") +
  scale_y_continuous(limits = shared_ylim) +
  theme_light(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1), legend.background = element_rect(fill = "white", color = "grey70"),
        legend.title = element_blank())
ggsave(combined_plot, filename = file.path(bench_dir, "03_output", "combined_boxplot.png"),
       device = ragg::agg_png, height = FIG_HEIGHT, width = FIG_WIDTH, units = "in", dpi = 300)
cat(sprintf("Wrote %s\n", file.path(bench_dir, "03_output", "combined_boxplot.png")))
