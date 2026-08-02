# ============================================================================
# Cluster-size time series, 2000-2023
#
# The single-year LISA map (03_lisa_map_2011.R) is a snapshot; it can't show
# whether the High-High/Low-Low cluster boundary is stable or moving. This
# script turns the year-by-year classification counts already computed in
# 01_lisa_year_screen.R (output/lisa_year_screen_summary.csv) into a stacked
# area chart: for every year 2000-2023, how many of Colombia's 1,122
# municipios fall into each LISA bucket (High-High, Low-Low, Outlier
# [High-Low + Low-High, combined because each is individually tiny], and Not
# significant). San Andres and Providencia (2 municipios, no queen-contiguity
# neighbors) are never tested and are excluded from the 1,122, so the bands
# sum to 1,120 every year.
#
# Two versions are produced:
#   - plain: the time series on its own
#   - annotated: the same chart with two national-level reference points
#     added -- a shaded band for the 2003-2006 AUC demobilization / Justice
#     and Peace Law window, and a dashed line at the 2016 FARC peace
#     agreement. These are marked as reference points for the reader, NOT a
#     causal claim -- see cluster/memo.md for why some of sndem's underlying
#     geopredictors (forced displacement, violent-days, homicides) overlap
#     with variables plausibly affected by demobilization, which is a reason
#     to flag this as a question for future work rather than assert it here.
#
# This section is meant to illustrate how the LISA panel could be used to
# show cluster dynamics over time, not to deliver a full analysis of what
# drove any single year's change.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

out_dir <- "G:/Shared drives/snvdem/snvdem-col/scripts/cluster/output"
summary_df <- read.csv(file.path(out_dir, "lisa_year_screen_summary.csv"))

n_total <- 1122
n_no_neighbors <- 2  # San Andres, Providencia -- never tested, excluded throughout

# --- Reshape into the four chart categories ---------------------------------
# High-Low and Low-High are each too small to read as separate bands (a few
# municipios out of 1,120 in most years), so they're combined into one
# "Outlier" category. "Not significant" is the residual: every municipio not
# already counted in HH, LL, or an outlier type.
wide_df <- summary_df %>%
  transmute(
    year,
    `High-High` = HH,
    `Low-Low` = LL,
    `Outlier (High-Low / Low-High)` = HL + LH,
    `Not significant` = (n_total - n_no_neighbors) - HH - LL - HL - LH
  )

write.csv(wide_df, file.path(out_dir, "lisa_cluster_size_timeseries.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "lisa_cluster_size_timeseries.csv"), "\n")

long_df <- wide_df %>%
  pivot_longer(-year, names_to = "category", values_to = "n_municipios") %>%
  mutate(category = factor(category, levels = c(
    "Not significant", "Low-Low", "High-High", "Outlier (High-Low / Low-High)"
  )))

# Same palette family as 03_lisa_map_2011.R (colorblind-safe RdBu + gray for
# "not part of a cluster") so a reader who has already seen the 2011 map
# recognizes the categories on sight.
pal <- c(
  "High-High"                     = "#CA0020",
  "Low-Low"                       = "#0571B0",
  "Outlier (High-Low / Low-High)" = "#F4A582",
  "Not significant"               = "#F0F0F0"
)

base_plot <- function(df) {
  ggplot(df, aes(x = year, y = n_municipios, fill = category)) +
    geom_area(color = "white", linewidth = 0.15) +
    scale_fill_manual(values = pal, name = "LISA classification",
                       breaks = c("High-High", "Low-Low", "Outlier (High-Low / Low-High)", "Not significant")) +
    scale_x_continuous(breaks = seq(2000, 2023, by = 4), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = NULL, y = "Number of municipios",
      title = "How many municipios fall into each LISA cluster type, 2000\u20132023",
      subtitle = "Subnational democracy index (sndem), queen contiguity weights, FDR-corrected p < 0.05"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "gray30"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# --- Plain version -----------------------------------------------------------
p_plain <- base_plot(long_df)
ggsave(file.path(out_dir, "lisa_cluster_size_timeseries_plain.png"), p_plain,
       width = 8, height = 5.5, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "lisa_cluster_size_timeseries_plain.png"), "\n")

# --- Annotated version --------------------------------------------------------
p_annotated <- base_plot(long_df) +
  annotate("rect", xmin = 2003, xmax = 2006, ymin = -Inf, ymax = Inf,
           fill = "gray20", alpha = 0.12) +
  annotate("text", x = 2004.5, y = 1120, label = "AUC demobilization /\nJustice and Peace Law",
           size = 2.7, color = "gray20", vjust = 1, lineheight = 0.9) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray20", linewidth = 0.4) +
  annotate("text", x = 2016.4, y = 1120, label = "2016 peace\nagreement",
           size = 2.7, color = "gray20", hjust = 0, vjust = 1, lineheight = 0.9)

ggsave(file.path(out_dir, "lisa_cluster_size_timeseries_annotated.png"), p_annotated,
       width = 8, height = 5.5, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "lisa_cluster_size_timeseries_annotated.png"), "\n")

# --- Zoomed variant: drop "Not significant" so the boundary movement reads ---
# On the 0-1120 axis above, "Not significant" (900+ municipios most years)
# compresses HH/LL/Outlier into a thin ribbon, muting the very jump the chart
# is meant to highlight. This version stacks only the three substantive
# categories, so the y-axis tops out around 300 instead of 1,120 and the
# 2004-2005 step (and the 2022-2023 decline) are visible at a glance. Keeps
# the same AUC/peace-agreement annotations as the full-scale version.
zoom_df <- long_df %>% filter(category != "Not significant") %>% droplevels()
y_max_zoom <- max(wide_df$`High-High` + wide_df$`Low-Low` + wide_df$`Outlier (High-Low / Low-High)`)

p_zoom <- ggplot(zoom_df, aes(x = year, y = n_municipios, fill = category)) +
  geom_area(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = pal, name = "LISA classification",
                     breaks = c("High-High", "Low-Low", "Outlier (High-Low / Low-High)")) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 4), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL, y = "Number of municipios",
    title = "Size of the significant LISA clusters only, 2000–2023",
    subtitle = "Same data as above, with “Not significant” dropped so cluster-boundary movement is visible"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  annotate("rect", xmin = 2003, xmax = 2006, ymin = -Inf, ymax = Inf,
           fill = "gray20", alpha = 0.12) +
  annotate("text", x = 2004.5, y = y_max_zoom, label = "AUC demobilization /\nJustice and Peace Law",
           size = 2.7, color = "gray20", vjust = 1, lineheight = 0.9) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray20", linewidth = 0.4) +
  annotate("text", x = 2016.4, y = y_max_zoom, label = "2016 peace\nagreement",
           size = 2.7, color = "gray20", hjust = 0, vjust = 1, lineheight = 0.9)

ggsave(file.path(out_dir, "lisa_cluster_size_timeseries_zoom.png"), p_zoom,
       width = 8, height = 5.5, dpi = 300, bg = "white")
cat("Saved", file.path(out_dir, "lisa_cluster_size_timeseries_zoom.png"), "\n")
