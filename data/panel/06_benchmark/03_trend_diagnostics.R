#---- Step 8c: Trend diagnostics -- compare municipal-level benchmarked means to the raw
#              national V-Dem anchors, and show the effect of global vs. per-country
#              standardization (raised during PM's review 2026-07-02, before sharing with MC) ----

# Background: an earlier version of 01_benchmark.R standardized EL_col_mt and qnorm(CL_col_mt)
# using scale() computed from Colombia's own 2000-2023 panel (mean 0 / sd 1 within Colombia).
# PM flagged that this is ipsative -- ties Colombia's scores to Colombia's own distribution,
# which would NOT be comparable to another country's own self-centered z-score if this
# pipeline is extended. Fixed in 01_benchmark.R by standardizing against FIXED global
# constants (full V-Dem panel, 2000-2023, all countries) instead. This script shows the
# practical effect of that fix and validates the corrected sndem_final against the raw
# national anchors it's benchmarked to.

library(tidyverse)
library(vdemdata)

d <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/07_final_snvdem_data/snvdem_col_final.rds")

#---- Recompute the same global reference constants used in 01_benchmark.R (do not hardcode) ----
v15_global <- vdemdata::vdem %>% filter(year >= 2000, year <= 2023)
EL_global_mean <- mean(v15_global$v2elffelr, na.rm = TRUE)
EL_global_sd   <- sd(v15_global$v2elffelr,   na.rm = TRUE)

# Must match 01_benchmark.R exactly: CLSNmean-style (v2clsnlpct-adjusted), not raw v2x_civlib.
v15_global     <- v15_global %>% mutate(CLSNmean_global = v2x_civlib * (100 - v2clsnlpct) / 100)
CLSNmean_valid <- v15_global$CLSNmean_global[!is.na(v15_global$CLSNmean_global) &
                                                v15_global$CLSNmean_global > 0 &
                                                v15_global$CLSNmean_global < 1]
CLz_global     <- qnorm(CLSNmean_valid)
CL_global_mean <- mean(CLz_global, na.rm = TRUE)
CL_global_sd   <- sd(CLz_global,   na.rm = TRUE)

#---- For comparison only: what the OLD (Colombia-only, ipsative) standardization gave ----
d <- d %>% mutate(
  EL_col_ipsative = as.numeric(scale(EL_col_mt)),
  CL_col_ipsative = as.numeric(scale(CL_col_z)),
  sndem_ipsative  = 0.5 * (EL_col_ipsative + CL_col_ipsative)
)

cat("Range comparison:\n")
cat("  sndem_final (global-standardized, current):", round(range(d$sndem_final, na.rm = TRUE), 3), "\n")
cat("  sndem_ipsative (Colombia-only scale(), superseded):", round(range(d$sndem_ipsative, na.rm = TRUE), 3), "\n")
cat("  Correlation (shape should be near-identical, both are linear rescales):",
    round(cor(d$sndem_final, d$sndem_ipsative, use = "complete.obs"), 4), "\n\n")

#---- Year-level summary: benchmarked means (global-standardized) + raw national anchors ----
trend <- d %>%
  group_by(year) %>%
  summarize(
    EL_mean             = mean(EL_col_gz,  na.rm = TRUE),
    CL_mean             = mean(CL_col_gz,  na.rm = TRUE),
    sndem_mean          = mean(sndem_final, na.rm = TRUE),
    sndem_mean_ipsative = mean(sndem_ipsative, na.rm = TRUE),
    v2elffelr_national  = first(v2elffelr),   # national anchor, constant within year
    CLSNmean_national   = first(CLSNmean),    # national anchor, constant within year
    .groups = "drop"
  )

print(trend, n = Inf)

#---- Plot 1: global-standardized benchmarked means vs. raw national anchors ----
# Raw anchors are on their own native scale (v2elffelr ~[-3.5,3.5], CLSNmean [0,1]) --
# plotted on a secondary axis per panel rather than force-standardized, so the comparison
# is against the actual V-Dem numbers you'd see in the codebook, not a re-derived version.
p_el <- ggplot(trend, aes(x = year)) +
  geom_line(aes(y = EL_mean, color = "Municipal mean (EL_col_gz, global std.)"), linewidth = 1) +
  geom_line(aes(y = (v2elffelr_national - EL_global_mean) / EL_global_sd,
                color = "National anchor (v2elffelr, global std.)"),
            linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  theme_light() +
  labs(title = "Elections: municipal mean vs. national anchor", x = "", y = "Global standard deviations",
       color = "")

p_cl <- ggplot(trend, aes(x = year)) +
  geom_line(aes(y = CL_mean, color = "Municipal mean (CL_col_gz, global std.)"), linewidth = 1) +
  geom_line(aes(y = (qnorm(CLSNmean_national) - CL_global_mean) / CL_global_sd,
                color = "National anchor (qnorm(CLSNmean), global std.)"),
            linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  theme_light() +
  labs(title = "Civil liberties: municipal mean vs. national anchor", x = "", y = "Global standard deviations",
       color = "")

library(patchwork)
p_combined <- p_el / p_cl +
  plot_annotation(title = "Benchmarked municipal means vs. national V-Dem anchors (Colombia)",
                   subtitle = "Both on the global-standardized scale (0 = full V-Dem 2000-2023 country-year average)")

ggsave(p_combined, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/trend_vs_national_anchors.png",
       width = 9, height = 8, dpi = 300)

#---- Plot 2: global standardization (current, correct) vs. Colombia-only scale() (superseded) ----
p2 <- trend %>%
  select(year, sndem_mean, sndem_mean_ipsative) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                          sndem_mean = "Global standardization [current]",
                          sndem_mean_ipsative = "Colombia-only scale() [superseded, not comparable across countries]")) %>%
  ggplot(aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dotted") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  labs(title = "sndem_final: global standardization vs. the superseded Colombia-only version",
       subtitle = "Same underlying trend shape -- but only one of these generalizes to other countries",
       x = "", y = "sndem_final", color = "")

ggsave(p2, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/diagnostics/global_vs_ipsative_standardization.png",
       width = 8, height = 6, dpi = 300)

#---- Plot 3: civil liberties in context -- raw national level AND standardized position ----
# Requested by PM (2026-07-02): the CL_col_gz map panel can look "dark" (low) relative to
# elections. That's a real, modest pattern, not a rendering error (see Section 2 of Memo 2 for
# the asymmetry that was inflating it before the fix) -- but it needs to be shown in context,
# not just asserted. Panel A is Colombia's actual civil liberties level in its native units
# (CLSNmean, v2clsnlpct-discounted, same [0,1] scale as v2x_civlib). Panel B is the same
# quantity after global standardization, with the reference line showing what CLSNmean value
# equates to the global average (pnorm(CL_global_mean) converts the latent-scale global mean
# back to the native [0,1] scale for a directly comparable horizontal reference).
CL_global_mean_raw_scale <- pnorm(CL_global_mean)

p_cl_raw <- ggplot(trend, aes(x = year)) +
  geom_line(aes(y = CLSNmean_national), color = "#b2182b", linewidth = 1) +
  geom_hline(yintercept = CL_global_mean_raw_scale, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2001, y = CL_global_mean_raw_scale, vjust = -0.6, hjust = 0,
           label = "Global 2000-2023 average", size = 3, color = "grey40") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_light() +
  labs(title = "Colombia's civil liberties, native V-Dem scale",
       subtitle = "CLSNmean (v2x_civlib discounted by v2clsnlpct) -- national, not municipal",
       x = "", y = "CLSNmean [0,1]")

p_cl_std <- ggplot(trend, aes(x = year)) +
  geom_line(aes(y = CL_mean), color = "#2166ac", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2001, y = 0, vjust = -0.6, hjust = 0,
           label = "Global average (0)", size = 3, color = "grey40") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  theme_light() +
  labs(title = "Same measure, standardized against the full V-Dem panel",
       subtitle = "CL_col_gz, municipal mean -- 0 = full V-Dem 2000-2023 country-year average",
       x = "", y = "Global standard deviations")

p_cl_context <- p_cl_raw / p_cl_std +
  plot_annotation(title = "Civil liberties in context: Colombia's own level, and where it sits globally")

ggsave(p_cl_context, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/civil_liberties_in_context.png",
       width = 8, height = 8, dpi = 300)

cat("\nSaved:\n  trend_vs_national_anchors.png\n  global_vs_ipsative_standardization.png\n  civil_liberties_in_context.png\n")
