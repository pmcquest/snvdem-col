#---- Step 8d (diagnostic): check whether benchmarking preserves or inverts spatial ranking ----

# Internal diagnostic, not part of the shared memo. Originally written 2026-07-02 after
# noticing the unbenchmarked and benchmarked 2023 maps put some municipality groupings in
# opposite positions. Found that weighted_range/wtdCL_range (04_vdem_data/01_weighting.R) were
# negative in all 24 years, inverting every municipality's benchmarked rank relative to its
# unbenchmarked rank (r = -1.0000 in every year, both dimensions). Fixed the same day by
# rewriting weighted_range/wtdCL_range as a coder-proportion-weighted average (see
# 04_vdem_data/01_weighting.R). Re-running this script now should show r = +1.0000 -- kept as
# a regression check, not a live bug report.

# 2026-07-06 reorg: moved from 06_benchmark/diagnostics/04_spatial_rank_check.R to
# 02_diagnostics/01_scripts/ (renumbered 03_, folded in with the other diagnostics scripts).

library(tidyverse)
library(haven)

d <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/03_output/snvdem_col_benchmarked.rds")

#---- Root cause: weighted_range / wtdCL_range by year (should be a magnitude, is negative) ----
range_by_year <- d %>% distinct(year, weighted_range, wtdCL_range) %>% arrange(year)
cat("weighted_range negative in", sum(range_by_year$weighted_range < 0), "of", nrow(range_by_year), "years\n")
cat("wtdCL_range negative in", sum(range_by_year$wtdCL_range < 0), "of", nrow(range_by_year), "years\n")

p1 <- range_by_year %>%
  pivot_longer(-year, names_to = "range_var", values_to = "value") %>%
  mutate(range_var = recode(range_var, weighted_range = "weighted_range (elections)",
                             wtdCL_range = "wtdCL_range (civil liberties)")) %>%
  ggplot(aes(x = year, y = value, fill = value < 0)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~range_var, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue"), guide = "none") +
  theme_light() +
  labs(title = "The national range multiplier is negative in every year (2000-2023)",
       subtitle = "Should represent a magnitude of within-country variation -- instead flips the sign of every municipal deviation",
       x = "", y = "Value")
ggsave(p1, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/02_diagnostics/02_outputs/weighted_range_negative_by_year.png",
       width = 8, height = 7, dpi = 300)

#---- Rank inversion: within-year correlation, unbenchmarked vs benchmarked ----
cors <- d %>% group_by(year) %>%
  summarize(cor_EL = cor(snelect, EL_col_mt, use="pairwise"),
            cor_CL = cor(sncivlib, CL_col_mt, use="pairwise"), .groups="drop")
cat("\ncor(snelect, EL_col_mt) negative in", sum(cors$cor_EL < 0), "of", nrow(cors), "years (mean r =", round(mean(cors$cor_EL),4), ")\n")
cat("cor(sncivlib, CL_col_mt) negative in", sum(cors$cor_CL < 0), "of", nrow(cors), "years (mean r =", round(mean(cors$cor_CL),4), ")\n")

p2 <- d %>% filter(year == 2023) %>%
  ggplot(aes(x = snelect, y = EL_col_mt)) +
  geom_point(alpha = 0.3, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  theme_light() +
  labs(title = "Unbenchmarked vs. benchmarked municipal elections score, 2023",
       subtitle = paste0("Within-year correlation r = ", round(cors$cor_EL[cors$year==2023], 4),
                          " -- a municipality's benchmarked rank is the exact mirror of its unbenchmarked rank"),
       x = "snelect (unbenchmarked, CDF-standardized)", y = "EL_col_mt (benchmarked)")
ggsave(p2, filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/02_diagnostics/02_outputs/rank_inversion_scatter_2023.png",
       width = 8, height = 6, dpi = 300)

#---- Neutral illustration: top/bottom decile swap, no regional grouping needed ----
d2023 <- filter(d, year == 2023) %>%
  mutate(unbenchmarked_decile = ntile(snelect, 10))

decile_check <- d2023 %>%
  group_by(unbenchmarked_decile) %>%
  summarize(mean_snelect_unbenchmarked = mean(snelect), mean_EL_col_mt_benchmarked = mean(EL_col_mt), .groups = "drop")
cat("\nTop unbenchmarked decile (10) mean EL_col_mt (benchmarked):",
    round(decile_check$mean_EL_col_mt_benchmarked[decile_check$unbenchmarked_decile == 10], 3), "\n")
cat("Bottom unbenchmarked decile (1) mean EL_col_mt (benchmarked):",
    round(decile_check$mean_EL_col_mt_benchmarked[decile_check$unbenchmarked_decile == 1], 3), "\n")
print(as.data.frame(round(decile_check, 3)))

cat("\nSaved: weighted_range_negative_by_year.png, rank_inversion_scatter_2023.png\n")
