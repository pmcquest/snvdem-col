# Leave-one-out recalculation (item A2, v3_ToDo_PoP.docx)
#
# For each criterion: drop it entirely from the index formula (both its
# numerator term AND its weight/denominator term), recompute snelect/sncivlib/
# sndem for every municipality-year, and report how much the index moves.
# This is a different question than tornado.R's weight-share chart: weight
# share asks "how much does the formula count this criterion"; this asks
# "if this criterion didn't exist, how different would the index actually be."
#
# Per the doc note, PCA and the post-hoc regression add-on are deferred --
# this only implements the core leave-one-out recalculation.

library(dplyr)
library(tidyr)
library(ggplot2)

options(width = 200)

# 1. Load Data ----
# Uses the PRE-benchmarked (unbenchmarked) index -- snelect/sncivlib/sndem,
# not sndem_final from 06_benchmark/. Per benchmarking_memo2_2026-07-06.md
# Section 5, the unbenchmarked measure is the within-Colombia relative
# comparison ("how different would municipalities' standing be without this
# criterion"), which is exactly the LOO question; the benchmarked measure
# applies a further nonlinear, globally-anchored transform on top for
# cross-country comparison, which isn't what's being asked here.
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds")

# 2. Reconstruct per-criterion numerator and weight terms (same formulas as
#    FA23.R / tornado.R, verified there to reproduce snelect/sncivlib exactly) ----
num_emel <- snvdem %>%
  transmute(
    urban       = avg0t1hi * el_Urban + avg0t1lo * el_Rural,
    econ_dev    = avg2t3hi * el_More_development + avg2t3lo * el_Less_development,
    prox_cap    = avg4t5hi * (1 - el_Inside_capital) + avg4t5lo * (1 - el_Outside_capital),
    north       = avg6 * el_North,
    south       = avg7 * el_South,
    west        = avg8 * el_West,
    east        = avg9 * el_East,
    nonviolent  = avg10t11 * wt_el_1011,
    pop_density = avg12 * el_Sparse_population,
    nonremote   = avg13 * el_Remote,
    nonindig    = avg14 * (1 - el_Indigenous),
    compete     = avg15t16hi * (1 - el_Ruling_party_strong) + avg15t16lo * (1 - el_Ruling_party_weak)
  )

den_emel <- snvdem %>%
  transmute(
    urban       = el_Urban + el_Rural,
    econ_dev    = el_More_development + el_Less_development,
    prox_cap    = (1 - el_Inside_capital) + (1 - el_Outside_capital),
    north       = el_North,
    south       = el_South,
    west        = el_West,
    east        = el_East,
    nonviolent  = wt_el_1011,
    pop_density = el_Sparse_population,
    nonremote   = el_Remote,
    nonindig    = (1 - el_Indigenous),
    compete     = (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak)
  )

num_cscw <- snvdem %>%
  transmute(
    urban       = avg0t1hi * cl_Urban + avg0t1lo * cl_Rural,
    econ_dev    = avg2t3hi * cl_More_development + avg2t3lo * cl_Less_development,
    prox_cap    = avg4t5hi * (1 - cl_Inside_capital) + avg4t5lo * (1 - cl_Outside_capital),
    north       = avg6 * cl_North,
    south       = avg7 * cl_South,
    west        = avg8 * cl_West,
    east        = avg9 * cl_East,
    nonviolent  = avg10t11 * wt_cl_1011,
    pop_density = avg12 * cl_Sparse_population,
    nonremote   = avg13 * cl_Remote,
    nonindig    = avg14 * (1 - cl_Indigenous),
    compete     = avg15t16hi * (1 - cl_Ruling_party_strong) + avg15t16lo * (1 - cl_Ruling_party_weak)
  )

den_cscw <- snvdem %>%
  transmute(
    urban       = cl_Urban + cl_Rural,
    econ_dev    = cl_More_development + cl_Less_development,
    prox_cap    = (1 - cl_Inside_capital) + (1 - cl_Outside_capital),
    north       = cl_North,
    south       = cl_South,
    west        = cl_West,
    east        = cl_East,
    nonviolent  = wt_cl_1011,
    pop_density = cl_Sparse_population,
    nonremote   = cl_Remote,
    nonindig    = (1 - cl_Indigenous),
    compete     = (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak)
  )

criteria <- names(num_emel)

# weighted_avg_narm_grouped(): mirrors weighted_avg_narm() in
# 05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R exactly,
# just operating on this script's 12 grouped criterion columns instead of the
# production script's 16 individual predictor columns. Confirmed empirically
# that every hi/lo pair grouped above (urban/rural, econ_dev, prox_cap,
# compete) shares an identical NA pattern between its hi and lo half, so
# pre-summing them before the missingness check is equivalent to the
# production script's column-by-column treatment -- not an approximation.
# A plain rowSums() (the prior version of this script) silently produces NA
# for the WHOLE row whenever any one grouped criterion is missing, which the
# production script never does (it drops the missing pair and renormalizes)
# -- and the original "reconstruction check" below masked this with
# na.rm=TRUE in the max(), so it never caught the mismatch.
weighted_avg_narm_grouped <- function(num_df, den_df, min_weight_fraction = 0.5) {
  num_mat <- as.matrix(num_df)
  den_mat <- as.matrix(den_df)
  den_avail <- den_mat
  den_avail[is.na(num_mat)] <- NA
  num <- rowSums(num_mat, na.rm = TRUE)
  den <- rowSums(den_avail, na.rm = TRUE)
  full_den <- rowSums(den_mat)  # den never itself NA -- weights are always present
  result <- num / den
  result[(den / full_den) < min_weight_fraction] <- NA
  result
}

# 3. Full index (verify against saved snelect/sncivlib before trusting LOO) ----
snelect_recon <- weighted_avg_narm_grouped(num_emel, den_emel)
sncivlib_recon <- weighted_avg_narm_grouped(num_cscw, den_cscw)
sndem_recon <- 0.5 * (snelect_recon + sncivlib_recon)

cat("Reconstruction check (max abs diff vs. saved columns, non-NA rows only):\n")
cat("  snelect: ", max(abs(snelect_recon - snvdem$snelect), na.rm = TRUE), "\n")
cat("  sncivlib:", max(abs(sncivlib_recon - snvdem$sncivlib), na.rm = TRUE), "\n")
cat("  sndem:   ", max(abs(sndem_recon - snvdem$sndem), na.rm = TRUE), "\n")
cat("Reconstruction NA count vs. saved-column NA count (should match):\n")
cat("  snelect: ", sum(is.na(snelect_recon)), "vs.", sum(is.na(snvdem$snelect)), "\n")
cat("  sncivlib:", sum(is.na(sncivlib_recon)), "vs.", sum(is.na(snvdem$sncivlib)), "\n\n")

# 4. Leave-one-out: drop each criterion entirely (not just where it happens to
#    be missing), recompute via the same drop/renormalize/floor logic over the
#    remaining 11 criteria, and take the difference from the full index ----
loo_results <- lapply(criteria, function(cn) {
  keep <- setdiff(criteria, cn)
  loo_snelect <- weighted_avg_narm_grouped(num_emel[keep], den_emel[keep])
  loo_sncivlib <- weighted_avg_narm_grouped(num_cscw[keep], den_cscw[keep])
  loo_sndem <- 0.5 * (loo_snelect + loo_sncivlib)

  data.frame(
    Criterion = cn,
    year = snvdem$year,
    Impact_EMEL = loo_snelect - snelect_recon,
    Impact_CSCW = loo_sncivlib - sncivlib_recon,
    Impact_sndem = loo_sndem - sndem_recon
  )
}) %>% bind_rows()

cat("Rows dropped from LOO summary due to NA (either full or LOO index unavailable):",
    sum(is.na(loo_results$Impact_sndem)), "of", nrow(loo_results), "\n\n")
loo_results <- loo_results %>% filter(!is.na(Impact_sndem))

# 5. Summary table (mean/median/SD/5th-95th pct impact per criterion) ----
summary_tbl <- loo_results %>%
  group_by(Criterion) %>%
  summarise(
    Mean_EMEL = mean(Impact_EMEL), Mean_CSCW = mean(Impact_CSCW), Mean_sndem = mean(Impact_sndem),
    Median_sndem = median(Impact_sndem), SD_sndem = sd(Impact_sndem),
    P05_sndem = quantile(Impact_sndem, 0.05), P95_sndem = quantile(Impact_sndem, 0.95),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Mean_sndem))) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

cat("--- Leave-One-Out Impact Table (change in index when criterion is excluded) ---\n")
print(summary_tbl, row.names = FALSE)

write.csv(summary_tbl, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/leave_one_out_summary.csv", row.names = FALSE)

# 5b. Standardized robustness check -- does the raw-scale DIRECTION (sign) of
#     mean impact survive once each criterion is put on a common scale?
#     8 of 12 criteria are single CDF-rank predictors (mean ~0.50 by
#     construction); the other 4 (urban/econ_dev/prox_cap/compete) are hi/lo
#     pairs zero-filled on the inactive half every row, which structurally
#     caps their own average well below 0.50 -- a pure encoding artifact, not
#     a data pattern (see leave_one_out_memo.md Section 6 for the full
#     derivation). Z-score each criterion's own num/den ratio to its own
#     mean 0 / SD 1 before rebuilding num_std = z * den (weight unchanged,
#     only scale), and rerun the identical LOO procedure on that. This is a
#     diagnostic only -- not a candidate replacement for sndem. ----
standardize_group <- function(num_df, den_df) {
  ratio <- num_df / den_df
  z <- as.data.frame(scale(ratio))
  list(num = z * den_df, den = den_df)
}

emel_std <- standardize_group(num_emel, den_emel)
cscw_std <- standardize_group(num_cscw, den_cscw)

snelect_std_full <- weighted_avg_narm_grouped(emel_std$num, emel_std$den)
sncivlib_std_full <- weighted_avg_narm_grouped(cscw_std$num, cscw_std$den)
sndem_std_full <- 0.5 * (snelect_std_full + sncivlib_std_full)

loo_std <- lapply(criteria, function(cn) {
  keep <- setdiff(criteria, cn)
  loo_el <- weighted_avg_narm_grouped(emel_std$num[keep], emel_std$den[keep])
  loo_cl <- weighted_avg_narm_grouped(cscw_std$num[keep], cscw_std$den[keep])
  loo_sndem <- 0.5 * (loo_el + loo_cl)
  data.frame(Criterion = cn, Impact_sndem_std = loo_sndem - sndem_std_full)
}) %>% bind_rows() %>% filter(!is.na(Impact_sndem_std))

summary_std <- loo_std %>%
  group_by(Criterion) %>%
  summarise(Mean_std = mean(Impact_sndem_std), SD_std = sd(Impact_sndem_std), .groups = "drop") %>%
  left_join(summary_tbl %>% select(Criterion, Mean_sndem), by = "Criterion") %>%
  mutate(sign_flipped = sign(Mean_std) != sign(Mean_sndem)) %>%
  arrange(desc(abs(Mean_std)))

cat("--- Standardized LOO robustness check (raw sign vs. scale-corrected sign) ---\n")
print(summary_std, row.names = FALSE)
cat("Criteria with sign flip after standardization:", sum(summary_std$sign_flipped), "of 12\n\n")

write.csv(summary_std, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/leave_one_out_standardized_check.csv", row.names = FALSE)

# 6. Tornado figure: sndem (the composite index) ----
# Ordered by MAGNITUDE of mean impact (a true tornado -- most influential
# criterion, regardless of sign, at the top). MAGNITUDE ONLY -- no direction
# fill.
#
# A signed (red/green) version was tried and dropped. Per leave_one_out_memo.md
# Section 6, the sign of the composite-level mean impact is NOT a usable
# signal: every one of the 12 underlying predictors correlates POSITIVELY
# with sndem (no criterion actually pulls the index the "wrong way"), and a
# standardized robustness check (z-scoring each criterion to a common scale
# before re-running LOO) showed the raw-scale sign isn't even approximately
# right -- 6 of 12 criteria flip sign once the hi/lo-split-vs-single-predictor
# encoding-scale difference is removed, and the rest shrink to well within
# noise (mean impact orders of magnitude smaller than its own SD). The
# apparent direction in an earlier version of this figure was an artifact of
# that encoding difference, not a real finding -- see the memo before adding
# any direction claim back to this figure.
order_sndem <- summary_tbl %>% arrange(abs(Mean_sndem)) %>% pull(Criterion)

p_sndem <- loo_results %>%
  mutate(Criterion = factor(Criterion, levels = order_sndem)) %>%
  ggplot(aes(x = Criterion, y = Impact_sndem)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, fill = "#7570b3", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  coord_flip() +
  labs(title = "Change in the SNVDEM Index When Each Criterion Is Excluded",
       subtitle = "Distribution across all municipality-years, 2000-2023 -- sorted by |mean impact|.\nMagnitude only -- sign is not a reliable signal here; see leave_one_out_memo.md Sec. 8.",
       x = NULL, y = "Change in index (excluded - full)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 8))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/imgs/leave_one_out_sndem.png",
       p_sndem, width = 9, height = 6, dpi = 150)

# 7. Tornado figure: EMEL vs. CSCW mean impact, sorted ----
# MAGNITUDE ONLY, same reasoning as Section 6 above -- EMEL and CSCW are each
# weighted averages of the same 12 raw-scale num/den ratios (just with
# different weight sets), so they inherit the identical hi/lo-split-vs-
# single-predictor scale artifact. Plotting |Mean_EMEL|/|Mean_CSCW| instead
# of the signed values keeps this figure's actual purpose intact -- comparing
# which pillar each criterion influences more -- without implying a
# direction claim that leave_one_out_memo.md Sec. 8/11's standardized check showed isn't reliable.
order_pillar <- summary_tbl %>% arrange(pmax(abs(Mean_EMEL), abs(Mean_CSCW))) %>% pull(Criterion)

p_pillar <- summary_tbl %>%
  select(Criterion, Mean_EMEL, Mean_CSCW) %>%
  pivot_longer(-Criterion, names_to = "Pillar", values_to = "Mean_Impact") %>%
  mutate(Criterion = factor(Criterion, levels = order_pillar),
         Pillar = sub("Mean_", "", Pillar),
         Abs_Impact = abs(Mean_Impact)) %>%
  ggplot(aes(x = Criterion, y = Abs_Impact, fill = Pillar)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c(EMEL = "#2c7bb6", CSCW = "#d7191c")) +
  labs(title = "Mean |Impact| on Each Pillar When Criterion Is Excluded",
       subtitle = "Magnitude only -- sign is not a reliable signal here; see leave_one_out_memo.md Sec. 8.",
       x = NULL, y = "Mean |change in index| (excluded - full)", fill = "Pillar") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/imgs/leave_one_out_pillars.png",
       p_pillar, width = 9, height = 6, dpi = 150)

# 8. Time-trend line chart: three criteria whose year-by-year trajectory
#    illustrates the LOO mechanism itself (see leave_one_out_memo.md Sec. 9)
#    -- nonviolent (impact tracks its own V-Dem weight), nonindig (tracks the
#    Sec 6.1 double-inversion open question), compete (tracks its own
#    predictor's cross-municipal mean, not its weight -- unpacked below) ----
highlight_crit <- c("nonviolent", "nonindig", "compete")
yr_highlight <- loo_results %>%
  filter(Criterion %in% highlight_crit) %>%
  group_by(Criterion, year) %>%
  summarise(mean_impact = mean(Impact_sndem), .groups = "drop")

p_trend <- ggplot(yr_highlight, aes(x = year, y = mean_impact, color = Criterion)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  scale_color_manual(values = c(compete = "#1b9e77", nonindig = "#d95f02", nonviolent = "#7570b3")) +
  labs(title = "Mean LOO Impact Over Time, Selected Criteria",
       subtitle = "Effect-size trajectory, 2000-2023 -- each criterion keeps one sign throughout, so this is a\nwithin-criterion comparison over time, not a cross-criterion direction claim.",
       x = NULL, y = "Mean change in index when excluded (excluded - full)", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 8))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/imgs/leave_one_out_time_trend.png",
       p_trend, width = 9, height = 5.5, dpi = 150)

# 9. Own-score decomposition for the three highlighted criteria
#    (leave_one_out_memo.md Sec. 9). For any criterion that ISN'T a hi/lo
#    split, the weight cancels algebraically out of its own num/den ratio
#    (e.g. nonviolent: avg10t11 * wt_el_1011 / wt_el_1011 = avg10t11 exactly),
#    so its "own score" is just the Step-3 CDF geopredictor value, independent
#    of the Step-4 V-Dem weight. That means a LOO trend for one of these 8
#    criteria can come from the geopredictor changing over time, the weight
#    changing, or both -- checked explicitly below for nonviolent and
#    nonindig, rather than assuming the weight alone (as an earlier version of
#    this analysis did for both). compete IS a hi/lo split, so weight and
#    predictor don't cleanly separate the same way -- its own ratio is
#    checked directly instead. ----
own_score_check <- function(num_col, den_col, weight_col_expr) {
  data.frame(year = snvdem$year, own_score = num_col / den_col, weight = weight_col_expr) %>%
    group_by(year) %>%
    summarise(mean_own_score = mean(own_score, na.rm = TRUE), mean_weight = mean(weight, na.rm = TRUE), .groups = "drop")
}

nonviolent_impact_by_year <- loo_results %>% filter(Criterion == "nonviolent") %>%
  group_by(year) %>% summarise(mean_impact = mean(Impact_sndem), .groups = "drop")
nonviolent_decomp <- own_score_check(num_emel$nonviolent, den_emel$nonviolent, snvdem$wt_el_1011) %>%
  left_join(nonviolent_impact_by_year, by = "year")
cat("--- nonviolent: own score (=avg10t11 exactly) vs weight vs impact, by year ---\n")
print(as.data.frame(nonviolent_decomp), row.names = FALSE, digits = 3)
cat("cor(mean_own_score, mean_impact):", cor(nonviolent_decomp$mean_own_score, nonviolent_decomp$mean_impact), "\n")
cat("cor(mean_weight, mean_impact):", cor(nonviolent_decomp$mean_weight, nonviolent_decomp$mean_impact), "\n\n")

nonindig_impact_by_year <- loo_results %>% filter(Criterion == "nonindig") %>%
  group_by(year) %>% summarise(mean_impact = mean(Impact_sndem), .groups = "drop")
nonindig_decomp <- own_score_check(num_emel$nonindig, den_emel$nonindig, 1 - snvdem$el_Indigenous) %>%
  left_join(nonindig_impact_by_year, by = "year")
cat("--- nonindig: own score (=avg14 exactly) vs weight (1-el_Indigenous) vs impact, by year ---\n")
print(as.data.frame(nonindig_decomp), row.names = FALSE, digits = 3)
cat("2000-2017 own-score range:", round(range(nonindig_decomp$mean_own_score[nonindig_decomp$year <= 2017]), 3),
    " | weight range same period:", round(range(nonindig_decomp$mean_weight[nonindig_decomp$year <= 2017]), 3), "\n")
cat("2018-2023 own-score range:", round(range(nonindig_decomp$mean_own_score[nonindig_decomp$year >= 2018]), 3),
    " | weight range same period:", round(range(nonindig_decomp$mean_weight[nonindig_decomp$year >= 2018]), 3), "\n\n")

compete_own_emel_by_year <- data.frame(year = snvdem$year, own_emel = num_emel$compete / den_emel$compete) %>%
  group_by(year) %>%
  summarise(mean_own_emel = mean(own_emel, na.rm = TRUE))
cat("--- compete's own EMEL score by year (drives its LOO impact swings) ---\n")
print(as.data.frame(compete_own_emel_by_year), row.names = FALSE)

# 10. PCA validation check: is LOO influence just modernization theory in
#     disguise? (leave_one_out_memo.md Sec. 10) Recombine each hi/lo split
#     back into one continuous predictor first (avoids the artificial
#     collinearity a zero-filled split would introduce into a PCA), then
#     compare each criterion's LOO-magnitude rank to its |PC1 loading| rank ----
recombine <- function(hi, lo) ifelse(hi > 0, hi, lo)
pca_df <- data.frame(
  urban = recombine(snvdem$avg0t1hi, snvdem$avg0t1lo),
  econ_dev = recombine(snvdem$avg2t3hi, snvdem$avg2t3lo),
  prox_cap = recombine(snvdem$avg4t5hi, snvdem$avg4t5lo),
  north = snvdem$avg6, south = snvdem$avg7, west = snvdem$avg8, east = snvdem$avg9,
  nonviolent = snvdem$avg10t11, pop_density = snvdem$avg12, nonremote = snvdem$avg13,
  nonindig = snvdem$avg14, compete = recombine(snvdem$avg15t16hi, snvdem$avg15t16lo)
)
pca_complete <- pca_df[complete.cases(pca_df), ]
pca_fit <- prcomp(pca_complete, scale. = TRUE)
var_exp <- summary(pca_fit)$importance[2, ]
loadings_pc1 <- pca_fit$rotation[, 1]
pc1_scores <- pca_fit$x[, 1]
sndem_complete <- snvdem$sndem[complete.cases(pca_df)]

pca_rank <- data.frame(Criterion = names(loadings_pc1), abs_loading = abs(loadings_pc1)) %>%
  mutate(pca_rank = rank(-abs_loading))
loo_rank <- summary_tbl %>% mutate(loo_rank = rank(-abs(Mean_sndem))) %>% select(Criterion, Mean_sndem, loo_rank)
pca_comparison <- loo_rank %>% left_join(pca_rank, by = "Criterion") %>% arrange(loo_rank)

cat("\n--- PCA validation check ---\n")
cat("N complete rows:", nrow(pca_complete), "of", nrow(pca_df), "\n")
cat("Variance explained, PC1-PC4:", round(var_exp[1:4] * 100, 1), "%\n")
cat("cor(PC1, sndem):", cor(pc1_scores, sndem_complete, use = "complete.obs"), "\n")
cat("LOO-magnitude rank vs. |PC1 loading| rank:\n")
print(pca_comparison, row.names = FALSE)
cat("Spearman correlation between the two rankings:",
    cor(pca_comparison$loo_rank, pca_comparison$pca_rank, method = "spearman"), "\n\n")

# 10b. Robustness check: north/south/east/west are time-invariant (a
#      municipality's region never changes year to year), unlike the other
#      8 criteria which vary both between AND within municipalities over
#      time -- that structural difference could pull a pooled PCA in a
#      direction that has nothing to do with development. Confirm the
#      time-invariance, then rerun the PCA on the remaining 8 criteria. ----
nsew_within_sd <- snvdem %>%
  group_by(MPIO_CDPMP) %>%
  summarise(sd_north = sd(avg6, na.rm = TRUE), sd_south = sd(avg7, na.rm = TRUE),
            sd_west = sd(avg8, na.rm = TRUE), sd_east = sd(avg9, na.rm = TRUE))
cat("--- NSEW time-invariance check (max within-municipality SD across all 1,122 municipalities) ---\n")
cat("north:", max(nsew_within_sd$sd_north, na.rm = TRUE), " south:", max(nsew_within_sd$sd_south, na.rm = TRUE),
    " west:", max(nsew_within_sd$sd_west, na.rm = TRUE), " east:", max(nsew_within_sd$sd_east, na.rm = TRUE), "\n\n")

pca_df_nonsew <- pca_df %>% select(-north, -south, -west, -east)
pca_complete_nonsew <- pca_df_nonsew[complete.cases(pca_df_nonsew), ]
pca_fit_nonsew <- prcomp(pca_complete_nonsew, scale. = TRUE)
var_exp_nonsew <- summary(pca_fit_nonsew)$importance[2, ]
loadings_pc1_nonsew <- pca_fit_nonsew$rotation[, 1]
pc1_scores_nonsew <- pca_fit_nonsew$x[, 1]
sndem_complete_nonsew <- snvdem$sndem[complete.cases(pca_df_nonsew)]

pca_rank_nonsew <- data.frame(Criterion = names(loadings_pc1_nonsew), abs_loading = abs(loadings_pc1_nonsew)) %>%
  mutate(pca_rank = rank(-abs_loading))
loo_rank_nonsew <- summary_tbl %>% filter(!Criterion %in% c("north", "south", "west", "east")) %>%
  mutate(loo_rank = rank(-abs(Mean_sndem))) %>% select(Criterion, Mean_sndem, loo_rank)
pca_comparison_nonsew <- loo_rank_nonsew %>% left_join(pca_rank_nonsew, by = "Criterion") %>% arrange(loo_rank)

cat("--- PCA validation check, NSEW excluded (robustness check) ---\n")
cat("Variance explained, PC1-PC4:", round(var_exp_nonsew[1:4] * 100, 1), "%\n")
cat("cor(PC1, sndem):", cor(pc1_scores_nonsew, sndem_complete_nonsew, use = "complete.obs"), "\n")
cat("LOO-magnitude rank vs. |PC1 loading| rank (8 criteria):\n")
print(pca_comparison_nonsew, row.names = FALSE)
cat("Spearman correlation between the two rankings:",
    cor(pca_comparison_nonsew$loo_rank, pca_comparison_nonsew$pca_rank, method = "spearman"), "\n\n")

write.csv(pca_comparison_nonsew, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/leave_one_out_pca_check_noNSEW.csv", row.names = FALSE)

write.csv(pca_comparison, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/LOO/leave_one_out_pca_check.csv", row.names = FALSE)

cat("\nSaved:\n")
cat(" - leave_one_out_summary.csv (full LOO results table)\n")
cat(" - leave_one_out_standardized_check.csv (sign/scale robustness check)\n")
cat(" - leave_one_out_pca_check.csv (PCA validation check)\n")
cat(" - leave_one_out_pca_check_noNSEW.csv (PCA validation check, NSEW excluded)\n")
cat(" - imgs/leave_one_out_sndem.png (tornado, composite index)\n")
cat(" - imgs/leave_one_out_pillars.png (tornado, EMEL vs CSCW)\n")
cat(" - imgs/leave_one_out_time_trend.png (time-trend line chart, 3 criteria)\n")
