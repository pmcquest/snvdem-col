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
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_weighted.rds")

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

# 3. Full index (verify against saved snelect/sncivlib before trusting LOO) ----
total_num_emel <- rowSums(num_emel)
total_den_emel <- rowSums(den_emel)
total_num_cscw <- rowSums(num_cscw)
total_den_cscw <- rowSums(den_cscw)

snelect_recon <- total_num_emel / total_den_emel
sncivlib_recon <- total_num_cscw / total_den_cscw
sndem_recon <- 0.5 * (snelect_recon + sncivlib_recon)

cat("Reconstruction check (max abs diff vs. saved columns):\n")
cat("  snelect: ", max(abs(snelect_recon - snvdem$snelect), na.rm = TRUE), "\n")
cat("  sncivlib:", max(abs(sncivlib_recon - snvdem$sncivlib), na.rm = TRUE), "\n")
cat("  sndem:   ", max(abs(sndem_recon - snvdem$sndem), na.rm = TRUE), "\n\n")

# 4. Leave-one-out: drop each criterion, recompute, take the difference ----
loo_results <- lapply(criteria, function(cn) {
  loo_snelect <- (total_num_emel - num_emel[[cn]]) / (total_den_emel - den_emel[[cn]])
  loo_sncivlib <- (total_num_cscw - num_cscw[[cn]]) / (total_den_cscw - den_cscw[[cn]])
  loo_sndem <- 0.5 * (loo_snelect + loo_sncivlib)

  data.frame(
    Criterion = cn,
    year = snvdem$year,
    Impact_EMEL = loo_snelect - snelect_recon,
    Impact_CSCW = loo_sncivlib - sncivlib_recon,
    Impact_sndem = loo_sndem - sndem_recon
  )
}) %>% bind_rows()

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

write.csv(summary_tbl, "G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/leave_one_out_summary.csv", row.names = FALSE)

# 6. Tornado figure: sndem (the composite index), sorted by median impact ----
order_sndem <- summary_tbl %>% arrange(Median_sndem) %>% pull(Criterion)

p_sndem <- loo_results %>%
  mutate(Criterion = factor(Criterion, levels = order_sndem)) %>%
  ggplot(aes(x = Criterion, y = Impact_sndem)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, fill = "#7570b3", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  coord_flip() +
  labs(title = "Change in the SNVDEM Index When Each Criterion Is Excluded",
       subtitle = "Distribution across all municipality-years, 2000-2023",
       x = NULL, y = "Change in index (excluded - full)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/imgs/leave_one_out_sndem.png",
       p_sndem, width = 9, height = 6, dpi = 150)

# 7. Tornado figure: EMEL vs. CSCW mean impact, sorted ----
order_pillar <- summary_tbl %>% arrange(pmax(abs(Mean_EMEL), abs(Mean_CSCW))) %>% pull(Criterion)

p_pillar <- summary_tbl %>%
  select(Criterion, Mean_EMEL, Mean_CSCW) %>%
  pivot_longer(-Criterion, names_to = "Pillar", values_to = "Mean_Impact") %>%
  mutate(Criterion = factor(Criterion, levels = order_pillar),
         Pillar = sub("Mean_", "", Pillar)) %>%
  ggplot(aes(x = Criterion, y = Mean_Impact, fill = Pillar)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  coord_flip() +
  scale_fill_manual(values = c(EMEL = "#2c7bb6", CSCW = "#d7191c")) +
  labs(title = "Mean Change in Each Pillar When Criterion Is Excluded",
       x = NULL, y = "Mean change in index (excluded - full)", fill = "Pillar") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/imgs/leave_one_out_pillars.png",
       p_pillar, width = 9, height = 6, dpi = 150)

cat("\nSaved:\n")
cat(" - leave_one_out_summary.csv (full table)\n")
cat(" - imgs/leave_one_out_sndem.png (tornado, composite index)\n")
cat(" - imgs/leave_one_out_pillars.png (tornado, EMEL vs CSCW)\n")
