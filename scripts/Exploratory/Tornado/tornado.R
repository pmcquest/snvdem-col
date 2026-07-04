# Tornado chart: relative influence of each criterion on the snvdem index
#
# "Influence" here is each criterion's WEIGHT SHARE in the snelect/sncivlib
# formula (07_weighting/01_weight_predictors.R), i.e. weight_i / sum(all weights),
# for a given pillar-year. This is a direct read of the weighted-average index
# formula, not a derived statistical model -- see FA23.R in ../02_FA for the
# separate (factor-analytic) take on "influence".
#
# Weights (el_*/cl_*) vary by year only, not by municipality, so "year by year"
# falls out of the same columns already in snvdem_col_weighted.rds.

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# 1. Load Data ----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_final_snvdem_data/snvdem_col_weighted.rds")

# 2. Collapse to one row per year (weights are constant across municipalities
#    within a year -- verify before trusting the collapse) ----
weight_cols <- c("el_Urban","el_Rural","el_More_development","el_Less_development",
                  "el_Inside_capital","el_Outside_capital","el_North","el_South",
                  "el_West","el_East","wt_el_1011","el_Sparse_population",
                  "el_Remote","el_Indigenous","el_Ruling_party_strong","el_Ruling_party_weak",
                  "cl_Urban","cl_Rural","cl_More_development","cl_Less_development",
                  "cl_Inside_capital","cl_Outside_capital","cl_North","cl_South",
                  "cl_West","cl_East","wt_cl_1011","cl_Sparse_population",
                  "cl_Remote","cl_Indigenous","cl_Ruling_party_strong","cl_Ruling_party_weak")

within_year_sd <- snvdem %>%
  group_by(year) %>%
  summarise(across(all_of(weight_cols), \(x) sd(x, na.rm = TRUE)), .groups = "drop")

max_sd <- max(as.matrix(within_year_sd[, -1]), na.rm = TRUE)
cat("Max within-year SD across all weight columns:", max_sd, "\n")
if (max_sd > 1e-8) {
  warning("Weights vary within a year across municipalities -- the year-level collapse below is not exact.")
}

weights_by_year <- snvdem %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, all_of(weight_cols))

# 3. Compute each criterion's weight (denominator contribution) per pillar-year ----
# Grouping mirrors exactly how el_den/cl_den are built in 01_weight_predictors.R,
# with north/south/west/east kept separate (per the FA23.R update).
weights_grouped <- weights_by_year %>%
  transmute(
    year,
    EMEL_urban       = el_Urban + el_Rural,
    EMEL_econ_dev    = el_More_development + el_Less_development,
    EMEL_prox_cap    = (1 - el_Inside_capital) + (1 - el_Outside_capital),
    EMEL_north       = el_North,
    EMEL_south       = el_South,
    EMEL_west        = el_West,
    EMEL_east        = el_East,
    EMEL_nonviolent  = wt_el_1011,
    EMEL_pop_density = el_Sparse_population,
    EMEL_nonremote   = el_Remote,
    EMEL_nonindig    = (1 - el_Indigenous),
    EMEL_compete     = (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak),

    CSCW_urban       = cl_Urban + cl_Rural,
    CSCW_econ_dev    = cl_More_development + cl_Less_development,
    CSCW_prox_cap    = (1 - cl_Inside_capital) + (1 - cl_Outside_capital),
    CSCW_north       = cl_North,
    CSCW_south       = cl_South,
    CSCW_west        = cl_West,
    CSCW_east        = cl_East,
    CSCW_nonviolent  = wt_cl_1011,
    CSCW_pop_density = cl_Sparse_population,
    CSCW_nonremote   = cl_Remote,
    CSCW_nonindig    = (1 - cl_Indigenous),
    CSCW_compete     = (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak)
  )

# 4. Pivot to long form and compute shares (should sum to 1 within pillar-year) ----
weights_long <- weights_grouped %>%
  pivot_longer(-year, names_to = c("Pillar", "Criterion"), names_pattern = "^(EMEL|CSCW)_(.*)$", values_to = "Weight") %>%
  group_by(year, Pillar) %>%
  mutate(Share = Weight / sum(Weight)) %>%
  ungroup()

# Sanity check: shares should sum to 1 for every pillar-year
share_check <- weights_long %>%
  group_by(year, Pillar) %>%
  summarise(total = sum(Share), .groups = "drop")
cat("\nShare-sum range (should be ~1.0):", range(share_check$total), "\n")

# 5. Overall Tornado (average share across all years) ----
overall_share <- weights_long %>%
  group_by(Pillar, Criterion) %>%
  summarise(Share = mean(Share), .groups = "drop")

overall_order <- overall_share %>%
  filter(Pillar == "EMEL") %>%
  arrange(Share) %>%
  pull(Criterion)

overall_share <- overall_share %>%
  mutate(Criterion = factor(Criterion, levels = overall_order))

p_overall <- ggplot(overall_share, aes(x = Criterion, y = Share, fill = Pillar)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(EMEL = "#2c7bb6", CSCW = "#d7191c")) +
  labs(title = "Relative Influence of Each Criterion on the SNVDEM Index",
       subtitle = "Average weight share, 2000-2023",
       x = NULL, y = "Share of pillar weight", fill = "Pillar") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/imgs/tornado_overall.png",
       p_overall, width = 8, height = 6, dpi = 150)

# 6. Year-by-Year: weight share trajectories ----
weights_long <- weights_long %>%
  mutate(Criterion = factor(Criterion, levels = overall_order))

p_trend <- ggplot(weights_long, aes(x = year, y = Share, color = Criterion)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Pillar, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(title = "Criterion Weight Share Over Time",
       subtitle = "By pillar, 2000-2023",
       x = "Year", y = "Share of pillar weight", color = "Criterion") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/imgs/tornado_trend.png",
       p_trend, width = 10, height = 8, dpi = 150)

# 7. Year-by-Year: faceted tornado for a representative subset of years ----
# (all 24 years as separate tornados is too dense to read; pick evenly spaced ones)
sample_years <- weights_long %>% distinct(year) %>% pull(year) %>%
  {.[round(seq(1, length(.), length.out = 6))]}

p_faceted <- weights_long %>%
  filter(year %in% sample_years) %>%
  ggplot(aes(x = Criterion, y = Share, fill = Pillar)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  facet_wrap(~year, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(EMEL = "#2c7bb6", CSCW = "#d7191c")) +
  labs(title = "Relative Influence of Each Criterion, Selected Years",
       x = NULL, y = "Share of pillar weight", fill = "Pillar") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 7))

ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/Tornado/imgs/tornado_faceted_years.png",
       p_faceted, width = 12, height = 8, dpi = 150)

# 8. Print overall table for reference ----
cat("\n--- Overall Weight Share by Criterion (sorted, EMEL) ---\n")
overall_share %>%
  pivot_wider(names_from = Pillar, values_from = Share) %>%
  arrange(desc(EMEL)) %>%
  mutate(across(where(is.numeric), ~sprintf("%.1f%%", . * 100))) %>%
  print(row.names = FALSE)

cat("\nSaved plots to Exploratory/Tornado/imgs/:\n")
cat(" - tornado_overall.png (single overall tornado, EMEL vs CSCW)\n")
cat(" - tornado_trend.png (weight share trajectories, 2000-2023)\n")
cat(" - tornado_faceted_years.png (tornado snapshots for 6 representative years)\n")
