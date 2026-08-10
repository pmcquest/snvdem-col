# Validation: V-Dem coder comments (electoral fairness), current pipeline
#
# Successor to valid_com.R, whose input (outliers3.rds) no longer exists.
# Reads the rebuilt expert-comment panel from review/vdemcoder_review.R
# (expert_only_panel.rds) and the current pre-benchmark panel, and reproduces
# valid_com.R's published methodology (t-test, Cohen's d, correlation,
# boxplot) -- see VALIDATION_REFRESH_2026-08-09.md for the full before/after
# writeup and the manuscript/appendix text edits this feeds.

library(dplyr)
library(effsize)
library(broom)
library(gt)
library(ggplot2)

expert_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/scripts/Validation/VDem-com/review/expert_only_panel.rds")
snvdem    <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_weighting/03_output/snvdem_col_weighted.rds")

normalize_01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
snvdem <- snvdem %>%
  mutate(snelect_norm = normalize_01(snelect)) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP), year = as.numeric(year))

index_scores <- snvdem %>% select(MPIO_CDPMP, year, snelect_norm)

final_df <- expert_df %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  left_join(index_scores, by = c("MPIO_CDPMP", "year_num" = "year")) %>%
  filter(!is.na(snelect_norm)) %>%
  filter(Contradiction == 0) %>%
  mutate(moreless = ifelse(More == 1, "More", "Less"))

cat("Final matched obs (contradictions excluded):", nrow(final_df), "\n")
print(table(final_df$moreless))

# --- 1. T-test ----
t_result <- t.test(snelect_norm ~ moreless, data = final_df)
print(t_result)

validation_summary <- final_df %>%
  group_by(moreless) %>%
  summarise(
    obs_count = n(),
    avg_snelect = mean(snelect_norm, na.rm = TRUE),
    sd_snelect  = sd(snelect_norm, na.rm = TRUE)
  )
print(validation_summary)

# --- 2. Cohen's d ----
d_val <- cohen.d(snelect_norm ~ moreless, data = final_df, na.rm = TRUE)$estimate
cat("Cohen's d:", round(d_val, 3), "\n")

# --- 3. Correlation ----
cor_data <- final_df %>% mutate(more_dummy = ifelse(moreless == "More", 1, 0))
cor_snelect <- cor(cor_data$more_dummy, cor_data$snelect_norm, use = "complete.obs")
cat("Correlation (More dummy vs snelect_norm):", round(cor_snelect, 3), "\n")

# --- Table G.1: Welch Two-Sample T-Test Results ----
t_stats <- tidy(t_result)
table_g1_data <- validation_summary %>%
  mutate(
    t_stat = t_stats$statistic,
    df = t_stats$parameter,
    p_val = t_stats$p.value,
    d = d_val
  ) %>%
  select(moreless, obs_count, avg_snelect, sd_snelect, t_stat, df, p_val, d)

table_g1 <- table_g1_data %>%
  gt() %>%
  tab_header(
    title = "Table G.1. Welch Two-Sample T-Test Results",
    subtitle = "Comparing Electoral Democracy (snelect_norm) by Comments Group"
  ) %>%
  cols_label(
    moreless = "Group", obs_count = "N", avg_snelect = "Mean", sd_snelect = "SD",
    t_stat = "t", df = "df", p_val = "p", d = "Cohen's d"
  ) %>%
  fmt_number(columns = c(avg_snelect, sd_snelect, t_stat, df, d), decimals = 3) %>%
  text_transform(
    locations = cells_body(columns = p_val),
    fn = function(x) { val <- as.numeric(x); ifelse(val < 0.001, "< .001", sprintf("%.3f", val)) }
  ) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_options(table.border.top.color = "black", table.border.bottom.color = "black", table.font.size = px(14))

gtsave(table_g1, "G:/Shared drives/snvdem/snvdem-col/scripts/Validation/VDem-com/Table_G1_TTest_Results.rtf")

# --- Table G.2: Cohen's d summary ----
less_row <- validation_summary %>% filter(moreless == "Less")
more_row <- validation_summary %>% filter(moreless == "More")
table_g2_data <- tibble::tibble(
  Metric = c("N Obs. in comments", "Avg. sn index score", "Standard Deviation of obs. in comments", "Validation Strength"),
  `Less Free` = c(as.character(less_row$obs_count), sprintf("%.3f", less_row$avg_snelect), sprintf("%.3f", less_row$sd_snelect), "Diff. / SD (pooled)"),
  `More Free` = c(as.character(more_row$obs_count), sprintf("%.3f", more_row$avg_snelect), sprintf("%.3f", more_row$sd_snelect), ""),
  Difference  = c("—", sprintf("+%.3f", more_row$avg_snelect - less_row$avg_snelect), "No change", sprintf("Large (d = %.2f)", d_val))
)
table_g2 <- table_g2_data %>%
  gt() %>%
  tab_header(title = "Table G.2. Cohen's d Results") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_options(table.border.top.color = "black", table.border.bottom.color = "black", table.font.size = px(14))

gtsave(table_g2, "G:/Shared drives/snvdem/snvdem-col/scripts/Validation/VDem-com/Table_G2_CohensD_Results.rtf")

# --- Figure G.2: Box plot ----
p_boxplot <- ggplot(cor_data, aes(x = moreless, y = snelect_norm, fill = moreless)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("Less" = "#ef8a62", "More" = "#67a9cf")) +
  labs(
    title = "Validation: Distribution of snelect Index by Expert Assessment",
    subtitle = paste("Overall Correlation (r) =", round(cor_snelect, 3)),
    x = "Expert Qualitative Assessment",
    y = "Normalized snelect Index",
    caption = "Diamond represents the group mean."
  )
ggsave("G:/Shared drives/snvdem/snvdem-col/scripts/Validation/VDem-com/imgs/boxplot-moreless-2026.png",
       plot = p_boxplot, width = 7, height = 6, dpi = 300, bg = "white")

cat("\nDone. Tables and Figure G.2 saved to scripts/Validation/VDem-com/.\n")
