# Validation: V-Dem coder comments

# How well does the textual data correlate to our quantitative measure?
# Because we can calculate how many unique experts "agreed about" one or another municipality as either more or less fair, we can calculated weighted correlations. We can also look at the cases on contradictions, where some experts mentioned a municipality as strong and another the same municipality as weak. The results show that our index is sensitive to these categories.

library(dplyr)
library(stringi)
library(ggplot2)
library(tidyr)
library(sf)
library(readxl)
library(effsize)
library(broom)
library(weights)
library(janitor)

# Load data ----

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")

# SNVDEM
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")

# Function for Min-Max Normalization
normalize_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply to your main dataframe
snvdem <- snvdem %>%
  mutate(
    snelect_norm = normalize_01(snelect),
    sncivlib_norm = normalize_01(sncivlib),
    sndem_norm   = normalize_01(sndem)
  )
# Verify the new range
summary(snvdem[c("snelect_norm", "sncivlib_norm", "sndem_norm")])


# --- 1. DATA PREPARATION ---
# Load V-Dem coder comments
expert_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/review/expert_only_panel.rds")


## Descriptive stats ----
# Summarize based on the original flags in expert_df
expert_observation_summary <- expert_df %>%
  mutate(Status = case_when(
    Contradiction == 1 ~ "Contradicted (Both)",
    More == 1          ~ "Clear More Free",
    Less == 1          ~ "Clear Less Free",
    TRUE               ~ "No Signal" 
  )) %>%
  group_by(Status) %>%
  summarise(
    Municipalities_Years = n(),
    Avg_Coders_Less = mean(n_coders_Less, na.rm = TRUE),
    Avg_Coders_More = mean(n_coders_More, na.rm = TRUE)
  ) %>%
  adorn_totals("row") 

print(expert_observation_summary)
#Status               Municipalities_Years Avg_Coders_Less Avg_Coders_More
#Clear Less Free                7060        1.190935               0
#Clear More Free                1033        0.000000               1
#Contradicted (Both)             393        1.000000               1
#Total                          8486        2.190935               2

# Gemini: "The expert validation dataset consists of 8,486 municipality-year observations. The sample is characterized by a high volume of identified deteriorations in democratic quality ($N=7,060$), a smaller set of clear improvements ($N=1,033$), and a group of high-complexity cases where experts provided conflicting signals ($N=393$). On average, 'Clear Less Free' signals were corroborated by 1.19 experts, providing a robust qualitative baseline for the $snelect$ index."


# 1. Create the summary by year and status
temporal_summary <- expert_df %>%
  mutate(Status = case_when(
    Contradiction == 1 ~ "Contradicted",
    More == 1          ~ "Clear More",
    Less == 1          ~ "Clear Less"
  )) %>%
  group_by(year_num, Status) %>%
  tally() %>%
  # Pivot so years are rows and categories are columns
  pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
  arrange(year_num)

# 2. View the table
print(temporal_summary)



# Prepare data for plotting
plot_data <- expert_df %>%
  mutate(Status = case_when(
    Contradiction == 1 ~ "Contradicted",
    More == 1          ~ "Clear More",
    Less == 1          ~ "Clear Less"
  ))

ggplot(plot_data, aes(x = factor(year_num), fill = Status)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = c("Clear More" = "#0571b0", 
                               "Clear Less" = "#ca0020", 
                               "Contradicted" = "#fdae61")) +
  labs(
    title = "Expert Qualitative Signals Over Time (2000-2023)",
    subtitle = "Observations used for Validation of the snelect Index",
    x = "Year",
    y = "Number of Municipalities Mentioned",
    fill = "Expert Consensus"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Optional: Export to CSV for your appendix
# write.csv(temporal_summary, "expert_signals_by_year.csv", row.names = FALSE)


# Count unique departments
total_depts <- expert_df %>%
  summarise(unique_depts = n_distinct(Dept_Name))

# Create a summary of mentions per department
dept_mention_summary <- expert_df %>%
  group_by(Dept_Name) %>%
  summarise(
    Total_Observations = n(),
    Years_Covered = n_distinct(year_num),
    Pct_Less = round(mean(Less) * 100, 1),
    Pct_More = round(mean(More) * 100, 1)
  ) %>%
  arrange(desc(Total_Observations))

print(paste("Total Unique Departments Mentioned:", total_depts))
print(head(dept_mention_summary, 10))

# Correlations ----
# Sync Index Scores
index_scores <- snvdem %>%
  select(MPIO_CDPMP, year, snelect_norm) %>%
  mutate(year_num = as.numeric(year))

validation_data <- expert_df %>%
  left_join(index_scores, by = c("MPIO_CDPMP", "year_num")) %>%
  filter(!is.na(snelect_norm)) %>%
  filter(Contradiction == 0) %>%
  mutate(
    group = ifelse(More == 1, "More", "Less"),
    group_binary = ifelse(group == "More", 1, 0),
    # WEIGHT: Sum of unique coders who mentioned this depto/year
    expert_weight = n_coders_Less + n_coders_More
  )

# --- UNWEIGHTED ANALYSIS (Standard)
# T-Test
unw_t <- t.test(snelect_norm ~ group, data = validation_data)
# Cohen's d
unw_d <- cohen.d(snelect_norm ~ group, data = validation_data)
# Correlation
unw_cor <- cor.test(validation_data$snelect_norm, validation_data$group_binary)

# --- WEIGHTED ANALYSIS (Consensus-Based) ---
# Weighted T-Test (via Linear Model)
w_model <- lm(snelect_norm ~ group, data = validation_data, weights = expert_weight)
w_summary <- summary(w_model) # Use summary to get Degrees of Freedom
w_t_stats <- tidy(w_model)

# Extract t-stat and df correctly
t_val <- w_t_stats$statistic[2]
df_val <- w_summary$df[2] # Corrected way to pull residual degrees of freedom

# Weighted Cohen's d formula: d = t * sqrt(1/n1 + 1/n2) 
# Approximation using t-statistic and df for large samples:
w_d_est <- 2 * t_val / sqrt(df_val)

# Weighted Correlation
w_cor_results <- wtd.cor(validation_data$snelect_norm, validation_data$group_binary, weight = validation_data$expert_weight)

# --- COMPARISON TABLE
results_comparison <- data.frame(
  Metric = c("T-Statistic", "P-Value", "Effect Size (Cohen's d)", "Correlation (r)"),
  
  Unweighted = c(
    round(unw_t$statistic, 3),
    format.pval(unw_t$p.value, digits = 3),
    round(unw_d$estimate, 3),
    round(unw_cor$estimate, 3)
  ),
  
  Weighted = c(
    round(t_val, 3),
    format.pval(w_t_stats$p.value[2], digits = 3),
    round(w_d_est, 3),
    round(w_cor_results[1], 3)
  )
)

print("--- VALIDATION COMPARISON ---")
print(results_comparison)
#Metric Unweighted Weighted
#1             T-Statistic    -30.565   27.381
#2                 P-Value     <2e-16   <2e-16
#3 Effect Size (Cohen's d)     -0.984    0.609
#4         Correlation (r)      0.312    0.291

# Gemini: "The $snelect$ index demonstrates robust concurrent validity across both unweighted and weighted specifications. The unweighted analysis shows a large effect size (Cohen's $d = -0.984$), indicating that the index clearly distinguishes between expert-defined categories. When weighting by coder consensus to account for expert agreement, the relationship remains highly significant ($t = 27.38, p < 0.001$) with a substantial effect size ($d = 0.609$). While the correlation ($r \approx 0.3$) indicates that the index and experts capture different nuances of the electoral process, the consistent significance across models confirms that the index reliably tracks the qualitative democratic trends identified by regional observers."


# Do contradictions have index scores in the middle?

# --- 1. PREPARE THE 3-GROUP DATASET ---
# We use the full expert_only_panel to include the contradictions
anova_data <- expert_df %>%
  left_join(index_scores, by = c("MPIO_CDPMP", "year_num")) %>%
  filter(!is.na(snelect_norm)) %>%
  mutate(Expert_Status = case_when(
    Contradiction == 1 ~ "3. Contradicted",
    More == 1          ~ "1. Clear More Free",
    Less == 1          ~ "2. Clear Less Free"
  ))

# --- 2. RUN THE ANOVA ---
# This tests: Is there ANY difference between these three groups?
res_anova <- aov(snelect_norm ~ Expert_Status, data = anova_data)
summary(res_anova)

# --- 3. RUN TUKEY HSD (POST-HOC) ---
# This tests: WHICH specific groups are different from each other?
tukey_results <- TukeyHSD(res_anova)
print(tukey_results)

# --- 4. VISUALIZE THE RESULTS ---
ggplot(anova_data, aes(x = Expert_Status, y = snelect_norm, fill = Expert_Status)) +
  geom_boxplot(notch = TRUE, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "ANOVA: Index Sensitivity to Expert Consensus",
    subtitle = "Comparing Clear vs. Ambiguous (Contradicted) Expert Signals",
    x = "Expert Status",
    y = "snelect_norm Score"
  ) +
  scale_fill_manual(values = c("1. Clear More Free" = "#0571b0", 
                               "2. Clear Less Free" = "#ca0020", 
                               "3. Contradicted" = "#fdae61")) +
  theme(legend.position = "none")

# Gemini: "The $snelect$ index demonstrates a high degree of sensitivity to expert consensus. A one-way ANOVA $[F(2, 8483) = 458.1, p < .001]$ confirmed significant differences across all expert categories. Crucially, post-hoc Tukey HSD tests revealed that municipalities with 'Contradicted' expert signals—where observers disagreed on the direction of democratic quality—yielded index scores $(M_{diff} = 0.06$ from Less; $M_{diff} = -0.07$ from More) that occupied a statistically distinct middle ground. This indicates that the quantitative index is capable of capturing the underlying political ambiguity identified by qualitative experts, providing strong evidence for its construct validity."



