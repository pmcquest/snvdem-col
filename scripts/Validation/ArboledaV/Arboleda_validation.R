# Validatiaon: Arboleda student survey

# Snvdem index
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/01_Expand/snvdem2.rds")

# Arboleda 
Arb <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/survey/Arb_final.rds")


#--> To validate, we keep in mind that survey respondents are asked to think historically and identify areas with any degree of specificity. We could plausibly assume that their guesses are not accurate to the year, or that they may remember standout or influential cases that do not reflect the specific scores we are calculating in our index. 
# To address this issue, first we can create a rolling 3-year average to capture these less-than-specific trends in our data. 
# We then run a linear probability model to control for respondent bias: "Holding the specific expert constant, does a higher score on the SNDEM index correlate with a positive survey response?"

library(dplyr)
library(zoo)
library(stringr)

# Clean and join data ----
# SNDEM Processing 
sndem_rolling <- snvdem %>%
  # Ensure MPIO_CDPMP is a 5-digit string (e.g., 5001 becomes 05001)
  mutate(MPIO_CDPMP = str_pad(as.character(as.numeric(MPIO_CDPMP)), 5, pad = "0"),
         year = as.numeric(year)) %>%
  mutate(
    sndem_norm    = (sndem - min(sndem, na.rm=T)) / (max(sndem, na.rm=T) - min(sndem, na.rm=T)),
    snelect_norm  = (snelect - min(snelect, na.rm=T)) / (max(snelect, na.rm=T) - min(snelect, na.rm=T)),
    sncivlib_norm = (sncivlib - min(sncivlib, na.rm=T)) / (max(sncivlib, na.rm=T) - min(sncivlib, na.rm=T))
  ) %>%
  group_by(MPIO_CDPMP) %>%
  arrange(year) %>%
  mutate(
    sndem_roll_norm    = rollapply(sndem_norm, width = 3, FUN = function(x) mean(x), align = "right", fill = NA, partial = TRUE),
    snelect_roll_norm  = rollapply(snelect_norm, width = 3, FUN = function(x) mean(x), align = "right", fill = NA, partial = TRUE),
    sncivlib_roll_norm = rollapply(sncivlib_norm, width = 3, FUN = function(x) mean(x), align = "right", fill = NA, partial = TRUE)
  ) %>%
  ungroup()


# Check total NAs in the rolling variable
sum(is.na(sndem_rolling$sndem_roll_norm))

# Compare NAs in the original vs the rolling
sndem_rolling %>%
  summarise(
    orig_na = sum(is.na(sndem_norm)),
    roll_na = sum(is.na(sndem_roll_norm))
  )


# 2. Arboleda Processing
# Reduce to binary variables (this will reduce the number of mun-year obs to around half)
arb_nominations <- Arb %>%
  # Crucial: Filter for non-NA BEFORE we try to string-pad to avoid errors
  filter(!is.na(Q_id), !is.na(MPIO_CDPMP), year >= 2000 & year <= 2023) %>% # we remove any obs. with NAs in Q_id or Municipalities--the expanded version does not
  mutate(
    # Use the same exact padding logic as above
    MPIO_CDPMP = str_pad(as.character(as.numeric(MPIO_CDPMP)), 5, pad = "0"),
    year = as.numeric(year),
    is_high_quality = ifelse(Q_id %in% c(2, 3), 1, 0)
  )

## The Validation Join ----
validation_set <- arb_nominations %>%
  inner_join(sndem_rolling, by = c("MPIO_CDPMP", "year"))


# Diagnostic Check
print(paste("Original nominations in Arb:", nrow(arb_nominations)))
print(paste("Total matches in validation_set:", nrow(validation_set)))


# Create the subset for diagnosis
diagnosis_df <- validation_set %>%
  # Select variables 1:12 and 82:90
  select(1:12, 82:90) %>%
  # Filter for rows where sndem_norm is missing
  filter(is.na(sndem_norm))

# View the result
print(diagnosis_df)

# Quick summary of the 'Lost' years and municipalities
table(diagnosis_df$year)
unique(diagnosis_df$municipio)

# Explore Arboleda data ----

# total count in the raw survey dataset (2000-2024 only)
total_raw_count <- Arb %>% 
  filter(year >= 2000 & year <= 2024) %>% 
  nrow()
print(paste("Total Observations in Arb Dataset (2000-2024):", total_raw_count))


## Visualize ----

library(ggplot2)

# 1. Filter raw nominations for the 2000-2024 window and summarize
time_series_data_full <- arb_nominations %>%
  filter(year >= 2000 & year <= 2023) %>%
  mutate(Quality = ifelse(is_high_quality == 1, "Strong", "Weak")) %>%
  group_by(year, Quality) %>%
  summarize(Mentions = n(), .groups = "drop")

# 2. Confirm the Total Count
total_mentions <- sum(time_series_data_full$Mentions)
print(paste("Total municipality-year mentions (2000-2023):", total_mentions))

# 3. Bar plot
pmentions <- ggplot(time_series_data_full, aes(x = factor(year), y = Mentions, fill = Quality)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  # Adding text labels on top of bars to show counts per category
  geom_text(aes(label = Mentions), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Strong" = "#2c7bb6", "Weak" = "#d7191c")) +
  theme_minimal() +
  labs(
    title = "Respondent Municipality Mentions Over Time (2000-2024)",
    subtitle = paste0("Total Mentions: ", total_mentions, " | Comparing 'Strong' vs. 'Weak' democratic mentions"),
    x = "Year",
    y = "Number of Municipalities Mentioned",
    fill = "Respondent Label"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/ArboledaV/Municipality_Mentions_Plot.png",
  plot = pmentions,
  width = 10,
  height = 6,
  dpi = 300,        # Essential for publication quality
  bg = "white"      # Ensures background isn't transparent
)

## Linear Probability model ----
library(modelsummary)
library(fixest)


# Model 1: Overall Index (Rolling)
# Tests if sustained democratic performance predicts expert opinion
mod_overall_roll <- feols(is_high_quality ~ sndem_roll_norm | Arb_id, data = validation_set)

# Model 2: Overall Index (Annual) 
# THE MAIN ROBUSTNESS CHECK - usually the strongest result
mod_overall_annual <- feols(is_high_quality ~ sndem_norm | Arb_id, data = validation_set)

# Model 3: Components (Annual Normalized)
# Tests which pillar (Elections vs Liberties) experts actually 'feel'
mod_comp_annual <- feols(is_high_quality ~ snelect_norm + sncivlib_norm | Arb_id, data = validation_set)

# Model 4: Components (Raw Asymmetric Scale)
# Tests the components without the 0-1 normalization stretching
mod_comp_raw <- feols(is_high_quality ~ snelect + sncivlib | Arb_id, data = validation_set)

# Compare results in console
etable(mod_overall_roll, mod_overall_annual, mod_comp_annual, mod_comp_raw, 
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1))

#--> The scores tend to predict a positive rating from these survey respondents. The logistic regression suggests that moving from a lower to a higher score on the SNVDEM index raises the probability by 34% that a respondent identifies a municipality with stronger or weaker democratic features (elections and civil liberties). 
# The regression results for the component measures are not statistically significant (respondents may be unable to distinguish between elections or civil liberties on their own).

#--> With year-to-year data, the probability increases to 1, statistical significance increases (p < 0.001), and the R^2 jumps to just over 0.10. (adjusted 0.55). In other words, a move from lowest to highest score in the SNVDEM index is associated with a 98% probability that the respondent from Arboleda positivelly assessed that municipality.
# Subcomponents remain statistically insignificant at the p = 0.10 level.

library(dplyr)

# Find rows in Arboleda that are NOT in sndem
missing_munis <- arb_nominations %>%
  anti_join(sndem_rolling, by = c("MPIO_CDPMP", "year")) %>%
  distinct(MPIO_CDPMP) # Get unique list of codes

print(missing_munis)

# Test if the relationship is better explained by a non-linear curve

# 1. Prepare Data: Calculate Quartiles & Median Split
validation_clean <- validation_set2 %>%
  filter(!is.na(sndem_norm)) %>%
  mutate(
    # Create 4 equal groups based on index score
    sndem_quartile = ntile(sndem_norm, 4),
    # Create a simple binary classification for the index
    index_high = ifelse(sndem_norm > median(sndem_norm, na.rm=TRUE), 1, 0)
  )

# 2. Non-Linear Factor Model
# Using i() creates a dummy variable for each quartile (except the first, as reference)
mod_nonlinear <- feols(is_high_quality ~ i(sndem_quartile) | Arb_id, 
                       data = validation_clean, 
                       cluster = ~Arb_id)

# Plotting the "S-Curve" of validation
iplot(mod_nonlinear, 
      main = "Effect of Index Quartiles on High-Quality Nominations",
      xlab = "SNDEM Index Quartile (1 = Lowest)",
      ylab = "Change in Prob. of High Quality Label")

# 3. Mismatch Matrix
mismatch_table <- table(
  Index_Quartile = validation_clean$sndem_quartile, 
  Expert_Label = validation_clean$is_high_quality
)

print("--- Mismatch Matrix: Index Quartiles vs. Expert Labels ---")
print(mismatch_table)

# 4. Accuracy Rate (Median Split)
mismatches <- validation_clean %>%
  count(is_mismatch = (index_high != is_high_quality)) %>%
  mutate(percent = n / sum(n) * 100)

print("--- Accuracy Rate (Median Split) ---")
print(mismatches)


## Export tables ----
library(modelsummary)

# 1. Update the list to reflect your strongest findings
list_of_models <- list(
  "Overall (Rolling)"  = mod_overall_roll,
  "Overall (Annual)"   = mod_overall_annual,
  "Components (Annual)" = mod_comp_annual
)

# 2. Define the file path (ensure the directory exists or adjust to a local path)
export_path <- "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/ArboledaV/SNDEM_Validation_Results2.docx"

# 3. Generate and Export Table
modelsummary(
  list_of_models,
  output = export_path,
  stars = TRUE,
  fmt = 3, # Round to 3 decimal places
  title = "Table 1: Validation of Subnational Democracy Index (SNDEM) against Respondent Nominations",
  # Map internal R names to professional labels for your thesis
  coef_map = c(
    "sndem_roll_norm"    = "SNDEM (3-Year Rolling Average)",
    "sndem_norm"         = "SNDEM (Annual Normalized Score)",
    "snelect_norm"       = "Electoral Component (Annual)",
    "sncivlib_norm"      = "Civil Liberties Component (Annual)"
  ),
  # Standardize Goodness of Fit statistics
  gof_omit = "AIC|BIC|Log.Lik|Std.Error",
  notes = "Note: All models are Linear Probability Models (LPM) with Respondent (Arb_id) Fixed Effects. Standard errors are clustered at the Respondent level."
)

print(paste("Table successfully exported to:", export_path))

## Plots ----
library(scales)

# Rebuild clean dataset to ensure it contains all 349 observations (pre-NA filter)
validation_clean2 <- validation_set2 %>%
  filter(!is.na(sndem_norm)) %>%
  mutate(
    # Bins for visualization
    index_bin = cut(sndem_norm, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE),
    label_name = ifelse(is_high_quality == 1, "High Quality", "Low Quality")
  )

# Summarize probability per bin
bin_data <- validation_clean2 %>%
  group_by(index_bin) %>%
  summarize(
    midpoint = mean(sndem_norm, na.rm = TRUE),
    prob_high = mean(is_high_quality, na.rm = TRUE),
    n = n()
  )

ggplot(bin_data, aes(x = midpoint, y = prob_high)) +
  # Red dashed line: The 0.978 coefficient (Robust Model)
  geom_abline(intercept = -0.06, slope = 0.978, color = "darkred", linetype = "dashed", size = 1) +
  # Points: Larger points mean more expert agreement in that score range
  geom_point(aes(size = n), color = "midnightblue", alpha = 0.8) +
  # Raw data jitter: Shows the 0s and 1s behind the percentages
  geom_jitter(data = validation_clean, aes(x = sndem_norm, y = is_high_quality), 
              height = 0.04, alpha = 0.1, size = 0.8, color = "gray0") +
  theme_minimal() +
  scale_y_continuous(labels = percent, limits = c(-0.05, 1.05)) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = "Validation: SNDEM Index vs. Respondent Consensus",
    subtitle = "The index effectively separates 'Low Quality' (Bottom Left) from 'High Quality' (Top Right)",
    x = "SNDEM Index (Normalized 0-1)",
    y = "Respondent Consensus (% High Quality Nominations)",
    size = "Number of Observations"
  )

ggplot(validation_clean2, aes(x = label_name, y = sndem_norm, fill = label_name)) +
  geom_violin(alpha = 0.3, color = NA) + # Shows the 'density' of scores
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.color = "red") +
  scale_fill_manual(values = c("High Quality" = "#2c7bb6", "Low Quality" = "#d7191c")) +
  theme_minimal() +
  labs(
    title = "Distribution of SNDEM Scores by Respondent Label",
    subtitle = "Higher index scores clearly correlate with 'High Quality' labels",
    x = "Respondent Nomination Category",
    y = "SNDEM Score (0-1)",
    fill = "Respondent Label"
  ) +
  theme(legend.position = "none")




# Maps ----

library(sf)
library(viridis)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

##Geospatial data----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# Outliers...
# 1. Identify "Index-Expert Mismatches"
outliers <- validation_clean2 %>%
  mutate(
    type = case_when(
      sndem_norm > 0.7 & is_high_quality == 0 ~ "Over-performer (Index High, Expert Low)",
      sndem_norm < 0.3 & is_high_quality == 1 ~ "Under-performer (Index Low, Expert High)",
      TRUE ~ "Consistent"
    )
  ) %>%
  filter(type != "Consistent") %>%
  select(year, MPIO_CDPMP, type, sndem_norm, is_high_quality)

# 2. Join with municipality names for readability

outlier_report %>% 
  arrange(desc(sndem_norm)) %>%
  select(year, MPIO_CNMBR, type, sndem_norm, is_high_quality) %>%
  print(n = 30)

# 1. Group by Municipality first, then Year
outlier_grouped <- outlier_report %>%
  arrange(MPIO_CNMBR, year) %>%
  select(MPIO_CNMBR, year, sndem_norm, is_high_quality, type)

# 2. View the grouped result
print(outlier_grouped, n = 30)

# 3. Export this version
write.csv(outlier_grouped, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/ArboledaV/SNDEM_Outliers.csv", row.names = FALSE)

# 4. Optional: Export a clean version to Word using modelsummary/flextable
library(flextable)
save_as_docx(flextable(outlier_sorted), path = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/ArboledaV/SNDEM_Outlier_Table_Final.docx")


# 1. Aggregate mentions across the entire study period
cumulative_mentions <- validation_set %>%
  group_by(MPIO_CDPMP) %>%
  summarize(
    times_mentioned = n(),
    avg_quality_label = mean(is_high_quality, na.rm = TRUE)
  )

# 2. Join with your municipality shapefile
# Ensure col_mpios is your sf object
all_time_map <- muni_geo %>%
  left_join(cumulative_mentions, by = "MPIO_CDPMP")

# 3. Create the Density Map
ggplot(data = all_time_map) +
  # Base layer: The "Invisible Colombia" (No mentions)
  geom_sf(fill = "gray90", color = "white", size = 0.05) +
  # Expert Layer: Color by frequency of mentions
  geom_sf(aes(fill = times_mentioned), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis", 
    direction = -1, 
    na.value = "gray90",
    name = "Total Mentions\n(2006-2022)"
  ) +
  theme_void() +
  labs(
    title = "Respondent Coverage",
    subtitle = "Cumulative respondent mentions from the Arboleda Survey",
    caption = "Gray areas represent municipalities with zero expert mentions."
  ) +
  theme(legend.position = "right")


# By category
# 1. Aggregate mentions by Category
mentions_by_type <- validation_set %>%
  filter(!is.na(is_high_quality)) %>%
  mutate(Type = ifelse(is_high_quality == 1, "High Quality Nominations", "Low Quality Nominations")) %>%
  group_by(MPIO_CDPMP, Type) %>%
  summarize(count = n(), .groups = "drop")

# 2. Join with Shapefile
# We use 'complete' to ensure every municipality exists for both "High" and "Low" categories in the plot
map_data_split <- muni_geo %>%
  left_join(mentions_by_type, by = "MPIO_CDPMP") %>%
  filter(!is.na(Type)) # Keep only municipalities mentioned at least once

ggplot() +
  # Background: All of Colombia in light gray
  geom_sf(data = muni_geo, fill = "gray95", color = "white", size = 0.05) +
  # Foreground: The mentioned municipalities
  geom_sf(data = map_data_split, aes(fill = count), color = "white", size = 0.1) +
  # Facet by Type (Strong vs Weak)
  facet_wrap(~Type) +
  # Use a clear color scale
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Total Mentions") +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  labs(
    title = "Geographic Distribution of Respondent Mentions",
    subtitle = "Comparing where respondents perceive 'Strong' vs. 'Weak' Democratic Quality"
  )
# Mentioned municipalities ----
# To test whether survey respondents are mentioning the more extreme cases in the SNVDEM data, we can compare the ones they mentioned to those they didn't. We can then assess whether respondents nominate a representative sample or if they are noticing the more extreme cases (threshold effects).

library(ggplot2)

# 1. Create a list of unique mentioned municipality-years from Arb
mentioned_cases <- Arb %>%
  filter(!is.na(MPIO_CDPMP), !is.na(year)) %>%
  mutate(MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"),
         year = as.numeric(year)) %>%
  select(MPIO_CDPMP, year) %>%
  distinct() %>%
  mutate(is_mentioned = 1)

# 2. Join this back to the FULL sndem dataset
selection_bias_df <- sndem %>%
  mutate(MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"),
         year = as.numeric(year)) %>%
  left_join(mentioned_cases, by = c("MPIO_CDPMP", "year")) %>%
  mutate(is_mentioned = ifelse(is.na(is_mentioned), 0, 1))

# Respondent bias: do respondents choose non-representative municipalities?
# Compare the mean sndem score of mentioned vs. non-mentioned cases
bias_test <- t.test(sndem ~ is_mentioned, data = selection_bias_df)
print(bias_test)

#--> Results suggest that respondents are 3.2% more likely to mention high-performing municipalities rather than low-performing ones. Results are statistically significant (p < -.001). This aligns with the bias presumably among respondents from major cities (e.g., Bogota or Medellin) who are not familiar with remote low-performing or data-scarce municipalities.

ggplot(selection_bias_df, aes(x = sndem, fill = as.factor(is_mentioned))) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = mean(sndem[is_mentioned == 0], na.rm=T)), color = "gray30", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(sndem[is_mentioned == 1], na.rm=T)), color = "red", linetype = "dashed") +
  scale_fill_manual(values = c("0" = "gray70", "1" = "red"), 
                    labels = c("0" = "National Baseline", "1" = "Respondent Mentioned")) +
  theme_minimal() +
  labs(title = "Expert Selection Bias: Visibility vs. Reality",
       subtitle = "Respondents tend to mention municipalities with slightly higher SNDEM scores.",
       x = "SNDEM Index Score",
       y = "Density",
       fill = "Group")

#--> Plot shows that respondents are mentioning more higher-performing municipalities on average than lower ones. 

## Remoteness bias----
nominated_keys <- Arb %>%
  filter(!is.na(MPIO_CDPMP), !is.na(year)) %>%
  mutate(
    MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"),
    year = as.numeric(year)
  ) %>%
  select(MPIO_CDPMP, year) %>%
  distinct() %>%
  mutate(is_mentioned = 1)

nomination_bias_df <- sndem %>%
  mutate(
    MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"),
    year = as.numeric(year),
    # Feature Engineering: Creating the Inverse Scores
    remoteness = 1 - avg12,  # High = More Remote
    sparsity   = 1 - avg13,   # High = More Sparse
    prox_bogota = avg4t5      # High = Closer to Bogota
  ) %>%
  left_join(nominated_keys, by = c("MPIO_CDPMP", "year")) %>%
  mutate(is_mentioned = ifelse(is.na(is_mentioned), 0, 1))

print(table(nomination_bias_df$is_mentioned, useNA = "always"))
# Very few nominations overall

active_years <- nomination_bias_df %>%
  group_by(year) %>%
  summarize(n = sum(is_mentioned)) %>%
  filter(n > 0) %>%
  pull(year)

mod_visibility <- feols(is_mentioned ~ sndem + prox_bogota + remoteness + sparsity | year, 
                        data = filter(nomination_bias_df, year %in% active_years))

summary(mod_visibility)

# Bias: Distinguishing between components ----
