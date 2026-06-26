# Combining weights and geocoded predictor variables
# Author: MC
# Revised by: PM (Jan 11, 2026)

library(tidyverse)
library(haven)


weights <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide.dta")
weights_col <- filter(weights, country_text_id == "COL" & year>1999)
# Average the weights for civil unrest and illicit activities
weights_col <- weights_col %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity)/2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity)/2)

# Which paired variables have high values that disfavor democracy?


predictors <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/v1/CDF_select.rds")
library(dplyr)

# Define the list of variables to remove
vars_to_remove <- c("axis_ns", "axis_we", "disp_ns", "disp_we", 
                    "Desp_1011", "VDays_1011", "HHomix_1011",  "PropEtn_14", "nAllRds_13", 
                    "lAllRds_13", "lMjRds_13", "nAR_13pkm", 
                    "lMjRds_13pkm", "road_density_km", "RulParD_15t16")

# Filter the dataframe
predictors_filtered <- predictors %>%
  select(-all_of(vars_to_remove))

# Verify the remaining columns
names(predictors_filtered)

predictors_filtered %>%
  dplyr::summarize(across(4:17, \(x) median(x, na.rm = TRUE)))

# The median is about 0.5 for all predictors except avg7 (South) and avg9 (East).
# Create hi and lo versions of each paired variable.


# 1. Define the predictors you want to split (everything except IDs and year)
cols_to_split <- c("IndRur_0t1", "PIB_2t3", "IDF_2t3", "ViolInd_1011", 
                   "DenPob_12", "DisMer_13", "lAR_13pkm", "RulPar_15t16", "DisBog_4t5")

# 2. Generate the Hi/Lo versions while retaining original suffixes
predict_hilo_granular <- predictors_filtered %>%
  mutate(
    # Create 'hi' versions: value if > 0.5, else 0
    across(all_of(cols_to_split), ~ ifelse(. > 0.5, ., 0), .names = "{.col}_hi"),
    # Create 'lo' versions: value if <= 0.5, else 0
    across(all_of(cols_to_split), ~ ifelse(. <= 0.5, ., 0), .names = "{.col}_lo")
  )

# Verify the new names
# You will see columns like PIB_2t3_hi and PIB_2t3_lo
colnames(predict_hilo_granular)

# Plotting using the new suffix-preserved names
ggplot(predict_hilo_granular, aes(x = IndRur_0t1_hi + IndRur_0t1_lo, 
                                  y = PIB_2t3_hi + PIB_2t3_lo)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relationship: Rurality (0t1) vs. GDP (2t3)",
       x = "IndRur_0t1 (Original Scale)",
       y = "PIB_2t3 (Original Scale)")

# Proceed cautiously
# Merge predictors and weights
Indices <- merge(weights_col, predict_hilo_granular, by = c("year"), all.y = TRUE)


# Constructing each index: FF elections
Indices_both <- Indices %>%
  mutate(
    sum_weights_el = el_Urban + el_Rural + wt_el_1011 + el_Sparse_population + el_Remote + 
      (1 - el_Indigenous) + (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak) +
      el_More_development + el_Less_development + (1 - el_Inside_capital) + 
      (1 - el_Outside_capital) + el_North + el_South + el_West + el_East,
    
    sum_weights_cl = cl_Urban + cl_Rural + wt_cl_1011 + cl_Sparse_population + cl_Remote + 
      (1 - cl_Indigenous) + (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak) +
      cl_More_development + cl_Less_development + (1 - cl_Inside_capital) + 
      (1 - cl_Outside_capital) + cl_North + cl_South + cl_West + cl_East,
    
    snelect = (
      (IndRur_0t1_hi * el_Urban + IndRur_0t1_lo * el_Rural) +
        (ViolInd_1011 * wt_el_1011) +
        (DenPob_12 * el_Sparse_population) +
        ((DisMer_13 + lAR_13pkm)/2 * el_Remote) +
        (PropInd_14 * (1 - el_Indigenous)) + 
        (RulPar_15t16_hi * (1 - el_Ruling_party_strong) + RulPar_15t16_lo * (1 - el_Ruling_party_weak)) +
        ((PIB_2t3_hi + IDF_2t3_hi)/2 * el_More_development + (PIB_2t3_lo + IDF_2t3_lo)/2 * el_Less_development) +
        (DisBog_4t5_hi * (1 - el_Inside_capital) + DisBog_4t5_lo * (1 - el_Outside_capital)) +
        (north6 * el_North + south7 * el_South + west8 * el_West + east9 * el_East)
    ) / sum_weights_el,
    
    sncivlib = (
      (IndRur_0t1_hi * cl_Urban + IndRur_0t1_lo * cl_Rural) +
        (ViolInd_1011 * wt_cl_1011) +
        (DenPob_12 * cl_Sparse_population) +
        ((DisMer_13 + lAR_13pkm)/2 * cl_Remote) +
        (PropInd_14 * (1 - cl_Indigenous)) +
        (RulPar_15t16_hi * (1 - cl_Ruling_party_strong) + RulPar_15t16_lo * (1 - cl_Ruling_party_weak)) +
        ((PIB_2t3_hi + IDF_2t3_hi)/2 * cl_More_development + (PIB_2t3_lo + IDF_2t3_lo)/2 * cl_Less_development) +
        (DisBog_4t5_hi * (1 - cl_Inside_capital) + DisBog_4t5_lo * (1 - cl_Outside_capital)) +
        (north6 * cl_North + south7 * cl_South + west8 * cl_West + east9 * cl_East)
    ) / sum_weights_cl
  ) %>%
  mutate(sndem = 0.5 * (snelect + sncivlib))

# Save
write_rds(Indices_both, "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/Indices_select.rds")



# Statistical table for the three main indices
index_summary <- Indices_both %>%
  select(snelect, sncivlib, sndem) %>%
  summary()

print(index_summary)

# Detailed standard deviations to check for dispersion
Indices_both %>%
  summarise(across(c(snelect, sncivlib, sndem), 
                   list(sd = ~sd(., na.rm = TRUE), 
                        iqr = ~IQR(., na.rm = TRUE))))


library(ggplot2)

Indices_both %>%
  select(year, snelect, sncivlib, sndem) %>%
  pivot_longer(cols = -year, names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Index)) +
  geom_density(alpha = 0.4) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Distribution of Granular SN-VDEM Indices",
       subtitle = "Comparing Electoral, Civil Liberties, and Combined Democracy Scores",
       x = "Index Value (0 to 1)",
       y = "Density")

# Summary by year
yearly_summary <- Indices_both %>%
  group_by(year) %>%
  summarise(across(c(snelect, sncivlib, sndem), mean, na.rm = TRUE))

# Plotting the trend
ggplot(yearly_summary, aes(x = year)) +
  geom_line(aes(y = snelect, color = "Electoral"), size = 1) +
  geom_line(aes(y = sncivlib, color = "Civil Liberties"), size = 1) +
  geom_line(aes(y = sndem, color = "Combined"), linetype = "dashed", size = 1.2) +
  theme_minimal() +
  labs(title = "National Trend of SN-VDEM Indices (2000-2023)",
       subtitle = "Mean scores across all municipalities",
       y = "Mean Index Value",
       color = "Metric")


# Correlation matrix: very highly correlated
cor_matrix <- cor(Indices_both[, c("snelect", "sncivlib", "sndem")], use = "complete.obs")
print(cor_matrix)


