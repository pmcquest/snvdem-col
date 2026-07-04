#---- Step 5: Analyzing geolocated levels of democracy (disaggregated)

# Step 1: Wrangle raw data, clean it, then impute missing values (Folders 01-04)
# Step 2: Calculate averages of Empirical CDF data  (Folder 05)
# Step 3: Subset V-Dem data, calculate criteria averages, apply national range (Folder 06)
# Step 4: Interact Averaged CDF data by V-Dem data (Folders 05-06)
# Step 5 (this script): Analyze geolocated levels of democracy (Folder 09)

## ----Setup ----
library(tidyverse)
library(vdemdata)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readxl)
library(corrr)

# snvdem data----
# The "full_df" is an important dataset for understanding the individual criteria, their relationship to each other, and their relationship to other outcomes such as those related to development

full_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

# the df contains muncipal-year time-series data for Colombia between 2000-2020. The variables include 2 sets of criteria relevant for: electoral freeness and fairness, and civil liberties strength. Both dimensions are measured by the same criteria (#0-16) all of which are on a continuous scale from 0-1. They are standardized scores interacted with "relevance" scores from V-Dem survey questions. 

# Correlation matrix shows high correlations across criteria in both dimensions particularly 0_1, 2_3, and 4_5...
cor_matrix <- cor(full_df[,3:26], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")


# The task here is to measure which criteria (in either dimension) are correlated with variables related to different (non-criteria-related) outcomes such as health (e.g., infant mortality), education (e.g., drop-out rates), employment, or access to municipal services. I would like to see which criteria in full_df explain the most variation in these other variables.

# Outcomes----
panel_raw <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_raw.rds")
panel_imp <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")


# The code below is organized to compute both non-spatial correlation analysis and spatial correlation analysis, and then comparing their results in a table or graphic form. I have similar code for both multivariate regression analysis. 
# Then, I have code to compare results from both non-spatial causal analysis models (propensity score matching specifically) and explanatory spatial analysis (three models). 
# Note: * indicates not-recommended

# Non-spatial methods----
## Correlation analysis----
# identify linear relationships between variables
panel_avg <- panel_raw %>%
  select(-year) %>%
  group_by(MPIO_CDPMP) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)), .groups = 'drop')

# Merge with full_df
df_corr_combined <- panel_avg %>%
  inner_join(full_df, by = "MPIO_CDPMP") %>%
  select(-MPIO_CDPMP)
df_corr_final <- na.omit(df_corr_combined)

# --- Define the variable sets for visualization ---
full_df1 <- full_df
full_df_columns <- names(full_df1)
full_df_columns <- full_df_columns[full_df_columns != "MPIO_CDPMP"] 

panel_columns <- names(panel_avg)
panel_columns <- panel_columns[panel_columns != "MPIO_CDPMP"]

# Calculate the Cross-Correlation and Visualize
full_corr_matrix <- df_corr_final %>%
  correlate()
matrix_cols <- names(full_corr_matrix)
matrix_cols <- matrix_cols[matrix_cols != "term"]

target_cols <- intersect(full_df_columns, matrix_cols)

tidy_cross_correlations <- full_corr_matrix %>%
  filter(term %in% panel_columns) %>%
  select(term, all_of(target_cols)) %>%
  pivot_longer(
    cols = all_of(target_cols),
    names_to = "Full_DF_Variable",
    values_to = "Correlation"
  )
# Cross-Correlation Heatmap
ggplot(tidy_cross_correlations, aes(x = Full_DF_Variable, y = term, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", high = "blue", mid = "white", 
    midpoint = 0, limit = c(-1, 1), space = "Lab", 
    name = "Pearson\nCorrelation"
  ) +
  
  labs(
    title = "Correlation Heatmap: Dev't Vars. vs. SNVDEM results (2000-2020)",
    x = "SNVDEM results (dimension and criterion)",
    y = "Development vars. (Nat. Averages)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )



# 1. Identify the strongest 10 correlations (positive or negative)
top_10_correlations <- tidy_cross_correlations %>%
  mutate(Abs_Correlation = abs(Correlation)) %>%
  arrange(desc(Abs_Correlation)) %>%
  head(10) %>%
  select(term, Full_DF_Variable, Correlation) # Drop the absolute value helper column

library(knitr)
kable(top_10_correlations, 
      caption = "Top 10 Strongest Correlations (Absolute Value)",
      col.names = c("Development Variable", "SNVDEM Variable", "Correlation Coeff."))

## Multivariate regression analysis*----
# quantify effect of multiple criteria simultaneously on single outcome

### A. Pooled OLS*---- 
# issue: obs not necessarily independent

### B. FE----
# time invariante unobserved differences between municipalities that influence outcomes
# Hausman test (FE vs. RE)

### C. RE model*----
# assumes differences uncorrelated with criteria)



## Causal analysis----

### A. DiD*----
# requires control and treatment groups, intervention

### B. Granger causality test----
# determine if one time series can forecast another (e.g., past violence on future education) -- predictive causality

### C. Instrumental variable or 2SLS*----
# address endogeneity where criteria is correlated with error term

### D. Propensity score matching (PSM)----
# create control group in non-experimental settings: probability of a municipality having a certain criteria value, then match with similarly scored municipalities


# Spatial analysis ----

## Correlative spatial analysis (diagnostics)----
### A. Construct spatial weights matrix (W)----

### B. Testing for spatial autocorrelation
### Moran's I test (positive value = clustering)
### library(spdep)
## local indicators of spatial association (LISA)
## identify hot-spots of intense clustering


## Explanatory spatial analysis (modeling)----
# spatial econometrics model (extension of FE)
# library(splm)

### A. Spatial lag model (SAR)----
# outcome in one municipality influenced by neighbors (spillover or diffusion)

### B. Spatial error model (SEM)----
# correlation present in unobserved factors (error term)

### C. Spatial Durbin model (SDM)----
# general model: lagged depdendent variable and spatial lags of independent (criteria) variables

