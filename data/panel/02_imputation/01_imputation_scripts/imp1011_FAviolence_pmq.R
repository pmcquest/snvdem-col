# Creating factor scores for violence data
# (updated 12/2025)

library(readr)
library(haven)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(psych)
library(Hmisc)


# Load data

viol <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/imp1011.rds")

names(viol)
summary(viol)

# Clean and Transform
viol_clean <- viol %>%
  # Handle zeros by adding 1 before logging
  mutate(
    log_Desp = log10(Desp_1011 + 1),
    log_Homi = log10(HHomix_1011 + 1),
    log_VDays = log10(VDays_1011 + 1)
  ) %>%
  # FA cannot handle NAs; we filter them out for the analysis subset
  filter(!is.na(log_Desp), !is.na(log_Homi), !is.na(log_VDays))

# Check distributions: not normally distributed, but right-skewed. Use Principal Axis factoring
hist(viol_clean$log_Desp)
hist(viol_clean$log_Homi)
hist(viol_clean$log_VDays)

# Select only the logged columns for correlation
cor_data <- viol_clean %>% select(log_Desp, log_Homi, log_VDays)

# Get r and p-values
cor_results <- rcorr(as.matrix(cor_data))
print(cor_results$r) # Coefficients: between 0.52 - 0.72
print(cor_results$P) # P-values: <0.05


# 1. Check KMO (Measure of Sampling Adequacy)
# A value > 0.6 is required. We see between 0.66-0.80
kmo_result <- KMO(cor_data)
print(kmo_result)

# 2. Run the Factor Analysis
# We use 'pa' for non-normal data and 'tenBerge' for scores to handle skewness better
fa_viol <- fa(cor_data, nfactors = 1, fm = "pa", scores = "tenBerge")

# 3. Inspect the results: High loadings (0.646-0.888) and 0.623 proportional variance explained
print(fa_viol$loadings, cutoff = 0.3)


viol_FA <- viol_clean %>%
  mutate(ViolInd_1011 = as.numeric(fa_viol$scores)) %>%
  # Keep only identifiers and the original three variables as requested
  select(MPIO_CDPMP, year, Desp_1011, VDays_1011, HHomix_1011, ViolInd_1011)



# Check the range of the new index
summary(viol_FA$ViolInd_1011)

# Save factor data (will standardize later: 02_imputation/03_merge_imputed/01_merge_imputed.R)
write_rds(viol_FA, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/imp1011FA.rds")

# Visualizations ----
## Correlations
# Create a subset of the variables of interest
vars_to_cor <- viol_FA %>% 
  select(ViolInd_1011, Desp_1011, VDays_1011, HHomix_1011)

# Calculate Spearman correlation matrix
cor_matrix <- cor(vars_to_cor, method = "spearman", use = "complete.obs")

# Print the matrix rounded for readability
print(round(cor_matrix, 3))

library(corrplot)

# Plot the matrix
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Add the correlation coefficient text
         tl.col = "black",      # Text label color
         diag = FALSE,          # Don't show the diagonal (correlation with itself)
         title = "Spearman Correlation: Violence Index vs. Original Variables",
         mar = c(0,0,1,0))


library(GGally)

# Create a pairs plot
ggpairs(viol_FA, 
        columns = c("ViolInd_1011", "Desp_1011", "VDays_1011", "HHomix_1011"),
        upper = list(continuous = wrap("cor", method = "spearman", color = "blue")),
        lower = list(continuous = wrap("points", alpha = 0.1, size = 0.5))) +
  theme_minimal() +
  labs(title = "Bivariate Relationships and Spearman Correlations")
