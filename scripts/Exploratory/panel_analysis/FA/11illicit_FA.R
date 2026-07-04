#---- Step 2: Factor analysis of Illicit activity ----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2 (this script): Data reduction (calculate factor scores)
# Step 3: merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel")

library(tidyverse)
library(knitr)
library(psych)
library(psychTools)
library(GPArotation)
library(kableExtra)
library(dplyr)
library(tidyr)
library(corrplot)
library(reshape2)

# Load the data
colvars_cdf <- read_rds("final_data/colvars_cdf.rds")
colnames(colvars_cdf)


# ---- Factor Analysis (FA) ----
## ---- Pooled FA ----
# Run Factor Analysis on selected variables
fa_11 <- fa(colvars_cdf[, c(20:21, 23:26)], nfactors = 3, rotate = "varimax", scores = "regression", fm = "ml")


# View the factor loadings
fa_11$loadings

fa.diagram(fa_pooled_vm)



### ----- Export factor loadings -----

fa_11[["Structure"]]


library(psych)


# View the factor loadings
print(fa_11$loadings, cutoff = 0.3) #cutoff removes small loadings.

# View the factor scores
factor_scores <- fa_11$scores

# Create the index (unweighted average)
illicit_index <- rowMeans(factor_scores)

# Add the index to your data frame
colvars_cdf$illicit_index <- illicit_index

# Validation (example: correlation with another variable)
print(cor(colvars_cdf$illicit_index, colvars_cdf$some_other_variable, use = "complete.obs"))

# Optional: Weighted average
weights <- fa_11$Vaccounted[2,] #use proportion variance explained as weights.
weighted_index <- rowSums(factor_scores * weights)
colvars_cdf$weighted_index <- weighted_index

summary(colvars_cdf$illicit_index)
summary(colvars_cdf$weighted_index)

#----Normalize and merge----

# Normalize illicit_index using CDF
colvars_cdf <- colvars_cdf %>%
  mutate(
    IAindex_11 = ecdf(illicit_index)(illicit_index)
  )

# Normalize weighted_index using CDF
colvars_cdf <- colvars_cdf %>%
  mutate(
    IAwavg_11 = ecdf(weighted_index)(weighted_index)
  )

# Check the summaries of the normalized variables
summary(colvars_cdf$IAindex_11)
summary(colvars_cdf$IAwavg_11)

# write the new cdf
write_rds(colvars_cdf, "G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/colvars_cdf2.rds")

