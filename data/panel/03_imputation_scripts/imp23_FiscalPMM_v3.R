##----Predictive Mean Matching----
# For fiscal performance (#2-3), relatively few NAs. We need to calculate data for 2023.

library(mice)
library(tidyverse)
library(dplyr)
library(lattice)
library(caret)

#load df with "cleaned" data
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")
# Master list of municipalities and years
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/MunYrs.rds")




# 1. Prepare Data with Group Means (The "Fixed Effects" trick for Imputation)
# This captures the 'personality' of each town without using 1,125 factor levels
mice_input <- df_all %>%
  select(MPIO_CDPMP, year, IDF_2t3, IndRur_0t1, PIB_2t3, PobTot_12, DenPob_12) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(IDF_mean = mean(IDF_2t3, na.rm = TRUE)) %>%
  ungroup()

# 2. Configure MICE Predictor Matrix
# We initialize it to tell MICE: "Use these variables, but don't try to impute the IDs"
ini <- mice(mice_input, maxit = 0)
pred <- ini$predictorMatrix

# Turn off imputation for everything except IDF_2t3
# (And ensure MPIO_CDPMP is NOT used as a predictor, only the IDF_mean is)
pred[, "MPIO_CDPMP"] <- 0 
pred["MPIO_CDPMP", ] <- 0
pred["IDF_mean", ]   <- 0 

# 3. Run Imputation (CART)
# This will automatically fill the 2023 NAs based on the 2000-2022 trends and predictors
imputed_set <- mice(
  mice_input,
  m = 1,           # Set to 5 if you want to pool results, 1 for a single clean panel
  method = "cart",
  predictorMatrix = pred,
  seed = 42,
  printFlag = FALSE
)

# 4. Extract and Finalize
# We extract the data and immediately filter for your master panel list
imp23b <- complete(imputed_set) %>%
  select(MPIO_CDPMP, year, IDF_2t3) %>%
  # Ensure we only keep the 1,125 valid municipalities from your master list
  filter(MPIO_CDPMP %in% unique(MunYrs$MPIO_CDPMP))

# Validate results ----

# 1. Density Plot: Blue = Observed, Red = Imputed
# We use the 'imputed_set' object, not 'imp23b'
densityplot(imputed_set, ~IDF_2t3)

# 2. Stripplot: Check for outliers
# This shows the spread of the imputed values (red) vs observed (blue)
stripplot(imputed_set, IDF_2t3 ~ .imp, pch = 20, cex = 1.2)

# 3. XY Plot: Relationship between year and IDF_2t3
# This helps see if your CART method captured the time trend correctly
xyplot(imputed_set, IDF_2t3 ~ year)


# Save ----
summary(imp23b)
saveRDS(imp23b, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp23b.rds")
