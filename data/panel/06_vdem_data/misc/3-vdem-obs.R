#---- Step 3: V-Dem data and weighting (means)----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2: Data reduction (calculate factor scores)
# Step 3 (this script): merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel")

## ----Setup ----
# load needed packages
library(tidyverse)
library(vdemdata)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


# V-Dem data for Colombia (2000-2020) -- from script 3a-vdem
col_weights <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/vdem-col0020.rds")

# observational data for Colombia (2000-2020): CDF
colvars_cdf <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/colvars_cdf.rds")

colvars_cdfw <- colvars_cdf[, c(1:6, # year and categorical data
                                16:17, #0-1, 2-3a
                                27, #2-3b
                                7, #4-5
                                8:11, #6-9 
                                18:19, #10
                                20, 22:25, #11
                                28, #12
                                33, 38, #13
                                29, #14
                                32)] # 15-16
colvars_cdfw <- colvars_cdfw %>%
  rename(north_6 = 11, south_7 = 12, west_8 = 13, east_9 = 14) %>%
  rename(lAR_13 = 24) %>%
  mutate(lAR_13 = 1-lAR_13) # invert (high = less road density) to take average with distance to market (high = remote)


# 1. Apply Weights Directly
library(dplyr)

# Assuming col_weights and colvars_cdfw are already loaded

# 1. Calculate Averages for Criteria with Multiple Variables
averaged_data <- colvars_cdfw %>%
  mutate(
    avg_2t3 = rowMeans(select(., PIB_2t3, IDF_2t3), na.rm = TRUE),
    avg_10 = rowMeans(select(., Desp_10, Errad_10), na.rm = TRUE),
    avg_11 = rowMeans(select(., Hurto_11, HHomi_11, HDesa_11, HSecu_11, HRecl_11), na.rm = TRUE),
    avg_13 = rowMeans(select(., DisMer_13, lAR_13), na.rm = TRUE)
  ) %>%
  select(-PIB_2t3, -IDF_2t3, -Desp_10, -Errad_10, -Hurto_11, -HHomi_11, -HDesa_11, -HSecu_11, -HRecl_11, -DisMer_13, -lAR_13)

# 2. Weight the Averages Using col_weights
weighted_data <- averaged_data %>%
  left_join(col_weights, by = "year") %>%
  mutate(
    # Elections Weights
    lfc_0t1_weighted = IndRur_0t1 * rowMeans(select(., v2elsnlfc_0, v2elsnlfc_1), na.rm = TRUE),
    lfc_2t3_weighted = avg_2t3 * rowMeans(select(., v2elsnlfc_2, v2elsnlfc_3), na.rm = TRUE),
    lfc_4t5_weighted = DisBog_4t5 * rowMeans(select(., v2elsnlfc_4, v2elsnlfc_5), na.rm = TRUE),
    lfc_6_weighted = north_6 * v2elsnlfc_6,
    lfc_7_weighted = south_7 * v2elsnlfc_7,
    lfc_8_weighted = west_8 * v2elsnlfc_8,
    lfc_9_weighted = east_9 * v2elsnlfc_9,
    lfc_10_weighted = avg_10 * v2elsnlfc_10,
    lfc_11_weighted = avg_11 * v2elsnlfc_11,
    lfc_12_weighted = DenPob_12 * v2elsnlfc_12,
    lfc_13_weighted = avg_13 * v2elsnlfc_13,
    lfc_14_weighted = PobInd_14 * v2elsnlfc_14,
    lfc_15_weighted = RulParD_15t16 * v2elsnlfc_15,
    
    mrfc_0t1_weighted = IndRur_0t1 * rowMeans(select(., v2elsnmrfc_0, v2elsnmrfc_1), na.rm = TRUE),
    mrfc_2t3_weighted = avg_2t3 * rowMeans(select(., v2elsnmrfc_2, v2elsnmrfc_3), na.rm = TRUE),
    mrfc_4t5_weighted = DisBog_4t5 * rowMeans(select(., v2elsnmrfc_4, v2elsnmrfc_5), na.rm = TRUE),
    mrfc_6_weighted = north_6 * v2elsnmrfc_6,
    mrfc_7_weighted = south_7 * v2elsnmrfc_7,
    mrfc_8_weighted = west_8 * v2elsnmrfc_8,
    mrfc_9_weighted = east_9 * v2elsnmrfc_9,
    mrfc_10_weighted = avg_10 * v2elsnmrfc_10,
    mrfc_11_weighted = avg_11 * v2elsnmrfc_11,
    mrfc_12_weighted = DenPob_12 * v2elsnmrfc_12,
    mrfc_13_weighted = avg_13 * v2elsnmrfc_13,
    mrfc_14_weighted = PobInd_14 * v2elsnmrfc_14,
    mrfc_15_weighted = RulParD_15t16 * v2elsnmrfc_15,
    
    # Civil Liberties Weights
    cw_0t1_weighted = IndRur_0t1 * rowMeans(select(., v2clrgwkch_0, v2clrgwkch_1), na.rm = TRUE),
    cw_2t3_weighted = avg_2t3 * rowMeans(select(., v2clrgwkch_2, v2clrgwkch_3), na.rm = TRUE),
    cw_4t5_weighted = DisBog_4t5 * rowMeans(select(., v2clrgwkch_4, v2clrgwkch_5), na.rm = TRUE),
    cw_6_weighted = north_6 * v2clrgwkch_6,
    cw_7_weighted = south_7 * v2clrgwkch_7,
    cw_8_weighted = west_8 * v2clrgwkch_8,
    cw_9_weighted = east_9 * v2clrgwkch_9,
    cw_10_weighted = avg_10 * v2clrgwkch_10,
    cw_11_weighted = avg_11 * v2clrgwkch_11,
    cw_12_weighted = DenPob_12 * v2clrgwkch_12,
    cw_13_weighted = avg_13 * v2clrgwkch_13,
    cw_14_weighted = PobInd_14 * v2clrgwkch_14,
    cw_15_weighted = RulParD_15t16 * v2clrgwkch_15,
    
    cs_0t1_weighted = IndRur_0t1 * rowMeans(select(., v2clrgstch_0, v2clrgstch_1), na.rm = TRUE),
    cs_2t3_weighted = avg_2t3 * rowMeans(select(., v2clrgstch_2, v2clrgstch_3), na.rm = TRUE),
    cs_4t5_weighted = DisBog_4t5 * rowMeans(select(., v2clrgstch_4, v2clrgstch_5), na.rm = TRUE),
    cs_6_weighted = north_6 * v2clrgstch_6,
    cs_7_weighted = south_7 * v2clrgstch_7,
    cs_8_weighted = west_8 * v2clrgstch_8,
    cs_9_weighted = east_9 * v2clrgstch_9,
    cs_10_weighted = avg_10 * v2clrgstch_10,
    cs_11_weighted = avg_11 * v2clrgstch_11,
    cs_12_weighted = DenPob_12 * v2clrgstch_12,
    cs_13_weighted = avg_13 * v2clrgstch_13,
    cs_14_weighted = PobInd_14 * v2clrgstch_14,
    cs_15_weighted = RulParD_15t16 * v2clrgstch_15
  )

# 3. Combine Paired Sets and Calculate Scores
colwtd <- weighted_data %>%
  rowwise() %>%
  mutate(
    emel_score = mean(c(lfc_0t1_weighted, lfc_2t3_weighted, lfc_4t5_weighted, lfc_6_weighted, lfc_7_weighted, lfc_8_weighted, lfc_9_weighted, lfc_10_weighted, lfc_11_weighted, lfc_12_weighted, lfc_13_weighted, lfc_14_weighted, lfc_15_weighted, mrfc_0t1_weighted, mrfc_2t3_weighted, mrfc_4t5_weighted, mrfc_6_weighted, mrfc_7_weighted, mrfc_8_weighted, mrfc_9_weighted, mrfc_10_weighted, mrfc_11_weighted, mrfc_12_weighted, mrfc_13_weighted, mrfc_14_weighted, mrfc_15_weighted), na.rm = TRUE),
    cscw_score = mean(c(cw_0t1_weighted, cw_2t3_weighted, cw_4t5_weighted, cw_6_weighted, cw_7_weighted, cw_8_weighted, cw_9_weighted, cw_10_weighted, cw_11_weighted, cw_12_weighted, cw_13_weighted, cw_14_weighted, cw_15_weighted, cs_0t1_weighted, cs_2t3_weighted, cs_4t5_weighted, cs_6_weighted, cs_7_weighted, cs_8_weighted, cs_9_weighted, cs_10_weighted, cs_11_weighted, cs_12_weighted, cs_13_weighted, cs_14_weighted, cs_15_weighted), na.rm = TRUE),
    sndem_mean = rowMeans(cbind(emel_score, cscw_score), na.rm = TRUE)
  ) %>%
  select(MPIO_CDPMP, year, emel_score, cscw_score, sndem_mean)




##----Visualize distributions----

# check the mean averages per variable
ctd <- colwtd %>%
  group_by(year) %>%
  summarize(
    across(2:4, ~mean(., na.rm = TRUE)))
ctd <- ctd %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
ggplot(ctd, aes(x = year, y = value, color = variable)) + geom_line()



# Check the summaries
summary(colwtd$emel_score)
hist(colwtd$emel_score)
summary(colwtd$cscw_score)
hist(colwtd$cscw_score)
summary(colwtd$sndem_mean)
hist(colwtd$sndem_mean) 




#---- Write to rds----
write_rds(colwtd, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted-diff.rds")
