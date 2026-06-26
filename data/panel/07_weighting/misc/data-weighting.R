#---- Step 3: V-Dem data and weighting ----

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


#----V-Dem import and weights----
# Create a data frame of version 13 dataset:
v13 <- vdem
# this df appears to be the same as MC's "full + others" data set: read_dta("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V13/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v13.dta")

# Filter to just Colombia since 1899:
v13_col <- filter(v13, country_name=="Colombia" & year > 1899)

# Select just the ID and subnational variables
# (Note: v2elsnless and v2elsnmore are not in this df. We will make sure to 
# include them in our request to the data manager.)
v13_col_sn <- v13_col %>%
  select(country_id, country_name, country_text_id, historical_date, year, 
         v2elsnlfc_0:v2elsnlfc_21, v2elsnmrfc_0:v2elsnmrfc_21,
         v2clrgstch_0:v2clrgstch_21, v2clrgwkch_0:v2clrgwkch_21)
# create subset of V-Dem data for 2018 
v13_col_sn_0020<- subset(v13_col_sn, year >= 2000) 
v13_col_sn_0020 <- v13_col_sn_0020 %>%
  select(-matches("_17$|_18$|_19$|_20$|_21$")) # remove responses not relevant for Colombia
df2rm = c("v13", "v13_col", "v13_col_sn", "df2rm")
rm(list = df2rm)


## ---- Extract V-Dem weights ----
# rearrange data into a df for adding weight values
selected_vars <- grep("v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch", colnames(v13_col_sn_0020), value = TRUE)
selected_df <- v13_col_sn_0020 %>%
  select(year, all_of(selected_vars)) # Include 'year'

###---- Write to rds----
write_rds(selected_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/validation/vdem-col0020.rds")

# visualize V-Dem data in other script entitled "colvdem0020"

selected_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/vdem-col0020.rds")

# 1. Calculate yearly averages and weights
# The new variables (emel_* and cscw_*) measures the *relevance* of a given criteria for either elections or civil liberties, over time. This is done by taking the absolute value of the difference between Election pairs [abs(more free-less free)] and Civil liberties pairs [abs(strong-weak)]. 
#A score of 0 indicates either: (a) the criteria of interest has no relevance for elections or civil liberties at the subnational-level (all coders respond "no" for both pairs), or (b) coders are in full disagreement on whether the criteria is associated with free or unfree elections or strong or weak civil liberties; that is, the proportions cancel each other out. For example, a coder score of 1 for elections *less* free in rural areas (criteria 0) and a score of 1 for elections *more* free in rural areas (criteria 0)) would render the criteria of "rural areas" irrelevant.
#Conversely, a score of 1 indicates full agreement (e.g., coder score of 1 for elections less free in rural areas (criteria 0) and score of 0 for elections more free in rural areas (criteria 0)). These scores indicate that the criteria is totally relevant for subnational democracy components.


col_weights1a <- selected_df %>%
  pivot_longer(cols = starts_with(c("v2elsnlfc", "v2elsnmrfc", "v2clrgstch", "v2clrgwkch")),
               names_to = c("variable", "subset"),
               names_pattern = "(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch)_(\\d+)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    emel = abs(v2elsnmrfc - v2elsnlfc), #absolute values
    cscw = abs(v2clrgstch - v2clrgwkch),
    emels = v2elsnmrfc - v2elsnlfc, # scale of 1 (more fair EL) to -1 (less fair EL)
    cscws = v2clrgstch - v2clrgwkch # scale of 1 (stronger CL) to -1 (weaker CL) 
  ) %>%
  pivot_longer(cols = c(emel, cscw, emels, cscws), names_to = "weight_type", values_to = "weight_value") %>%
  mutate(weight_name = paste(weight_type, subset, sep = "_")) %>%
  select(year, weight_name, weight_value) %>%
  pivot_wider(names_from = weight_name, values_from = weight_value)


##---- Average weights ----
# Calculating average emel and cscw values for specified subsets of observations

# Option 1: absolute values. Here we are concerned with relevance of a criteria.
col_weights_abs <- col_weights1a %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = c("weight_type", "subset"), 
               names_pattern = "(emel|cscw)_(\\d+)",
               values_to = "weight_value") %>%
  mutate(group = case_when(
    subset %in% c("0", "1") ~ "0_1", 
    subset %in% c("2", "3") ~ "2_3",
    subset %in% c("4", "5") ~ "4_5",
    subset %in% c("15", "16") ~ "15_16",
    TRUE ~ subset
  )) %>%
  group_by(year, group, weight_type) %>%
  summarise(mean_weight = mean(weight_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = weight_type, values_from = mean_weight) %>%
  filter(group %in% c("0_1", "2_3", "4_5", "15_16")) %>%
  rename(row_id = group)

# Create a vector of row_ids that should remain the same
unchanged_row_ids <- c(6:14)

# Filter the original dataframe to retain rows with row_ids that should remain the same
unchanged_df1 <- col_weights1a %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = c("weight_type", "subset"), 
               names_pattern = "(emel|cscw)_(\\d+)",
               values_to = "weight_value") %>%
  mutate(subset = as.character(subset)) %>%
  filter(subset %in% unchanged_row_ids) %>%
  group_by(year, subset, weight_type) %>%
  summarise(mean_weight = mean(weight_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = weight_type, values_from = mean_weight) %>%
  rename(row_id = subset)

# Combine the averaged subset and the unchanged rows
cr_df <- bind_rows(col_weights_abs, unchanged_df1)
cr_df <- cr_df %>%
  filter(year <= 2020)



# 1. Reshape the data
long_data <- cr_df %>%
  pivot_longer(cols = c(cscw, emel), names_to = "variable", values_to = "value") %>%
  rename(Criteria = row_id) %>% # Rename row_id to Criteria
  mutate(Criteria = case_when(
    Criteria == "0_1" ~ "Rural/Urban",
    Criteria == "2_3" ~ "Socioeconomics",
    Criteria == "4_5" ~ "Dist. Capital",
    Criteria == "6" ~ "North",
    Criteria == "7" ~ "South",
    Criteria == "8" ~ "West",
    Criteria == "9" ~ "East",
    Criteria == "10" ~ "Civ. Unrest",
    Criteria == "11" ~ "Illicit Act.",
    Criteria == "12" ~ "Pop. Density",
    Criteria == "13" ~ "Remoteness",
    Criteria == "14" ~ "Ind. Population",
    Criteria == "15_16" ~ "Ruling Party",
    TRUE ~ Criteria # Keep other values unchanged
  ))


# 2. Create the faceted line plot
ggplot(long_data, aes(x = year, y = value, color = variable, group = variable)) + #added group = variable
  geom_line() +
  facet_wrap(~ Criteria) +
  labs(title = "CSCW and EMEL Over Time by Criteria",
       x = "Year",
       y = "Value",
       color = "Variable") +
  theme_minimal() +
  ylim(0,1) #set y axis limits to 0-1


###---- Write to rds----
write_rds(cr_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-weighted.rds")


#---- Calculate average scores for criteria ----
# We import the CDF dataset of all variables and select which variables to average for the individual criteria to be weighted
cr_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-weighted.rds")
colvars_cdf <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/colvars_cdf.rds")
colnames(colvars_cdf)

# Compute the correlation matrix to make sure there are no perfectly correlated variables
cor_matrix <- cor(colvars_cdf[, c(16:17, #0-1, 2-3a
                                  27, #2-3b
                                  7, #4-5
                                  8:11, #6-9 
                                  18:19, #10
                                  20:25, #11 (includes index and weighted avg)
                                  28, #12
                                  33, 38, #13
                                  29:30, #14
                                  31:32) #15-16
                              ], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

#subset the data for weighting
colvars_cdfw <- colvars_cdf[, c(1:6, # year and categorical data
                                16:17, #0-1, 2-3a
                                27, #2-3b
                                7, #4-5
                                8:11, #6-9 
                                18:19, #10
                                20, 22:25, #11: homicides were highly correlated (.96); has 6 variables (avg may be too drastic)
                                28, #12
                                33, 38, #13
                                29, #14: Ethnic highly correlated to Indigenous (.78)
                                32)] # 15-16: try dichotomous measure (2/3 voted for ruling party)


##----Ensure correct weight directions----
# MC (04/02/25)// To ensure that the index is calculated correctly, we have to make sure that score_it for each dimension is calculated so that the high values are associated with greater democracy and the low values are associated with less democracy. In many cases, that is straightforward, because we expect high values of per capita GDP, for example, are associated with more democracy. That is true for urban % of population, too. ... make sure high values go with expected higher democracy and low values go with expected lower democracy.
colvars_cdfw <- colvars_cdfw %>%
  rename(north_6 = 11, south_7 = 12, west_8 = 13, east_9 = 14) %>%
  rename(lAR_13 = 24) %>%
  mutate(DisBog_4t5 = 1 - DisBog_4t5) %>%
  mutate(north_6 = 1 - north_6) %>%
  mutate(south_7 = 1 - south_7) %>%
  mutate(west_8 = 1 - west_8) %>%
  mutate(east_9 = 1 - east_9) %>%
  mutate(Desp_10 = 1 - Desp_10) %>%
  mutate(Errad_10 = 1 - Errad_10) %>%
  mutate(Hurto_11 = 1 - Hurto_11) %>%
  mutate(HHomi_11 = 1 - HHomi_11) %>%
  mutate(HDesa_11 = 1 - HDesa_11) %>%
  mutate(HSecu_11 = 1 - HSecu_11) %>%
  mutate(HRecl_11 = 1 - HRecl_11) %>%
  mutate(DisMer_13 = 1 - DisMer_13) %>% # invert: high = non-remote to take average with road density (low = remote)
  mutate(RulParD_15t16 = 1 - RulParD_15t16)


summary(colvars_cdfw)

#check to see some trends
# econ variables are showing steady improvements, but nothing like a spike in 2017
df1 <- colvars_cdfw %>%
  group_by(year) %>%
  summarize(
    across(6:13, ~mean(., na.rm = TRUE)))
# crime variables are much more varying and if anything show a dip in 2017
df2 <- colvars_cdfw %>%
  group_by(year) %>%
  summarize(
    across(14:20, ~mean(., na.rm = TRUE)))
# ruling party shows a dip in 2017 but not the others
df3 <- colvars_cdfw %>%
  group_by(year) %>%
  summarize(
    across(21:25, ~mean(., na.rm = TRUE)))

df1 <- df1 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df2 <- df2 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df3 <- df3 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")

ggplot(df1, aes(x = year, y = value, color = variable)) + geom_line()

  
##---- Calculate average of the CDF'd individual variables---- 
# for each criteria, being attuned to whether the valence between variables is positive or not:
# Select the variables you want to average (columns 7 to 27)
selected_vars <- colvars_cdfw[, 7:26]
# Extract the ending digits (e.g., "0t1", "2t3", etc.)
ending_digits <- str_extract(names(selected_vars), "\\_.*")
# Create a list to store the averages
averages_list <- list()
# Loop through unique ending digits and calculate averages
for (digit in unique(ending_digits)) {
  vars_to_avg <- selected_vars[, ending_digits == digit, drop = FALSE] # drop=FALSE to keep as dataframe even if there is only one column.
  avg_name <- paste0("avg", str_remove(digit, "_")) # Create a name for the average variable
  averages_list[[avg_name]] <- rowMeans(vars_to_avg, na.rm = TRUE)
}
# Combine the averages into a data frame
averages_df <- bind_cols(averages_list)
# Add MPIO_CDPMP and year to the averages data frame
averages_df <- cbind(colvars_cdfw[, 1:2], averages_df)

summary(averages_df)

# check the mean averages per variable
dfavg <- averages_df %>%
  group_by(year) %>%
  summarize(
    across(2:14, ~mean(., na.rm = TRUE)))
dfavg <- dfavg %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
ggplot(dfavg, aes(x = year, y = value, color = variable)) + geom_line()
# Ruling Party seems to be the main dip in averages, and illicit activity is in second place
# Note: Criteria #11 (6 variables) has been averaged. An alternative would be to create loadings with FA, or another data reduction technique.

###---- Write to rds----
write_rds(averages_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-averages2.rds")


## ---- Join averages_df with cr_df ----

# 1. Join averages_df with cr_df
weighted_df <- averages_df %>%
  left_join(cr_df, by = c("year"))

print(table(averages_df$year))
print(table(cr_df$year))


# 2. Apply Weights
weighted_df <- weighted_df %>%
  mutate(
    emel0_1 = avg0t1 * emel,
    emel3 = avg2t3 * emel,
    emel4_5 = avg4t5 * emel,
    emel6 = avg6 * emel,
    emel7 = avg7 * emel,
    emel8 = avg8 * emel,
    emel9 = avg9 * emel,
    emel10 = avg10 * emel,
    emel11 = avg11 * emel,
    emel12 = avg12 * emel,
    emel13 = avg13 * emel,
    emel14 = avg14 * emel,
    emel1516 = avg15t16 * emel,
    cscw0_1 = avg0t1 * cscw,
    cscw3 = avg2t3 * cscw,
    cscw4_5 = avg4t5 * cscw,
    cscw6 = avg6 * cscw,
    cscw7 = avg7 * cscw,
    cscw8 = avg8 * cscw,
    cscw9 = avg9 * cscw,
    cscw10 = avg10 * cscw,
    cscw11 = avg11 * cscw,
    cscw12 = avg12 * cscw,
    cscw13 = avg13 * cscw,
    cscw14 = avg14 * cscw,
    cscw1516 = avg15t16 * cscw
  )

summary(weighted_df)

# 3. Combine Weighted Variables into Overall Scores

result_df <- weighted_df %>%
  group_by(MPIO_CDPMP, year) %>%
  summarize(
    emel_score = mean(c(emel0_1, emel3, emel4_5, emel6, emel7, emel8, emel9, emel10, emel11, emel12, emel13, emel14, emel1516), na.rm = TRUE),
    cscw_score = mean(c(cscw0_1, cscw3, cscw4_5, cscw6, cscw7, cscw8, cscw9, cscw10, cscw11, cscw12, cscw13, cscw14, cscw1516), na.rm = TRUE)
  ) 

print(result_df)
summary(result_df)

###---- Write to rds----
write_rds(result_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/validation/joined-avg-weighted2.rds")
# validate the scores by checking correlation with nat-level v-dem scores

# emel distribution is slightly right-skewed
hist(result_df$emel_score)
# cscw distribution looks normal
hist(result_df$cscw_score)

##---- Calculate Mean SN score----
colwtd <- result_df %>%
  mutate(sndem_mean = (emel_score + cscw_score) / 2)

##----Visualize distributions----

# check the mean averages per variable
ctd <- colwtd %>%
  group_by(year) %>%
  summarize(
    across(2:4, ~mean(., na.rm = TRUE)))
ctd <- ctd %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
ggplot(ctd, aes(x = year, y = value, color = variable)) + geom_line()


# taking out xx avg values to check their influence on the spike
result_df2 <- weighted_df %>%
  group_by(MPIO_CDPMP, year) %>%
  summarize(
    emel_score = mean(c(emel10, emel11), na.rm = TRUE),
    cscw_score = mean(c(cscw0_1, cscw3, cscw4_5, cscw6, cscw7, cscw8, cscw9, cscw10, cscw11, cscw12, cscw13, cscw14, cscw1516), na.rm = TRUE)
  ) 
colwtd2 <- result_df2 %>%
  mutate(sndem_mean = (emel_score + cscw_score) / 2)
# check the mean averages per variable
ctd2 <- colwtd2 %>%
  group_by(year) %>%
  summarize(
    across(2:4, ~mean(., na.rm = TRUE)))
ctd2 <- ctd2 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
ggplot(ctd2, aes(x = year, y = value, color = variable)) + geom_line()


# Check the summaries
summary(colwtd$emel_score)
hist(colwtd$emel_score)
summary(colwtd$cscw_score)
hist(colwtd$cscw_score)
summary(colwtd$sndem_mean)
hist(colwtd$sndem_mean) # much shorter range than anticipated

#---- Write to rds----
write_rds(colwtd, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted2.rds")



#---- (if necessary) Assign weights to FA scores ----
# To use these weights, we would first make sure that the factor scores have 
# positive signs for more democracy and negative scores for less democracy;
# then convert them to a 0-1 scale with ecdf or pnorm; 
# then multiply the transformed factor scores by the above weights,
# add them up, and divide by the sum of the weights to get a weighted average. 

FAcol <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/FA/FAcol0020.rds")

# Step 1: flip and convert the factor scores (at the municipal level) according to theory
#ML2 : urban areas with illicit activity and indigenous populations (in theory, negative for democracy)
#ML5: 
#ML3: 
#ML1: 
#ML4: 

FAcol <- FAcol %>%
  mutate(
    ML2 = -ML2,  #ML2 : urban areas with illicit activity and indigenous populations (in theory, negative for democracy)
   #ML5: indigenous areas east or west of capital, with less support for ruling party (negative for democracy)
    #ML3: densely populated areas with roads and stronger fiscal performance (positive for democracy)
    ML1 = -ML1,#ML1: areas distant from Bogota with poor fiscal performance (negative for democracy)
    ML4 = -ML4, #areas with high unrest and illicit activity, and poorer fiscal performance (negative for democracy)
  ) 

# 1. Check Distributions Before Calculation

# Pivot data to long format for easier plotting
FAcol_long_before <- FAcol %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "value")

# Create histograms for each column before ranking
ggplot(FAcol_long_before, aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions Before Ranking", x = "Value", y = "Frequency") +
  theme_minimal()

# Create density plots for each column before ranking
ggplot(FAcol_long_before, aes(x = value)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots Before Ranking", x = "Value", y = "Density") +
  theme_minimal()


# 2. Perform the Calculation
FAcolc <- FAcol %>%
  mutate(across(c("ML2", "ML5", "ML3", "ML1", "ML4"), ~ rank(., na.last = "keep") / length(.)))


# 3. Check Distributions After Calculation

# Pivot data to long format for easier plotting
FAcolc_long_after <- FAcolc %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "value")


# Create density plots for each column after ranking
ggplot(FAcolc_long_after, aes(x = value)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Plots After Ranking", x = "Ranked Value", y = "Density") +
  theme_minimal()

# Step 2: multiply the converted factor scores by the 2 weights for elections and civil liberties respectively (again, at the municipal level) using values in FA scores/loadings only above 0.5.
# this should result in two columns (one for elections and another for civil liberties) of raw weighted scores for each factor (8 total columns), per year.

# Calculate yearly averages for emel and cscw in cr_df
cr_df_yearly <- cr_df %>%
  group_by(year) %>%
  mutate(
    emel2_avg = mean(emel[row_id %in% c("0_1", "11")]),
    cscw2_avg = mean(cscw[row_id %in% c("0_1", "11")]),
    emel5_avg = mean(emel[row_id %in% c("14", "9", "8")]),
    cscw5_avg = mean(cscw[row_id %in% c("14", "9", "8")]),
    emel3_avg = mean(emel[row_id %in% c("12", "13")]),
    cscw3_avg = mean(cscw[row_id %in% c("12", "13")]),
    emel1_avg = mean(emel[row_id %in% c("4_5", "6", "7")]),
    cscw1_avg = mean(cscw[row_id %in% c("4_5", "6", "7")]),
    emel4_avg = mean(emel[row_id %in% c("11")]),
    cscw4_avg = mean(cscw[row_id %in% c("11")])
  ) %>%
  select(year, emel1_avg, cscw1_avg, emel2_avg, cscw2_avg, emel3_avg, cscw3_avg, emel4_avg, cscw4_avg, emel5_avg, cscw5_avg) %>%
  distinct()

# Join FAcol with yearly averages
FAcol_yearly <- FAcol %>%
  left_join(cr_df_yearly, by = "year")

# Calculate weighted scores for each year
FAcol_yearly <- FAcol_yearly %>%
  mutate(
    ML1c_emel = ML1 * emel1_avg,
    ML2c_emel = ML2 * emel2_avg,
    ML3c_emel = ML3 * emel3_avg,
    ML4c_emel = ML4 * emel4_avg,
    ML5c_emel = ML5 * emel5_avg,
    ML1c_cscw = ML1 * cscw1_avg,
    ML2c_cscw = ML2 * cscw2_avg,
    ML3c_cscw = ML3 * cscw3_avg,
    ML4c_cscw = ML4 * cscw4_avg,
    ML5c_cscw = ML5 * cscw5_avg
  )

# Calculate average scores and democracy score
FAcol_yearly <- FAcol_yearly %>%
  mutate(
    MLm_emel = (ML1c_emel + ML2c_emel + ML3c_emel + ML4c_emel + ML5c_emel) / 5,
    MLm_cscw = (ML1c_cscw + ML2c_cscw + ML3c_cscw + ML4c_cscw + ML5c_cscw) / 5,
    MLm_dem = (MLm_emel + MLm_cscw) / 2
  )




#---- To fix...---- 
## ---- Summary line graph (fix) ----
library(dplyr)
library(tidyr)
library(ggplot2)


# Calculate yearly averages and confidence intervals for original variables
yearly_stats <- selected_df %>%
  group_by(year) %>%
  summarize(
    avg_v2elsnlfc = mean(c_across(starts_with("v2elsnlfc")), na.rm = TRUE),
    se_v2elsnlfc = sd(c_across(starts_with("v2elsnlfc")), na.rm = TRUE) / sqrt(n()),
    avg_v2elsnmrfc = mean(c_across(starts_with("v2elsnmrfc")), na.rm = TRUE),
    se_v2elsnmrfc = sd(c_across(starts_with("v2elsnmrfc")), na.rm = TRUE) / sqrt(n()),
    avg_v2clrgstch = mean(c_across(starts_with("v2clrgstch")), na.rm = TRUE),
    se_v2clrgstch = sd(c_across(starts_with("v2clrgstch")), na.rm = TRUE) / sqrt(n()),
    avg_v2clrgwkch = mean(c_across(starts_with("v2clrgwkch")), na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(
    ci_v2elsnlfc = 1.96 * se_v2elsnlfc,
    ci_v2elsnmrfc = 1.96 * se_v2elsnmrfc,
    ci_v2clrgstch = 1.96 * se_v2clrgstch,
    ci_v2clrgwkch = 1.96 * se_v2clrgwkch
  )

# Calculate yearly averages for emel and cscw
yearly_agreement <- col_weights %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = "weight_name", values_to = "weight_value") %>%
  group_by(year) %>%
  summarize(
    avg_emel = mean(weight_value[grepl("emel", weight_name)], na.rm = TRUE),
    avg_cscw = mean(weight_value[grepl("cscw", weight_name)], na.rm = TRUE)
  )

plot_data <- yearly_stats %>%
  left_join(yearly_agreement, by = "year") %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  filter(year <= 2020) %>%
  left_join(yearly_stats %>% pivot_longer(cols = starts_with(c("ci_v2elsnlfc", "ci_v2elsnmrfc", "ci_v2clrgstch", "ci_v2clrgwkch")), names_to = "variable_ci", values_to = "ci") %>% mutate(variable = gsub("ci_", "avg_", variable_ci)) %>% select(year, variable, ci), by = c("year", "variable")) %>%
  mutate(ymin = value - ci, ymax = value + ci)

# Filter for the avg_v2... variables
plot_data_v2 <- plot_data %>%
  filter(grepl("avg_v2", variable))

# Create faceted plots with confidence intervals
ggplot(plot_data_v2, aes(x = year, y = value)) +
  geom_line() +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "gray") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Yearly Trends of SN V-Dem Data (Faceted with CIs)",
       x = "Year",
       y = "Value") +
  theme_minimal()

# Calculate yearly averages and standard deviations for original variables
yearly_stats <- selected_df %>%
  group_by(year) %>%
  summarize(
    avg_v2elsnlfc = mean(c_across(starts_with("v2elsnlfc")), na.rm = TRUE),
    sd_v2elsnlfc = sd(c_across(starts_with("v2elsnlfc")), na.rm = TRUE),
    avg_v2elsnmrfc = mean(c_across(starts_with("v2elsnmrfc")), na.rm = TRUE),
    sd_v2elsnmrfc = sd(c_across(starts_with("v2elsnmrfc")), na.rm = TRUE),
    avg_v2clrgstch = mean(c_across(starts_with("v2clrgstch")), na.rm = TRUE),
    sd_v2clrgstch = sd(c_across(starts_with("v2clrgstch")), na.rm = TRUE),
    avg_v2clrgwkch = mean(c_across(starts_with("v2clrgwkch")), na.rm = TRUE),
    sd_v2clrgwkch = sd(c_across(starts_with("v2clrgwkch")), na.rm = TRUE)
  )

# Calculate yearly averages for emel and cscw
yearly_agreement <- col_weights %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = "weight_name", values_to = "weight_value") %>%
  group_by(year) %>%
  summarize(
    avg_emel = mean(weight_value[grepl("emel", weight_name)], na.rm = TRUE),
    avg_cscw = mean(weight_value[grepl("cscw", weight_name)], na.rm = TRUE)
  )


plot_data <- yearly_stats %>%
  left_join(yearly_agreement, by = "year") %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  filter(year <= 2020) %>%
  left_join(yearly_stats %>% pivot_longer(cols = starts_with(c("ci_v2elsnlfc", "ci_v2elsnmrfc", "ci_v2clrgstch", "ci_v2clrgwkch")), names_to = "variable_ci", values_to = "ci") %>% mutate(variable = gsub("ci_", "avg_", variable_ci)) %>% select(year, variable, ci), by = c("year", "variable")) %>%
  mutate(ymin = value - ci, ymax = value + ci)

# Create the line plot with shaded confidence regions
ggplot(plot_data, aes(x = year, y = value, color = variable, group = variable)) +
  geom_line(aes(size = ifelse(variable %in% c("avg_emel", "avg_cscw"), 1.2,
                              ifelse(variable %in% c("avg_v2clrgstch", "avg_v2clrgwkch", "avg_v2elsnlfc", "avg_v2elsnmrfc"), 0.8, 0.5)),
                linetype = ifelse(variable %in% c("avg_v2clrgstch", "avg_v2clrgwkch", "avg_v2elsnlfc", "avg_v2elsnmrfc"), "dashed", "solid"))) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = color), alpha = 0.2, color = NA, data = plot_data %>% filter(!is.na(ymin))) +
  scale_color_manual(values = c(
    "avg_emel" = "#1B9E77",
    "avg_cscw" = "#D95F02",
    "avg_v2clrgstch" = "#7570B3",
    "avg_v2clrgwkch" = "#E7298A",
    "avg_v2elsnlfc" = "#66A61E",
    "avg_v2elsnmrfc" = "#E6AB02"
  ),
  labels = c(
    "CivLib Agreement",
    "Election Agreement",
    "Avg. Strong CL score",
    "Avg. Weak CL score",
    "Avg. Less Free El. Score",
    "Avg. More Free El. Score"
  )) +
  scale_size_identity() +
  scale_shape_identity() +
  scale_linetype_identity() +
  labs(title = "Yearly Trends of SN V-Dem Data and Agreement Scores for Colombia (2000-2020)",
       x = "Year",
       y = "Value",
       color = "Variable") +
  theme_minimal()


# Create the summary table
summary_table <- plot_data %>%
  pivot_wider(names_from = variable, values_from = value)

print(summary_table)

## ---- Summary weight table ----
#for table: first row with the year (2000-2020), the second row with 3 merged columns entitled "Elections", "Civil Liberties", and "Agreement"; and the third row to have 5 sub-columns entitled "More Free", "Less Free", "Stronger", "Weaker", "Abs", and then rows 4-21 to contain: the name of each variable _0 to _16 (see below) and the values of the original values for v2elsnlfc, v2elsnmrfc, v2clrgstch, and v2clrgwkch, as well as the absolute values emel_* and cscw_* for each variable:

#Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(purrr)
library(kableExtra)
library(insight)

new_row_names <- c(
  "Rural", "Urban", "Less econ devt", "More econ devt", "Inside capital", 
  "Outside capital", "North", "South", "West", "East", "Civil unrest", 
  "Illicit activity", "Sparse pop.", "Remote", "Indigenous", 
  "Ruling party strong", "Ruling party weak"
)

# Rename columns and modify row_id variable
summary_table <- col18_weights %>%
  mutate(row_id = new_row_names[as.numeric(row_id) + 1]) %>%
  rename(
    `Variable` = row_id,
    `El. Less free` = v2elsnlfc,
    `El. More free` = v2elsnmrfc,
    `CL Stronger` = v2clrgstch,
    `CL Weaker` = v2clrgwkch,
    `Abs(EL)` = emel,
    `Abs(CL)` = cscw
  ) 

summary_table %>%
  kable(format = "latex", booktabs = TRUE, caption = "V-Dem response scores for Colombia") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE)


# 2. Merge with Factor Results and Calculate Weighted Averages
# Assuming fa_results has a year column.
yearly_weighted_avg <- fa_results %>%
  left_join(yearly_avg_weights, by = "year") %>%
  group_by(year) %>%
  summarize(
    weighted_factor1 = weighted.mean(Factor1, emel, na.rm = TRUE),
    weighted_factor2 = weighted.mean(Factor2, cscw, na.rm = TRUE),
    weighted_factor3 = weighted.mean(Factor3, emel, na.rm = TRUE),
    weighted_factor4 = weighted.mean(Factor4, cscw, na.rm = TRUE),
    sum_emel = sum(emel, na.rm = TRUE),
    sum_cscw = sum(cscw, na.rm = TRUE)
  )

# 3. Calculate Pooled Averages
pooled_weighted_avg <- yearly_weighted_avg %>%
  summarize(
    pooled_weighted_factor1 = mean(weighted_factor1, na.rm = TRUE),
    pooled_weighted_factor2 = mean(weighted_factor2, na.rm = TRUE),
    pooled_weighted_factor3 = mean(weighted_factor3, na.rm = TRUE),
    pooled_weighted_factor4 = mean(weighted_factor4, na.rm = TRUE)
  )

print(yearly_weighted_avg)
print(pooled_weighted_avg)






