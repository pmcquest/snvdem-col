#---- Step 3: V-Dem data extract ----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2: Data reduction (calculate factor scores)
# Step 3: Subset V-Dem data (a -- this script), then calculate criteria averages (b)
# Step 4: combine V-Dem variables with Empirical data (a), then weight obs. data by coder-level analysis (b)
# Step 5: Map geolocated levels of democracy


# Install latest version of VDem (v15, as of 12/28/2025)
# devtools::install_github("vdeminstitute/vdemdata", force = TRUE)
library(vdemdata)
max(vdem$year) # This should return 2024

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel")

## ----Setup ----
# load needed packages
library(tidyverse)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

#----V-Dem import and weights----
# Create a data frame of version 13 dataset:
v15 <- vdem
# this df appears to be the same as MC's "full + others" data set: read_dta("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V13/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v13.dta")

# Filter to just Colombia since 1899:
v15_col <- filter(v15, country_name=="Colombia" & year > 1899)

# Select just the ID and subnational variables
# (Note: v2elsnless and v2elsnmore are not in this df. We will make sure to 
# include them in our request to the data manager.)
v15_col_sn <- v15_col %>%
  select(country_id, country_name, country_text_id, historical_date, year, 
         v2elsnlfc_0:v2elsnlfc_21, v2elsnmrfc_0:v2elsnmrfc_21,
         v2clrgstch_0:v2clrgstch_21, v2clrgwkch_0:v2clrgwkch_21)
# create subset of V-Dem data for 2000-2023
v15_col_sn_0023 <- subset(v15_col_sn, year >= 2000) 
v15_col_sn_0023 <- v15_col_sn_0023 %>%
  select(-matches("_17$|_18$|_19$|_20$|_21$")) # remove responses not relevant for Colombia
df2rm = c("v15", "v15_col", "v15_col_sn", "df2rm")
rm(list = df2rm)

# rearrange data into a df for adding weight values
selected_vars <- grep("v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch", colnames(v15_col_sn_0023), value = TRUE)
v15_col_sn_0023 <- v15_col_sn_0023 %>%
  select(year, all_of(selected_vars)) # Include 'year'

#----Creating emel and cscw----
# The new variables (emel_* and cscw_*) measures the *relevance* of a given criteria for either elections or civil liberties, over time. This is done by taking the absolute value of the difference between Election pairs [abs(more free-less free)] and Civil liberties pairs [abs(strong-weak)]. 
#A score of 0 indicates either: (a) the criteria of interest has no relevance for elections or civil liberties at the subnational-level (all coders respond "no" for both pairs), or (b) coders are in full disagreement on whether the criteria is associated with free or unfree elections or strong or weak civil liberties; that is, the proportions cancel each other out. For example, a coder score of 1 for elections *less* free in rural areas (criteria 0) and a score of 1 for elections *more* free in rural areas (criteria 0)) would render the criteria of "rural areas" irrelevant.
#Conversely, a score of 1 indicates full agreement (e.g., coder score of 1 for elections less free in rural areas (criteria 0) and score of 0 for elections more free in rural areas (criteria 0)). These scores indicate that the criteria is totally relevant for subnational democracy components.

col_vdem_rel <- v15_col_sn_0023 %>%
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
# Calculating average emel and cscw "relevance" of criteria "pairs". 
col_pairs <- col_vdem_rel %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = c("weight_type", "subset"), 
               names_pattern = "(emel|cscw)_(\\d+)",
               values_to = "weight_value") %>%
  mutate(group = case_when(
    subset %in% c("0", "1") ~ "0_1", 
    subset %in% c("2", "3") ~ "2_3",
    subset %in% c("4", "5") ~ "4_5",
    subset %in% c("10", "11") ~ "10_11", 
    subset %in% c("15", "16") ~ "15_16",
    TRUE ~ subset
  )) %>%
  group_by(year, group, weight_type) %>%
  summarise(mean_weight = mean(weight_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = weight_type, values_from = mean_weight) %>%
  filter(group %in% c("0_1", "2_3", "4_5", "10_11", "15_16")) %>%
  rename(row_id = group)

# Calculating average emel and cscw "relevance" of criteria "pairs". 
col_pairs <- col_vdem_rel %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")), 
               names_to = c("weight_type", "subset"), 
               names_pattern = "(emel|cscw)_(\\d+)",
               values_to = "weight_value") %>%
  filter(subset %in% c("0", "1", "2", "3", "4", "5", "10", "11", "15", "16")) %>% 
  mutate(group = case_when(
    subset %in% c("0", "1") ~ "0_1", 
    subset %in% c("2", "3") ~ "2_3",
    subset %in% c("4", "5") ~ "4_5",
    subset %in% c("10", "11") ~ "10_11", 
    subset %in% c("15", "16") ~ "15_16",
    TRUE ~ "ERROR_GROUP" 
  )) %>%
  group_by(year, group, weight_type) %>%
  summarise(mean_weight = mean(weight_value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = weight_type, values_from = mean_weight) %>%
  rename(row_id = group)

# Create a vector of "non-pair" row_ids
unchanged_row_ids <- c(6:9, 12:14)

# Filter the original dataframe to retain rows with row_ids that should remain the same
unchanged <- col_vdem_rel %>%
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
cr_df <- bind_rows(col_pairs, unchanged)
cr_df <- cr_df %>%
  filter(year <= 2023)


## ----Visualize results----
# 1. Reshape the data
long_data <- cr_df %>%
  pivot_longer(cols = c(cscw, emel), names_to = "variable", values_to = "value") %>%
  rename(Criteria = row_id) %>% # Rename row_id to Criteria
  mutate(Criteria = case_when(
    Criteria == "0_1" ~ "Rural/Urban",
    Criteria == "2_3" ~ "Econ. Dev't",
    Criteria == "4_5" ~ "Dist. Capital",
    Criteria == "6" ~ "North",
    Criteria == "7" ~ "South",
    Criteria == "8" ~ "West",
    Criteria == "9" ~ "East",
    Criteria == "10_11" ~ "Civ. Unrest/Ill. Act.",
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


#---- Write to rds----
write_rds(cr_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/v1/vdem_col0023_cleaned.rds")
