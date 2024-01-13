# R script to create a data file of subnational ratings for Colombia, 1900-2022

# make a clean slate
rm(list = ls())

# load needed packages
library(tidyverse)

# Create a data frame of the "full + others" data set
v13 <- read_dta("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V13/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v13.dta")

# Filter to just Colombia since 1899:
v13_col <- filter(v13, country_name=="Colombia" & year > 1899)

# Select just the ID and subnational variables
# (Note: v2elsnless and v2elsnmore are not in this df. We will make sure to 
# include them in our request to the data manager.)
v13_col_sn <- v13_col %>%
  select(country_id, country_name, country_text_id, historical_date, year, 
         v2elsnlfc_0:v2elsnlfc_21, v2elsnmrfc_0:v2elsnmrfc_21,
         v2clrgstch_0:v2clrgstch_21, v2clrgwkch_0:v2clrgwkch_21)

# Listing the variable names to check our work:
names(v13_col_sn)

# Export the result to a CSV file:
write.csv(v13_col_sn, file = "v13_col_sn.csv")

# For reference. Responses:
0: Rural. (0=No, 1=Yes) [v2elsnlfc_0]
1: Urban. (0=No, 1=Yes) [v2elsnlfc_1]
2: Areas that are less economically developed. (0=No, 1=Yes) [v2elsnlfc_2]
3: Areas that are more economically developed. (0=No, 1=Yes) [v2elsnlfc_3]
4: Inside the capital city. (0=No, 1=Yes) [v2elsnlfc_4]
5: Outside the capital city. (0=No, 1=Yes) [v2elsnlfc_5]
6: North. (0=No, 1=Yes) [v2elsnlfc_6]
7: South. (0=No, 1=Yes) [v2elsnlfc_7]
8: West. (0=No, 1=Yes) [v2elsnlfc_8]
9: East. (0=No, 1=Yes) [v2elsnlfc_9]
10: Areas of civil unrest (including areas where insurgent groups are active). (0=No, 1=Yes) [v2elsnlfc_10]
11: Areas where illicit activity is widespread. (0=No, 1=Yes) [v2elsnlfc_11]
12: Areas that are very sparsely populated. (0=No, 1=Yes) [v2elsnlfc_12]
13: Areas that are remote (difficult to reach by available transportation, for example). 
(0=No, 1=Yes) [v2elsnlfc_13]
14: Areas where there are indigenous populations. (0=No, 1=Yes) [v2elsnlfc_14]
15: Areas where the national ruling party or group is strong. (0=No, 1=Yes) [v2elsnlfc_15]
16: Areas where the national ruling party or group is weak. (0=No, 1=Yes) [v2elsnlfc_16]
17: Areas that were subject to a longer period of foreign rule. (0=No, 1=Yes) [v2elsnlfc_17]
18: Areas that were subject to a shorter period of foreign rule. (0=No, 1=Yes) [v2elsnlfc_18]
19: Areas that were recently subject to foreign rule. (0=No, 1=Yes) [v2elsnlfc_19]
20: Areas that have not recently been subject to foreign rule. (0=No, 1=Yes) [v2elsnlfc_20]
21: None of the above. (0=No, 1=Yes) [v2elsnlfc_21]

# Ways to explore relationships. Examples using v2elsnlfc_*


# Make a scatterplot to check the relationship between rural (0) and urban (1)
# (by default, for all years)
ggplot(v13_col_sn, aes(x =  v2elsnlfc_0, y =  v2elsnlfc_1)) +
  geom_point()

# Correlate variables that we hope are opposite poles
rich_poor <- select(v13_col_sn, c(v2elsnlfc_2, v2elsnlfc_3))
cor(rich_poor, use = "pairwise")

# Correlate all variables (except *_16 and beyond, which are all NA)
allvars <- select(v13_col_sn, v2elsnlfc_0:v2elsnlfc_15)
cor(allvars, use = "pairwise")

# Scatterplot matrix, which gives both a scatterplot and correlations.
# I've inserted labels to make it easier to read.
install.packages("GGally")
library(GGally)
ggpairs(allvars, columns = c("v2elsnlfc_0", "v2elsnlfc_1", "v2elsnlfc_2", "v2elsnlfc_3", 'v2elsnlfc_4', "v2elsnlfc_5", "v2elsnlfc_6", 'v2elsnlfc_7', 'v2elsnlfc_8', "v2elsnlfc_9", "v2elsnlfc_10", "v2elsnlfc_11", "v2elsnlfc_12", "v2elsnlfc_13", "v2elsnlfc_14", "v2elsnlfc_15"),
        columnLabels = c("rural", "urban", "poor", "rich", 'capital', "not_cap", "N", 'S', 'E', "W", "unrest", "illicit", "sparse", "remote", "indig", "par_strong"),
        switch = "both",
        mapping = ggplot2::aes(alpha = .2), theme(text = element_text(size = 1)),
        title="subnational variables") 
# Export this to a 10x15" pdf.
