#---- Step 3: V-Dem data and weighting ----

setwd("G:/Shared drives/snvdem/snvdem-col/report/analysis")
## For MC:
setwd("C:/Users/mcoppedg/Dropbox/External (1)")

# load needed packages
library(tidyverse)
# install.packages("vdemdata") Not available for this version of R.
# library(vdemdata)

# Create a data frame of version 14 dataset:
v14 <- read_dta("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V13/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v13.dta")

# Filter to just Colombia since 1899:
v14_col <- filter(v14, country_name=="Colombia" & year > 1899)

# Select just the ID and subnational variables
# (Note: v2elsnless and v2elsnmore are not in this df. We will make sure to 
# include them in our request to the data manager.)
v14_col_sn <- v14_col %>%
  select(country_id, country_name, country_text_id, historical_date, year, 
         v2elsnlfc_0:v2elsnlfc_21, v2elsnmrfc_0:v2elsnmrfc_21,
         v2clrgstch_0:v2clrgstch_21, v2clrgwkch_0:v2clrgwkch_21)
# create subset of V-Dem data for 2018 
v14_col_sn_2018 <- subset(v14_col_sn, year == 2018) 
v14_col_sn_2018 <- v14_col_sn_2018 %>%
  select(-matches("_17$|_18$|_19$|_20$|_21$")) # remove responses not relevant for Colombia
df2rm = c("v14", "v14_col", "v14_col_sn", "df2rm")
rm(list = df2rm)

# rearrange data into a df for adding weight values
selected_vars <- grep("v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch", colnames(v14_col_sn_2018), value = TRUE)
selected_df <- v14_col_sn_2018 %>%
  select(all_of(selected_vars))
summary_df <- selected_df %>%
  pivot_longer(cols = everything()) %>%
  mutate(row_id = gsub(".*_(\\d+)$", "\\1", name)) %>%
  mutate(column_id = gsub(".*(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch).*", "\\1", name)) %>%
  pivot_wider(names_from = column_id, values_from = value) %>%
  select(-name) %>%
  mutate(row_id = as.numeric(row_id)) %>% 
  group_by(row_id) %>%
  summarise(across(everything(), ~ifelse(all(is.na(.)), NA, first(na.omit(.))))) %>%
  ungroup()

## ---- Assign weights ----
col18_weights <- summary_df %>%
  mutate(emel = abs(v2elsnmrfc - v2elsnlfc)) %>%  ## MC: I changed it to more minus less: the reverse.
  mutate(cscw = abs(v2clrgstch - v2clrgwkch))



### ---- Average weights (incomplete) ----
# Not sure this will be necessary, but if so, the code needs to be completed:

# Define the groups of row_id for which you want to calculate the average
groups <- list(c(0, 1), c(2, 3), c(4, 5))
groups2 <- list(c(6:14))
# Custom names for each group_id
custom_names <- c("Rural-Urban", "EconDevt", "Capital-Outside")
custom_names2 <- c("North", "South", "West", "East", "Civil unrest", 
                   "Illicit activity", "Sparse pop.", "Remote", "Indigenous")

# Create a new data frame with the average value of elem and cscw for each groupcol18_cr_weights <- col18_weights %>%
# MC added means for cscw to this df.
filter(row_id %in% unlist(groups)) %>%
  group_by(group_id = (row_id %/% 2) + 1) %>%
  summarize(emel = mean(emel, na.rm = TRUE),
            cscw = mean(cscw, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(group_name = custom_names)

col18_cr_weights2 <- col18_weights %>%
  filter(row_id %in% unlist(groups2)) %>%
  group_by(group_id = row_id) %>%
  summarize(emel, cscw) %>%
  ungroup() %>%
  mutate(group_name = custom_names2)

# Combine the groups into one table (MC)
col18_cr_allweights <- rbind(col18_cr_weights, col18_cr_weights2)

# Export to Excel
write.csv(col18_cr_allweights, file = "Col18_allweights_v14.csv")

# No code modified below this point
#---- Assign V-Dem data weights to FA scores ----
# To use these weights,  we would first make sure that the factor scores have 
# positive signs for more democracy and negative scores for less democracy;
# then convert them to a 0-1 scale with ecdf or pnorm (whatever you've been doing); 
# then multiply the transformed factor scores by the above weights,
# add them up, and divide by the sum of the weights to get a weighted average. 

library(readr)
FAcol18 <- read_csv("H:/Shared drives/snvdem/snvdem24/report/analysis/2_FAcol18.csv")




