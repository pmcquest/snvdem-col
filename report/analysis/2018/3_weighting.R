#---- Step 3: V-Dem data and weighting ----

# Step 1: create country data df and ecdf country data df (standardized)
# Step 2: Data reduction (calculate factor scores)
# Step 3 (this script): merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem-col/report/analysis")


# ----Setup ----
# load needed packages
library(tidyverse)
library(vdemdata)

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
v13_col_sn_2018 <- subset(v13_col_sn, year == 2018) 
v13_col_sn_2018 <- v13_col_sn_2018 %>%
  select(-matches("_17$|_18$|_19$|_20$|_21$")) # remove responses not relevant for Colombia
df2rm = c("v13", "v13_col", "v13_col_sn", "df2rm")
rm(list = df2rm)

# rearrange data into a df for adding weight values
selected_vars <- grep("v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch", colnames(v13_col_sn_2018), value = TRUE)
selected_df <- v13_col_sn_2018 %>%
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

# ---- Assign weights ----
col18_weights <- summary_df %>%
  mutate(emel = abs(v2elsnlfc - v2elsnmrfc)) %>%
  mutate(cscw = abs(v2clrgstch - v2clrgwkch))

write.csv(col18_weights, file = "G:/Shared drives/snvdem/snvdem-col/report/analysis/3_weighttable.csv", row.names = FALSE)

# Load necessary libraries
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
  kable(format = "latex", booktabs = TRUE, caption = "V-Dem response scores for Colombia in 2018") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE)



## ---- Average weights ----

col18_cr_weights <- col18_weights %>%
  select(row_id, emel, cscw) 

# Calculating average emel and cscw values for specified subsets of observations
col18_cr_weights <- col18_weights %>%
  mutate(group = case_when(
    row_id %in% c(0, 1) ~ "0_1",
    row_id %in% c(2, 3) ~ "2_3",
    row_id %in% c(4, 5) ~ "4_5",
    row_id %in% c(15, 16) ~ "15_16",
    TRUE ~ as.character(row_id)
  )) %>%
  group_by(group) %>%
  summarise(
    emel = mean(emel),
    cscw = mean(cscw)
  ) %>%
  ungroup() %>%
  filter(group %in% c("0_1", "2_3", "4_5", "15_16")) %>%
  rename(row_id = `group`)


# Create a vector of row_ids that should remain the same
unchanged_row_ids <- c(6:14)

# Filter the original dataframe to retain rows with row_ids that should remain the same
unchanged_df <- col18_weights %>%
  filter(row_id %in% unchanged_row_ids) %>%
  select(row_id, emel, cscw) %>%
  mutate(row_id = as.character(row_id))

# Combine the averaged subset and the unchanged rows
cr_df <- bind_rows(col18_cr_weights, unchanged_df)





#---- Assign V-Dem data weights to FA scores ----
# To use these weights, we would first make sure that the factor scores have 
# positive signs for more democracy and negative scores for less democracy;
# then convert them to a 0-1 scale with ecdf or pnorm (whatever you've been doing); 
# then multiply the transformed factor scores by the above weights,
# add them up, and divide by the sum of the weights to get a weighted average. 

library(readr)
FAcol18 <- read_csv("G:/Shared drives/snvdem/snvdem-col/report/analysis/2_FAcol18.csv")

# Step 1: flip and convert the factor scores (at the municipal level) according to theory
FAcol18 <- FAcol18 %>%
  mutate(
    MR1 = -MR1, # indigenous areas with less support for ruling party 
    MR4 = -MR4, # poor and sparsely populated areas, distant from Bogota
    MR3 = -MR3 # areas with victims of violence and unrest
    # MR2: wealthy, densely populated urban areas (w/ petty crime) -> keep
  ) 

eCDF_MR1 <- ecdf(FAcol18$MR1)
FAcol18$MR1c <- eCDF_MR1(FAcol18$MR1)
eCDF_MR2 <- ecdf(FAcol18$MR2)
FAcol18$MR2c <- eCDF_MR2(FAcol18$MR2)
eCDF_MR4 <- ecdf(FAcol18$MR4)
FAcol18$MR4c <- eCDF_MR4(FAcol18$MR4)
eCDF_MR3 <- ecdf(FAcol18$MR3)
FAcol18$MR3c <- eCDF_MR3(FAcol18$MR3)


# Step 2: multiply the converted factor scores by the 2 weights for elections and civil liberties respectively (again, at the municipal level)
# this should result in two columns (one for elections and another for civil liberties) of raw weighted scores for each factor (8 total columns) 
# PM: will try with values in FA scores only above .05 

# MR1: indigenous areas with less support for ruling party 
cr_df_MR1 <- cr_df %>% 
  filter(row_id %in% c("14", "15_16")) 
# Calculate the average of emel and cscw for indigenous (row_id == 14) and ruling party (15_16)
emel1_avg <- mean(cr_df_MR1$emel)
cscw1_avg <- mean(cr_df_MR1$cscw)

# MR2: wealthy, densely populated urban areas (w/ petty crime) : positive
cr_df_MR2 <- cr_df %>% 
  filter(row_id %in% c("0_1", "2_3", "11", "12", "14")) 
# Calculate the average of emel and cscw
emel2_avg <- mean(cr_df_MR2$emel)
cscw2_avg <- mean(cr_df_MR2$cscw)

# MR4: poor areas distant from Bogota
cr_df_MR4 <- cr_df %>% 
  filter(row_id %in% c("2_3", "4_5"))
# Calculate the average of emel and cscw
emel4_avg <- mean(cr_df_MR4$emel)
cscw4_avg <- mean(cr_df_MR4$cscw)

# MR3: areas with victims of violence and unrest
cr_df_MR3 <- cr_df %>% 
  filter(row_id %in% c("10", "11"))
# Calculate the average of emel and cscw
emel3_avg <- mean(cr_df_MR3$emel)
cscw3_avg <- mean(cr_df_MR3$cscw)


FAcol18 <- FAcol18 %>%
  mutate(MR1c_emel = MR1c*(emel1_avg)) %>% # apply average only of indigenous (14) and RP (15_16) from V-Dem weights
  mutate(MR2c_emel = MR2c*(emel2_avg)) %>%
  mutate(MR4c_emel = MR4c*(emel4_avg)) %>%
  mutate(MR3c_emel = MR3c*(emel3_avg)) %>%
  mutate(MR1c_cscw = MR1c*(cscw1_avg)) %>% # apply average only of indigenous (14) and RP (15_16) from V-Dem weights
  mutate(MR2c_cscw = MR2c*(cscw2_avg)) %>%
  mutate(MR4c_cscw = MR4c*(cscw4_avg)) %>%
  mutate(MR3c_cscw = MR3c*(cscw3_avg))


# Step 3: add up the 4 weighted scores for each variable (elections and civil liberties), 
# and then divide each column by 4 to get individual municipal-level average scores
FAcol18 <- FAcol18 %>%
  mutate(MRm_emel = (MR1c_emel+MR2c_emel+MR3c_emel+MR4c_emel)/4) %>%
  mutate(MRm_cscw = (MR1c_cscw+MR2c_cscw+MR3c_cscw+MR4c_cscw)/4)
  
# Step 4: average those two scores to get municipal-level "democracy" scores (thinly conceptualized) 
FAcol18 <- FAcol18 %>%
  mutate(MRm_dem = (MRm_emel+MRm_cscw)/2)

write.csv(FAcol18, file = "G:/Shared drives/snvdem/snvdem-col/report/analysis/3_FAcol18.csv", row.names = FALSE)

# ----- Merge weighted scores w/ geometries to make maps -----
## Maps
library(sf)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)

col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)

FAcol18 <- read_csv("G:/Shared drives/snvdem/snvdem-col/report/analysis/3_FAcol18.csv")

colmap_data <- merge(x = FAcol18, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data <- st_as_sf(colmap_data)

ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MRm_dem)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "SN democracy level", 
       caption = "Democracy scores")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/3Weight_figs/wf_dem18_map.png", 
       height = 10, width = 10, device = "png", units = "in")

# emel only
ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MRm_emel)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "SN democracy level", 
       caption = "Elections free and fair (scores)")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/3Weight_figs/wf_emel18_map.png", 
       height = 10, width = 10, device = "png", units = "in")

# cscw only
ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MRm_cscw)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "SN democracy level", 
       caption = "Civil liberties (scores)")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/3Weight_figs/wf_cscw18_map.png", 
       height = 10, width = 10, device = "png", units = "in")

# median score of these scales, then take this to be calibrated as a 0. this converted to score - median