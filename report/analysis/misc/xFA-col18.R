#----- Data reduction for subnational V-Dem project -----


##---- Step 1: import V-Dem data ----
# make a clean slate
rm(list = ls())
# load needed packages
library(tidyverse)
library(vdemdata)
library(ggplot2)
library(gridExtra)

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

# Listing the variable names to check our work:
#names(v13_col_sn)
# Export the result to a CSV file:
#write.csv(v13_col_sn, file = "v13_col_sn.csv")


# create subset of V-Dem data for 2018 
v13_col_sn_2018 <- subset(v13_col_sn, year == 2018) 
v13_col_sn_2018 <- v13_col_sn_2018 %>%
  select(-matches("_17$|_18$|_19$|_20$|_21$")) # remove responses not relevant for Colombia

df2rm = c("v13", "v13_col", "v13_col_sn", "df2rm")
rm(list = df2rm)



#---- 0-1 Interaction ----#

# 0: proportion who selected Rural -- 
IR18 <- IR18 %>% 
  mutate(el_c0 = CDF_0t1*v13_col_sn_2018$v2elsnlfc_0) %>%
  mutate(em_c0 = CDF_0t1*v13_col_sn_2018$v2elsnmrfc_0) %>%
  mutate(cs_c0 = CDF_0t1*v13_col_sn_2018$v2clrgstch_0) %>%
  mutate(cw_c0 = CDF_0t1*v13_col_sn_2018$v2clrgwkch_0)
# 1: proportion who selected Urban
IR18 <- IR18 %>% 
  mutate(el_c1 = CDF_0t1*v13_col_sn_2018$v2elsnlfc_1) %>%
  mutate(em_c1 = CDF_0t1*v13_col_sn_2018$v2elsnmrfc_1) %>%
  mutate(cs_c1 = CDF_0t1*v13_col_sn_2018$v2clrgstch_1) %>%
  mutate(cw_c1 = CDF_0t1*v13_col_sn_2018$v2clrgwkch_1)

#---- 2-3 Interaction ----#
# interact V-Dem with ECDF variables

# Department-level data
# 2: proportion who selected less economically developed
ED18 <- ED18 %>% 
  mutate(el_c2d = eCDF_2t3d*v13_col_sn_2018$v2elsnlfc_2) %>%
  mutate(em_c2d = eCDF_2t3d*v13_col_sn_2018$v2elsnmrfc_2) %>%
  mutate(cs_c2d = eCDF_2t3d*v13_col_sn_2018$v2clrgstch_2) %>%
  mutate(cw_c2d = eCDF_2t3d*v13_col_sn_2018$v2clrgwkch_2)
# 3: proportion who selected more economically developed
ED18 <- ED18 %>% 
  mutate(el_c3d = eCDF_2t3d*v13_col_sn_2018$v2elsnlfc_3) %>%
  mutate(em_c3d = eCDF_2t3d*v13_col_sn_2018$v2elsnmrfc_3) %>%
  mutate(cs_c3d = eCDF_2t3d*v13_col_sn_2018$v2clrgstch_3) %>%
  mutate(cw_c3d = eCDF_2t3d*v13_col_sn_2018$v2clrgwkch_3)

# Municipal-level data
# 2: proportion who selected less economically developed
ED18 <- ED18 %>% 
  mutate(el_c2m = eCDF_2t3m*v13_col_sn_2018$v2elsnlfc_2) %>%
  mutate(em_c2m = eCDF_2t3m*v13_col_sn_2018$v2elsnmrfc_2) %>%
  mutate(cs_c2m = eCDF_2t3m*v13_col_sn_2018$v2clrgstch_2) %>%
  mutate(cw_c2m = eCDF_2t3m*v13_col_sn_2018$v2clrgwkch_2)
# 3: proportion who selected more economically developed
ED18 <- ED18 %>% 
  mutate(el_c3m = eCDF_2t3m*v13_col_sn_2018$v2elsnlfc_3) %>%
  mutate(em_c3m = eCDF_2t3m*v13_col_sn_2018$v2elsnmrfc_3) %>%
  mutate(cs_c3m = eCDF_2t3m*v13_col_sn_2018$v2clrgstch_3) %>%
  mutate(cw_c3m = eCDF_2t3m*v13_col_sn_2018$v2clrgwkch_3)


#---- 4-5 Interaction ----#

# 4-5. proportion who selected inside capital city and outside
CEDE18 <- CEDE18 %>% 
  mutate(el_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2elsnlfc_4, v13_col_sn_2018$v2elsnlfc_5*eCDF_4t5)) %>%
  mutate(em_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2elsnmrfc_4, v13_col_sn_2018$v2elsnmrfc_5*eCDF_4t5)) %>%
  mutate(cs_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2clrgstch_4, v13_col_sn_2018$v2clrgstch_5*eCDF_4t5)) %>%
  mutate(cw_c4t5 = ifelse(MPIO_CDPMP == 11001, v13_col_sn_2018$v2clrgwkch_4, v13_col_sn_2018$v2clrgwkch_5*eCDF_4t5))


#---- 6-9 Interaction ----#

# "less free and fair subnational elections"
reg <- reg %>% 
  mutate(el_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2elsnlfc_6,  
    cdir6t9 == "South" ~ v13_col_sn_2018$v2elsnlfc_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2elsnlfc_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2elsnlfc_9,
    TRUE ~ NA_real_ 
  ))
# "more free and fair subnational elections"
reg <- reg %>% 
  mutate(em_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2elsnmrfc_6,
    cdir6t9 == "South" ~ v13_col_sn_2018$v2elsnmrfc_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2elsnmrfc_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2elsnmrfc_9,
    TRUE ~ NA_real_ 
  ))
# "stronger civil liberties"
reg <- reg %>% 
  mutate(cs_t6t9 = case_when(
    cdir6t9 == "North" ~ v13_col_sn_2018$v2clrgstch_6, 
    cdir6t9 == "South" ~ v13_col_sn_2018$v2clrgstch_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2clrgstch_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2clrgstch_9,
    TRUE ~ NA_real_
  ))
# "weaker civil liberties"
reg <- reg %>% 
  mutate(cw_t6t9 = case_when( 
    cdir6t9 == "North" ~ v13_col_sn_2018$v2clrgwkch_6,  
    cdir6t9 == "South" ~ v13_col_sn_2018$v2clrgwkch_7,
    cdir6t9 == "West" ~ v13_col_sn_2018$v2clrgwkch_8,  
    cdir6t9 == "East" ~ v13_col_sn_2018$v2clrgwkch_9,
    TRUE ~ NA_real_ 
  ))

#---- 15-16 Interaction ----#

# Option #1: interact V-Dem with margin-of-victory
eCDF_15t6 <- ecdf(RP18$MOV_pct) #abnormal distribution, so we use eCDF
RP18$eCDF_15t6 <- eCDF_15t6(RP18$MOV_pct) # municipalities

# 15-16. proportion votes for ruling party (Duque) 
# 15: proportion of respondents who selected "Areas where the national ruling party or group is strong":
RP18 <- RP18 %>% 
  mutate(el_c15 = eCDF_15t6*v13_col_sn_2018$v2elsnlfc_15) %>%
  mutate(em_c15 = eCDF_15t6*v13_col_sn_2018$v2elsnmrfc_15) %>%
  mutate(cs_c15 = eCDF_15t6*v13_col_sn_2018$v2clrgstch_15) %>%
  mutate(cw_c15 = eCDF_15t6*v13_col_sn_2018$v2clrgwkch_15)
# 16: proportion of respondents who selected "Areas where the national ruling party or group is weak":
RP18 <- RP18 %>% 
  mutate(el_c16 = eCDF_15t6*v13_col_sn_2018$v2elsnlfc_16) %>%
  mutate(em_c16 = eCDF_15t6*v13_col_sn_2018$v2elsnmrfc_16) %>%
  mutate(cs_c16 = eCDF_15t6*v13_col_sn_2018$v2clrgstch_16) %>%
  mutate(cw_c16 = eCDF_15t6*v13_col_sn_2018$v2clrgwkch_16)


