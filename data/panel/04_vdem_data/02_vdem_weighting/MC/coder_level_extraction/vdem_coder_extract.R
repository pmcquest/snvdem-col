# Code for extracting V-Dem data 
# Includes: Highest posterior density (HPD) and coder-level data
# Author: Michael Coppedge (Summer 2025)
# Revised by: Patrick McQuestion (January 10, 2026)

library(tidyverse)

v15cl <- read.csv("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/V-Dem/Coder-Level-Dataset-v15.csv")
v15cl$year <- lubridate::year(v15cl$historical_date)

9 Electoral Indicators:
  Regional government elected (A,C) (v2elsrgel) 
  Local government elected (A,C) (v2ellocelc)
  Subnational elections free and fair (C) (v2elffelr) # used here
  Subnational elections held (C) (v2elffelrbin)
  Subnational election unevenness (C) (v2elsnlsff) # used here
  Subnational election area less free and fair name (C) (v2elsnless)
  Subnational election area less free and fair characteristics (C) (v2elsnlfc) # used here
  Subnational election area more free and fair name (C) (v2elsnmore)  
  Subnational election area more free and fair characteristics (C) (v2elsnmrfc) # used here

4 Civil Liberties Indicators: 
  Subnational civil liberties unevenness (C) (v2clrgunev) # used here
  Stronger civil liberties characteristics (C) (v2clrgstch) # used here
  Weaker civil liberties population (C) (v2clsnlpct) # used here
  Weaker civil liberties characteristics (C) (v2clrgwkch) # used here

Also: 
  Regional offices relative power (C) (v2elrgpwr), 
  Local offices relative power (C) (v2ellocpwr).  

summary(v15cl$v2clrgunev_beta)

Concepts we can measure:
  national-level clean elections
  subnational elections free and fair 
  Whether subnational elections are held
  subnational election unevenness
  where subnational elections are less free and fair
  where subnational elections are more free and fair
  
So the subnational level can set the mean, 
  unevenness (0 = same, 1 = somewhat, 2 = significant) can set the range of variation around the mean,
    the Highest Posterior Density (HPD) for level can determine a range for "somewhat" different,
    and 2*HPD could be the range for "significantly" different.
and "where" can determine how much each location varies within that range.
Unevenness can interact with the coder scores. 
  subnational level +/- (% of coders * range of variation (0 or 1 HPD or 2 HPD) * 
                            relevance of the criterion)

A bit different for civil liberties
  We have national civil liberties level
  And % of population with less CL (but not more CL)
    These two could serve to define the mean SN level:
      national level - (range of national level*% less).
  Also CL unevenness, which could define the range of variation.
  And as with elections, location could determine the how much a location varies within that range.
  
# Subnational elections less free and fair ----

  # Unevenness (range of variation around the mean):
snlsff <- filter(v15cl, year>1899 & !is.na(v2elsnlsff)) %>%
  group_by(country_text_id, year, v2elsnlsff) %>%
  summarise(freq = n(), .groups = "drop") %>%
pivot_wider(
  names_from = v2elsnlsff,
  values_from = freq,
  values_fill = list(freq = 0)) %>%
  rename(snlsff_2 = "2", # count of coders who believe there is no significant unevenness
         snlsff_1 = "1", # count of coders who believe some unevenness
         snlsff_0 = "0") # count of coders who believe there is significant unevenness

  # SN Elections free and fair (count of coders per response)
ffelr <- filter(v15cl, year>1899 & !is.na(v2elffelr)) %>%
  group_by(country_text_id, year, v2elffelr) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elffelr,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(ffelr_4 = "4", # No not at all fair
         ffelr_3 = "3", # Not really fair
         ffelr_2 = "2", # Ambiguous
         ffelr_1 = "1", # Yes, somehwat
         ffelr_0 = "0") %>% # Yes free and fair
  select(country_text_id, year, ffelr_0, ffelr_1, ffelr_2, ffelr_3, ffelr_4)

# Getting the HPDs ----
library(haven)
library(vdemdata)
v15 <- vdem

HPDs <- filter(v15, year>1899) %>%
  mutate(HPD = v2elffelr_codehigh - v2elffelr_codelow) %>%
  select(country_text_id, year, HPD, v2elffelr)

# Merge into snlsff
snlsffHPD <- merge(snlsff, HPDs, by=c("country_text_id", "year"), all.x = TRUE)
  
# Now you can calculate the maximum weighted range and bounds
snlsffHPD <- snlsffHPD %>%
  mutate(weighted_range = ((2-snlsff_1)*HPD + (2-snlsff_0)*HPD*2)/(snlsff_0 + snlsff_1 + snlsff_2))

## Plot distribution ----
# all countries
ggplot(snlsffHPD, aes(x = weighted_range)) +
  geom_line(stat = "density")

# Colombia only
ggplot(filter(snlsffHPD, country_text_id=="COL"), aes(x = weighted_range)) +
  geom_line(stat = "density")
# That's only 0.2=0.6, a much smaller range, probably because there is little
# uncertainty about how uneven subnational elections are. 

# Range plot
ggplot(filter(snlsffHPD, country_text_id=="COL"), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = v2elffelr+weighted_range, ymin = v2elffelr - weighted_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(-3.6,3.3)) +
  theme_light() + labs(x = "", title = "Colombia",
                       y = "maximum national range for free and\nfair subnational elections in Colombia")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/colrange-elections.png", device = "png", height = 4, width = 6, units = "in", dpi = 300)

# Subnational civil liberties ----
## Getting the HPDs ----

# Scaling CL index to reflect percentange of subnational population affected by weak CL
CLHPDs <- filter(v15, year>1899) %>%
  mutate(CLSNmean = v2x_civlib*(100-v2clsnlpct)/100,
         CLHPD = v2x_civlib_codehigh*(100-v2clsnlpct)/100 - v2x_civlib_codelow*(100-v2clsnlpct)/100) %>%
  select(country_text_id, year, CLHPD, CLSNmean, v2x_civlib, v2x_civlib_codelow, v2x_civlib_codehigh)

# making SN unevenness factor variables
clrgunev <- filter(v15cl, year>1899 & !is.na(v2clrgunev)) %>%
  group_by(country_text_id, year, v2clrgunev) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2clrgunev,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(clrgunev_2 = "2", # count of experts who say CL are equally protected across SN regions
         clrgunev_1 = "1", # count of experts who say there is some geographic variation
         clrgunev_0 = "0") # count of experts who say there is significant geographic variation

# SN HPD (merged El. and CL) ----
# Merge into snlsff (which already has the clean elections variables)
SNHPD <- merge(snlsffHPD, clrgunev, by=c("country_text_id", "year"), all.x = TRUE)
SNHPD <- merge(CLHPDs, clrgunev, by=c("country_text_id", "year"), all.x = TRUE)

# Now you can calculate the maximum weighted range and bounds
SNHPD <- SNHPD %>%
  mutate(wtdCL_range = ((2-clrgunev_1)*CLHPD + (2-clrgunev_0)*CLHPD*2)/(clrgunev_0 + clrgunev_1 + clrgunev_2))


## Save HPD dataframe ----
write_rds(SNHPD, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/SNHPD.rds")

# Calculating and graphing the subnational CL means and ranges
ggplot(filter(SNHPD, country_text_id=="COL"), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = CLSNmean+wtdCL_range, ymin = CLSNmean - wtdCL_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light() + labs(x = "", title = "Colombia",
                       y = "maximum national range for\nsubnational civil liberties")
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/colrange-CL.png", device = "png", height = 4, width = 6, units = "in", dpi = 300)


## Replicating this for other countries ----
## Note: The max range is shown by the two black horizontal lines.

country <- "ARG"

# Elections
ggplot(filter(snlsffHPD, country_text_id==country), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = v2elffelr+weighted_range, ymin = v2elffelr - weighted_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  geom_hline(yintercept = c(-3.6, 3.3)) +
  theme_light() + labs(x = "", title = country,
                       y = "maximum national range for free and\nfair subnational elections")
# CL
ggplot(filter(SNHPD, country_text_id==country), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = CLSNmean+wtdCL_range, ymin = CLSNmean - wtdCL_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light() + labs(x = "", title = country,
                       y = "maximum national range for\nsubnational civil liberties")


# Expert weights ("relevance") for criteria ----

# Use v2elsnlfc and v2elsnmrfc in v15cl
# table(v15cl$v2elsnlfc_1)

# Logic: V-Dem experts are asked "In which areas are elections less (lfc) or more (mrfc) free and fair?" followed by 21 criteria for which to respond. 
# We will extract the number of experts that respond (value = 1) and that don't respond (value = 0) for each of the criteria.

# Function to process and merge criteria 0 through 21
get_vdem_criteria_all <- function(data, prefix) {
  df_list <- list()
  for (i in 0:21) {
    var_name <- paste0(prefix, i)
    # Process the specific variable
    tmp <- data %>%
      filter(year > 1899 & !is.na(.data[[var_name]])) %>%
      group_by(country_text_id, year, !!sym(var_name)) %>%
      summarise(freq = n(), .groups = "drop") %>%
      pivot_wider(
        names_from = !!sym(var_name),
        values_from = freq,
        values_fill = list(freq = 0)
      )
    # Rename columns to the specific standard you set
    # Note: Using col names like "1" and "0" from pivot_wider
    new_col_1 <- paste0(var_name, "_1")
    new_col_0 <- paste0(var_name, "_0")
    # Ensure columns "1" or "0" exist before renaming (handles cases with missing responses)
    if ("1" %in% colnames(tmp)) tmp <- rename(tmp, !!new_col_1 := "1")
    if ("0" %in% colnames(tmp)) tmp <- rename(tmp, !!new_col_0 := "0")
    df_list[[i + 1]] <- tmp
  }
  # 3. Use reduce to join all dataframes in the list by country and year
  # This replaces the complicated cbind/indexing approach
  final_df <- df_list %>% reduce(full_join, by = c("country_text_id", "year"))
  
  return(final_df)
}

## Elections less free and fair ----

snlfc_all <- get_vdem_criteria_all(v15cl, "v2elsnlfc_")

### Merge in coder frequencies ----

# Merge in the frequencies of coders who said sn elections were the same subnationally (snlsff_2)
snlfc_all_even <- merge(snlsffHPD[,1:3], snlfc_all, by = c("country_text_id", "year"), 
                    all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cn_proportions <- snlfc_all_even %>%
  mutate(pr_0 = v2elsnlfc_0_1/(v2elsnlfc_0_1 + v2elsnlfc_0_0 + snlsff_2),
         pr_1 = v2elsnlfc_1_1/(v2elsnlfc_1_1 + v2elsnlfc_1_0 + snlsff_2),
         pr_2 = v2elsnlfc_2_1/(v2elsnlfc_2_1 + v2elsnlfc_2_0 + snlsff_2), 
         pr_3 = v2elsnlfc_3_1/(v2elsnlfc_3_1 + v2elsnlfc_3_0 + snlsff_2), 
         pr_4 = v2elsnlfc_4_1/(v2elsnlfc_4_1 + v2elsnlfc_4_0 + snlsff_2), 
         pr_5 = v2elsnlfc_5_1/(v2elsnlfc_5_1 + v2elsnlfc_5_0 + snlsff_2),
         pr_6 = v2elsnlfc_6_1/(v2elsnlfc_6_1 + v2elsnlfc_6_0 + snlsff_2),
         pr_7 = v2elsnlfc_7_1/(v2elsnlfc_7_1 + v2elsnlfc_7_0 + snlsff_2),
         pr_8 = v2elsnlfc_8_1/(v2elsnlfc_8_1 + v2elsnlfc_8_0 + snlsff_2),
         pr_9 = v2elsnlfc_9_1/(v2elsnlfc_9_1 + v2elsnlfc_9_0 + snlsff_2),
         pr_10 = v2elsnlfc_10_1/(v2elsnlfc_10_1 + v2elsnlfc_10_0 + snlsff_2),
         pr_11 = v2elsnlfc_11_1/(v2elsnlfc_11_1 + v2elsnlfc_11_0 + snlsff_2),
         pr_12 = v2elsnlfc_12_1/(v2elsnlfc_12_1 + v2elsnlfc_12_0 + snlsff_2),
         pr_13 = v2elsnlfc_13_1/(v2elsnlfc_13_1 + v2elsnlfc_13_0 + snlsff_2),
         pr_14 = v2elsnlfc_14_1/(v2elsnlfc_14_1 + v2elsnlfc_14_0 + snlsff_2),
         pr_15 = v2elsnlfc_15_1/(v2elsnlfc_15_1 + v2elsnlfc_15_0 + snlsff_2),
         pr_16 = v2elsnlfc_16_1/(v2elsnlfc_16_1 + v2elsnlfc_16_0 + snlsff_2),
         pr_17 = v2elsnlfc_17_1/(v2elsnlfc_17_1 + v2elsnlfc_17_0 + snlsff_2),
         pr_18 = v2elsnlfc_18_1/(v2elsnlfc_18_1 + v2elsnlfc_18_0 + snlsff_2),
         pr_19 = v2elsnlfc_19_1/(v2elsnlfc_19_1 + v2elsnlfc_19_0 + snlsff_2),
         pr_20 = v2elsnlfc_20_1/(v2elsnlfc_20_1 + v2elsnlfc_20_0 + snlsff_2),
         pr_21 = v2elsnlfc_21_1/(v2elsnlfc_21_1 + v2elsnlfc_21_0 + snlsff_2))
cn_proportions <- cn_proportions[c(1:2, 48:69)]

# Full sample
colnames(cn_proportions) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cn_table_long <- filter(cn_proportions, !is.na(Rural)) %>%
  pivot_longer(cols = !c(country_text_id, year),
               names_to = "dimension",
               values_to = "weight")


library(ggridges)

ggplot(filter(cn_table_long, !is.nan(weight)), aes(y=reorder(dimension, weight), x=weight)) +
geom_density_ridges(scale = 2, fill="red", alpha = .5) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in full sample", 
       subtitle = "Subnational elections less free and fair",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgelfc.png", device = "png", height=9, width =6, units="in", dpi=300)

## Elections more free and fair ----

snmrfc_all <- get_vdem_criteria_all(v15cl, "v2elsnmrfc_")

### Merge in coder frequencies ----

# Merge in the frequencies of coders who said sn elections were the same subnationally (snlsff_2)
snmrfc_all_even <- merge(snlsffHPD[,1:3], snmrfc_all, by = c("country_text_id", "year"), 
                        all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cnm_proportions <- snmrfc_all_even %>%
  mutate(pr_0 = v2elsnmrfc_0_1/(v2elsnmrfc_0_1 + v2elsnmrfc_0_0 + snlsff_2),
         pr_1 = v2elsnmrfc_1_1/(v2elsnmrfc_1_1 + v2elsnmrfc_1_0 + snlsff_2),
         pr_2 = v2elsnmrfc_2_1/(v2elsnmrfc_2_1 + v2elsnmrfc_2_0 + snlsff_2), 
         pr_3 = v2elsnmrfc_3_1/(v2elsnmrfc_3_1 + v2elsnmrfc_3_0 + snlsff_2), 
         pr_4 = v2elsnmrfc_4_1/(v2elsnmrfc_4_1 + v2elsnmrfc_4_0 + snlsff_2), 
         pr_5 = v2elsnmrfc_5_1/(v2elsnmrfc_5_1 + v2elsnmrfc_5_0 + snlsff_2),
         pr_6 = v2elsnmrfc_6_1/(v2elsnmrfc_6_1 + v2elsnmrfc_6_0 + snlsff_2),
         pr_7 = v2elsnmrfc_7_1/(v2elsnmrfc_7_1 + v2elsnmrfc_7_0 + snlsff_2),
         pr_8 = v2elsnmrfc_8_1/(v2elsnmrfc_8_1 + v2elsnmrfc_8_0 + snlsff_2),
         pr_9 = v2elsnmrfc_9_1/(v2elsnmrfc_9_1 + v2elsnmrfc_9_0 + snlsff_2),
         pr_10 = v2elsnmrfc_10_1/(v2elsnmrfc_10_1 + v2elsnmrfc_10_0 + snlsff_2),
         pr_11 = v2elsnmrfc_11_1/(v2elsnmrfc_11_1 + v2elsnmrfc_11_0 + snlsff_2),
         pr_12 = v2elsnmrfc_12_1/(v2elsnmrfc_12_1 + v2elsnmrfc_12_0 + snlsff_2),
         pr_13 = v2elsnmrfc_13_1/(v2elsnmrfc_13_1 + v2elsnmrfc_13_0 + snlsff_2),
         pr_14 = v2elsnmrfc_14_1/(v2elsnmrfc_14_1 + v2elsnmrfc_14_0 + snlsff_2),
         pr_15 = v2elsnmrfc_15_1/(v2elsnmrfc_15_1 + v2elsnmrfc_15_0 + snlsff_2),
         pr_16 = v2elsnmrfc_16_1/(v2elsnmrfc_16_1 + v2elsnmrfc_16_0 + snlsff_2),
         pr_17 = v2elsnmrfc_17_1/(v2elsnmrfc_17_1 + v2elsnmrfc_17_0 + snlsff_2),
         pr_18 = v2elsnmrfc_18_1/(v2elsnmrfc_18_1 + v2elsnmrfc_18_0 + snlsff_2),
         pr_19 = v2elsnmrfc_19_1/(v2elsnmrfc_19_1 + v2elsnmrfc_19_0 + snlsff_2),
         pr_20 = v2elsnmrfc_20_1/(v2elsnmrfc_20_1 + v2elsnmrfc_20_0 + snlsff_2),
         pr_21 = v2elsnmrfc_21_1/(v2elsnmrfc_21_1 + v2elsnmrfc_21_0 + snlsff_2))
cnm_proportions <- cnm_proportions[c(1:2, 48:69)]


# First look, 2018 only
# cn_table2018 <- select(cn_table2018, c(1:2, 48:69))
cnm_table2018 <- filter(cnm_proportions, year==2018) 
colnames(cnm_table2018) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cnm_table2018_long <- filter(cnm_table2018[,c(1, 3:24)], !is.na(Rural)) %>%
  pivot_longer(cols = !country_text_id, 
               names_to = "dimension",
               values_to = "weight")


library(ggridges)

ggplot(filter(cnm_table2018_long, !is.nan(weight)), 
       aes(y=reorder(dimension, weight), x=weight)) +
  geom_density_ridges(scale = 2, fill="blue") +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in 2018, all countries", 
       subtitle = "Subnational elections more free and fair",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgemrfc2018.png", device = "png", height=9, width =6, units="in", dpi=300)


### Full sample, all countries 1900-2024 ----
colnames(cnm_proportions) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cnm_table_long <- filter(cnm_proportions, !is.na(Rural)) %>%
  pivot_longer(cols = !c(country_text_id, year),
               names_to = "dimension",
               values_to = "weight")


ggplot(filter(cnm_table_long, !is.nan(weight)), aes(y=reorder(dimension, weight), x=weight)) +
  geom_density_ridges(scale = 2, fill="blue", alpha = .5) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in full sample", 
       subtitle = "Subnational elections more free and fair",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgemrfc.png", device = "png", height=9, width =6, units="in", dpi=300)


## Civil liberties stronger ----

snclst_all <- get_vdem_criteria_all(v15cl, "v2clrgstch_")

### Merge in coder frequencies ----

# Merge in the frequencies of coders who said sn civil liberties were the same subnationally (clrgunev_2)
snclst_all_even <- merge(SNHPD[,c(1,2,14)], snclst_all, by = c("country_text_id", "year"), 
                        all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cnc_proportions <- snclst_all_even %>%
  mutate(pr_0 = v2clrgstch_0_1/(v2clrgstch_0_1 + v2clrgstch_0_0 + clrgunev_2),
         pr_1 = v2clrgstch_1_1/(v2clrgstch_1_1 + v2clrgstch_1_0 + clrgunev_2),
         pr_2 = v2clrgstch_2_1/(v2clrgstch_2_1 + v2clrgstch_2_0 + clrgunev_2), 
         pr_3 = v2clrgstch_3_1/(v2clrgstch_3_1 + v2clrgstch_3_0 + clrgunev_2), 
         pr_4 = v2clrgstch_4_1/(v2clrgstch_4_1 + v2clrgstch_4_0 + clrgunev_2), 
         pr_5 = v2clrgstch_5_1/(v2clrgstch_5_1 + v2clrgstch_5_0 + clrgunev_2),
         pr_6 = v2clrgstch_6_1/(v2clrgstch_6_1 + v2clrgstch_6_0 + clrgunev_2),
         pr_7 = v2clrgstch_7_1/(v2clrgstch_7_1 + v2clrgstch_7_0 + clrgunev_2),
         pr_8 = v2clrgstch_8_1/(v2clrgstch_8_1 + v2clrgstch_8_0 + clrgunev_2),
         pr_9 = v2clrgstch_9_1/(v2clrgstch_9_1 + v2clrgstch_9_0 + clrgunev_2),
         pr_10 = v2clrgstch_10_1/(v2clrgstch_10_1 + v2clrgstch_10_0 + clrgunev_2),
         pr_11 = v2clrgstch_11_1/(v2clrgstch_11_1 + v2clrgstch_11_0 + clrgunev_2),
         pr_12 = v2clrgstch_12_1/(v2clrgstch_12_1 + v2clrgstch_12_0 + clrgunev_2),
         pr_13 = v2clrgstch_13_1/(v2clrgstch_13_1 + v2clrgstch_13_0 + clrgunev_2),
         pr_14 = v2clrgstch_14_1/(v2clrgstch_14_1 + v2clrgstch_14_0 + clrgunev_2),
         pr_15 = v2clrgstch_15_1/(v2clrgstch_15_1 + v2clrgstch_15_0 + clrgunev_2),
         pr_16 = v2clrgstch_16_1/(v2clrgstch_16_1 + v2clrgstch_16_0 + clrgunev_2),
         pr_17 = v2clrgstch_17_1/(v2clrgstch_17_1 + v2clrgstch_17_0 + clrgunev_2),
         pr_18 = v2clrgstch_18_1/(v2clrgstch_18_1 + v2clrgstch_18_0 + clrgunev_2),
         pr_19 = v2clrgstch_19_1/(v2clrgstch_19_1 + v2clrgstch_19_0 + clrgunev_2),
         pr_20 = v2clrgstch_20_1/(v2clrgstch_20_1 + v2clrgstch_20_0 + clrgunev_2),
         pr_21 = v2clrgstch_21_1/(v2clrgstch_21_1 + v2clrgstch_21_0 + clrgunev_2))
cnc_proportions <- cnc_proportions[c(1:2, 48:69)]

# Full sample
colnames(cnc_proportions) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cnc_table_long <- filter(cnc_proportions, !is.na(Rural)) %>%
  pivot_longer(cols = !c(country_text_id, year),
               names_to = "dimension",
               values_to = "weight")


ggplot(filter(cnc_table_long, !is.nan(weight)), aes(y=reorder(dimension, weight), x=weight)) +
  geom_density_ridges(scale = 2, fill="green", alpha = .5) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in full sample", 
       subtitle = "Subnational civil liberties stronger",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgeclst.png", device = "png", height=9, width =6, units="in", dpi=300)


## Civil liberties weaker ----

snclwk_all <- get_vdem_criteria_all(v15cl, "v2clrgwkch_")

### Merge in coder frequencies ----

# Merge in the frequencies of coders who said sn civil liberties were the same subnationally (clrgunev_2)
snclwk_all_even <- merge(SNHPD[,c(1,2,14)], snclwk_all, by = c("country_text_id", "year"), 
                         all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cnk_proportions <- snclwk_all_even %>%
  mutate(pr_0 = v2clrgwkch_0_1/(v2clrgwkch_0_1 + v2clrgwkch_0_0 + clrgunev_2),
         pr_1 = v2clrgwkch_1_1/(v2clrgwkch_1_1 + v2clrgwkch_1_0 + clrgunev_2),
         pr_2 = v2clrgwkch_2_1/(v2clrgwkch_2_1 + v2clrgwkch_2_0 + clrgunev_2), 
         pr_3 = v2clrgwkch_3_1/(v2clrgwkch_3_1 + v2clrgwkch_3_0 + clrgunev_2), 
         pr_4 = v2clrgwkch_4_1/(v2clrgwkch_4_1 + v2clrgwkch_4_0 + clrgunev_2), 
         pr_5 = v2clrgwkch_5_1/(v2clrgwkch_5_1 + v2clrgwkch_5_0 + clrgunev_2),
         pr_6 = v2clrgwkch_6_1/(v2clrgwkch_6_1 + v2clrgwkch_6_0 + clrgunev_2),
         pr_7 = v2clrgwkch_7_1/(v2clrgwkch_7_1 + v2clrgwkch_7_0 + clrgunev_2),
         pr_8 = v2clrgwkch_8_1/(v2clrgwkch_8_1 + v2clrgwkch_8_0 + clrgunev_2),
         pr_9 = v2clrgwkch_9_1/(v2clrgwkch_9_1 + v2clrgwkch_9_0 + clrgunev_2),
         pr_10 = v2clrgwkch_10_1/(v2clrgwkch_10_1 + v2clrgwkch_10_0 + clrgunev_2),
         pr_11 = v2clrgwkch_11_1/(v2clrgwkch_11_1 + v2clrgwkch_11_0 + clrgunev_2),
         pr_12 = v2clrgwkch_12_1/(v2clrgwkch_12_1 + v2clrgwkch_12_0 + clrgunev_2),
         pr_13 = v2clrgwkch_13_1/(v2clrgwkch_13_1 + v2clrgwkch_13_0 + clrgunev_2),
         pr_14 = v2clrgwkch_14_1/(v2clrgwkch_14_1 + v2clrgwkch_14_0 + clrgunev_2),
         pr_15 = v2clrgwkch_15_1/(v2clrgwkch_15_1 + v2clrgwkch_15_0 + clrgunev_2),
         pr_16 = v2clrgwkch_16_1/(v2clrgwkch_16_1 + v2clrgwkch_16_0 + clrgunev_2),
         pr_17 = v2clrgwkch_17_1/(v2clrgwkch_17_1 + v2clrgwkch_17_0 + clrgunev_2),
         pr_18 = v2clrgwkch_18_1/(v2clrgwkch_18_1 + v2clrgwkch_18_0 + clrgunev_2),
         pr_19 = v2clrgwkch_19_1/(v2clrgwkch_19_1 + v2clrgwkch_19_0 + clrgunev_2),
         pr_20 = v2clrgwkch_20_1/(v2clrgwkch_20_1 + v2clrgwkch_20_0 + clrgunev_2),
         pr_21 = v2clrgwkch_21_1/(v2clrgwkch_21_1 + v2clrgwkch_21_0 + clrgunev_2))
cnk_proportions <- cnk_proportions[c(1:2, 48:69)]

# Full sample
colnames(cnk_proportions) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cnk_table_long <- filter(cnk_proportions, !is.na(Rural)) %>%
  pivot_longer(cols = !c(country_text_id, year),
               names_to = "dimension",
               values_to = "weight")


ggplot(filter(cnk_table_long, !is.nan(weight)), aes(y=reorder(dimension, weight), x=weight)) +
  geom_density_ridges(scale = 2, fill="pink", alpha = .5) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in full sample", 
       subtitle = "Subnational civil liberties weaker",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgeclwk.png", device = "png", height=9, width =6, units="in", dpi=300)



## Calculate More minus Less free elections, Strong minus Weak civil liberties ----
# Rename weight in the long proportions dfs
# Elections
cnm_long <- cnm_table_long %>% 
  rename(weight_more = "weight")
cn_long <- cn_table_long %>%
  rename(weight_less = "weight")
# Civil liberties
cnc_long <- cnc_table_long %>% 
  rename(weight_strong = "weight")
cnk_long <- cnk_table_long %>%
  rename(weight_weak = "weight")

# merge them, keeping all observations
more_vs_less <- merge(cnm_long, cn_long, by = c("country_text_id", "year", "dimension"), all = TRUE)
strong_vs_weak <- merge(cnc_long, cnk_long, by = c("country_text_id", "year", "dimension"), all = TRUE)

# Calculate the difference in weights (more - less)
more_vs_less <- more_vs_less %>%
  mutate(weight_diff = weight_more - weight_less)
order <- more_vs_less%>%
  group_by(dimension) %>%
  summarize(rank = mean(weight_diff, na.rm = TRUE))
more_vs_less <- merge(more_vs_less, order, by = "dimension", all.x = TRUE)
# Calculate the difference in weights (strong - weak)
strong_vs_weak <- strong_vs_weak %>%
  mutate(weight_diff = weight_strong - weight_weak)
order <- strong_vs_weak%>%
  group_by(dimension) %>%
  summarize(rank = mean(weight_diff, na.rm = TRUE))
strong_vs_weak <- merge(strong_vs_weak, order, by = "dimension", all.x = TRUE)


#---- Save datasets ----
# Elections Long
write_rds(more_vs_less, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/more_vs_less.rds")
# Elections Wide
more_vs_less_wide <- more_vs_less %>%
  pivot_wider(id_cols = c(country_text_id, year),
              names_from=dimension,
              values_from=weight_diff) %>%
  arrange(country_text_id, year)
colnames(more_vs_less_wide) <- c("country_text_id", "year", "Civil_unrest", "East", "Illicit_activity", "Indigenous", "Inside_capital", "Less_development", "Longer_foreign_rule", "More_development", "No_foreign_rule", "None_of_the_above", "North", "Outside_capital", "Recent_foreign_rule", "Remote", "Ruling_party_strong", "Ruling_party_weak", "Rural", "Shorter_foreign_rule", "South", "Sparse_population", "Urban", "West")
write_rds(more_vs_less_wide, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/more_vs_less_wide.rds")

# Civil liberties Long
write_rds(strong_vs_weak, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/strong_vs_weak.rds")
# Civil Liberties Wide
strong_vs_weak_wide <- strong_vs_weak %>%
  pivot_wider(id_cols = c(country_text_id, year),
              names_from=dimension,
              values_from=weight_diff) %>%
  arrange(country_text_id, year)
colnames(strong_vs_weak_wide) <- c("country_text_id", "year", "Civil_unrest", "East", "Illicit_activity", "Indigenous", "Inside_capital", "Less_development", "Longer_foreign_rule", "More_development", "No_foreign_rule", "None_of_the_above", "North", "Outside_capital", "Recent_foreign_rule", "Remote", "Ruling_party_strong", "Ruling_party_weak", "Rural", "Shorter_foreign_rule", "South", "Sparse_population", "Urban", "West")
write_rds(strong_vs_weak_wide, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/strong_vs_weak_wide.rds")


# ---- Assemble ELCLweights_wide-v2.dta ----
# Reproduces MC's ELCLweights_wide.dta format with corrected denominators.
# MC's weighting_summer2025.R had a copy-paste bug: criteria 2 (Less development),
# 3 (More development), and 4 (Inside capital) used v2elsnlfc_1_0 in the denominator
# instead of the correct criterion-specific _0 count, for both lfc and mrfc proportions.
# This file uses the fixed proportions from get_vdem_criteria_all().

cols_criteria <- c("country_text_id", "year",
                   "Rural", "Urban",
                   "Less_development", "More_development",
                   "Inside_capital", "Outside_capital",
                   "North", "South", "West", "East",
                   "Civil_unrest", "Illicit_activity",
                   "Sparse_population", "Remote", "Indigenous",
                   "Ruling_party_strong", "Ruling_party_weak")

el_wide_v2 <- more_vs_less_wide %>%
  select(all_of(cols_criteria)) %>%
  rename_with(~ paste0("el_", .), .cols = -c(country_text_id, year))

cl_wide_v2 <- strong_vs_weak_wide %>%
  select(all_of(cols_criteria)) %>%
  rename_with(~ paste0("cl_", .), .cols = -c(country_text_id, year))

ELCLweights_wide_v2 <- inner_join(el_wide_v2, cl_wide_v2, by = c("country_text_id", "year"))

write_dta(ELCLweights_wide_v2,
          "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide-v2.dta")


# Read in data ----

library(haven)
more_vs_less_wide <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/more_vs_less_wide.rds")
strong_vs_weak_wide <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/strong_vs_weak_wide.rds")

# Ridgeline plot ----
ggplot(filter(more_vs_less, !is.nan(weight_diff)), 
       aes(y=reorder(dimension, rank), x=weight_diff)) +
  geom_density_ridges(scale = 2, fill="purple3", alpha = .7) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(subtitle="Expert weights in full sample", 
       title = "Difference in subnational elections\nmore and less free and fair",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/Ridgemoreless.png", device = "png", height=9, width =6, units="in", dpi=300)

# Historical comparisons for selected countries
selected <- c("COL", "USA", "RUS", "IND")

ggplot(filter(more_vs_less, country_text_id %in% selected & !str_detect(dimension, "foreign")),
       aes(x = year, y = weight_diff, color = country_text_id)) +
  geom_line(linewidth = .5) +
  facet_wrap(~reorder(dimension, rank), ncol = 6) +
  theme_light() + labs(x = "", color = "country", y = "elections better minus elections worse",
                       title = "Relevance of selected criteria for free and fair subnational elections",
                       caption = "Four criteria related to foreign rule are omitted because they are rarely considered important.")

ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/imgs/RelevanceFacets.png", device = "png", height=6.5, width =9, units="in", dpi=300)


# Colombia ----
COL_mvl <- more_vs_less_wide %>%
  filter(country_text_id == "COL")
COL_svw <- strong_vs_weak_wide %>%
  filter(country_text_id == "COL")

# Join weight data by year
# 1. Prefix variables for COL_mvl (except the first two ID columns)
COL_mvl2 <- COL_mvl %>%
  rename_with(~ paste0("emel_", .), -c(1:2))
# 2. Prefix variables for COL_svw (except the first two ID columns)
COL_svw2 <- COL_svw %>%
  rename_with(~ paste0("cscw_", .), -c(1:2))

# 3. Join them using the key variables (country_text_id and year)
COL_combined <- inner_join(COL_mvl2, COL_svw2, 
                           by = c("country_text_id", "year"))

## Convert to Long ----
# Define the ordered criteria mapping
criteria_map <- c(
  "0" = "Rural", "1" = "Urban", "2" = "Less development", "3" = "More development",
  "4" = "Inside capital", "5" = "Outside capital", "6" = "North", "7" = "South",
  "8" = "West", "9" = "East", "10" = "Civil unrest", "11" = "Illicit activity",
  "12" = "Sparse population", "13" = "Remote", "14" = "Indigenous",
  "15" = "Ruling party strong", "16" = "Ruling party weak"
)

# Convert map to a dataframe for easier joining
map_df <- enframe(criteria_map, name = "subset", value = "criteria_name")

# Prepare the wide data
COL_comb_long <- COL_combined %>%
  # Pivot ALL emel and cscw variables into a long format
  pivot_longer(
    cols = starts_with(c("emel_", "cscw_")),
    names_to = c(".value", "criteria_name"),
    names_pattern = "(emel|cscw)_(.*)"
  ) %>%
  # Clean up criteria names (replace underscores with spaces to match your list)
  mutate(criteria_name = str_replace_all(criteria_name, "_", " ")) %>%
  # Join with the map to get the 0-16 subset ID
  inner_join(map_df, by = "criteria_name") %>%
  # Keep only the columns we need for the final comparison
  select(year, subset, emel_score = emel, cscw_score = cscw)


# Save Colombia weights
write_rds(COL_comb_long, "G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/COL_combined.rds")

# Compare this to MC's data ----
COL_comb_long <- readRDS("data/panel/06_vdem_data/coder-level/COL_combined.rds")
ELCLweights_wide <- read_dta("data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide.dta")
View(ELCLweights_wide)

# MC's data
COL_sndem <- ELCLweights_wide %>%
  filter(country_text_id == "COL")

# Prepare the wide data
COL_comb_longMC <- COL_sndem %>%
  # 1. Pivot using the actual prefixes el_ and cl_
  pivot_longer(
    cols = starts_with(c("el_", "cl_")),
    names_to = c(".value", "criteria_name"),
    names_pattern = "(el|cl)_(.*)" # Changed pattern to match el_ and cl_
  ) %>%
  # 2. Clean up criteria names (replace underscores with spaces)
  mutate(criteria_name = str_replace_all(criteria_name, "_", " ")) %>%
  # 3. Join with map_df (ensure subset is character for joining)
  inner_join(map_df, by = "criteria_name") %>%
  # 4. Rename to your target names
  select(year, subset, emel_score = el, cscw_score = cl)


# Join the two dataframes to compare them directly
comparison_df <- COL_comb_long %>%
  rename(emel_1 = emel_score, cscw_1 = cscw_score) %>%
  inner_join(COL_comb_longMC, by = c("year", "subset"))

# Plotting Election scores comparison
ggplot(comparison_df, aes(x = emel_1, y = emel_score)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Consistency Check: emel_score comparison",
       x = "Score from COL_comb_long",
       y = "Score from COL_comb_longMC")
