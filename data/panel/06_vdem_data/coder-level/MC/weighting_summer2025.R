library(tidyverse)

Coder-Level-Dataset-v15 <- read.csv("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V15/V-Dem-Coder-Level-v15_csv/Coder-Level-Dataset-v15.csv")
`Coder-Level-Dataset-v15`$year <- lubridate::year(`Coder-Level-Dataset-v15`$historical_date)
v15cl <- `Coder-Level-Dataset-v15`

Regional government elected (A,C) (v2elsrgel) 
Local government elected (A,C) (v2ellocelc)
Subnational elections free and fair (C) (v2elffelr)
Subnational elections held (C) (v2elffelrbin)
Subnational election unevenness (C) (v2elsnlsff)
Subnational election area less free and fair name (C) (v2elsnless)
Subnational election area less free and fair characteristics (C) (v2elsnlfc) 
Subnational election area more free and fair name (C) (v2elsnmore)  
Subnational election area more free and fair characteristics (C) (v2elsnmrfc) 

4 Civil Liberties Indicators 
Subnational civil liberties unevenness (C) (v2clrgunev) 
Stronger civil liberties characteristics (C) (v2clrgstch) 
Weaker civil liberties population (C) (v2clsnlpct) 
Weaker civil liberties characteristics (C) (v2clrgwkch) 

Also Regional offices relative power (C) (v2elrgpwr), 
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
    the HPD for level can determine a range for "somewhat" different,
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
  
# Subnational elections less free and fair

snlsff <- filter(v15cl, year>1899 & !is.na(v2elsnlsff)) %>%
  group_by(country_text_id, year, v2elsnlsff) %>%
  summarise(freq = n(), .groups = "drop") %>%
pivot_wider(
  names_from = v2elsnlsff,
  values_from = freq,
  values_fill = list(freq = 0)) %>%
  rename(snlsff_2 = "2", 
         snlsff_1 = "1",
         snlsff_0 = "0")

ffelr <- filter(v15cl, year>1899 & !is.na(v2elffelr)) %>%
  group_by(country_text_id, year, v2elffelr) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elffelr,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(ffelr_4 = "4",
         ffelr_3 = "3",
         ffelr_2 = "2",
         ffelr_1 = "1",
         ffelr_0 = "0") %>%
  select(country_text_id, year, ffelr_0, ffelr_1, ffelr_2, ffelr_3, ffelr_4)

# Getting the HPDs
library(haven)
v15 <- read_dta("C:/Users/mcoppedg/Dropbox/MC/VDemFiles/Archive/V15/V-Dem-CY-Full+Others-v15.dta")

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

# distribution?
ggplot(snlsffHPD, aes(x = weighted_range)) +
  geom_line(stat = "density")
# Colombia only
ggplot(filter(snlsffHPD, country_text_id=="COL"), aes(x = weighted_range)) +
  geom_line(stat = "density")
# That's only 0.2=0.6, a much smaller range, probably because there is little
# uncertainty about how uneven subnational elections are. 
# 
ggplot(filter(snlsffHPD, country_text_id=="COL"), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = v2elffelr+weighted_range, ymin = v2elffelr - weighted_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(-3.6,3.3)) +
  theme_light() + labs(x = "", title = "Colombia",
                       y = "maximum national range for free and\nfair subnational elections")
ggsave(filename = "colrange.png", device = "png", height = 4, width = 6, units = "in", dpi = 300)

##The calculation for civil liberties
# Getting the HPDs

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
  rename(clrgunev_2 = "2", 
         clrgunev_1 = "1",
         clrgunev_0 = "0")

# Merge into snlsff (which already has the clean elections variables)
SNHPD <- merge(snlsffHPD, clrgunev, by=c("country_text_id", "year"), all.x = TRUE)
SNHPD <- merge(CLHPDs, clrgunev, by=c("country_text_id", "year"), all.x = TRUE)


# Now you can calculate the maximum weighted range and bounds
SNHPD <- SNHPD %>%
  mutate(wtdCL_range = ((2-clrgunev_1)*CLHPD + (2-clrgunev_0)*CLHPD*2)/(clrgunev_0 + clrgunev_1 + clrgunev_2))

# Calculating and graphing the subnational CL means and ranges
ggplot(filter(SNHPD, country_text_id=="COL"), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = CLSNmean+wtdCL_range, ymin = CLSNmean - wtdCL_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light() + labs(x = "", title = "Colombia",
                       y = "maximum national range for\nsubnational civil liberties")
ggsave(filename = "colrangeCL.png", device = "png", height = 4, width = 6, units = "in", dpi = 300)


## Replicating this for other countries. 
## Note: The max range is shown by the two black horizontal lines.
country <- "ARG"
ggplot(filter(snlsffHPD, country_text_id==country), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = v2elffelr+weighted_range, ymin = v2elffelr - weighted_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  geom_hline(yintercept = c(-3.6, 3.3)) +
  theme_light() + labs(x = "", title = country,
                       y = "maximum national range for free and\nfair subnational elections")

ggplot(filter(SNHPD, country_text_id==country), 
       aes(x = year)) +
  geom_ribbon(fill = "skyblue", 
              aes(ymax = CLSNmean+wtdCL_range, ymin = CLSNmean - wtdCL_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_light() + labs(x = "", title = country,
                       y = "maximum national range for\nsubnational civil liberties")


#### Expert weights for relevance criteria
# Use v2elsnlfc and v2elsnmrfc in v15cl
#table(v15cl$v2elsnlfc_1)
snlfc_0 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_0)) %>%
group_by(country_text_id, year, v2elsnlfc_0) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_0,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_0_1 = "1",
         v2elsnlfc_0_0 = "0") 

snlfc_1 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_1)) %>%
  group_by(country_text_id, year, v2elsnlfc_1) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_1,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_1_1 = "1",
         v2elsnlfc_1_0 = "0") 

snlfc_2 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_2)) %>%
  group_by(country_text_id, year, v2elsnlfc_2) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_2,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_2_1 = "1",
         v2elsnlfc_2_0 = "0")

snlfc_3 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_3)) %>%
  group_by(country_text_id, year, v2elsnlfc_3) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_3,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_3_1 = "1",
         v2elsnlfc_3_0 = "0")

snlfc_4 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_4)) %>%
  group_by(country_text_id, year, v2elsnlfc_4) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_4,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_4_1 = "1",
         v2elsnlfc_4_0 = "0")

snlfc_5 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_5)) %>%
  group_by(country_text_id, year, v2elsnlfc_5) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_5,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_5_1 = "1",
         v2elsnlfc_5_0 = "0")

snlfc_6 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_6)) %>%
  group_by(country_text_id, year, v2elsnlfc_6) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_6,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_6_1 = "1",
         v2elsnlfc_6_0 = "0")

snlfc_7 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_7)) %>%
  group_by(country_text_id, year, v2elsnlfc_7) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_7,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_7_1 = "1",
         v2elsnlfc_7_0 = "0")

snlfc_8 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_8)) %>%
  group_by(country_text_id, year, v2elsnlfc_8) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_8,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_8_1 = "1",
         v2elsnlfc_8_0 = "0")

snlfc_9 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_9)) %>%
  group_by(country_text_id, year, v2elsnlfc_9) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_9,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_9_1 = "1",
         v2elsnlfc_9_0 = "0")

snlfc_10 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_10)) %>%
  group_by(country_text_id, year, v2elsnlfc_10) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_10,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_10_1 = "1",
         v2elsnlfc_10_0 = "0")

snlfc_11 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_11)) %>%
  group_by(country_text_id, year, v2elsnlfc_11) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_11,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_11_1 = "1",
         v2elsnlfc_11_0 = "0")

snlfc_12 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_12)) %>%
  group_by(country_text_id, year, v2elsnlfc_12) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_12,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_12_1 = "1",
         v2elsnlfc_12_0 = "0")

snlfc_13 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_13)) %>%
  group_by(country_text_id, year, v2elsnlfc_13) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_13,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_13_1 = "1",
         v2elsnlfc_13_0 = "0")

snlfc_14 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_14)) %>%
  group_by(country_text_id, year, v2elsnlfc_14) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_14,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_14_1 = "1",
         v2elsnlfc_14_0 = "0")

snlfc_15 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_15)) %>%
  group_by(country_text_id, year, v2elsnlfc_15) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_15,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_15_1 = "1",
         v2elsnlfc_15_0 = "0")

snlfc_16 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_16)) %>%
  group_by(country_text_id, year, v2elsnlfc_16) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnlfc_16,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnlfc_16_1 = "1",
         v2elsnlfc_16_0 = "0")
         
snlfc_17 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_17)) %>%
           group_by(country_text_id, year, v2elsnlfc_17) %>%
           summarise(freq = n(), .groups = "drop") %>%
           pivot_wider(
             names_from = v2elsnlfc_17,
             values_from = freq,
             values_fill = list(freq = 0)) %>%
           rename(v2elsnlfc_17_1 = "1",
                  v2elsnlfc_17_0 = "0")
      
      snlfc_18 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_18)) %>%
        group_by(country_text_id, year, v2elsnlfc_18) %>%
        summarise(freq = n(), .groups = "drop") %>%
        pivot_wider(
          names_from = v2elsnlfc_18,
          values_from = freq,
          values_fill = list(freq = 0)) %>%
        rename(v2elsnlfc_18_1 = "1",
               v2elsnlfc_18_0 = "0")

      snlfc_19 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_19)) %>%
        group_by(country_text_id, year, v2elsnlfc_19) %>%
        summarise(freq = n(), .groups = "drop") %>%
        pivot_wider(
          names_from = v2elsnlfc_19,
          values_from = freq,
          values_fill = list(freq = 0)) %>%
        rename(v2elsnlfc_19_1 = "1",
               v2elsnlfc_19_0 = "0")
      
      snlfc_20 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_20)) %>%
        group_by(country_text_id, year, v2elsnlfc_20) %>%
        summarise(freq = n(), .groups = "drop") %>%
        pivot_wider(
          names_from = v2elsnlfc_20,
          values_from = freq,
          values_fill = list(freq = 0)) %>%
        rename(v2elsnlfc_20_1 = "1",
               v2elsnlfc_20_0 = "0")
      
      snlfc_21 <- filter(v15cl, year>1899 & !is.na(v2elsnlfc_21)) %>%
        group_by(country_text_id, year, v2elsnlfc_21) %>%
        summarise(freq = n(), .groups = "drop") %>%
        pivot_wider(
          names_from = v2elsnlfc_21,
          values_from = freq,
          values_fill = list(freq = 0)) %>%
        rename(v2elsnlfc_21_1 = "1",
               v2elsnlfc_21_0 = "0")

      # Merge all these dfs
snlfc_all <- as.data.frame(cbind(snlfc_0[,3:4], snlfc_1, snlfc_2[,3:4], snlfc_3[,3:4], snlfc_4[,3:4], snlfc_5[,3:4], 
                                 snlfc_6[,3:4], snlfc_7[,3:4], snlfc_8[,3:4], snlfc_9[,3:4], snlfc_10[,3:4], 
                                 snlfc_11[,3:4], snlfc_12[,3:4], snlfc_13[,3:4], snlfc_14[,3:4], snlfc_15[,3:4], 
                                 snlfc_16[,3:4], snlfc_17[,3:4], snlfc_18[,3:4], snlfc_19[,3:4], snlfc_20[,3:4], snlfc_21[,3:4]))
# Reordering columns
snlfc_all <- select(snlfc_all, country_text_id, year, v2elsnlfc_0_0, v2elsnlfc_0_1:last_col())


# Merge in the frequencies of coders who said sn elections were the same subnationally
snlfc_alll <- merge(snlsffHPD[,1:3], snlfc_all, by = c("country_text_id", "year"), 
                    all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cn_proportions <- snlfc_alll %>%
  mutate(pr_0 = v2elsnlfc_0_1/(v2elsnlfc_0_1 + v2elsnlfc_0_0 + snlsff_2),
         pr_1 = v2elsnlfc_1_1/(v2elsnlfc_1_1 + v2elsnlfc_1_0 + snlsff_2),
         pr_2 = v2elsnlfc_2_1/(v2elsnlfc_2_1 + v2elsnlfc_1_0 + snlsff_2),
         pr_3 = v2elsnlfc_3_1/(v2elsnlfc_3_1 + v2elsnlfc_1_0 + snlsff_2),
         pr_4 = v2elsnlfc_4_1/(v2elsnlfc_4_1 + v2elsnlfc_1_0 + snlsff_2),
         pr_6 = v2elsnlfc_6_1/(v2elsnlfc_6_1 + v2elsnlfc_6_0 + snlsff_2),
         pr_5 = v2elsnlfc_5_1/(v2elsnlfc_5_1 + v2elsnlfc_5_0 + snlsff_2),
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

cn_table_long <- filter(cn_proportions[,c(1:2, 3:24)], !is.na(Rural)) %>%
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

ggsave(filename = "Ridgelfc.png", device = "png", height=9, width =6, units="in", dpi=300)



#### Expert weights for relevance criteria: More free and fair ----
#### 
#table(v15cl$v2elsnmrfc_1)
snmrfc_0 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_0)) %>%
  group_by(country_text_id, year, v2elsnmrfc_0) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_0,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_0_1 = "1",
         v2elsnmrfc_0_0 = "0") 

snmrfc_1 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_1)) %>%
  group_by(country_text_id, year, v2elsnmrfc_1) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_1,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_1_1 = "1",
         v2elsnmrfc_1_0 = "0") 

snmrfc_2 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_2)) %>%
  group_by(country_text_id, year, v2elsnmrfc_2) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_2,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_2_1 = "1",
         v2elsnmrfc_2_0 = "0")

snmrfc_3 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_3)) %>%
  group_by(country_text_id, year, v2elsnmrfc_3) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_3,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_3_1 = "1",
         v2elsnmrfc_3_0 = "0")

snmrfc_4 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_4)) %>%
  group_by(country_text_id, year, v2elsnmrfc_4) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_4,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_4_1 = "1",
         v2elsnmrfc_4_0 = "0")

snmrfc_5 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_5)) %>%
  group_by(country_text_id, year, v2elsnmrfc_5) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_5,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_5_1 = "1",
         v2elsnmrfc_5_0 = "0")

snmrfc_6 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_6)) %>%
  group_by(country_text_id, year, v2elsnmrfc_6) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_6,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_6_1 = "1",
         v2elsnmrfc_6_0 = "0")

snmrfc_7 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_7)) %>%
  group_by(country_text_id, year, v2elsnmrfc_7) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_7,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_7_1 = "1",
         v2elsnmrfc_7_0 = "0")

snmrfc_8 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_8)) %>%
  group_by(country_text_id, year, v2elsnmrfc_8) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_8,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_8_1 = "1",
         v2elsnmrfc_8_0 = "0")

snmrfc_9 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_9)) %>%
  group_by(country_text_id, year, v2elsnmrfc_9) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_9,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_9_1 = "1",
         v2elsnmrfc_9_0 = "0")

snmrfc_10 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_10)) %>%
  group_by(country_text_id, year, v2elsnmrfc_10) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_10,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_10_1 = "1",
         v2elsnmrfc_10_0 = "0")

snmrfc_11 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_11)) %>%
  group_by(country_text_id, year, v2elsnmrfc_11) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_11,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_11_1 = "1",
         v2elsnmrfc_11_0 = "0")

snmrfc_12 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_12)) %>%
  group_by(country_text_id, year, v2elsnmrfc_12) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_12,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_12_1 = "1",
         v2elsnmrfc_12_0 = "0")

snmrfc_13 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_13)) %>%
  group_by(country_text_id, year, v2elsnmrfc_13) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_13,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_13_1 = "1",
         v2elsnmrfc_13_0 = "0")

snmrfc_14 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_14)) %>%
  group_by(country_text_id, year, v2elsnmrfc_14) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_14,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_14_1 = "1",
         v2elsnmrfc_14_0 = "0")

snmrfc_15 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_15)) %>%
  group_by(country_text_id, year, v2elsnmrfc_15) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_15,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_15_1 = "1",
         v2elsnmrfc_15_0 = "0")

snmrfc_16 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_16)) %>%
  group_by(country_text_id, year, v2elsnmrfc_16) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_16,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_16_1 = "1",
         v2elsnmrfc_16_0 = "0")

snmrfc_17 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_17)) %>%
  group_by(country_text_id, year, v2elsnmrfc_17) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_17,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_17_1 = "1",
         v2elsnmrfc_17_0 = "0")

snmrfc_18 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_18)) %>%
  group_by(country_text_id, year, v2elsnmrfc_18) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_18,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_18_1 = "1",
         v2elsnmrfc_18_0 = "0")

snmrfc_19 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_19)) %>%
  group_by(country_text_id, year, v2elsnmrfc_19) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_19,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_19_1 = "1",
         v2elsnmrfc_19_0 = "0")

snmrfc_20 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_20)) %>%
  group_by(country_text_id, year, v2elsnmrfc_20) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_20,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_20_1 = "1",
         v2elsnmrfc_20_0 = "0")

snmrfc_21 <- filter(v15cl, year>1899 & !is.na(v2elsnmrfc_21)) %>%
  group_by(country_text_id, year, v2elsnmrfc_21) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = v2elsnmrfc_21,
    values_from = freq,
    values_fill = list(freq = 0)) %>%
  rename(v2elsnmrfc_21_1 = "1",
         v2elsnmrfc_21_0 = "0")

# Merge all these dfs
snmrfc_all <- as.data.frame(cbind(snmrfc_0[,3:4], snmrfc_1, snmrfc_2[,3:4], snmrfc_3[,3:4], snmrfc_4[,3:4], snmrfc_5[,3:4], 
                                  snmrfc_6[,3:4], snmrfc_7[,3:4], snmrfc_8[,3:4], snmrfc_9[,3:4], snmrfc_10[,3:4], 
                                  snmrfc_11[,3:4], snmrfc_12[,3:4], snmrfc_13[,3:4], snmrfc_14[,3:4], snmrfc_15[,3:4], 
                                  snmrfc_16[,3:4], snmrfc_17[,3:4], snmrfc_18[,3:4], snmrfc_19[,3:4], snmrfc_20[,3:4], snmrfc_21[,3:4]))
# Reordering columns
snmrfc_all <- select(snmrfc_all, country_text_id, year, v2elsnmrfc_0_0, v2elsnmrfc_0_1:last_col())


# Merge in the frequencies of coders who said sn elections were the same subnationally
snmrfc_alll <- merge(snlsffHPD[,1:3], snmrfc_all, by = c("country_text_id", "year"), 
                     all.x = TRUE)   

# Calculate proportions of CEs who chose each criterion
cnm_proportions <- snmrfc_alll %>%
  mutate(pr_0 = v2elsnmrfc_0_1/(v2elsnmrfc_0_1 + v2elsnmrfc_0_0 + snlsff_2),
         pr_1 = v2elsnmrfc_1_1/(v2elsnmrfc_1_1 + v2elsnmrfc_1_0 + snlsff_2),
         pr_2 = v2elsnmrfc_2_1/(v2elsnmrfc_2_1 + v2elsnmrfc_1_0 + snlsff_2),
         pr_3 = v2elsnmrfc_3_1/(v2elsnmrfc_3_1 + v2elsnmrfc_1_0 + snlsff_2),
         pr_4 = v2elsnmrfc_4_1/(v2elsnmrfc_4_1 + v2elsnmrfc_1_0 + snlsff_2),
         pr_6 = v2elsnmrfc_6_1/(v2elsnmrfc_6_1 + v2elsnmrfc_6_0 + snlsff_2),
         pr_5 = v2elsnmrfc_5_1/(v2elsnmrfc_5_1 + v2elsnmrfc_5_0 + snlsff_2),
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

ggsave(filename = "Ridgemrfc.png", device = "png", height=9, width =6, units="in", dpi=300)

# Full sample, all countries 1900-2024
#### Expert weights for relevance criteria
# Use v2elsnmrfc in v15cl
#table(v15cl$v2elsnmrfc_1)

colnames(cnm_proportions) <- c("country_text_id", "year", "Rural", "Urban", "Less development", "More development", "Inside capital", "Outside capital", "North", "South", "West", "East", "Civil unrest", "Illicit activity", "Sparse population", "Remote", "Indigenous", "Ruling party strong", "Ruling party weak", "Longer foreign rule", "Shorter foreign rule", "Recent foreign rule", "No foreign rule", "None of the above")

cnm_proportions_long <- filter(cnm_proportions[,c(1:2, 3:24)], !is.na(Rural)) %>%
  pivot_longer(cols = !c(country_text_id, year),
               names_to = "dimension",
               values_to = "weight")

library(ggridges)

ggplot(filter(cnm_proportions_long, !is.nan(weight)), aes(y=reorder(dimension, weight), x=weight)) +
  geom_density_ridges(scale = 2, fill="blue", alpha = .5) +
  theme_ridges(font_size=12) + 
  theme(legend.position = "none") +
  labs(title="Expert weights in full sample", 
       subtitle = "Subnational elections more free and fair",
       y="", x="", 
       caption="Distributions are of country-years. Source: V-Dem v.15") +
  theme(axis.title.x = element_text(hjust=0.5), legend.position = "none", plot.background = element_rect(fill = "white")) +
  scale_fill_brewer(palette = "Paired")

ggsave(filename = "Ridgemrfc.png", device = "png", height=9, width =6, units="in", dpi=300)

# Calculate More minus Less
# Rename weight in the long proportions dfs
cnm_long <- cnm_proportions_long %>%
  rename(weight_more = "weight")
cn_long <- cn_table_long %>%
  rename(weight_less = "weight")

# merge them, keeping all observations
more_vs_less <- merge(cnm_long, cn_long, by = c("country_text_id", "year", "dimension"), all.y = TRUE)

# Calculate the difference in weights (more - less)
more_vs_less <- more_vs_less %>%
  mutate(weight_diff = weight_more - weight_less)

order <- more_vs_less%>%
  group_by(dimension) %>%
  summarize(rank = mean(weight_diff, na.rm = TRUE))

more_vs_less <- merge(more_vs_less, order, by = "dimension", all.x = TRUE)

write_dta(more_vs_less, path = "more_vs_less.dta")

more_vs_less_wide <- more_vs_less %>%
  pivot_wider(id_cols = c(country_text_id, year),
              names_from=dimension,
              values_from=weight_diff) %>%
  arrange(country_text_id, year)

colnames(more_vs_less_wide) <- c("country_text_id", "year", "Civil_unrest", "East", "Illicit_activity", "Indigenous", "Inside_capital", "Less_development", "Longer_foreign_rule", "More_development", "No_foreign_rule", "None_of_the_above", "North", "Outside_capital", "Recent_foreign_rule", "Remote", "Ruling_party_strong", "Ruling_party_weak", "Rural", "Shorter_foreign_rule", "South", "Sparse_population", "Urban", "West")

write_dta(more_vs_less_wide, path = "more_vs_less_wide.dta")
library(haven)

more_vs_less_wide <- read_dta("more_vs_less_wide.dta")

# Ridgeline plot
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

ggsave(filename = "Ridgemoreless.png", device = "png", height=9, width =6, units="in", dpi=300)

# Historical comparisons for selected countries
selected <- c("COL", "USA", "RUS", "IND")

ggplot(filter(more_vs_less, country_text_id %in% selected & !str_detect(dimension, "foreign")),
       aes(x = year, y = weight_diff, color = country_text_id)) +
  geom_line(linewidth = .5) +
  facet_wrap(~reorder(dimension, rank), ncol = 6) +
  theme_light() + labs(x = "", color = "country", y = "elections better minus elections worse",
                       title = "Relevance of selected criteria for free and fair subnational elections",
                       caption = "Four criteria related to foreign rule are omitted because they are rarely considered important.")

ggsave(filename = "RelevanceFacets.png", device = "png", height=6.5, width =9, units="in", dpi=300)

# Future reference
summary(v2elsrgel v2ellocelc v2elffelr v2elffelrbin v2elsnlsff v2elsnless v2elsnlfc v2elsnmore v2elsnmrfc)

# I don't think I've done all of this for civil liberties yet.
# 
# Loading the national data
merged_national_data <- read_dta("C:/Users/mcoppedg/Dropbox/External (1)/merged_national_data.dta")

emel_data <- merge(more_vs_less_wide, merged_national_data, by = c("country_text_id", "year"), all.x = TRUE)

# For figuring out directions
# emelcorrs <- emel_data %>%
# select(v2x_polyarchy, Urban, Rural, More_development, Less_development, Inside_capital, Outside_capital, 
#        Civil_unrest, Illicit_activity, Sparse_population, Remote, Indigenous, Ruling_party_strong, Ruling_party_weak) 
# cmat <- cor(emelcorrs, use="pairwise")
# write.csv(cmat, file = "corrmatrix_emel.csv")
# 
# # What's the relationship between the predictors and the weights?
# ggplot(filter(emel_data, year==2000), aes(x = UrbanRural, y = Urban-Rural)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth() +
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(y = More_development-Less_development, x = gdppcln_mm)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = MajDists10, y = Outside_capital-Inside_capital)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = CW_ave, y = Civil_unrest)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = corrupt_fact_norm, y = Illicit_activity)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = log(pop_density), y = Sparse_population)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = portdist10002, y = Remote)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = eur_pct100, y = Indigenous)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")
# ggplot(filter(emel_data, year==2000), aes(x = dominant_party, y = Ruling_party_strong-Ruling_party_weak)) +
#   geom_text(aes(label=country_text_id), size = 1.5) + geom_smooth()+
#   geom_hline(yintercept = 0, color = "red")

# Combining poles
emel_data <- emel_data %>%
  mutate(wtUrbanRural = Urban-Rural,
         wtDevelopment = More_development-Less_development,
         wtCapitalDistance = -Inside_capital+Outside_capital,
         wtRuling = Ruling_party_strong-Ruling_party_weak)

# Calculate the weighted average for FF elections
emel_data <- emel_data %>%
mutate(emel_score = (wtUrbanRural*UrbanRural + wtDevelopment*gdppcln_mm + 
                    wtCapitalDistance*MajDists10 + 
                    Civil_unrest*CW_ave + Illicit_activity*corrupt_fact_norm - 
                    Sparse_population*pop_density + Remote*portdist10002 - 
                    Indigenous*eur_pct100 + wtRuling*dominant_party)/(abs(wtUrbanRural) + abs(wtDevelopment) + abs(wtCapitalDistance)+ 
   abs(Civil_unrest) + abs(Illicit_activity) + abs(Sparse_population) + abs(Remote) + abs(Indigenous)+ abs(wtRuling)))

ggplot(emel_data, aes(x = emel_score, y = v2xel_frefair)) +
  geom_point(alpha = .3) + theme_light() + geom_smooth(method = "lm") +
  labs(title = "Predicting free & fair elections index",
  subtitle = "using subnational method using country-specific expert weights",
       x = "predicted free & fair elections using subnational method",
       y = "V-Dem's national clean elections index")

ggplot(emel_data, aes(x = log(5*(emel_score+.4)), y = v2xel_frefair)) +
  geom_point(alpha = .3) + theme_light() + geom_smooth(method = "lm") +
  labs(title = "Predicting free & fair elections index",
       subtitle = "using subnational method using country-specific expert weights",
       x = "predicted free & fair elections using subnational method",
       y = "V-Dem's national clean elections index")


# Regression
install.packages("jtools") #for summ(), plot_summs(), export_summs()
library(jtools)

reg_elem <- lm(v2xel_frefair ~ emel_score, data = emel_data)
summ(reg_elem, digits = 4)


reg_disag <- lm(v2xel_frefair ~ wtUrbanRural + UrbanRural + wtDevelopment + gdppcln_mm + 
                  wtCapitalDistance + MajDists10 + 
                  Civil_unrest + CW_ave + Illicit_activity + corrupt_fact_norm + 
                  Sparse_population + pop_density + Remote + portdist10002 + 
                  Indigenous + eur_pct100 + wtRuling + dominant_party, data = emel_data)
summ(reg_disag, digits = 4)

reg_xs <- lm(v2xel_frefair ~ UrbanRural +gdppcln_mm + 
               #MajDists10 + 
               CW_ave + corrupt_fact_norm + 
                  pop_density +portdist10002 + 
                  eur_pct100 + dominant_party, data = emel_data)
summ(reg_xs, digits = 4)

reg_xs$elec_hat <- reg_xs$fitted.values

elec_fit <- merge(reg_xs$model, reg_xs$elec_hat, by=0, all = TRUE)
elec_fit <- elec_fit %>%
  rename(elec_hat = y)

ggplot(elec_fit, aes(x = elec_hat, y = v2xel_frefair)) +
  geom_point() + theme_light() + geom_smooth()

# Compare to weights
emel_summ <- emel_data %>%
  select(wtUrbanRural, wtDevelopment, wtCapitalDistance, Civil_unrest, Illicit_activity, Sparse_population, Remote, Indigenous, wtRuling) %>%
  summary(across())
emel_summ
