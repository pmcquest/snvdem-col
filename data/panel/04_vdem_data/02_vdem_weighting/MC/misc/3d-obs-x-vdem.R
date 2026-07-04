#---- Step 4: Weighting geolocated data with V-Dem data ----

# Step 1: Wrangle raw data, clean it, then impute missing values (Folders 01-04)
# Step 2: Calculate averages of Empirical CDF data  (Folder 05)
# Step 3: Subset V-Dem data, calculate criteria averages, apply national range (Folder 06)
# Step 4 (this script): Weight Averaged CDF data by V-Dem data (Folder 07-08)
# Step 5: Map geolocated levels of democracy (Folder 09)


## ----Setup ----
# load needed packages
library(tidyverse)
library(vdemdata)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

cr_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-weighted.rds")

averages_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/col0020-averages2.rds")

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


###---- Optional: check ind. variable influence on the 2018 spike----
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
