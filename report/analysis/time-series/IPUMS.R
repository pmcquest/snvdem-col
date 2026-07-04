# IPUMS data
## (0-1) rural/urban
## (2-3) socio-economic
## (12) population density
## (14) indigenous

library(tidyverse)
library(survey)
library(ipumsr)
ddi <- read_ipums_ddi("G:/Shared drives/snvdem/snvdem-col/data/panel/IPUMSI/ipumsi_00007.xml")
data <- read_ipums_micro(ddi)

library(dplyr)

data64 <- filter(data, YEAR == 1964)
svy.w2 <- svydesign(ids = ~1, data = data64, weights = data64$HHWT/100)
svytable(~data64$URBAN, design = svy.w2)


data05 <- filter(data, YEAR == 2005)


svy.w4 <- svydesign(ids = ~1, data = data05, weights = data05$PERWT/100)
prop.table(table(data05$URBAN))
prop.table(svytable(~data05$URBAN, design = svy.w4))

# Aggregation

# Example: weighted person-level mean and median of Indigenous at municipal-level
# PERWT ... 
# Aggregate by YEAR and GEOLEV2 (municipal-level)
mundata <- data %>%
  group_by(YEAR, GEOLEV2) %>%
  summarise(
    mean_popdens = mean(POPDENSGEO2, na.rm = TRUE)
    #add more variables
  )
summary_table <- mundata %>%
  group_by(YEAR) %>%
  summarise(unique_geo = n_distinct(GEOLEV2)) %>%
  pivot_wider(names_from = YEAR, values_from = unique_geo)


# Save as a CSV file
write.csv(municipal_data, "path/to/save/municipal_data.csv", row.names = FALSE)

# Or save as an RDS file
saveRDS(municipal_data, "path/to/save/municipal_data.rds")