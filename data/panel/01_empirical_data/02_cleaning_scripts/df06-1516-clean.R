
#---- Ruling party support ----

##---- Criteria measured ----
# 15: Areas where the national ruling party or group is strong. (0=No, 1=Yes) [v2*_15]
# 16: Areas where the national ruling party or group is weak. (0=No, 1=Yes) [v2*_16]


##---- Data Sources ----
# Data 1: Registraduria (RNEC)

# Note: While direct elections for mayors and governors appeared in the 1980s, major changes--such as introduction of nationwide district for senatorial elections--emerged with the 1991 Const. See "15-16_RulingParty" folder for wrangling script.

#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

##---- 15-16 Support for National ruling party ----

###---- Presidential votes (2002-2022) ----
df06 <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/p9822_15t16.rds")
# NOTE 2026-07-03: p9822_15t16.rds is saved as a dplyr grouped_df (grouped by year,
# leftover from whatever upstream script produced it). Left grouped, the completeness
# mutate() below errors under current dplyr (1.2.1): rowSums(!is.na(select(., ...)))
# evaluates against the full 7804-row table while dplyr expects a per-group (e.g. 1084-row)
# or scalar result. This script's row-level completeness isn't meant to respect any
# grouping anyway -- it groups explicitly, on its own terms, further down for the
# by-year/by-municipality summaries -- so ungroup() here is the correct fix, not a workaround.
df06 <- ungroup(df06)
n_distinct(df06$MPIO_CDPMP)


##----- Completeness ----
# Calculate completeness across all variables per year
completeness_summary <- df06 %>%
  mutate(
    # Count non-missing values for each row excluding 'MPIO_CDPMP' and 'year'
    non_missing_values = rowSums(!is.na(select(., -MPIO_CDPMP, -year))),  
    
    # Calculate total possible values (excluding 'MPIO_CDPMP' and 'year')
    total_values = ncol(.) - 2,  # Total number of variables excluding 'MPIO_CDPMP' and 'year'
    
    # Calculate completeness percentage for each observation
    completeness_percentage = (non_missing_values / total_values) * 100
  )
print(completeness_summary)
summary(completeness_summary$completeness_percentage)

# Average completeness by year
completeness_by_year <- completeness_summary %>%
  group_by(year) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_year)

# average completeness by municipality
completeness_by_municipality <- completeness_summary %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_municipality)

library(ggplot2)
ggplot(completeness_by_year, aes(x = year, y = avg_completeness)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Data Completeness by Year", x = "Year", y = "Completeness (%)") +
  theme_minimal()

# Check for NA's 
na_counts <- colSums(is.na(df06))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))


#---- Save cleaned dataset ----
write_rds(df06, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df06_clean.rds")
