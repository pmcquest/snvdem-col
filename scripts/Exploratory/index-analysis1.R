# Analysis of index snvdem data for Colombia

library(tidyverse)
library(vdemdata)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# import final snvdem data for Colombia (2000-2020)
colwtd <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/final_snvdem_col.rds")

library(psych)

describe(colwtd[, c("emel_score", "cscw_score", "sndem_mean")])

library(modelsummary)
library(gt)

# Use the datasummary_skim function for quick summary statistics
# This is a very powerful function and can be customized with themes and output formats (e.g., 'markdown', 'html', 'latex')
summary_table_gt <- datasummary_skim(colwtd[, c("emel_score", "cscw_score", "sndem_mean")], 
                 fmt = 3,
                 output = "gt"
)
# 2. Save the 'gt' table object as a JPG file
gtsave(
  summary_table_gt, 
  # Set the desired file name
  filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/summary_statistics.png", 
  # Increase zoom for higher resolution/better quality image
  zoom = 2 
)


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