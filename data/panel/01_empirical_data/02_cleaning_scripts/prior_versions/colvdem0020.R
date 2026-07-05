#----Visualize sn V-Dem responses for Colombia 2000-2020----


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

colv13 <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/validation/vdem-col0020.rds")


col_weights <- colv13 %>%
  pivot_longer(cols = starts_with(c("v2elsnlfc", "v2elsnmrfc", "v2clrgstch", "v2clrgwkch")),
               names_to = c("variable", "subset"),
               names_pattern = "(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch)_(\\d+)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  pivot_longer(cols = starts_with(c("v2elsnlfc", "v2elsnmrfc", "v2clrgstch", "v2clrgwkch")),
               names_to = "variable", values_to = "value") %>% #added pivot longer
  mutate(weight_name = paste(variable, subset, sep = "_")) %>%
  select(year, weight_name, value) %>%
  pivot_wider(names_from = weight_name, values_from = value)

col_weights2 <- col_weights %>%
  pivot_longer(cols = starts_with(c("v2elsnlfc", "v2elsnmrfc", "v2clrgstch", "v2clrgwkch")),
               names_to = c("variable", "subset"),
               names_pattern = "(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch)_(\\d+)") %>%
  mutate(group = case_when(
    subset %in% c("0", "1") ~ "0_1",
    subset %in% c("2", "3") ~ "2_3",
    subset %in% c("4", "5") ~ "4_5",
    subset %in% c("15", "16") ~ "15_16",
    TRUE ~ subset
  )) %>%
  group_by(year, group, variable) %>%
  summarise(mean_weight = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_weight) %>%
  filter(group %in% c("0_1", "2_3", "4_5", "15_16")) %>%
  rename(row_id = group)

# Create a vector of row_ids that should remain the same
unchanged_row_ids <- c(6:14)

# Filter the original dataframe to retain rows with row_ids that should remain the same
unchanged_df <- col_weights %>%
  pivot_longer(cols = starts_with(c("v2elsnlfc", "v2elsnmrfc", "v2clrgstch", "v2clrgwkch")),
               names_to = c("variable", "subset"),
               names_pattern = "(v2elsnlfc|v2elsnmrfc|v2clrgstch|v2clrgwkch)_(\\d+)") %>%
  mutate(subset = as.character(subset)) %>%
  filter(subset %in% unchanged_row_ids) %>%
  group_by(year, subset, variable) %>%
  summarise(mean_weight = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = mean_weight) %>%
  rename(row_id = subset)

# Combine the averaged subset and the unchanged rows
cr_df1 <- bind_rows(col_weights2, unchanged_df)
cr_df1 <- cr_df1 %>%
  filter(year <= 2020)

# Reshape for plotting all 4 variables
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your dataframe is named cr_df1

# Create a named vector for renaming
row_id_mapping <- c(
  "0_1" = "0-1. Rurality",
  "2_3" = "2-3. Econ. Devt.",
  "4_5" = "4-5. Dist. Bog.",
  "6" = "6. North",
  "7" = "7. South",
  "8" = "8. West",
  "9" = "9. East",
  "10" = "10. Civil unrest",
  "11" = "11. Illicit activity", 
  "12" = "12. Pop. Sparse",
  "13" = "13. Remote",
  "14" = "14. Indigenous",
  "15_16" = "15-16. Rul. Party")

# Reshape for plotting all 4 variables AND rename row_id
long_data <- cr_df1 %>%
  pivot_longer(cols = c(v2elsnlfc, v2elsnmrfc, v2clrgstch, v2clrgwkch),
               names_to = "variable", values_to = "value") %>%
  mutate(row_id = recode(as.character(row_id), !!!row_id_mapping)) %>%
  mutate(row_id = factor(row_id, levels = c("0-1. Rurality", "2-3. Econ. Devt.", "4-5. Dist. Bog.", "6. North", "7. South", "8. West", "9. East", "10. Civil unrest", "11. Illicit activity", "12. Pop. Sparse", "13. Remote", "14. Indigenous", "15-16. Rul. Party")))

# Create the faceted line plot
ggplot(long_data, aes(x = year, y = value, color = variable, group = variable)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(~ row_id) +
  labs(title = "SN V-Dem Expert scores per criteria (Col. 2000-2020)",
       x = "Year",
       y = "Value",
       color = "Variable") +
  theme_minimal()


#----Explaining the 2018 spike----
weighted_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/Weighted/col0020-weighted2.rds")

# 1. Calculate Year-Over-Year Changes
changes_df <- weighted_df %>%
  filter(year %in% c(2017, 2018)) %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")),
               names_to = "variable", values_to = "value") %>%
  group_by(MPIO_CDPMP, variable) %>%
  arrange(year) %>%
  summarize(change = diff(value), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = change)

# 2. Analyze the Magnitude of Changes
summary(changes_df)

# Calculate absolute and percentage changes
changes_analysis <- changes_df %>%
  pivot_longer(cols = starts_with(c("emel", "cscw")),
               names_to = "variable", values_to = "change") %>%
  group_by(variable) %>%
  summarise(
    mean_abs_change = mean(abs(change), na.rm = TRUE),
    mean_change = mean(change, na.rm = TRUE),
    median_change = median(change, na.rm = TRUE),
    sd_change = sd(change, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_abs_change))

print(changes_analysis)

# 3. Visualize the Changes
# Boxplots
ggplot(changes_df %>% pivot_longer(cols = starts_with(c("emel", "cscw")), names_to = "variable", values_to = "change"), aes(x = variable, y = change)) +
  geom_boxplot() +
  labs(title = "Changes in Weighted Variables (2017-2018)")

# Line plots (example for emel0_1)
ggplot(weighted_df %>% filter(year %in% c(2017,2018)), aes(x = year, y = emel0_1, group = MPIO_CDPMP, color = MPIO_CDPMP)) +
  geom_line() +
  labs(title = "Emel0_1 over 2017-2018") +
  theme(legend.position = "none")

#example for cscw1516
ggplot(weighted_df %>% filter(year %in% c(2017,2018)), aes(x = year, y = cscw1516, group = MPIO_CDPMP, color = MPIO_CDPMP)) +
  geom_line() +
  labs(title = "cscw1516 over 2017-2018") +
  theme(legend.position = "none")

