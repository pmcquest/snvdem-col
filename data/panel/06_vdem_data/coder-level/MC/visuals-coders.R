
library(haven)
library(tidyverse)
ELCLweights_wide <- read_dta("data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide.dta")

COL_sndem <- ELCLweights_wide %>%
  filter(country_text_id == "COL")


criteria_map <- c(
  "0" = "Rural", "1" = "Urban", "2" = "Less development", "3" = "More development",
  "4" = "Inside capital", "5" = "Outside capital", "6" = "North", "7" = "South",
  "8" = "West", "9" = "East", "10" = "Civil unrest", "11" = "Illicit activity",
  "12" = "Sparse population", "13" = "Remote", "14" = "Indigenous",
  "15" = "Ruling party strong", "16" = "Ruling party weak"
)

# Convert map to a dataframe for easier joining
map_df <- enframe(criteria_map, name = "subset", value = "criteria_name")


COL_comb_longMC <- COL_sndem %>%
  pivot_longer(
    cols = starts_with(c("el_", "cl_")),
    names_to = c(".value", "criteria_name"),
    names_pattern = "(el|cl)_(.*)" # Changed pattern to match el_ and cl_
  ) %>%
  mutate(criteria_name = str_replace_all(criteria_name, "_", " ")) %>%
  inner_join(map_df, by = "criteria_name") %>%
  select(year, subset, emel_score = el, cscw_score = cl)

summary(COL_comb_longMC)


library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Re-run the join carefully to ensure 'criteria_name' is preserved
plot_trends <- COL_comb_longMC %>%
  filter(year >= 1990 & year <= 2023) %>%
  # Check: does criteria_name exist? If not, we join it here
  # inner_join(map_df, by = "subset") # Use this if joining by the '0', '1' codes
  pivot_longer(cols = c(emel_score, cscw_score), 
               names_to = "index_type", 
               values_to = "score") %>%
  mutate(index_type = ifelse(index_type == "emel_score", "Elections", "Civil Liberties"))

# 2. Corrected Plotting Code
# Note: replaced 'size' with 'linewidth' and fixed '++' syntax
ggplot(plot_trends, aes(x = year, y = score, color = index_type)) +
  geom_line(linewidth = 1, alpha = 0.8) + 
  facet_wrap(~subset, scales = "fixed", ncol = 4) + 
  theme_minimal() +
  scale_color_manual(values = c("Elections" = "#E41A1C", "Civil Liberties" = "#377EB8")) +
  labs(
    title = "Trends in Subnational Criteria Weights (1990-2023)",
    subtitle = "Comparing Electoral vs. Civil Liberty weights across subsets",
    x = "Year",
    y = "Weight Score (0-1)",
    color = "Index Type"
  ) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
