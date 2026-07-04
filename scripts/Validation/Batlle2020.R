# Validating snvdem with Batlle et al (2020)

library(dplyr)
library(purrr)
library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)
library(stringi)

# 1. Load Data ----
snvdemFA <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/02_FA/snvdem_FA.rds")

# 1. Standardize the 'depto' column and Define Periods
boyaca_correlations <- snvdemFA %>%
  mutate(
    # Create a temporary normalized version to catch all spellings
    depto_norm = stri_trans_general(depto, "Latin-ASCII"),
    # Standardize the actual depto column to the correct Spanish spelling
    depto = if_else(toupper(depto_norm) == "BOYACA", "Boyacá", depto)
  ) %>%
  # Filter strictly for the department of Boyacá or DANE codes starting with 15
  filter(depto == "Boyacá" | str_starts(MPIO_CDPMP, "15")) %>%
  
  # Categorize by the Batlle et al. (2020) timeframes
  mutate(period = case_when(
    year >= 2000 & year <= 2011 ~ "2000-2011 (Batlle Period)",
    year >= 2012 & year <= 2023 ~ "2012-2023 (Modern Era)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# 2. Calculate Correlation Coefficients for each period
cor_results <- boyaca_correlations %>%
  group_by(period) %>%
  summarise(
    # Primary Factor Correlation
    cor_primary = cor(EMEL_Development, CSCW_Safety, use = "complete.obs", method = "pearson"),
    # Composite Score Correlation
    cor_composite = cor(EMEL_Composite, CSCW_Composite, use = "complete.obs", method = "pearson"),
    # Sample Size
    n = n()
  )

print(cor_results)

# 3. Optional: Scatterplot to visualize the relationship shift
ggplot(boyaca_correlations, aes(x = EMEL_Composite, y = CSCW_Composite, color = period)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~period) +
  labs(title = "Pillar Convergence in Boyacá",
       subtitle = "Pearson correlation between Electoral Reach and Civil Safety",
       x = "Electoral Reach (EMEL)", y = "Civil Safety (CSCW)") +
  theme_minimal()
