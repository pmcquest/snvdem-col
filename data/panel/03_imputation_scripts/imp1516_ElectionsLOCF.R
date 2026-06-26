#----Last observation carried forward (and back) ----
# For #15 Ruling party, last election winner carried forward to complete data for 2000-2023.

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")
df06 <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df06_clean.rds") # 15-16


# 1. Create a "patch" dataframe from df06 for the anchor year (1998)
df06_patch <- df06 %>%
  filter(year == 1998) %>%
  select(MPIO_CDPMP, year, RulPar_15t16, RulParD_15t16)

# 2. Combine, Join, and Fill (Unified Flow)
imp1516 <- df_all %>%
  # Add the 1998 rows to act as a starting point for LOCF
  bind_rows(df06_patch) %>%
  # Join specific 2000-2001 data from df06
  left_join(
    df06 %>% filter(year %in% 2000:2001) %>% 
      select(MPIO_CDPMP, year, RulPar_15t16, RulParD_15t16),
    by = c("MPIO_CDPMP", "year"),
    suffix = c("", "_df06")
  ) %>%
  mutate(
    # Patch the main columns with joined data
    RulPar_15t16 = coalesce(RulPar_15t16, RulPar_15t16_df06),
    RulParD_15t16 = coalesce(RulParD_15t16, RulParD_15t16_df06)
  ) %>%
  select(-ends_with("_df06")) %>%
  # Apply LOCF: This now sees 1998 values and carries them into 2000+
  arrange(MPIO_CDPMP, year) %>%
  group_by(MPIO_CDPMP) %>%
  fill(RulPar_15t16, RulParD_15t16, .direction = "down") %>%
  ungroup() %>%
  # Drop the 1998 scaffolding and filtered out unwanted levels
  filter(year >= 2000)


# 3. Final Check: 172 NAs
colSums(is.na(imp1516[c("RulPar_15t16", "RulParD_15t16")]))
# See which municipalities still have NAs and in which years
imp1516 %>%
  filter(is.na(RulPar_15t16)) %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    Years_Missing = paste(year, collapse = ", "),
    Count = n()
  )

#----Sensitivity analysis----

# 1) LOESS Smoothing for Visualization
df_all_linear <- imp1516 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(RulPar_15t16_linear = if(sum(!is.na(RulPar_15t16)) >= 2) {
    approx(year, RulPar_15t16, year, method = "linear", rule = 2)$y
  } else {
    RulPar_15t16 # Keep as is if we can't interpolate
  }) %>%
  ungroup()

summary(imp1516$RulPar_15t16)  # LOCF-imputed values
summary(df_all_linear$RulPar_15t16_linear)  # Linear-interpolated values


#----Visualize----

ggplot(data = imp1516, aes(x = year, y = RulPar_15t16)) +
  # Spaghetti lines for 50 random municipalities
  geom_line(data = imp1516 %>% filter(MPIO_CDPMP %in% sample(unique(MPIO_CDPMP), 50)),
            aes(group = MPIO_CDPMP), alpha = 0.1, color = "gray20") +
  stat_summary(fun = mean, geom = "line", color = "darkblue", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", color = "darkblue", size = 2) +
  geom_smooth(method = "loess", color = "firebrick", linetype = "dashed", 
              se = FALSE, linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  labs(
    title = "Trend of Ruling Party support (RulPar_15t16)",
    subtitle = "Average of Colombian municipalities (2000-2023)",
    x = "Year",
    y = "% Vote for Ruling Party",
    caption = "Blue line = National Mean; Red dashed = Smoothed Trend; Gray = Sampled Individual Municipalities"
  ) +
  theme_minimal()



#----Save and document----
imp1516 <- imp1516 %>%
  select(MPIO_CDPMP, year, RulPar_15t16, RulParD_15t16)
write_rds(imp1516, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp1516.rds")
