#---- Step 4: Weighting geolocated data with V-Dem data ----

# Step 1: Wrangle raw data, clean it, then impute missing values (Folders 01-04)
# Step 2: Calculate averages of Empirical CDF data  (Folder 05)
# Step 3: Subset V-Dem data, calculate criteria averages, apply national range (Folder 06)
# Step 4 (this script): Interact Averaged CDF data by V-Dem data (Folders 05-06)
# Step 5: Analyze geolocated levels of democracy (Folder 09)


## ----Setup ----
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(knitr)

# V-Dem Colombia ratings (cr) data
cr_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/v1/vdem_col0023_cleaned.rds")
# Geolocated data from Colombia
geo_df <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/v1/CDF_averages_v1.rds")
geo_df <- geo_df %>%
  rename(avg0_1 = "avg0t1", avg2_3 = "avg2t3", avg4_5 = "avg4t5", avg10_11 = "avg10t11", avg15_16 = "avg15t16")


# Geocoded data review ----
# Correlations of concern (>0.5): avg6 and avg7 (-0.69), avg8 and avg9 (-0.63), avg12 and avg13 (0.60)
cor_matrix <- cor(geo_df[,3:14], use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")
summary(geo_df)

# V-Dem data review ----
# Prepare cr_df: Pivot it so each year is ONE row with 24 columns. This prevents row duplication during the join
cr_pivoted <- cr_df %>%
  pivot_wider(
    names_from = row_id, 
    values_from = c(emel, cscw),
    names_sep = ""
  )

## Notable: Democratic "Dip" in 2023 ----
cr_long <- cr_pivoted %>%
  pivot_longer(cols = -year, names_to = "component", values_to = "score") %>%
  mutate(Category = ifelse(grepl("^emel", component), "Electoral (EMEL)", "Civil Liberties (CSCW)"))

cr_annual <- cr_long %>%
  group_by(year, Category) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")

ggplot(cr_annual, aes(x = year, y = mean_score, color = Category)) +
  geom_line(size = 1) +
  geom_point(data = filter(cr_annual, year == 2023), size = 3) + # Highlight 2023
  geom_vline(xintercept = 2023, linetype = "dashed", color = "darkgrey") +
  theme_minimal() +
  labs(
    title = "Annual Trends in Democracy Components (2000-2023)",
    subtitle = "Comparing Electoral Fairness and Civil Liberties Scores",
    x = "Year",
    y = "Average Component Score",
    caption = "Data source: cr_pivoted summary"
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 2)) +
  theme(legend.position = "bottom")

## Pre- and post-2016 Accord percentages (2015 vs. 2023)----
emel_cols <- names(cr_pivoted)[grep("^emel", names(cr_pivoted))]
cscw_cols <- names(cr_pivoted)[grep("^cscw", names(cr_pivoted))]

change_calc <- cr_pivoted %>%
  filter(year %in% c(2015, 2023)) %>%
  rowwise() %>%
  mutate(
    EMEL_Index = mean(c_across(all_of(emel_cols)), na.rm = TRUE),
    CSCW_Index = mean(c_across(all_of(cscw_cols)), na.rm = TRUE)
  ) %>%
  select(year, EMEL_Index, CSCW_Index)

results_table <- change_calc %>%
  pivot_wider(names_from = year, values_from = c(EMEL_Index, CSCW_Index)) %>%
  mutate(
    EMEL_pct_change = ((EMEL_Index_2023 - EMEL_Index_2015) / EMEL_Index_2015) * 100,
    CSCW_pct_change = ((CSCW_Index_2023 - CSCW_Index_2015) / CSCW_Index_2015) * 100
  )

# EMEL change: +35.9%. CSCW change: -2.70%.
print(results_table)


## V-Dem Correlations overall---- 
# V-Dem ratings: 0.68 correlation between EMEL and CSCW
cor_matrix2 <- cor(cr_pivoted, use = "complete.obs")
corrplot(cor_matrix2, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

vdem_summary_table <- cr_pivoted %>%
  select(year, ends_with("0_1"), ends_with("2_3"), ends_with("4_5")) %>%
  arrange(year)
kable(vdem_summary_table, caption = "V-Dem Criteria Values (2000-2023)")

# ---- Join Geolocated with V-Dem data ----
full_df <- geo_df %>%
  inner_join(cr_pivoted, by = "year") %>%
  mutate(
    # Multiply each avg group by its corresponding emel/cscw column
    # We use a loop or map to handle the specific pairing of avg0_1 * emel0_1
    across(starts_with("avg"), ~ .x * get(str_replace(cur_column(), "avg", "emel")), .names = "emel{str_remove(.col, 'avg')}"),
    across(starts_with("avg"), ~ .x * get(str_replace(cur_column(), "avg", "cscw")), .names = "cscw{str_remove(.col, 'avg')}")
  ) %>%
  select(MPIO_CDPMP, year, starts_with("emel"), starts_with("cscw")) 


# Compute the correlation matrix to make sure there are no perfectly correlated variables, other than EMELxCSCW intersection (not quite perfect but close)
cor_matrix3 <- cor(full_df[,3:26], use = "complete.obs")
corrplot(cor_matrix3, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

summary(full_df)

# Distributions in violence data
hist(full_df$emel10_11) # slightly right-skewed
hist(full_df$cscw10_11) # more normal


## Calculate Index ----
index_df <- full_df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    emel_score = mean(c(emel0_1, emel2_3, emel4_5, emel6, 
                        emel7, emel8, emel9, emel10_11, 
                        emel12, emel13, emel14, emel15_16), 
                      na.rm = TRUE),
    cscw_score = mean(c(cscw0_1, cscw2_3, cscw4_5, cscw6, 
                        cscw7, cscw8, cscw9, cscw10_11, 
                        cscw12, cscw13, cscw14, cscw15_16), 
                      na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

## Calculate mean SN score ----
colwtd <- index_df %>%
  mutate(sndem_mean = (emel_score + cscw_score) / 2) %>%
  dplyr::select(MPIO_CDPMP, year, emel_score, cscw_score, sndem_mean)

## Join into one final dataframe ----
master_df <- full_df %>%
  left_join(colwtd, by = c("MPIO_CDPMP", "year"))
summary(master_df)

colSums(is.na(master_df))


# Visualizations ----

## Decile plots ----
decile_plot <- master_df %>%
  group_by(year) %>%
  summarise(
    Top_10 = mean(sndem_mean[sndem_mean >= quantile(sndem_mean, 0.9)], na.rm = TRUE),
    Bottom_10 = mean(sndem_mean[sndem_mean <= quantile(sndem_mean, 0.1)], na.rm = TRUE),
    National_Avg = mean(sndem_mean, na.rm = TRUE)
  ) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "The Democracy Gap: Top 10% vs Bottom 10% Municipalities",
       subtitle = "Reveals if the 2023 dip affected the most 'democratic' towns differently",
       y = "Score", x = "Year")

decile_plot

## Heatmap ----
ggplot(master_df, aes(x = year, y = MPIO_CDPMP, fill = sndem_mean)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), # Hide 1,125 municipality names
        panel.grid = element_blank()) +
  labs(title = "Heatmap of Democracy scores by Municipality",
       subtitle = "A vertical 'stripe' of darker color in 2023 would confirm a ubiquitous national drop",
       fill = "Score")

## Ridge plot ----
library(ggridges)
ggplot(master_df, aes(x = sndem_mean, y = as.factor(year), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  theme_ridges() + 
  labs(title = "Evolution of Score Distributions",
       y = "Year", x = "SN-Democracy Score")



#---- Write the unified dataframe to rds ----
write_rds(master_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/v1/master_snvdem_colv1.rds")
