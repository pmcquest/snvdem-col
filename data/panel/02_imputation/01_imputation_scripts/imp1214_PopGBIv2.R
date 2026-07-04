# ---- Growth-based Imputation for Demographic Variables ----
# Logic: 
# 1. Linear Growth between Census anchor points (2005, 2018)
# 2. Constant Imputation for municipalities with only 1 data point
# 3. Yearly Medians for municipalities with no data across all years
# 4. Logical caps to ensure subpopulations < total population

library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr) # NOTE 2026-07-03: added -- clean_mpio() below calls str_pad() but this
                 # script never loaded stringr, so the mapping section at the end always
                 # errored on a fresh run ("could not find function str_pad"). Harmless to the
                 # actual output (write_rds() for imp1214.rds happens earlier), but stopped the
                 # script from completing.

# 1. Load and Filter
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/04_merge_empirical/df_col_clean.rds")

exclude_codes <- c("01001", "05000", "08000", "13000", "15000", "17000", 
                   "18000", "19000", "20000", "23000", "25000", "27000", 
                   "41000", "44000", "47000", "50000", "52000", "54000", 
                   "63000", "66000", "68000", "70000", "73000", "76000", 
                   "81000", "85000", "86000", "88000", "91000", "94000", 
                   "95000", "97000", "99000")

df_clean <- df_all %>%
  filter(!MPIO_CDPMP %in% exclude_codes) %>%
  arrange(MPIO_CDPMP, year)

# 2. Linear Growth & Slope Calculation (MPIO-Level)
imp14 <- df_clean %>%
  group_by(MPIO_CDPMP) %>%
  mutate(
    # --- Indigenous Calculation ---
    n_ind = sum(!is.na(PobInd_14)),
    # We use first() to ensure the 'if' condition only sees 1 value per group
    y_first_ind = if(first(n_ind) >= 2) min(year[!is.na(PobInd_14)], na.rm = TRUE) else NA_real_,
    y_last_ind  = if(first(n_ind) >= 2) max(year[!is.na(PobInd_14)], na.rm = TRUE) else NA_real_,
    v_first_ind = if(!is.na(first(y_first_ind))) PobInd_14[year == first(y_first_ind)][1] else NA_real_,
    v_last_ind  = if(!is.na(first(y_last_ind)))  PobInd_14[year == first(y_last_ind)][1] else NA_real_,
    slope_ind   = (v_last_ind - v_first_ind) / (y_last_ind - y_first_ind),
    
    # --- Ethnic Calculation ---
    n_etn = sum(!is.na(PobEtn_14)),
    y_first_etn = if(first(n_etn) >= 2) min(year[!is.na(PobEtn_14)], na.rm = TRUE) else NA_real_,
    y_last_etn  = if(first(n_etn) >= 2) max(year[!is.na(PobEtn_14)], na.rm = TRUE) else NA_real_,
    v_first_etn = if(!is.na(first(y_first_etn))) PobEtn_14[year == first(y_first_etn)][1] else NA_real_,
    v_last_etn  = if(!is.na(first(y_last_etn)))  PobEtn_14[year == first(y_last_etn)][1] else NA_real_,
    slope_etn   = (v_last_etn - v_first_etn) / (y_last_etn - y_first_etn)
  ) %>%
  mutate(
    # --- Apply Imputation Logic ---
    PobInd_14 = case_when(
      !is.na(PobInd_14) ~ PobInd_14,
      !is.na(slope_ind) ~ pmax(v_first_ind + slope_ind * (year - y_first_ind), 0),
      n_ind == 1       ~ na.omit(PobInd_14)[1], # Constant imputation if only 1 point
      TRUE             ~ NA_real_
    ),
    PobEtn_14 = case_when(
      !is.na(PobEtn_14) ~ PobEtn_14,
      !is.na(slope_etn) ~ pmax(v_first_etn + slope_etn * (year - y_first_etn), 0),
      n_etn == 1       ~ na.omit(PobEtn_14)[1],
      TRUE             ~ NA_real_
    )
  ) %>%
  ungroup()

# 3. Final Gaps (Yearly Median) & Logical Constraints
# This fills the 52 municipalities that have zero data across all years.
imp14 <- imp14 %>%
  group_by(year) %>%
  mutate(
    PobInd_14 = if_else(is.na(PobInd_14), median(PobInd_14, na.rm = TRUE), PobInd_14),
    PobEtn_14 = if_else(is.na(PobEtn_14), median(PobEtn_14, na.rm = TRUE), PobEtn_14),
    PobTot_0t1 = if_else(is.na(PobTot_0t1), median(PobTot_0t1, na.rm = TRUE), PobTot_0t1)
  ) %>%
  ungroup() %>%
  mutate(
    # Robustness: Sub-populations cannot exceed Total Population
    PobInd_14 = pmin(PobInd_14, PobTot_12),
    PobEtn_14 = pmin(PobEtn_14, PobTot_12)
  )

# ---- Sensitivity Analysis: LOESS (Robust Version) ----
imp14_loess <- imp14 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(across(c(PobInd_14, PobEtn_14), 
                ~ {
                  # Count how many non-NA values we have in this specific column/group
                  n_valid = sum(!is.na(.))
                  # LOESS typically needs at least 4 points to be stable with span 0.75
                  if (n_valid >= 4) {
                    # Create the model
                    mod <- loess(. ~ year, span = 0.75, control = loess.control(surface = "direct"))
                    # Predict only for the NAs, otherwise keep the original
                    ifelse(is.na(.), pmax(predict(mod, newdata = cur_data()), 0), .)
                  } else {
                    # If not enough data for LOESS, fall back to our Linear Growth/Constant result
                    .
                  }
                },
                .names = "{.col}_loess")) %>%
  ungroup()

# ---- Visualization: Raw (Before) vs Imputed (After) ----
imp14_comparison <- bind_rows(
  # The original raw data with NAs
  df_clean %>% 
    select(year, PobInd_14, PobEtn_14) %>% 
    mutate(status = "Raw (Original)"),
  
  # Your final imputed data (Linear Growth result)
  imp14 %>% 
    select(year, PobInd_14, PobEtn_14) %>% 
    mutate(status = "Imputed (Linear)")
) %>%
  pivot_longer(cols = c(PobInd_14, PobEtn_14), names_to = "variable", values_to = "value")

ggplot(imp14_comparison, aes(x = log(value + 1), fill = status)) +
  geom_density(alpha = 0.4, color = NA) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Imputation Impact: Raw vs. Imputed", 
       subtitle = "Density comparison showing how imputation fills the distribution",
       x = "Log(Population + 1)") +
  theme_minimal()

# ---- Final Proportions and Saving ----
imp1214_final <- imp14 %>%
  mutate(PropInd_14 = PobInd_14 / PobTot_12,
         PropEtn_14 = PobEtn_14 / PobTot_12) %>%
  select(MPIO_CDPMP, year, DenPob_12, PropInd_14, PropEtn_14)

# Final Diagnostic Check
print("Missing Values Summary:")
print(colSums(is.na(imp1214_final)))

write_rds(imp1214_final, "G:/Shared drives/snvdem/snvdem-col/data/panel/02_imputation/02_imputation_outputs/imp1214.rds")


# Visualize Spaghetti----
# 1. First, ensure we have a stable seed so the 50 random municipalities 
# don't change every time you run the code
set.seed(123)
sample_mpio <- sample(unique(imp1214_final$MPIO_CDPMP), 50)

# 2. Plotting PropInd_14
ggplot(data = imp1214_final, aes(x = year, y = PropInd_14)) +
  # Spaghetti lines for 50 random municipalities (Background)
  geom_line(data = imp1214_final %>% filter(MPIO_CDPMP %in% sample_mpio),
            aes(group = MPIO_CDPMP), alpha = 0.1, color = "gray20") +
  
  # National Mean Trend (Main Blue Line)
  stat_summary(fun = mean, geom = "line", color = "darkblue", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", color = "darkblue", size = 2) +
  
  # Smoothed LOESS Trend (Red Dashed Line)
  geom_smooth(method = "loess", color = "firebrick", linetype = "dashed", 
              se = FALSE, linewidth = 0.8) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(2000, 2020, by = 2)) +
  labs(
    title = "Trend of Indigenous Population Proportion (PropInd_14)",
    subtitle = "Average of Colombian municipalities (2000-2020)",
    x = "Year",
    y = "% Indigenous Population",
    caption = "Blue line = National Mean; Red dashed = Smoothed Trend; Gray = 50 Sampled Municipalities"
  ) +
  theme_minimal()


# Visualize map ----
library(sf)
library(viridis)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

# Load Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

# 1. Filter for a specific year (e.g., 2018) to avoid duplicate geometries
map_data_2018 <- imp1214_final %>%
  filter(year == 2018)

# 2. Merge geospatial data with demographic data
muni_map <- muni_geo %>%
  left_join(map_data_2018, by = "MPIO_CDPMP")

# 3. Create the Map
ggplot(data = muni_map) +
  geom_sf(aes(fill = PropInd_14), color = "white", size = 0.05) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1,
    labels = scales::percent_format(accuracy = 1),
    name = "% Indigenous"
  ) +
  labs(
    title = "Geographic Distribution of Indigenous Population (2018)",
    subtitle = "Based on growth-imputed census data",
    caption = "Source: SNVDEM 2024 Panel Data"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
