# Observed data from HRDAG

library(readr)

td_homi_ym <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_HRDAG/Mun_Year/td_homi_ym.csv")
td_desa_ym <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_HRDAG/Mun_Year/td_desa_ym.csv")
td_secu_ym <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_HRDAG/Mun_Year/td_secu_ym.csv")
td_recl_ym <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_HRDAG/Mun_Year/td_recl_ym.csv")

colSums(is.na(td_homi_ym))
colSums(is.na(td_desa_ym))
colSums(is.na(td_secu_ym))
colSums(is.na(td_recl_ym))
n_distinct(td_homi_ym$MPIO_CDPMP)


# Let's look at missingness per year
library(tidyverse)
library(ggplot2)
MIN_YEAR <- 1985
MAX_YEAR <- 2022

# --- Set of Municipalities (Denominator) ---

TOTAL_MUNICIPALITIES <- bind_rows(td_homi_ym, td_desa_ym, td_secu_ym, td_recl_ym) %>%
  filter(!is.na(MPIO_CDPMP)) %>%
  summarise(N = n_distinct(MPIO_CDPMP)) %>%
  pull(N)


# --- Calculate Sparse Rates ---
# Function to calculate the yearly missingness rate for a single sparse DF
calculate_sparse_rate <- function(df, total_munis, var_name) {
  df %>%
    group_by(year) %>%
    summarise(Present_Municipalities = n_distinct(MPIO_CDPMP, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      Missing_Municipalities_Count = total_munis - Present_Municipalities,
      Missingness_Rate = Missing_Municipalities_Count / total_munis,
      Variable = var_name
    ) %>%
    select(year, Variable, Missingness_Rate)
}

# Apply and combine results
rate_homi <- calculate_sparse_rate(td_homi_ym, TOTAL_MUNICIPALITIES, "obs_homi")
rate_desa <- calculate_sparse_rate(td_desa_ym, TOTAL_MUNICIPALITIES, "obs_desa")
rate_secu <- calculate_sparse_rate(td_secu_ym, TOTAL_MUNICIPALITIES, "obs_secu")
rate_recl <- calculate_sparse_rate(td_recl_ym, TOTAL_MUNICIPALITIES, "obs_recl")
combined_sparse_rates <- bind_rows(rate_homi, rate_desa, rate_secu, rate_recl)


# --- Visualize

# The secondary axis transformation: Count = Rate * TOTAL_MUNICIPALITIES
COUNT_TRANSFORM <- ~ . * TOTAL_MUNICIPALITIES

p_sparse_rate_dual <- combined_sparse_rates %>%
  ggplot(aes(x = year, y = Missingness_Rate, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Yearly Missingness Rate and Count (Sparse Data)",
    subtitle = "Missingness based on absence of Municipality-Year row.",
    x = "Year",
    y = "Missingness Rate" # Primary Axis Label
  ) +
  scale_x_continuous(breaks = seq(MIN_YEAR, MAX_YEAR, by = 5)) +
  
  # --- ADD SECONDARY AXIS HERE ---
  scale_y_continuous(
    # Primary Axis (Rate) settings
    labels = scales::percent, 
    limits = c(0, 1),
    
    # Secondary Axis (Count) settings
    sec.axis = sec_axis(
      trans = COUNT_TRANSFORM, 
      name = "Missing Municipality count" # Secondary Axis Label
    )
  ) +
  # ----------------------------------

scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # Adjust secondary axis label alignment and size
    axis.title.y.right = element_text(margin = margin(l = 10))
  )

print(p_sparse_rate_dual)


##---- Distributions ----
# Define constants
MIN_YEAR <- 1985
MAX_YEAR <- 2022
TOTAL_YEARS <- MAX_YEAR - MIN_YEAR + 1 # 38 years

# --- 1. Create a Function to Calculate Sparse Missing Years ---

# Function to calculate the distribution of missing years for a single sparse DF
calculate_sparse_muni_missingness <- function(df, total_years, var_name) {
  df %>%
    
    # Group by municipality ID
    group_by(MPIO_CDPMP) %>%
    
    # Count the number of years PRESENT for each municipality
    summarise(
      Years_Present = n() # n() counts the number of rows (years) present
    ) %>%
    ungroup() %>%
    
    # Calculate the number of years MISSING
    mutate(
      Years_Missing = total_years - Years_Present,
      Variable = var_name
    ) %>%
    select(MPIO_CDPMP, Variable, Years_Missing)
}


# --- 2. Apply Function and Combine Results ---

# Apply the function to each of the four sparse DataFrames
muni_missing_homi <- calculate_sparse_muni_missingness(td_homi_ym, TOTAL_YEARS, "obs_homi")
muni_missing_desa <- calculate_sparse_muni_missingness(td_desa_ym, TOTAL_YEARS, "obs_desa")
muni_missing_secu <- calculate_sparse_muni_missingness(td_secu_ym, TOTAL_YEARS, "obs_secu")
muni_missing_recl <- calculate_sparse_muni_missingness(td_recl_ym, TOTAL_YEARS, "obs_recl")

# Combine all results into one long DataFrame for plotting
combined_muni_missingness <- bind_rows(muni_missing_homi, muni_missing_desa, muni_missing_secu, muni_missing_recl)

# Print a summary of the resulting table
cat("\n## Summary of Missing Years per Municipality:\n")
combined_muni_missingness %>%
  group_by(Variable) %>%
  summarise(
    Mean_Missing = round(mean(Years_Missing), 1),
    Median_Missing = median(Years_Missing),
    Max_Missing = max(Years_Missing)
  ) %>%
  print()


# --- 3. Generate the Faceted Histogram Graphic ---

p_muni_distribution <- combined_muni_missingness %>%
  ggplot(aes(x = Years_Missing)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white", alpha = 0.8) +
  
  # Create a separate histogram for each variable
  facet_wrap(~ Variable, scales = "free_y") + 
  
  labs(
    title = "Distribution of Missing Years per Municipality (Sparse Data)",
    subtitle = paste("Based on row absence across", TOTAL_YEARS, "years (1985-2022)"),
    x = "Number of Missing Years",
    y = "Count of Municipalities"
  ) +
  scale_x_continuous(breaks = seq(0, TOTAL_YEARS, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_muni_distribution)


#----Table of Missing municipalities per year----

# --- Municipality Universe ---

# Find the union of all unique municipality codes across all four DFs
# This establishes the maximum expected number (the denominator for the rate).
TOTAL_UNIVERSE_MUNICIPALITIES <- bind_rows(td_homi_ym, td_desa_ym, td_secu_ym, td_recl_ym) %>%
  filter(!is.na(MPIO_CDPMP)) %>%
  summarise(N = n_distinct(MPIO_CDPMP)) %>%
  pull(N)
cat(paste("Total Municipality Universe (Denominator for Rate):", TOTAL_UNIVERSE_MUNICIPALITIES, "\n"))

# --- 2. Create Aggregation Function and Apply to all DFs ---

# Function to calculate yearly unique municipalities, total observations, and missingness rate
yearly_summary_and_rate <- function(df, obs_var_name, universe) {
  df %>%
    # Ensure MPIO_CDPMP is character for consistent distinct counting
    mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>% 
    group_by(year) %>%
    summarise(
      # Count unique municipalities present in the data for that year
      Municipalities_Present = n_distinct(MPIO_CDPMP, na.rm = TRUE),
      # Sum the observation variable (e.g., obs_homi)
      Total_Observations = sum(.data[[obs_var_name]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Calculate Missingness Rate
    mutate(
      Missing_Count = universe - Municipalities_Present,
      Missingness_Rate = Missing_Count / universe,
      Variable = obs_var_name
    )
}

# Apply the function to each dataframe
summary_homi <- yearly_summary_and_rate(td_homi_ym, "obs_homi", TOTAL_UNIVERSE_MUNICIPALITIES)
summary_desa <- yearly_summary_and_rate(td_desa_ym, "obs_desa", TOTAL_UNIVERSE_MUNICIPALITIES)
summary_secu <- yearly_summary_and_rate(td_secu_ym, "obs_secu", TOTAL_UNIVERSE_MUNICIPALITIES)
summary_recl <- yearly_summary_and_rate(td_recl_ym, "obs_recl", TOTAL_UNIVERSE_MUNICIPALITIES)

# Combine the results into a single long dataframe
combined_yearly_summary <- bind_rows(
  summary_homi, summary_desa, summary_secu, summary_recl
)

# --- 3. Display Summary Table ---

# Display a sample of the yearly counts and totals
cat("\n## 📊 Sample of Yearly Summary (Counts & Totals):\n")
combined_yearly_summary %>%
  filter(year %in% c(1995, 2005, 2015)) %>%
  select(Variable, year, Municipalities_Present, Total_Observations) %>%
  pivot_wider(names_from = Variable, values_from = c(Municipalities_Present, Total_Observations)) %>%
  print()

# --- 4. Visualize the Missingness Rate ---

p_missing_rate_all <- combined_yearly_summary %>%
  ggplot(aes(x = year, y = Missingness_Rate, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Yearly Missing Municipality Rate by Violence Indicator",
    subtitle = paste("Rate calculated based on a universe of", TOTAL_UNIVERSE_MUNICIPALITIES, "municipalities."),
    x = "Year",
    y = "Missingness Rate (Proportion of Universe)"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(combined_yearly_summary$year), max(combined_yearly_summary$year), by = 5)) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_missing_rate_all)


library(scales) # for percentage formatting

# --- 1. Calculate the Average Missingness Rate per Variable ---

average_missingness_rates <- combined_yearly_summary %>%
  group_by(Variable) %>%
  summarise(
    Average_Missingness_Rate = mean(Missingness_Rate, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Formatted_Rate = scales::percent(Average_Missingness_Rate, accuracy = 0.1)
  ) %>%
  select(
    Variable, 
    Formatted_Rate
  ) %>%
  rename(
    Indicator = Variable,
    `Average Missingness Rate` = Formatted_Rate
  )

# --- 2. Print the Table ---

cat("\n## 📋 Average Missingness Rate Across All Years (Arithmetic Mean)\n")
print(average_missingness_rates)
