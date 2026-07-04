# ViPAA data analysis #

# Load the essential packages
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2) 
library(scales)

#----Load and Prepare ViPAA Data----
ViPAA_col <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_Osorio/ViPAA-Col/Database/VIPAA_v1.3.dta")

# Clean up the DF
ViPAA_col <- ViPAA_col %>%
  rename(MPIO_CDPMP = 10, year = 2) 
ViPAA_cols <- ViPAA_col %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  mutate(MPIO_CDPMP = ifelse(nchar(MPIO_CDPMP) == 4, paste0("0", MPIO_CDPMP), MPIO_CDPMP))

#---- Calculate violent days----

# Define actor groups for filtering
ACTORS_CIVIL_UNREST <- c(1, 2, 3, 7)
ACTOR_ILLICIT_ACTIVITY <- 4

# --- 1. Prepare Base Data and Ensure Unique Day Presence ---

# First, ensure data types are correct and create a unique identifier for 
# a single violent day (Muni-Year-Month-Day) regardless of how many times 
# the same set of actors is mentioned on that day.
base_days <- ViPAA_cols %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP),
         year = as.integer(year)) %>%
  select(MPIO_CDPMP, year, month, day, actor_main) %>%
  distinct() 

# --- 2. Calculate Total Violent Days ---
violent_days_total <- base_days %>%
  distinct(MPIO_CDPMP, year, month, day) %>% 
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    Violent_Days_Total = n(), # Count the number of unique days
    .groups = 'drop'
  )

# --- 3. Calculate Violent Days of Civil Unrest (Actors 1, 2, 3, 7) ---
# This counts days where AT LEAST ONE of the civil unrest actors was present.
violent_days_unrest <- base_days %>%
  filter(actor_main %in% ACTORS_CIVIL_UNREST) %>%
  distinct(MPIO_CDPMP, year, month, day) %>% 
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    Violent_Days_Civil_Unrest = n(),
    .groups = 'drop'
  )


# --- 4. Calculate Violent Days of Illicit Activity (Actor 4) ---
# This counts days where AT LEAST actor 4 was present.
violent_days_illicit <- base_days %>%
  filter(actor_main == ACTOR_ILLICIT_ACTIVITY) %>%
  distinct(MPIO_CDPMP, year, month, day) %>% 
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    Violent_Days_Illicit_Activity = n(),
    .groups = 'drop'
  )

# --- 5. Merge Results into Final DataFrame ---
ViPAA_1011 <- violent_days_total %>%
  full_join(violent_days_unrest, by = c("MPIO_CDPMP", "year")) %>%
  full_join(violent_days_illicit, by = c("MPIO_CDPMP", "year")) %>%
  # Replace NAs (where an activity type was completely absent for that muni-year) with 0
  mutate(
    across(starts_with("Violent_Days_"), ~replace_na(., 0))
  )

# Output summary
print(head(ViPAA_1011))

##----Export data (pending)----

#save as .rds for #10. Civil Unrest and #11. Illicit activity...
write_rds(ViPAA_1011, "data/panel/data_raw/10-11_Osorio/ViPAA-Col/Database/ViPAA_days_1011.rds")


# Misc ----
##----Geographic spread ----
# 915 municipalities mentioned in ViPAA
cat(paste("Unique Municipalities in ViPAA:", n_distinct(ViPAA_col$MPIO_CDPMP), "\n"))

# Load the complete reference panel
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/MunYrs.rds")
# Ensure the MPIO_CDPMP and year columns have compatible types for joining
MunYrs <- MunYrs %>%
  mutate(
    MPIO_CDPMP = as.character(MPIO_CDPMP),
    year = as.double(year) # Convert year to numeric for consistent joining
  )
# 1125 municipalities mentioned in MunYrs (master list)
cat(paste("Unique Municipalities in MunYrs:", n_distinct(MunYrs$MPIO_CDPMP), "\n"))

total_expected_per_year <- MunYrs %>%
  group_by(year) %>%
  summarise(Total_Expected = n_distinct(MPIO_CDPMP)) %>%
  ungroup()


present_in_vipaa_per_year <- ViPAA_col %>%
  mutate(
    MPIO_CDPMP = as.character(MPIO_CDPMP), 
    year = as.double(year)                
  ) %>%
  select(MPIO_CDPMP, year) %>%
  distinct() %>% 
  group_by(year) %>%
  summarise(Present_Count = n_distinct(MPIO_CDPMP)) %>%
  ungroup()

yearly_missingness_count <- total_expected_per_year %>%
  left_join(present_in_vipaa_per_year, by = "year") %>%
  mutate(
    # Handle cases where ViPAA_col has ZERO data for an entire year (NA)
    Present_Count = replace_na(Present_Count, 0),
    Missing_Count = Total_Expected - Present_Count
  ) %>%
  select(year, Missing_Count, Total_Expected) # Include Total_Expected for subtitle

print(head(yearly_missingness_count))


# --- 5. Visualize the Yearly Missingness Count ---
p_missing_count <- yearly_missingness_count %>%
  ggplot(aes(x = year, y = Missing_Count)) +
  geom_line(color = "#D95F02", linewidth = 1) +
  geom_point(color = "#D95F02") +
  labs(
    title = "Missing Municipal Observations in ViPAA_col",
    subtitle = paste("Based on the expected universe of", max(yearly_missingness_count$Total_Expected), "municipalities."),
    x = "Year",
    y = "Number of Municipalities Without Observations"
  ) +
  scale_x_continuous(breaks = seq(min(yearly_missingness_count$year), max(yearly_missingness_count$year), by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_missing_count)

# Average missingness with reduced study period (2000-2020)
START_YEAR <- 2000
END_YEAR <- 2020

# Filter the data for the 2000-2020 period
filtered_data <- yearly_missingness_count %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  # Calculate the yearly rate for the filtered period
  mutate(Yearly_Rate = Missing_Count / Total_Expected)

# --- 1. Calculate the Weighted Average (Overall Rate) ---
# This is the most accurate measure of total data loss.
weighted_summary <- filtered_data %>%
  summarise(
    Total_Missing = sum(Missing_Count, na.rm = TRUE),
    Total_Expected = sum(Total_Expected, na.rm = TRUE)
  )
overall_rate_2000_2020 <- weighted_summary$Total_Missing / weighted_summary$Total_Expected

# --- 2. Calculate the Arithmetic Mean (Simple Average) ---
# This is the average of the yearly rates.
arithmetic_mean_rate_2000_2020 <- filtered_data %>%
  summarise(
    Average_Rate = mean(Yearly_Rate, na.rm = TRUE)
  ) %>%
  pull(Average_Rate)

# --- Output Results ---
cat(paste("Total Missing Municipality-Years (2000-2020):", format(weighted_summary$Total_Missing, big.mark = ","), "\n"))
cat(paste("Total Expected Municipality-Years (2000-2020):", format(weighted_summary$Total_Expected, big.mark = ","), "\n"))
cat(paste("Weighted Overall Rate (2000-2020):", scales::percent(overall_rate_2000_2020, accuracy = 0.01), "\n\n"))
# Arithmetic mean
cat(paste("Arithmetic Mean of Yearly Rates (2000-2020):", scales::percent(arithmetic_mean_rate_2000_2020, accuracy = 0.01), "\n"))


##---- Summarize data ----
VIPAA_cols <- ViPAA_col %>%
  mutate(
    year = as.numeric(year),
    actor_main = as.factor(actor_main)
  )

# Group observations by actor and year
summary_table_count <- VIPAA_cols %>%
  group_by(actor_main, year) %>%
  summarize(
    Count_Obs = n(),
    .groups = 'drop' # Ungroup the data after calculation
  ) %>%
  arrange(actor_main, year)

print(summary_table_count)

###---- Grouped line chart ----

# Calculate the observation count for plotting
plot_data_count <- VIPAA_cols %>%
  group_by(actor_main, year) %>%
  summarize(
    Count_Obs = n(),
    .groups = 'drop'
  )

actor_labels <- c(
  "1" = "government",
  "2" = "insurgents",
  "3" = "paramilitaries",
  "4" = "criminal organizations",
  "7" = "FARC dissidents"
)

actor_colors <- c(
  "1" = "blue",          # Government
  "2" = "red",           # Insurgents
  "3" = "darkgreen",     # Paramilitaries
  "4" = "purple",        # Criminal Organizations
  "7" = "orange"         # FARC Dissidents
)


trend_plot_count <- ggplot(
  data = plot_data_count, 
  aes(
    x = year, 
    y = Count_Obs, 
    group = actor_main, 
    color = actor_main
  )
) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(
    name = "Main Actor",
    labels = actor_labels,
    values = actor_colors
  ) + 
  labs(
    title = "Time Trends of Observation Count by Main Actor",
    x = "Year",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(trend_plot_count)

##----Compiling actor presence----
###----All actors ----
actor_summary <- VIPAA_cols %>%
  filter(actor_main %in% c(1, 2, 3, 4, 7)) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    Actor_Count = n_distinct(actor_main), 
    .groups = 'drop' 
  ) %>%
  mutate(year = as.integer(year))

### Check sparseness ----
munyrs_complete <- MunYrs %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  mutate(year = as.integer(year)) %>%
  distinct()

# Calculate the total expected count of municipalities per year (our universe)
total_expected_per_year <- munyrs_complete %>%
  group_by(year) %>%
  summarise(
    Total_Expected = n_distinct(MPIO_CDPMP),
    .groups = 'drop'
  )

actor_summary_distinct <- actor_summary %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  group_by(year) %>%
  summarise(
    Municipalities_Mentioned = n_distinct(MPIO_CDPMP),
    .groups = 'drop'
  )

# Calculate Missingness
yearly_coverage_check <- total_expected_per_year %>%
  left_join(actor_summary_distinct, by = "year") %>%
  mutate(
    Municipalities_Mentioned = replace_na(Municipalities_Mentioned, 0),
    Municipalities_Not_Mentioned = Total_Expected - Municipalities_Mentioned
  ) %>%
  mutate(
    Missingness_Rate = Municipalities_Not_Mentioned / Total_Expected
  ) %>%
  select(year, Total_Expected, Municipalities_Mentioned, Municipalities_Not_Mentioned, Missingness_Rate)


# Results 
overall_rate <- sum(yearly_coverage_check$Municipalities_Not_Mentioned) / sum(yearly_coverage_check$Total_Expected)
cat(paste("\nOverall Weighted Missingness Rate (Across all Muni-Years):", 
          scales::percent(overall_rate, accuracy = 0.01), "\n"))


##----Separating by grouping----
# Define actor groups
ACTORS_V10 <- c(1, 2, 3, 7)
ACTOR_V11 <- 4

# --- 1. Define Unique Actor Presence (Muni-Day-Month-Year) ---
# This step ensures an actor is only counted once per day.
unique_actor_presence <- VIPAA_cols %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP),
         year = as.integer(year)) %>%
  filter(actor_main %in% c(ACTORS_V10, ACTOR_V11)) %>%
  distinct(MPIO_CDPMP, year, month, day, actor_main) %>% 
  
  # Now, collapse to the Muni-Month-Year-Actor level for the final month count
  # This ensures that even if an actor was present on 5 different days in Jan, 
  # they still only contribute 1 to the final month count.
  distinct(MPIO_CDPMP, year, month, actor_main) %>%
  ungroup()

# --- 2. Create ViPAA_10 (Actors 1, 2, 3, 7) ---
vipaa_10 <- unique_actor_presence %>%
  filter(actor_main %in% ACTORS_V10) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    ViPAA_10 = n_distinct(month), 
    .groups = 'drop'
  )

# --- 3. Create ViPAA_11 (Actor 4) ---
vipaa_11 <- unique_actor_presence %>%
  filter(actor_main == ACTOR_V11) %>%
  group_by(MPIO_CDPMP, year) %>%
  summarise(
    # ViPAA_11: Count the number of *unique months* Actor 4 was present
    ViPAA_11 = n_distinct(month), 
    .groups = 'drop'
  )

# --- 4. Merge Results into Final Summary DF ---
final_actor_variables <- full_join(vipaa_10, vipaa_11, by = c("MPIO_CDPMP", "year")) %>%
    # Replace NAs (where an actor group was completely absent for that muni-year) with 0
  mutate(
    ViPAA_10 = replace_na(ViPAA_10, 0),
    ViPAA_11 = replace_na(ViPAA_11, 0)
  )

# Output summary
print(head(final_actor_variables))


##---- Re-create the map (pending) ----
library(dplyr)
library(tidyr)
library(sf)
library(gganimate)
library(gifski)

col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)
# The interactive map can be found here: https://www.colombiaarmedactors.org/

