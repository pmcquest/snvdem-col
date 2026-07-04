# Validation: V-Dem coder comments

# How well does the textual data correlate to our quantitative measure?


library(dplyr)
library(stringi)
library(ggplot2)
library(tidyr)
library(sf)
library(readxl)
library(effsize)
library(gt)
library(broom)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")


# V-Dem coder comments
com <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/outliers3.rds")
com2 <- read_excel("data/panel/09_analysis_scripts/Validation/VDem-com/review/v2elsnless_v2elsnmore_colombia_pmq.xlsx")

# Count unique municipality names
unique_muni_count <- length(unique(com$MPIO_CNMBR))
cat("Number of unique municipalities:", unique_muni_count)
# Count how many times each value appears in moreless
moreless_counts <- table(com$moreless)
print(moreless_counts)
# To see it as a percentage of the total 26,682 observations:
prop.table(moreless_counts) * 100

# Calculate unique municipalities per year
muni_per_year <- com %>%
  group_by(Year) %>%
  summarise(
    Unique_Munis = n_distinct(MPIO_CNMBR),
    Total_Observations = n()
  )

print(muni_per_year)

ggplot(muni_per_year, aes(x = Year, y = Unique_Munis)) +
  geom_col(fill = "steelblue") +
  # Changed yheight to yintercept
  geom_hline(yintercept = 1102, linetype = "dashed", color = "red", size = 1) + 
  annotate("text", x = min(muni_per_year$Year) + 5, y = 1150, 
           label = "Total Colombian Municipalities (~1102)", color = "red") +
  theme_minimal() +
  labs(title = "Expert Coverage: Unique Municipalities per Year",
       subtitle = "Reference line shows the total number of municipalities in Colombia",
       y = "Count of Unique Municipalities",
       x = "Year")


# SNVDEM
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds")

# Function for Min-Max Normalization
normalize_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply to your main dataframe
snvdem <- snvdem %>%
  mutate(
    snelect_norm = normalize_01(snelect),
    sncivlib_norm = normalize_01(sncivlib),
    sndem_norm   = normalize_01(sndem)
  )
# Verify the new range
summary(snvdem[c("snelect_norm", "sncivlib_norm", "sndem_norm")])

# Municipality-year dataframe
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/MunYrs.rds")


# Clean and join data ----
# 1. Clean and prepare MunYrs
# We extract the Department code from MPIO_CDPMP and standardize names
MunYrs_clean <- MunYrs %>%
  mutate(
    year = as.numeric(year),
    # Extract the first two digits of the DANE code as Department Code
    DPTO_CCDGO = substr(MPIO_CDPMP, 1, 2),
    # Remove accents and convert to uppercase for robust matching
    MPIO_CNMBR = stri_trans_general(MPIO_CNMBR, "Latin-ASCII") %>% toupper()
  ) %>%
  # Ensure we have a unique mapping per Year-Dept-Name
  distinct(MPIO_CNMBR, DPTO_CCDGO, year, MPIO_CDPMP)

# 2. Clean and prepare the com dataframe
com_clean <- com %>%
  mutate(
    MPIO_CNMBR = stri_trans_general(MPIO_CNMBR, "Latin-ASCII") %>% toupper()
  )

# 3. Join com with MunYrs to get the MPIO_CDPMP code
# We join on Name, Dept Code, and Year to avoid errors with duplicate names
com_with_code <- com_clean %>%
  left_join(MunYrs_clean, by = c("MPIO_CNMBR", "DPTO_CCDGO", "Year" = "year"))

# 4. Finally, join with snvdem using the MPIO_CDPMP and Year
# We only select the three variables of interest from snvdem
final_df <- com_with_code %>%
  left_join(
    snvdem %>% select(MPIO_CDPMP, year, snelect_norm, sncivlib_norm, sndem_norm),
    by = c("MPIO_CDPMP", "Year" = "year")
  )
# Create z-scores for regression
final_df <- final_df %>%
  mutate(
    snelect_z = as.vector(scale(snelect_norm)),
    sndem_z   = as.vector(scale(sndem_norm))
  )

# Check the result
head(final_df)


# --- 1. Basic Descriptive Validation ----
## Perform the T-test ----
# snelect_norm ~ moreless means "compare snelect based on the moreless groups"
t_result <- t.test(snelect_norm ~ moreless, data = final_df)

# Print the results
print(t_result)

# To see the "t" value and "p" value specifically:
cat("T-statistic:", t_result$statistic, "\n")
cat("P-value:", t_result$p.value, "\n")

# Compare average snelect/sndem scores across the 'moreless' categories
validation_summary <- final_df %>%
  filter(!is.na(moreless), !is.na(snelect_norm)) %>%
  group_by(moreless) %>%
  summarise(
    obs_count = n(),
    avg_snelect = mean(snelect_norm, na.rm = TRUE),
    sd_snelect  = sd(snelect_norm, na.rm = TRUE),
    avg_sndem   = mean(sndem_norm, na.rm = TRUE),
    # Weighting the mean by number of coders to see if consensus matters
    weighted_avg_snelect = weighted.mean(snelect_norm, Ncoders, na.rm = TRUE)
  )

print(validation_summary)

### Table ----
# 1. Calculate Cohen's d
d_val <- cohen.d(snelect_norm ~ moreless, data = final_df, na.rm = TRUE)$estimate

# 2. Extract t-test stats using broom::tidy for a clean format
t_stats <- tidy(t_result)

# 3. Combine with your validation_summary
table_data <- validation_summary %>%
  mutate(
    t_stat = t_stats$statistic,
    df = t_stats$parameter,
    p_val = t_stats$p.value,
    d = d_val
  ) %>%
  select(moreless, obs_count, avg_snelect, sd_snelect, t_stat, df, p_val, d)

# 4. Create the Publication-Ready Table
main_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "Table 1. Welch Two-Sample T-Test Results",
    subtitle = "Comparing Electoral Democracy (snelect_norm) by Group"
  ) %>%
  cols_label(
    moreless = "Group",
    obs_count = "N",
    avg_snelect = "Mean",
    sd_snelect = "SD",
    t_stat = "t",
    df = "df",
    p_val = "p",
    d = "Cohen's d"
  ) %>%
  fmt_number(columns = c(avg_snelect, sd_snelect, t_stat, df, d), decimals = 2) %>%
  # Updated p-value formatting logic
  text_transform(
    locations = cells_body(columns = p_val),
    fn = function(x) {
      val <- as.numeric(x)
      ifelse(val < 0.001, "< .001", sprintf("%.3f", val))
    }
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    # Fixed argument name: table.font.size instead of table_font.size
    table.font.size = px(14) 
  )

# Preview the table
main_table

# 5. Save the table
gtsave(main_table, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/Table_TTest_Results.png") # Saves as an image
gtsave(main_table, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/Table1_TTest_Results.rtf") # Saves as an editable Word-compatible file




# --- 2. Correlation Analysis ---
# Convert 'moreless' to a dummy (More=1, Less=0) to check correlation
cor_data <- final_df %>%
  filter(!is.na(moreless)) %>%
  mutate(more_dummy = ifelse(moreless == "More", 1, 0))

cor_snelect <- cor(cor_data$more_dummy, cor_data$snelect_norm, use = "complete.obs")
cor_sndem   <- cor(cor_data$more_dummy, cor_data$sndem_norm, use = "complete.obs")

message("Correlation between 'More' coding and snelect: ", round(cor_snelect, 3))
message("Correlation between 'More' coding and sndem: ", round(cor_sndem, 3))


# Visualization 1: The "Gap" (Boxplot)
ggplot(cor_data, aes(x = moreless, y = snelect_norm, fill = moreless)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("Less" = "#ef8a62", "More" = "#67a9cf")) +
  labs(
    title = "Validation: Distribution of snelect Index by Expert Assessment",
    subtitle = paste("Overall Correlation (r) =", round(cor_snelect, 3)),
    x = "Expert Qualitative Assessment",
    y = "Normalized snelect Index",
    caption = "Diamond represents the group mean."
  )


# Calculate correlation by number of coders
consensus_check <- cor_data %>%
  group_by(Ncoders) %>%
  filter(n() > 50) %>% # Ignore groups with very few observations
  summarise(
    correlation = cor(more_dummy, snelect_norm, use = "complete.obs"),
    n_obs = n()
  )

print(consensus_check)


# Check correlation by Department
dept_correlations <- cor_data %>%
  group_by(Dept_Name) %>%
  summarise(correlation = cor(more_dummy, snelect_norm, use = "complete.obs")) %>%
  arrange(desc(correlation))

# Look at the top and bottom 5 departments
head(dept_correlations, 5)
tail(dept_correlations, 5)

# Identify the biggest 'mismatches'
mismatches <- cor_data %>%
  mutate(diff = abs(more_dummy - snelect_norm)) %>%
  select(MPIO_CNMBR, Year, moreless, snelect_norm, diff) %>%
  arrange(desc(diff))

head(mismatches, 10)


# Mapping responses ----

# 1. Prepare data with explicit handling of the sf object
map_comparison <- final_df %>%
  filter(Year %in% c(2002, 2007, 2011, 2015, 2019)) %>%
  mutate(
    # Use as.numeric to strip the attributes scale() adds, which can confuse ggplot
    index_z = as.numeric(scale(snelect_norm)),
    expert_val = ifelse(moreless == "More", 1, -1),
    discrepancy = index_z - expert_val
  )

# 2. Join and ensure the result is still an sf object
# We use inner_join here for the facets so we don't have thousands of empty polygons 
# slowing down the render, but keep a background layer for the whole country.
geo_mismatch <- muni_geo %>%
  inner_join(map_comparison, by = "MPIO_CDPMP")

# 3. Plot with a background layer for context
ggplot() +
  # Background: All municipalities in light grey
  geom_sf(data = muni_geo, fill = "grey90", color = "white", size = 0.01) +
  # Data: Only where we have discrepancies
  geom_sf(data = geo_mismatch, aes(fill = discrepancy), color = NA) +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_gradient2(
    low = "#ca0020",  # Red: Index is "pessimistic" compared to experts
    mid = "#f7f7f7",  # White: They agree
    high = "#0571b0", # Blue: Index is "optimistic" compared to experts
    midpoint = 0,
    name = "Mismatch Score"
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
    title = "Spatial Mismatch: snelect Index vs. Expert Comments",
    subtitle = "Blue = Index over-estimates democracy | Red = Index under-estimates democracy",
    caption = "Grey areas indicate no expert data for that year."
  )

# 1. Assign your plot to an object name (e.g., mismatch_plot)
mismatch_plot <- ggplot() +
  geom_sf(data = muni_geo, fill = "grey90", color = "white", size = 0.01) +
  geom_sf(data = geo_mismatch, aes(fill = discrepancy), color = NA) +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_gradient2(
    low = "#ca0020", 
    mid = "#f7f7f7", 
    high = "#0571b0", 
    midpoint = 0,
    name = "Mismatch Score"
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
    title = "Spatial Mismatch: snelect Index vs. Expert Comments",
    subtitle = "Blue = Index over-estimates democracy | Red = Index under-estimates democracy"
  )

# 2. Save the object directly to a file
# This will create a file named "mismatch_map.png" in your current folder
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/imgs/mismatch_map.png", plot = mismatch_plot, width = 12, height = 8, dpi = 300)


# 1. Calculate Department-level Discrepancy and Dissolve Boundaries
dept_geo_mismatch <- geo_mismatch %>%
  group_by(Dept_Name, Year) %>%
  summarise(
    # Mean discrepancy across all municipalities in the department
    mean_discrepancy = mean(discrepancy, na.rm = TRUE),
    # This line tells sf to merge the municipal shapes into one dept shape
    geometry = st_union(geometry), 
    .groups = "drop"
  )

# 2. Plot the Department Map
depto <- ggplot(dept_geo_mismatch) +
  geom_sf(aes(fill = mean_discrepancy), color = "white", size = 0.2) +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_gradient2(
    low = "#ca0020",  # Red: Index is too pessimistic
    mid = "#f7f7f7",  # White: Convergence
    high = "#0571b0", # Blue: Index is too optimistic
    midpoint = 0,
    name = "Avg Mismatch"
  ) +
  theme_void() +
  labs(
    title = "Regional Mismatch: snelect Index vs. Expert Comments",
    subtitle = "Aggregated at the Department Level",
    caption = "Blue = Index over-estimates democracy | Red = Index under-estimates democracy"
  )

ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/imgs/mismatch_map_depto.png", plot = depto, width = 12, height = 8, dpi = 300)


library(geodata)
library(sf)

# 1. Download official Dept boundaries (ADM1)
# path = "." saves the file to your current working directory
col_depts <- gadm(country = "COL", level = 1, path = ".") %>% 
  st_as_sf()

# 2. Join your aggregated data to this clean shapefile
# Ensure the department names match exactly (e.g., all caps)
dept_geo_mismatch <- col_depts %>%
  left_join(dept_discrepancy, by = c("NAME_1" = "Dept_Name"))

# 3. Save the map
dept_map <- ggplot(dept_geo_mismatch) +
  geom_sf(aes(fill = mean_discrepancy), color = "white") +
  facet_wrap(~Year) +
  scale_fill_gradient2(low = "#ca0020", mid = "#f7f7f7", high = "#0571b0") +
  theme_void()

ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/imgs/mismatch_map_depto.png", plot = dept_map, width = 8, height = 10)
