# Universidad de Medellin


library(readr)
library(dplyr)
library(stringi)
library(ggplot2)


UMedM <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/UMedellin/Datos_municipal.csv")
snvdem <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SNDEM_tentative.rds")
# bridge dataframe
MunYrs <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/01_raw_data/MunYrs.rds")

# --- Step A: Cleaning Function ---
# This removes accents, removes those "diamond" symbols, and trims whitespace
clean_col_text <- function(x) {
  x <- as.character(x)
  x <- stri_trans_general(x, "Latin-ASCII") # Convert ñ to n, á to a
  x <- gsub("[^[:alnum:][:space:]]", "", x) # Remove any remaining "diamond" symbols
  tolower(trimws(x))
}

# --- Step B: Prepare the Bridge (MunYrs) ---
bridge_clean <- MunYrs %>%
  mutate(
    match_mpio = clean_col_text(MPIO_CNMBR),
    match_dpto = clean_col_text(departamento),
    year = as.numeric(year)
  ) %>%
  select(MPIO_CDPMP, match_mpio, match_dpto, year) %>%
  distinct() # Ensure no duplicate rows

# --- Step C: Prepare Author's Dataset (UMedM) ---
UMedM_clean <- UMedM %>%
  rename(CdDS = `Valor CdDS`) %>%
  mutate(
    match_mpio = clean_col_text(Municipio),
    match_dpto = clean_col_text(Departamento),
    Year = as.numeric(Year)
  )

# --- Step D: Match DANE Codes to UMedM ---
# Joining by name, department, AND year to handle changes over time
UMedM_with_codes <- UMedM_clean %>%
  left_join(bridge_clean, by = c("match_mpio" = "match_mpio", 
                                 "match_dpto" = "match_dpto", 
                                 "Year" = "year"))

# --- Step E: Final Join with your Dataset (snvdem) ---
validation_df <- snvdem %>%
  mutate(year = as.numeric(year),
         MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
  filter(year >= 2016) %>%
  inner_join(UMedM_with_codes, by = c("MPIO_CDPMP", "year" = "Year"))

# --- Step F: Results & Validation ---
pearson_cor <- cor(validation_df$sndem, validation_df$CdDS, use = "complete.obs")
cat("The Pearson Correlation Coefficient is:", round(pearson_cor, 3))

ggplot(validation_df, aes(x = CdDS, y = sndem)) +
  geom_point(alpha = 0.1, color = "grey") +
  geom_density_2d(color = "steelblue") + # Adds "topographic" lines for density
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  labs(
    title = "Validation: snvdem vs. UMedM (2016-2023)",
    subtitle = paste("Pearson Correlation:", round(pearson_cor, 3)),
    x = "UMedM (CdDs Score 0-100)",
    y = "snvdem (sndem Score 0-1)"
  ) +
  theme_minimal()


## Sub-component analysis

library(reshape2)

# --- Step 1: Force Rename and Subset ---
# We create a specific dataframe for the correlation to ensure names are 100% correct
comp_data <- validation_df %>%
  select(
    # Your variables
    sndem, snelect, sncivlib,
    # Author variables renamed to short strings
    Law    = `Estado de Derecho`,
    Part   = `Participacion`,
    Comp   = `Competencia Electoral`,
    A_Elec = `A-Electoral`,
    A_Inst = `A-Institucional`,
    CivLib = `Libertades civiles`,
    Solid  = `Solidaridad igualdad`,
    CdDS   = CdDS
  )

# --- Step 2: Calculate Matrix ---
cor_matrix_full <- cor(comp_data, use = "pairwise.complete.obs")

# --- Step 3: Dynamic Subsetting (Prevents the Error) ---
# This looks for which of our 'target' names actually made it into the matrix
my_vars <- intersect(c("sndem", "snelect", "sncivlib"), rownames(cor_matrix_full))
author_vars <- intersect(c("Law", "Part", "Comp", "A_Elec", "A_Inst", "CivLib", "Solid", "CdDS"), colnames(cor_matrix_full))

# Subset the matrix using the confirmed existing names
cross_cor <- cor_matrix_full[my_vars, author_vars]

# --- Step 4: Visualize ---
melted_cor <- melt(cross_cor)
colnames(melted_cor) <- c("snvdem_Var", "UMedM_Var", "Correlation")

ggplot(melted_cor, aes(x = UMedM_Var, y = snvdem_Var, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0072B2", high = "#D55E00", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name="Pearson\nr") +
  geom_text(aes(label = round(Correlation, 2)), size = 3.5) +
  theme_minimal() +
  labs(title = "Subnational Democracy Validation Matrix",
       x = "UMedM Components (Shortened)",
       y = "Your Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check if your variables have enough 'spread' compared to theirs
validation_df %>%
  select(sndem, snelect, sncivlib, CdDS) %>%
  summarise(across(everything(), sd, na.rm = TRUE))


# Standardizing both variables
validation_df <- validation_df %>%
  mutate(
    sndem_z = (sndem - mean(sndem, na.rm = TRUE)) / sd(sndem, na.rm = TRUE),
    CdDS_z  = (CdDS - mean(CdDS, na.rm = TRUE)) / sd(CdDS, na.rm = TRUE)
  )

# Now, a "1" in both columns means the municipality is 1 SD above the average
# for that specific dataset.


# 1. Filter for a few key municipalities to make the plot readable
# You can change these names to any municipality in your dataset
target_cities <- c("bogota", "medellin", "cali", "barranquilla")

plot_data <- validation_df %>%
  filter(match_mpio %in% target_cities) %>%
  # Pivot the data to a "long" format for ggplot
  select(match_mpio, year, sndem_z, CdDS_z) %>%
  tidyr::pivot_longer(cols = c(sndem_z, CdDS_z), 
                      names_to = "Dataset", 
                      values_to = "Z_Score")

# 2. Create the Time-Series Plot
ggplot(plot_data, aes(x = year, y = Z_Score, color = Dataset, group = Dataset)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_wrap(~match_mpio, scales = "free_y") +
  scale_color_manual(values = c("sndem_z" = "#D55E00", "CdDS_z" = "#0072B2"),
                     labels = c("Your Index (sndem)", "Author's Index (CdDS)")) +
  labs(title = "Z-Score Comparison: Relative Democracy Levels Over Time",
       subtitle = "Standardized to Mean = 0, SD = 1",
       x = "Year",
       y = "Z-Score (Standard Deviations from Mean)") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Calculate correlation for each municipality across the years 2016-2023
municipality_agreement <- validation_df %>%
  group_by(MPIO_CDPMP, match_mpio, match_dpto) %>%
  summarise(
    local_cor = cor(sndem, CdDS, use = "complete.obs"),
    n_years = n(),
    .groups = "drop"
  ) %>%
  # Filter for municipalities with data for at least 5 years to ensure a valid correlation
  filter(n_years >= 5) %>%
  arrange(desc(local_cor))

# Top 10 Most Correlated Municipalities (High Agreement)
head(municipality_agreement, 10)

# Top 10 Least Correlated (Highest Disagreement)
tail(municipality_agreement, 10)



# 1. Set the Output Directory
output_path <- "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/UMedellin/"

# Ensure the directory exists (prevents error if drive isn't mapped correctly)
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# --- PLOT 1: GEOGRAPHIC VALIDATION MAP ---

# Load shapefile (using the path from your snippet)
col_shp <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")

# Merge the correlation results with the map
# Ensure MPIO_CCDGO is character to match MPIO_CDPMP
map_data <- col_shp %>%
  mutate(MPIO_CCDGO = as.character(MPIO_CCDGO)) %>%
  left_join(municipality_agreement, by = c("MPIO_CCDGO" = "MPIO_CDPMP"))

map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = local_cor), color = "white", size = 0.05) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Correlation\n(2016-2023)", na.value = "grey90") +
  labs(title = "Geographic Validation: Where do snvdem and UMedM Agree?",
       subtitle = "Red indicates high agreement on local democratic trends (2016-2023)") +
  theme_void()

# Save Map
ggsave(paste0(output_path, "map_validation_geog.png"), plot = map_plot, width = 10, height = 12, dpi = 300)


# --- PLOT 2: YEARLY FACETED SCATTER PLOTS ---

facet_plot <- ggplot(validation_df, aes(x = CdDS_z, y = sndem_z)) +
  geom_point(alpha = 0.1, color = "steelblue", size = 0.8) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  facet_wrap(~year, ncol = 4) +
  labs(
    title = "Yearly Validation: snvdem vs. UMedM",
    subtitle = "Standardized Z-Scores (Mean=0, SD=1)",
    x = "Author's Index (CdDS Z-Score)",
    y = "Your Index (sndem Z-Score)"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(face = "bold")
  )

# Save Facet Plot
ggsave(paste0(output_path, "facet_yearly_scatter.png"), plot = facet_plot, width = 12, height = 8, dpi = 300)


# --- EXPORT STATS TABLE ---

yearly_stats <- validation_df %>%
  group_by(year) %>%
  summarize(
    Pearson_r = cor(sndem, CdDS, use = "complete.obs"),
    Spearman_r = cor(sndem, CdDS, method = "spearman", use = "complete.obs"),
    n_municipalities = n()
  ) %>%
  arrange(year)

write.csv(yearly_stats, paste0(output_path, "yearly_correlation_stats.csv"), row.names = FALSE)

