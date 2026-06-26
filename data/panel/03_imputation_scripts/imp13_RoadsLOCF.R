#----Last observation carried forward (and back) ----
# For remoteness (#13), we distance to market (available from CEDE-Los Andes) and road density (provided by M. Sisk using OpenStreetMaps). Both will be carried forward and backward to complete data for 2000-2020.

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyr)
library(naniar)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")


# Check missing values
missing_summary <- df_all %>%
  summarise(across(c(DisMer_13, nAllRds_13, lAllRds_13, lMjRds_13, road_density_km), ~ sum(is.na(.))))

# 1. Prepare and apply LOCF
df_all <- df_all %>%
  # Arrange by municipality and year is CRITICAL for LOCF
  arrange(MPIO_CDPMP, year) %>%  
  group_by(MPIO_CDPMP) %>%
  # fill() is the standard tool for LOCF. 
  # "downup" carries the first available value both forward AND backward
  fill(DisMer_13, nAllRds_13, lAllRds_13, lMjRds_13, road_density_km, .direction = "downup") %>%
  ungroup() %>%
  
  # 2. Calculate proportions after filling NAs
  mutate(
    nAR_13pkm    = nAllRds_13 / AREAkm,
    lAR_13pkm    = lAllRds_13 / AREAkm,
    lMjRds_13pkm = lMjRds_13 / AREAkm
  )

# 3. Filter for final timeframe and clean levels
imp13 <- df_all %>%
  select(MPIO_CDPMP, year, DisMer_13, nAllRds_13, lAllRds_13, 
         lMjRds_13, nAR_13pkm, lAR_13pkm, lMjRds_13pkm, road_density_km) %>%
  filter(year >= 2000)
# Check remaining NAs
colSums(is.na(imp13))

# 4. Save
write_rds(imp13, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/imp13.rds")


# Correlations ----

library(corrplot)

# 1. Select only the numeric variables of interest
# We exclude MPIO_CDPMP and year as they are identifiers
cor_data <- imp13 %>%
  select(DisMer_13, nAllRds_13, lAllRds_13, lMjRds_13, 
         nAR_13pkm, lAR_13pkm, lMjRds_13pkm, road_density_km) %>%
  drop_na() # Removes the 48 NA rows to allow calculation

# 2. Calculate the Correlation Matrix
M <- cor(cor_data)

# 3. Create the Plot
corrplot(M, 
         method = "color",       # Use colored squares
         type = "upper",         # Only show the top triangle (cleaner)
         order = "hclust",       # Cluster similar variables together
         addCoef.col = "black",  # Add the correlation coefficient numbers
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Rotate text labels for readability
         diag = FALSE,           # Don't show the correlation of a variable with itself
         sig.level = 0.01, insig = "blank") # Only show significant correlations
