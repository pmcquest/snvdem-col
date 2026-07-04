# creating a municipality-year df

library(readxl)
library(tidyverse)
library(sf)

# 1. Load and Standardize Geospatial Metadata
col_shp <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")

# Create a clean lookup table for Names and Dept Codes
# We use st_drop_geometry to keep this a simple dataframe for joining
muni_lookup <- col_shp %>%
  st_drop_geometry() %>%
  mutate(
    MPIO_CDPMP = str_pad(as.character(MPIO_CDPMP), 5, pad = "0"),
    DPTO_CCDGO = str_sub(MPIO_CDPMP, 1, 2) # Extracts the Dept code from the Mpio code
  ) %>%
  select(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO) %>%
  distinct()

# 2. Load and Clean the Population/Admin Dataset
# We use .name_repair to handle any unexpected symbols in excel headers
sp_12_raw <- read_excel(
  "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/ColOpenData/population_projections.xlsx",
  .name_repair = "universal"
)

sp_12_clean <- sp_12_raw %>%
  filter(area == "total") %>%
  mutate(
    # Ensure DANE codes are strings with leading zeros
    MPIO_CDPMP = str_pad(as.character(codigo_municipio), 5, pad = "0"),
    DPTO_CCDGO = str_pad(as.character(codigo_departamento), 2, pad = "0")
  ) %>%
  # Remove the old numeric columns to avoid confusion
  select(-codigo_municipio, -codigo_departamento)

# 3. Final Join
df_muni_year_final <- sp_12_clean %>%
  # Include BOTH IDs in the join to prevent .x and .y suffixes
  left_join(muni_lookup, by = c("MPIO_CDPMP", "DPTO_CCDGO")) %>%
  
  # Clean up column names
  rename(
    year = ano,
    area_tipo = area,
    PobTot_12 = total
  ) %>%
  
  # Now DPTO_CCDGO exists as a single column again
  select(MPIO_CDPMP, MPIO_CNMBR, DPTO_CCDGO, year, everything())


# --- Integrity Check ---
missing_names <- sum(is.na(df_muni_year_final$MPIO_CNMBR))
if(missing_names > 0) {
  warning(paste(missing_names, "rows failed to match a municipality name! Check DANE codes."))
}

# Expected municipio count, verified against DANE's official DIVIPOLA list
# (geoportal.dane.gov.co/descargas/divipola/DIVIPOLA_Municipios.xlsx, checked 2026-07-03).
# Update this constant (and re-verify against DANE) if Colombia's municipio count
# genuinely changes; otherwise a mismatch here means something upstream drifted silently,
# which is exactly what happened to MunYrs.rds between Jan and Mar 2026 -- three
# non-current codes (27086, 99572, 99760) rode along in the panel for months undetected.
EXPECTED_MPIO_COUNT <- 1122
actual_mpio_count <- n_distinct(df_muni_year_final$MPIO_CDPMP)
if (actual_mpio_count != EXPECTED_MPIO_COUNT) {
  warning(sprintf(
    "Municipio count is %d, expected %d. Verify against DANE's current DIVIPOLA list before proceeding -- do not assume this run is correct just because it completed.",
    actual_mpio_count, EXPECTED_MPIO_COUNT
  ))
}

# Diff against the previously saved MunYrs.rds (if any) so any drift in the
# municipio universe is visible immediately, not discovered months later downstream.
old_path <- "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/MunYrs.rds"
if (file.exists(old_path)) {
  old_codes <- unique(readRDS(old_path)$MPIO_CDPMP)
  new_codes <- unique(df_muni_year_final$MPIO_CDPMP)
  dropped <- setdiff(old_codes, new_codes)
  added   <- setdiff(new_codes, old_codes)
  if (length(dropped) > 0) cat("Dropped since last saved MunYrs.rds:", paste(dropped, collapse = ", "), "\n")
  if (length(added)   > 0) cat("Added since last saved MunYrs.rds:  ", paste(added, collapse = ", "), "\n")
}

# 4. Save
write_rds(df_muni_year_final, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/MunYrs.rds")

