#----- Static variables ----
# we can "fill in" NAs for "static" variables using Last Observation Carried Forward (LOCF)

# Load libraries
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# Load cleaned dataset
df_all <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")
n_distinct(df_all$MPIO_CDPMP)

# Define the static columns you want to fill
static_cols <- c("DPTO_CCDGO", "depto", "provincia", "municipio", 
                 "DisBog_4t5", "north6", "south7", "west8", 
                 "east9", "axis_ns", "axis_we", "disp_ns", "disp_we", "LATITUD", "LONGITUD")

imp_static <- df_all %>%
  filter(!is.na(MPIO_CDPMP), MPIO_CDPMP != "") %>% 
  arrange(MPIO_CDPMP, year) %>% 
  group_by(MPIO_CDPMP) %>%
  fill(all_of(static_cols), .direction = "downup") %>%
  ungroup() %>%
  filter(year <= 2024) %>%
  select(MPIO_CDPMP, year, all_of(static_cols))

# check for NAs
missing_codes <- imp_static %>% filter(is.na(depto)) %>% pull(MPIO_CDPMP) %>%   unique()
print(missing_codes)




#----Save and document----
write_rds(imp_static, "G:/Shared drives/snvdem/snvdem-col/data/panel/04_imputed_intermediate/impStatic.rds")

