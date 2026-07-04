# Validation: Environmental outcomes
# The research question is: does poor quality of subnational democracy correlate with rates of forest cover loss? 
# To answer this, we use snvdem data available for years 2000-2023. These are estimates of quality of subnational democracy at the municipality level. 
# 

## SNVDEM Panel (2000-2023)----
master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
# Load Geospatial data
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))