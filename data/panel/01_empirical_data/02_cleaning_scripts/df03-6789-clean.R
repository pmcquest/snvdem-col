
#---- Cardinal directions ----

##---- Criteria measured ----
# 6: North. (0=No, 1=Yes) [v2*_6] 
# 7: South. (0=No, 1=Yes) [v2*_7] 
# 8: West. (0=No, 1=Yes) [v2*_8] 
# 9: East. (0=No, 1=Yes) [v2*_9] 

##---- Data Sources ----
# Data 1: Original calculations by M. Sisk. 
# This continuous data provides more variation than the categorical "regions" of the country. 
# Note: The measure is an estimate of the municipalities' cardinal "direction-ness" away from country centroid. This is a static variable (distance does not change over time). 


#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(data.table)

##---- 6-9 NSWE ----

###---- Distance from center-point (ratio) ----

df03 <- fread("G:/Shared drives/snvdem/snvdem-col/data/geospatial/6-9_NSWE/COL_NSEW.csv")
# convert this numeric variable to character and then assure each observation has the corresponding 5 digits
df03$MPIO_CDPMP <- as.character(df03$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
df03$MPIO_CDPMP <- ifelse(nchar(df03$MPIO_CDPMP) == 4, paste0("0", df03$MPIO_CDPMP), df03$MPIO_CDPMP)

# 1. Create a template of all years and all municipalities
years_df <- data.frame(year = 2000:2023)
municipalities <- df03 %>% 
  rename(north6 = 2, south7 = 3, west8 = 5, east9 = 4) %>%
  select(MPIO_CDPMP, north6, south7, east9, west8)
panel_df <- municipalities %>%
  cross_join(years_df) # Every municipality now has 24 year rows


df03a <- panel_df %>%
  mutate(
    # 1. Linear Axes (The Coordinate Grid)
    # Positive = North/West, Negative = South/East
    axis_ns = north6 - south7,
    axis_we = west8 - east9,
    # 2. Total Displacement (V-Shape / Absolute Distance from Center)
    # This measures "How far from the center is this municipality?" 
    disp_ns = north6 + south7,
    disp_we = west8 + east9
  )

# Identifying specific municipalities for diagnosis
# Apartado (05045), Puerto Leguizamo (86573), Cali (76001), Cucuta (54001)
city_names <- data.frame(
  MPIO_CDPMP = c("05045", "86573", "76001", "54001"),
  City_Label = c("Apartado", "Pto Leguizamo", "Cali", "Cucuta")
)

diagnostic_results <- df03a %>%
  filter(year == 2000) %>%
  inner_join(city_names, by = "MPIO_CDPMP") %>%
  select(City_Label, MPIO_CDPMP, north6, south7, east9, west8, axis_ns, axis_we, disp_ns, disp_we)
print(diagnostic_results)

# Miscellaneous: Coordinates
DIVIPOLA_Municipios1223 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/geospatial/DIVIPOLA_Municipios1223.xlsx", range = "A11:G1132")
DivPol <- DIVIPOLA_Municipios1223 %>%
  rename(MPIO_CDPMP = 3) %>%
  select(3|6:7)

df03b <- left_join(df03a, DivPol, by = "MPIO_CDPMP")



#---- Save cleaned dataset ----
write_rds(df03b, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df03_clean.rds")


# Diagnose the cardinal direction scoring method ----
# Must have cr_pivoted loaded (see: z3_assemble_final_data.R)
# Calculate the yearly "Net Bias" from expert ratings
bias_vectors <- cr_pivoted %>%
  mutate(
    # If emel6 (North) is 0.7 and emel7 (South) is 0.5, net_emel_ns is +0.2
    net_emel_ns = emel6 - emel7,
    net_emel_we = emel8 - emel9,
    
    net_cscw_ns = cscw6 - cscw7,
    net_cscw_we = cscw8 - cscw9
  ) %>%
  select(year, starts_with("net_"))

df03s <- df03a %>%
  mutate(across(2:5|7:10, ~ rank(.) / length(.)))

master_df <- df03s %>%
  # panel_df should already have axis_ns and axis_we based on CDF-standardized north6, etc.
  left_join(bias_vectors, by = "year") %>%
  mutate(
    # Interaction: Municipal Location * Annual Bias
    weighted_emel_ns = axis_ns * net_emel_ns,
    weighted_emel_we = axis_we * net_emel_we,
    
    weighted_cscw_ns = axis_ns * net_cscw_ns,
    weighted_cscw_we = axis_we * net_cscw_we
  )

#2000
diagnostic_results2 <- master_df %>%
  filter(year == 2000) %>%
  inner_join(city_names, by = "MPIO_CDPMP") %>%
  select(City_Label, MPIO_CDPMP, north6, south7, east9, west8, axis_ns, axis_we, disp_ns, disp_we,
         net_emel_ns, net_emel_we, net_cscw_ns, net_cscw_we, 
         weighted_emel_ns, weighted_emel_we, weighted_cscw_ns, weighted_cscw_we)
print(diagnostic_results2)
#2005
diagnostic_results3 <- master_df %>%
  filter(year == 2005) %>%
  inner_join(city_names, by = "MPIO_CDPMP") %>%
  select(City_Label, MPIO_CDPMP, north6, south7, east9, west8, axis_ns, axis_we, disp_ns, disp_we,
         net_emel_ns, net_emel_we, net_cscw_ns, net_cscw_we, 
         weighted_emel_ns, weighted_emel_we, weighted_cscw_ns, weighted_cscw_we)
print(diagnostic_results3)
