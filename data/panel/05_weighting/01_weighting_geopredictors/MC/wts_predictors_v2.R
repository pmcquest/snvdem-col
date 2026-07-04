# Combining weights and geocoded predictor variables
# Author: MC
# Revised by: PM (Jan 11, 2026)

library(tidyverse)
library(haven)


weights <- read_dta("G:/Shared drives/snvdem/snvdem-col/data/panel/06_vdem_data/coder-level/MC/ELCLweights_wide.dta")
weights_col <- filter(weights, country_text_id == "COL" & year>1999)
# Average the weights for civil unrest and illicit activities
weights_col <- weights_col %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity)/2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity)/2)

# Which paired variables have high values that disfavor democracy?


predictors <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/v1/CDF_averages_v1.rds")
names(predictors)
#[1] "MPIO_CDPMP" "year"       "avg2t3"     "avg12"      "avg13"      "avg0t1"    
#[7] "avg4t5"     "avg6"       "avg7"       "avg8"       "avg9"       "avg10t11"  
#[13] "avg14"      "avg15t16"  

predictors %>%
  dplyr::summarize(across(avg2t3:avg15t16, \(x) median(x, na.rm = TRUE)))

#avg2t3     avg12     avg13    avg0t1    avg4t5      avg6      avg7      avg8      avg9
#1 0.5053241 0.5000185 0.4933519 0.4999815 0.4999815 0.4999815 0.6110926 0.4999815 0.5857593
#avg10t11     avg14 avg15t16
#1 0.5001667 0.4999815 0.499963

# The median is about 0.5 for all predictors except avg7 (South) and avg9 (East).
# Create hi and lo versions of each paired variable.
predict_hilo <- predictors %>%
  mutate(avg0t1hi = ifelse(avg0t1>.5, avg0t1, 0),
         avg0t1lo = ifelse(avg0t1<=.5, avg0t1, 0),
         avg2t3hi = ifelse(avg2t3>.5, avg2t3, 0),
         avg2t3lo = ifelse(avg2t3<=.5, avg2t3, 0),
         avg4t5hi = ifelse(avg4t5>.5, avg4t5, 0),
         avg4t5lo = ifelse(avg4t5<=.5, avg4t5, 0),
         avg10t11hi = ifelse(avg10t11>.5, avg10t11, 0),
         avg10t11lo = ifelse(avg10t11<=.5, avg10t11, 0),
         avg15t16hi = ifelse(avg15t16>.5, avg15t16, 0),
         avg15t16lo = ifelse(avg15t16<=.5, avg15t16, 0))

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg2t3hi + avg2t3lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg4t5hi +avg4t5lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg10t11hi +avg10t11lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1, y = avg15t16)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg10t11, y = avg2t3)) +
  geom_point(alpha = .1) + geom_smooth()
# Looks very random.

# Instead of adding them, use the 'Hi/Lo' split to color the plot
# This actually shows if 'High' areas behave differently than 'Low' areas
ggplot(predictors, aes(x = avg10t11, y = avg2t3)) +
  geom_point(aes(color = avg10t11 > 0.5), alpha = 0.1) + 
  geom_smooth(aes(group = avg10t11 > 0.5), method = "lm") +
  labs(title = "Checking for Asymmetric Effects",
       color = "Is Stable (High Rank)?")

# rewrite
predict_groups <- predictors %>%
  mutate(
    # Create categorical status for key variables based on the 0.5 median
    stability_status = case_when(
      avg10t11 > 0.5 ~ "High Stability",
      avg10t11 <= 0.5 ~ "Low Stability"
    ),
    dev_status = case_when(
      avg2t3 > 0.5 ~ "High Development",
      avg2t3 <= 0.5 ~ "Low Development"
    ),
    rural_status = case_when(
      avg0t1 > 0.5 ~ "More Urban", # Assuming 0t1 is Rural-Urban scale
      avg0t1 <= 0.5 ~ "More Rural"
    )
  )

ggplot(predict_groups, aes(x = avg10t11, y = avg2t3)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(option = "magma") +
  # This creates two side-by-side plots based on Rural/Urban status
  facet_wrap(~rural_status) +
  geom_smooth(method = "lm", color = "cyan") +
  theme_minimal() +
  labs(
    title = "Development vs. Stability: Rural vs. Urban Comparison",
    subtitle = "Faceted analysis reveals if geography changes the stability-development link",
    x = "Stability Rank (1 - Violence)",
    y = "Development Rank"
  )

# Check this version:
colvars_cdf <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/imputed_cdf_panel.rds")
colnames(colvars_cdf)
summary(colvars_cdf)

ggplot(colvars_cdf, aes(x = IndRur_0t1, y = DisBog_4t5)) +
  geom_point(alpha = .1) + geom_smooth()
# It looks pretty much the same.


# 1. Filter for the "Extreme Rural" subset (e.g., top 10% of rurality)
extreme_rural_df <- colvars_cdf %>%
  filter(IndRur_0t1 >= 0.90)

# 2. Run Spearman correlation on the full dataset for comparison
full_cor <- cor(colvars_cdf$IndRur_0t1, colvars_cdf$DisBog_4t5, method = "spearman")

# 3. Run Spearman correlation specifically for the extreme rural subset
extreme_cor <- cor(extreme_rural_df$IndRur_0t1, extreme_rural_df$DisBog_4t5, method = "spearman")

# 4. Print results
print(paste("Full Dataset Spearman Rho:", round(full_cor, 3))) # -0.036
print(paste("Extreme Rural Subset Spearman Rho:", round(extreme_cor, 3))) # 0.489

# Identifying the extreme municipalities... 
# We will define 'extreme' as being in the top/bottom 10% of both variables
# Adjust the thresholds (0.1 and 0.9) if you want more or fewer results

extreme_quadrants <- colvars_cdf %>%
  mutate(quadrant = case_when(
    # Top-Right: Far from Bogota AND Very Rural
    IndRur_0t1 > 0.9 & DisBog_4t5 > 0.9 ~ "Extreme Remote Rural",
    
    # Bottom-Right: Close to Bogota AND Very Rural
    IndRur_0t1 > 0.9 & DisBog_4t5 < 0.1 ~ "Extreme Close Rural",
    
    # Top-Left: Far from Bogota AND Very Urban
    IndRur_0t1 < 0.1 & DisBog_4t5 > 0.9 ~ "Extreme Remote Urban",
    
    # Bottom-Left: Close to Bogota AND Very Urban
    IndRur_0t1 < 0.1 & DisBog_4t5 < 0.1 ~ "Extreme Close Urban",
    
    TRUE ~ "Interior"
  )) %>%
  filter(quadrant != "Interior") %>%
  # Select identifying columns to see who these are
  select(MPIO_CDPMP, year, IndRur_0t1, DisBog_4t5, quadrant) %>%
  arrange(quadrant, desc(IndRur_0t1))

# View the results
print(extreme_quadrants)

# Summary count per quadrant
extreme_quadrants %>% count(quadrant)


#---- Merging geopredictors and weights ----
# Proceed cautiously

Indices <- predict_hilo %>%
  left_join(weights_col, by = "year")

# 2. Constructing the Indices with distinct denominators
Indices_both <- Indices %>%
  mutate(
    # --- ELECTORAL INDEX (snelect) ---
    el_num = (avg0t1hi * el_Urban) + (avg0t1lo * el_Rural) + 
      (avg10t11 * wt_el_1011) + (avg12 * el_Sparse_population) + 
      (avg13 * el_Remote) + (avg14 * (1 - el_Indigenous)) + 
      (avg15t16hi * (1 - el_Ruling_party_strong)) + (avg15t16lo * (1 - el_Ruling_party_weak)) + 
      (avg2t3hi * el_More_development) + (avg2t3lo * el_Less_development) + 
      (avg4t5hi * (1 - el_Inside_capital)) + (avg4t5lo * (1 - el_Outside_capital)) + 
      (avg6 * el_North) + (avg7 * el_South) + (avg8 * el_West) + (avg9 * el_East),
    
    el_den = el_Urban + el_Rural + wt_el_1011 + el_Sparse_population + 
      el_Remote + (1 - el_Indigenous) + (1 - el_Ruling_party_strong) + (1 - el_Ruling_party_weak) + 
      el_More_development + el_Less_development + (1 - el_Inside_capital) + (1 - el_Outside_capital) + 
      el_North + el_South + el_West + el_East,
    
    snelect = el_num / el_den,
    
    # --- CIVIL LIBERTIES INDEX (sncivlib) ---
    cl_num = (avg0t1hi * cl_Urban) + (avg0t1lo * cl_Rural) + 
      (avg10t11 * wt_cl_1011) + (avg12 * cl_Sparse_population) + 
      (avg13 * cl_Remote) + (avg14 * (1 - cl_Indigenous)) + 
      (avg15t16hi * (1 - cl_Ruling_party_strong)) + (avg15t16lo * (1 - cl_Ruling_party_weak)) + 
      (avg2t3hi * cl_More_development) + (avg2t3lo * cl_Less_development) + 
      (avg4t5hi * (1 - cl_Inside_capital)) + (avg4t5lo * (1 - cl_Outside_capital)) + 
      (avg6 * cl_North) + (avg7 * cl_South) + (avg8 * cl_West) + (avg9 * cl_East),
    
    cl_den = cl_Urban + cl_Rural + wt_cl_1011 + cl_Sparse_population + 
      cl_Remote + (1 - cl_Indigenous) + (1 - cl_Ruling_party_strong) + (1 - cl_Ruling_party_weak) + 
      cl_More_development + cl_Less_development + (1 - cl_Inside_capital) + (1 - cl_Outside_capital) + 
      cl_North + cl_South + cl_West + cl_East,
    
    sncivlib = cl_num / cl_den
  ) %>%
  # Clean up temporary calculation columns and redundant variables
  select(-el_num, -el_den, -cl_num, -cl_den)

# 3. Save the result
write_rds(Indices_both, "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/Indices_both.rds")

# Check if any values exceeded the 0-1 bounds in the original version
summary(Indices_both$sncivlib)



##---- Missing data! ----
# Identify which years are missing weight data
missing_years <- Indices_both %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    wt_missing = any(is.na(cl_Less_development)),
    count = n()
  ) %>%
  dplyr::filter(wt_missing == TRUE)

print("Years with missing weights:")
print(missing_years) # 2001, 2002, 2004 -- specifically Civil Liberties weights

# Identify if specific predictors have NAs -- they don't
# This checks if any of the 'avg' columns are broken
predictor_na <- predict_hilo %>%
  dplyr::summarize(across(starts_with("avg"), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  dplyr::filter(na_count > 0)

print("Predictor variables with NAs:")
print(predictor_na)


## Visual exploration ----
ggplot(Indices_both, aes(x = snelect, y = sncivlib)) +
  geom_point(alpha = .1) +
  theme_light()

# Getting coordinates
df03 <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df03_clean.rds")
coords <- select(df03, MPIO_CDPMP, year, LATITUD, LONGITUD)

Indices_both_coords <- merge(Indices_both, coords, by = c("MPIO_CDPMP", "year"), all.x = TRUE)

library(viridis)
library(rnaturalearth)
library(sf)

# 1. Get Colombia's Department boundaries
# 'states' in rnaturalearth provides the Admin-1 (Department) level for Colombia
colombia_admin1 <- ne_states(country = "colombia", returnclass = "sf")

# 2. Preparation: Scale the indices for visualization
# We will use the raw indices for size to make high performers stand out
plot_data <- Indices_both_coords %>% 
  filter(year == 2020) %>%
  filter(!is.na(LATITUD) & !is.na(LONGITUD)) # Remove rows with missing coordinates

# 3. Create the Election Index Map
plot_el <- ggplot() +
  # Layer 1: Department Boundaries
  geom_sf(data = colombia_admin1, fill = "gray98", color = "gray80", size = 0.2) +
  # Layer 2: Bubble Layer (Proportional Symbols)
  # We map 'size' to 'snelect' and 'color' to 'snelect' for double emphasis
  geom_point(data = plot_data, 
             aes(x = LONGITUD, y = LATITUD, color = snelect, size = snelect),
             alpha = 0.6) +
  # Aesthetic Adjustments
  scale_color_viridis_c(option = "magma", direction = -1, name = "Election Score") +
  scale_size_continuous(range = c(0.5, 4), name = "Score Magnitude") + # Adjust range for bubble sizes
  coord_sf() + # Crucial for keeping map proportions correct
  theme_minimal() +
  labs(title = "Subnational Election Index, 2020",
       subtitle = "Larger bubbles indicate higher electoral performance",
       x = "Longitude", y = "Latitude")

print(plot_el)

# 4. Create the Civil Liberties Map
plot_cl <- ggplot() +
  geom_sf(data = colombia_admin1, fill = "gray98", color = "gray80", size = 0.2) +
  geom_point(data = plot_data, 
             aes(x = LONGITUD, y = LATITUD, color = sncivlib, size = sncivlib),
             alpha = 0.6) +
  scale_color_viridis_c(option = "viridis", name = "CivLib Score") +
  scale_size_continuous(range = c(0.5, 4), name = "Score Magnitude") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Subnational Civil Liberties Index, 2020",
       subtitle = "Bubble size represents Civil Liberties performance",
       x = "Longitude", y = "Latitude")

print(plot_cl)

# Save the updated versions
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/snelect_bubble_2020.png", plot_el, height = 8, width = 7, bg = "white")
ggsave("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/imgs/sncivlib_bubble_2020.png", plot_cl, height = 8, width = 7, bg = "white")



# Final snvdem index (mean) ----
Indices_both <- Indices_both %>%
  mutate(sndem = 0.5*(snelect + sncivlib))

write_rds(Indices_both, "G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/MC/SNDEM_tentative.rds")


# Visualizations


