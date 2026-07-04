
#---- Violence in Colombia ----

##---- Criteria measured ----
# 10: Areas of civil unrest (including areas where insurgent groups are active). (0=No, 1=Yes) [v2*_10] 
# 11: Areas where illicit activity is widespread. (0=No, 1=Yes) [v2*_11] 

# Nov 26, 2025: The team discussed combining illicit activities and civil unrest into one category, with plans to conduct a robustness test in a future paper.


##---- Data Sources ----
# Data 1: CEDE (U de Los Andes) Panel Municipal Violencia (2022) (https://datoscede.uniandes.edu.co/catalogo-de-datos/). 
# Data 2: Violent Presence of Armed Actors in Colombia (ViPAA) data was created by Javier Osorio using data from National Research Center for Popular Education (CINEP). See information on the dataset here: https://www.colombiaarmedactors.org/.
# Data 3: Human rights Data Analysis Group (HRDAG) collaborated with Colombia's Truth Commission (CEV) to calculate homicides, disappearances (forced or not), sequesters, and recruitment (1985-2018). These data include documented, imputed, and estimated numbers of victims of Colombia's armed conflict. More information on HRDAG's 'verdata' package here: https://github.com/HRDAG/verdata/blob/main/inst/docs/README-en.md.

# Note: Data on civil unrest and illicit activity are tricky. Official data include reports of displacement or confinements to MinDefense, but there may be reporting issues. (CEDE also offers historic dummy variables of presence of La Violencia and Land Conflicts, which are very thin.) HRDAG-CEV in Colombia produced estimates based on reports from hundreds of NGOs. ViPAA produced estimates using a large-language model (LLM) software to extract data from CINEP's database "Noche y Niebla". 

# These data have the virtue of representing different dimensions of violence according to a wide range of sources: ViPAA captures violent actor presence as reported in local news and compiled by experts at CINEP; HRDAG captures reported homicides from over 40 different official and civil society sources; CEDE captures individuals’ reports of displacement to the UARIV. While all data grapple with the issue of missingness, we believe these indicators provide adequate proxies for civil unrest and illicit activities.

# Nov. 26, 2025: Combining Illicit Activities and Unrest
# The team discussed combining illicit activities and civil unrest into one category for the current paper, with plans to conduct a robustness test in a future paper. M. Coppedge presented coefficients for different scenarios, and the team agreed to use an average weight of 0.9 for more free and fair elections. They also decided to lump together various categories of actors (government, insurgents, paramilitaries, FARC dissidents, criminals) for the ViPAA data, and to use displacement CEDE data and observed homicide HRDAG data. 
# ViPAA and HRDAG do not contain NAs, while CEDE uses it but lacks a clear definition. The team noted that ViPAA and HRDAG are more trustworthy due to their detailed methodology, though CEDE has face validity.


#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)


##--- CEDE data: Displacements and Homicides (1993-2020) ----
CEDE03 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/a2-CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")

CEDE03 <- CEDE03 %>%
  rename(MPIO_CDPMP = 1, year = 2) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP),
         MPIO_CDPMP = ifelse(nchar(MPIO_CDPMP) == 4, paste0("0", MPIO_CDPMP), MPIO_CDPMP),
         year = as.numeric(year)) %>%
  select(MPIO_CDPMP, year, d_desplaza, homicidios) %>%
  rename(Desp_1011 = d_desplaza, Homi_1011 = homicidios)
#colSums(is.na(CEDE03)) # 3134 NAs for displacement

## --- ViPAA variables (1977-2019) ----
ViPAA <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_Osorio/ViPAA-Col/Database/VIPAA_days_1011.rds")
ViPAA <- ViPAA %>%
  rename(VDays_1011 = 3, VDays_10 = 4, VDays_11 = 5) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))

## --- HRDAG variables (1985-2018) ----
HRDAG <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/td_HRDAG_ym.csv")

HRDAG <- HRDAG %>%
  select(1:3|6|9|12) %>%
  rename(HHomi_1011 = 3, HDesa_11 = 4, HSecu_11 = 5, HRecl_11 = 6) %>%
  select(MPIO_CDPMP, year, HHomi_1011) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP))



# 1. Join the datasets
homicide_comp <- inner_join(CEDE03, HRDAG, by = c("MPIO_CDPMP", "year"))

# 2. Filter for 2013-2018 and calculate correlation
# Note: we use use = "pairwise.complete.obs" to handle the NAs in both sets
hom_results <- homicide_comp %>%
  filter(year >= 2013 & year <= 2018) %>%
  group_by(year) %>%
  summarize(
    correlation = cor(homicidios, HHomi_1011, use = "pairwise.complete.obs"),
    avg_cede = mean(homicidios, na.rm = TRUE),
    avg_hrdag = mean(HHomi_1011, na.rm = TRUE),
    n_municipalities = n()
  )

print(hom_results)

# Patch missing years to create longer homicide time-series:
# 1. Perform a full join to keep all years from both sources
df_homicide_extended <- full_join(HRDAG, CEDE03, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    # Create the 'patched' column: 
    # Use HRDAG value if it exists, otherwise fill with CEDE03
    HHomi_combined = coalesce(HHomi_1011, homicidios)
  ) %>%
  # 2. Cleanup: Remove the source columns and filter for your desired range
  select(MPIO_CDPMP, year, HHomi_combined) %>%
  filter(year >= 2000 & year <= 2021) %>%
  arrange(MPIO_CDPMP, year)

# 3. Quick check of the new counts per year
df_homicide_extended %>%
  group_by(year) %>%
  summarise(
    n_municipalities = n(),
    avg_homicides = mean(HHomi_combined, na.rm = TRUE)
  )

#----Merging the data ----
#The team decided to treat missing values as zeros rather than NAs, though noted this is a questionable decision that should be transparently reported.
# --- 1. Merge CEDE03 and ViPAA ---
CEDE03 <- CEDE03 %>% select(1:3)
df04 <- full_join(CEDE03, ViPAA, by = c("MPIO_CDPMP", "year"))
cols_in_cede03 <- names(CEDE03)
new_vipaa_cols <- setdiff(names(df04), cols_in_cede03)
df04 <- df04 %>%
  mutate(
    across(all_of(new_vipaa_cols), ~replace_na(., 0))
  )
# colSums(is.na(df04)) #displacement NAs still remain

# --- 2. Merge df04 and HRDAG ---
# df04b <- full_join(df04, HRDAG, by = c("MPIO_CDPMP", "year"))
# --- 2a. Merge df04 and df_homicide_extended
df04b <- full_join(df04, df_homicide_extended, by = c("MPIO_CDPMP", "year"))
# Identify new columns
cols_in_df04 <- names(df04)
new_homi_cols <- setdiff(names(df04b), cols_in_df04)
# Replace NA values with 0 ONLY in the columns added from Homicide
df04b <- df04b %>%
  mutate(
    # Target only the new columns where NA implies absence of Homicide data
    across(all_of(new_homi_cols), ~replace_na(., 0))
  )
colSums(is.na(df04b)) #new NAs...

#---- Save cleaned dataset ----
write_rds(df04b, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df04_clean_v4.rds")



##----Visualization over time----
selected_vars <- df04 %>%
  select(year, matches("(_10|_11)$"))
mean_per_year <- selected_vars %>%
  group_by(year) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
mean_long <- mean_per_year %>%
  pivot_longer(-year, names_to = "variable", values_to = "mean_value")

ggplot(mean_long, aes(x = year, y = mean_value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Means of Civil Unrest and Illicit Activity variables over time",
       x = "Year",
       y = "Mean Value",
       color = "Variable") +
  theme(legend.position = "bottom")




