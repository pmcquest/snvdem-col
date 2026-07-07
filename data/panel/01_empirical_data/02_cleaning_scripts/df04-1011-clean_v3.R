
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
library(ggplot2)


##--- CEDE data: Displacements and Homicides (1993-2020) ----
CEDE03 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/a2-CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")

CEDE03 <- CEDE03 %>%
  rename(MPIO_CDPMP = 1, year = 2) %>%
  mutate(MPIO_CDPMP = as.character(MPIO_CDPMP),
         MPIO_CDPMP = ifelse(nchar(MPIO_CDPMP) == 4, paste0("0", MPIO_CDPMP), MPIO_CDPMP),
         year = as.numeric(year)) %>%
  select(MPIO_CDPMP, year, d_desplaza, homicidios) %>%
  rename(Desp_1011 = d_desplaza, Homi_1011 = homicidios)
# colSums(is.na(CEDE03)) # 3134 NAs for displacement

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



#---- Validate: does CEDE's homicide count agree with HRDAG's? ----
# HRDAG is the source the team trusts more for homicides (line 22: "more
# trustworthy due to detailed methodology"), built from 40+ official/civil
# society sources, vs. CEDE's less-documented compilation. Before leaning on
# CEDE to patch years HRDAG doesn't cover (below), check that the two
# actually agree where they overlap -- if they diverged, patching HRDAG with
# CEDE would be stitching together two different measures, not just filling
# gaps in one.

# 1. Join the datasets
# inner_join (not full_join) is deliberate here: this block is purely a
# validation check, so we only want muni-years where BOTH sources have a row
# to compare like-for-like. (The full_join used below, to build the actual
# patched series, is a different join for a different purpose.)
homicide_comp <- inner_join(CEDE03, HRDAG, by = c("MPIO_CDPMP", "year"))

# 2. Filter for 2013-2018 and calculate correlation
# Note: we use use = "pairwise.complete.obs" to handle the NAs in both sets
# Why 2013-2018 specifically: this is a recent-years spot check, not a
# data-driven cutoff -- verified 2026-07-05 that correlation is actually
# >0.98 for every year from 2003 onward (as far as HRDAG goes, 2018), and is
# NA before 2003 only because CEDE's own `homicidios` column is 100% missing
# for 1993-2002 (not a disagreement between sources, CEDE simply doesn't have
# the variable populated that far back). If a future reader wants the full
# picture rather than a snapshot, rerun this over 2003:2018.
hom_results <- homicide_comp %>%
  filter(year >= 2013 & year <= 2018) %>%
  group_by(year) %>%
  summarize(
    correlation = cor(Homi_1011, HHomi_1011, use = "pairwise.complete.obs"),
    avg_cede = mean(Homi_1011, na.rm = TRUE),
    avg_hrdag = mean(HHomi_1011, na.rm = TRUE),
    n_municipalities = n()
  )

print(hom_results)

# Patch missing years to create longer homicide time-series:
# Given the >0.98 correlation just confirmed, we treat CEDE as an acceptable
# stand-in for HRDAG in the years HRDAG doesn't cover, rather than dropping
# those years or leaving them NA. HRDAG covers 1985-2018; CEDE separately
# covers 1993-2021 (its `homicidios` column is actually populated through
# 2021, confirmed 2026-07-05 -- only displacement (Desp_1011) goes fully NA
# in 2021).
# 1. Perform a full join to keep all years from both sources
df_homicide_extended <- full_join(HRDAG, CEDE03, by = c("MPIO_CDPMP", "year")) %>%
  mutate(
    # Create the 'patched' column:
    # Use HRDAG value if it exists, otherwise fill with CEDE03
    # (HRDAG preferred per the team's trust ranking above; CEDE only fills
    # the years/rows HRDAG has no value for)
    HHomi_combined = coalesce(HHomi_1011, Homi_1011)
  ) %>%
  # 2. Cleanup: Remove the source columns and filter for your desired range
  select(MPIO_CDPMP, year, HHomi_combined) %>%
  # Upper bound (2021) matches CEDE's actual coverage confirmed above. Lower
  # bound (2000) is the project's analytical start year for this time-series
  # (2000-2023) -- narrower than the final panel's own 1998 start (set in
  # 01_merge_empirical.R to capture the 1998 elections and 2005/2018
  # Censuses for other criteria), so 1998-1999 will show NA for homicides
  # specifically once this joins the final panel, even though CEDE/HRDAG do
  # have data those two years.
  filter(year >= 2000 & year <= 2021) %>%
  arrange(MPIO_CDPMP, year)

# 3. Quick check of the new counts per year
# Sanity check before merging this into df04 below: confirms the patched
# series has a plausible municipality count and mean per year (e.g., no
# sudden drop from the coalesce/filter logic above going wrong).
df_homicide_extended %>%
  group_by(year) %>%
  summarise(
    n_municipalities = n(),
    avg_homicides = mean(HHomi_combined, na.rm = TRUE)
  )

#----Merging the data ----
#The team decided to treat missing values as zeros rather than NAs, though noted this is a questionable decision that should be transparently reported.
# --- 1. Merge CEDE03 and ViPAA ---
# Only CEDE03's own MPIO_CDPMP/year/Desp_1011 columns are kept here (select
# 1:3) -- Homi_1011 is dropped because homicide handling is done separately
# below via df_homicide_extended, not straight from CEDE03. Displacement
# (Desp_1011) has no HRDAG/ViPAA equivalent, so CEDE is its only source and
# its NAs are left alone (not zero-filled) in this step.
CEDE03 <- CEDE03 %>% select(1:3)
df04 <- full_join(CEDE03, ViPAA, by = c("MPIO_CDPMP", "year"))
# Identify which columns this join actually added (the ViPAA ones), so the
# zero-fill just below only ever touches those -- never CEDE03's own
# Desp_1011, whose NAs mean "unknown" and must stay NA, not become 0.
cols_in_cede03 <- names(CEDE03)
new_vipaa_cols <- setdiff(names(df04), cols_in_cede03)
df04 <- df04 %>%
  mutate(
    # A muni-year with no ViPAA row means ViPAA recorded no violent-actor
    # presence there, i.e. a real, meaningful zero -- not a missing
    # observation -- so replace_na(0) is appropriate here (per the team's
    # NA-as-zero decision noted above), unlike Desp_1011 above.
    across(all_of(new_vipaa_cols), ~replace_na(., 0))
  )
# colSums(is.na(df04)) #displacement NAs still remain

# --- 2. Merge df04 and HRDAG ---
# df04b <- full_join(df04, HRDAG, by = c("MPIO_CDPMP", "year"))
# Superseded by 2a: raw HRDAG alone stops at 2018 with no CEDE fallback, so
# joining it directly would leave 2019-2021 with no real homicide signal at
# all (they'd just get zero-filled below, indistinguishable from "no
# homicides" when it's actually "no HRDAG coverage"). df_homicide_extended
# (built above) already patches those years with CEDE's real values first,
# so joining it instead avoids manufacturing false zeros for 2019-2021.
# --- 2a. Merge df04 and df_homicide_extended
df04b <- full_join(df04, df_homicide_extended, by = c("MPIO_CDPMP", "year"))
# Identify new columns
cols_in_df04 <- names(df04)
new_homi_cols <- setdiff(names(df04b), cols_in_df04)
# Replace NA values with 0 ONLY in the columns added from Homicide
df04b <- df04b %>%
  mutate(
    # Same logic as the ViPAA fill above: a muni-year absent from
    # df_homicide_extended means neither HRDAG nor CEDE recorded a homicide
    # there, treated as a real zero rather than a gap.
    across(all_of(new_homi_cols), ~replace_na(., 0))
  )
# Expect only Desp_1011 (CEDE's displacement column, which has no other
# source to patch it) to still show real NAs here -- if any other column
# does, the setdiff-based targeting above missed something.
colSums(is.na(df04b)) #new NAs...

#---- Save cleaned dataset ----
write_rds(df04b, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df04_clean_v4.rds")



##----Visualization over time----
# Face-validity check, not analysis: plot each criterion-10/11 variable's
# yearly mean (regex grabs columns ending in _10 or _11, i.e. the actual
# criterion variables, excluding MPIO_CDPMP/year) to eyeball whether trends
# look historically plausible and whether the zero-filling above introduced
# any artificial jumps or discontinuities.
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




