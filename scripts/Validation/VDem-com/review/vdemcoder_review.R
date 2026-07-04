# Validation: V-Dem coder comments

# How well does the textual data correlate to our quantitative measure?


library(dplyr)
library(stringi)
library(ggplot2)
library(tidyr)
library(sf)
library(readxl)
library(stringr)
library(stringdist)
library(lubridate)

# Data cleaning
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
##Geospatial data----
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")


# V-Dem coder comments
com <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/outliers3.rds")
com2 <- read_excel("data/panel/09_analysis_scripts/Validation/VDem-com/review/v2elsnless_v2elsnmore_colombia_pmq.xlsx")
df_col_clean <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")



# --- STEP 1: PREPARE DICTIONARIES ---
# Get unique depto names from your reference data
dept_dictionary <- unique(df_col_clean$depto)

# Create a 'clean' version for matching (no accents, lowercase)
# This turns "Chocó" into "choco"
dept_clean_norm <- dept_dictionary %>%
  stri_trans_general("Latin-ASCII") %>%
  tolower() %>%
  str_trim()

# Regex pattern: looks for any of our depto names as whole words
dept_pattern <- paste0("\\b(", paste(dept_clean_norm, collapse = "|"), ")\\b")

# --- STEP 2: CLEAN & SCRAPE CODER COMMENTS ---
coder_matched <- com2 %>%
  mutate(
    # A. Fix the encoding mess (ChocÃ³ -> Chocó)
    # We use iconv with transliteration to force it into readable ASCII
    text_fixed = iconv(text_answer, from = "UTF-8", to = "ASCII//TRANSLIT"),
    text_norm = tolower(text_fixed),
    
    # B. Handle dates and categories
    Year = year(historical_date),
    sentiment = case_when(
      question_id == 411 ~ "Less",
      question_id == 414 ~ "More",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filter for your specific thesis timeframe
  filter(Year >= 2000 & Year <= 2023) %>%
  
  # C. Extract ALL mentioned departments in the text
  # Using str_extract_all because experts often list 5-6 departments at once
  mutate(dept_found = str_extract_all(text_norm, dept_pattern)) %>%
  
  # D. Expand the list: If 1 row had 3 deptos, it becomes 3 rows
  unnest(dept_found) %>%
  
  # E. Map back to official spelling
  mutate(dept_final = dept_dictionary[match(dept_found, dept_clean_norm)])

# --- STEP 3: CREATE THE DEPARTMENT-YEAR FLAGS ---
dept_year_flags <- coder_matched %>%
  group_by(dept_final, Year, sentiment) %>%
  summarise(coder_count = n_distinct(coder_id), .groups = "drop") %>%
  pivot_wider(
    names_from = sentiment, 
    values_from = coder_count, 
    values_fill = 0,
    names_prefix = "n_coders_"
  ) %>%
  mutate(
    # Binary flags for backward compatibility with your previous steps
    Less = ifelse(n_coders_Less > 0, 1, 0),
    More = ifelse(n_coders_More > 0, 1, 0)
  )

# --- STEP 4: COLLATE INTO FINAL MUNICIPAL PANEL ---
# Create a bridge between MPIO codes and Dept names from your existing clean data
muni_dept_bridge <- df_col_clean %>%
  select(MPIO_CDPMP, Dept_Name = depto) %>%
  distinct()

final_panel_expert <- MunYrs %>%
  left_join(muni_dept_bridge, by = "MPIO_CDPMP") %>%
  mutate(year_num = as.numeric(year)) %>%
  left_join(dept_year_flags, by = c("Dept_Name" = "dept_final", "year_num" = "Year")) %>%
  mutate(
    across(c(n_coders_Less, n_coders_More, Less, More), ~coalesce(., 0)),
    Contradiction = ifelse(Less == 1 & More == 1, 1, 0),
    # Total unique coder mentions for this municipality-year
    total_coders_mentioned = n_coders_Less + n_coders_More
  ) %>%
  filter(year_num >= 2000 & year_num <= 2023)

# --- STEP 5: FILTER FOR EXPERT-ONLY VALIDATION SET ---
expert_only_panel <- final_panel_expert %>%
  filter(Less == 1 | More == 1)

# --- DIAGNOSTICS & SUMMARY ---
cat("\n--- Unique Coders 2000-2023 ---\n")
coders_in_window <- com2 %>%
  mutate(Year = year(historical_date)) %>%
  filter(Year >= 2000 & Year <= 2023) %>%
  summarise(unique_coders = n_distinct(coder_id), total_comments = n())
print(coders_in_window)

cat("\n--- Coder Count Summary per Category ---\n")
summary(expert_only_panel[c("n_coders_Less", "n_coders_More")])


# Save ----
saveRDS(expert_only_panel, "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Validation/VDem-com/review/expert_only_panel.rds")
