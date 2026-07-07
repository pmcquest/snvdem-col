#---- Step 1 (final stage): Merge cleaned criterion datasets into one empirical panel ----
# Folder: 01_empirical_data/04_merge_empirical
#
# Synthesizes z1_merge_all_raw.R and z1_merge_all_raw2.R (both kept alongside, for reference,
# not run as part of the canonical pipeline) into one script. See inline notes for what was
# kept, fixed, or dropped from each and why -- verified against the actual data 2026-07-03.

library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)

base <- "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/"

#---- 1. Load cleaned per-criterion datasets ----
df01 <- read_rds(paste0(base, "03_clean_outputs/df01_clean.rds"))
df02 <- read_rds(paste0(base, "03_clean_outputs/df02_clean.rds"))
df03 <- read_rds(paste0(base, "03_clean_outputs/df03_clean.rds"))
df04 <- read_rds(paste0(base, "03_clean_outputs/df04_clean_v4.rds"))
df05 <- read_rds(paste0(base, "03_clean_outputs/df05_clean.rds"))
df06 <- read_rds(paste0(base, "03_clean_outputs/df06_clean.rds"))
df07 <- read_rds(paste0(base, "03_clean_outputs/df07_clean.rds"))

# Rename ambiguous positional column (kept from z1_merge_all_raw.R)
df04 <- df04 %>% rename(HHomix_1011 = 7)

#---- 2. Drop legacy/duplicate municipality codes ----
# Verified 2026-07-03: three municipalities appear under two different DIVIPOLA codes in the
# raw cleaned data -- a current/correct code (used consistently across most datasets) and a
# legacy code that appears only in df02 (Puerto Libertador) or df04 (the Vichada pair). Checked
# every overlapping year side by side: the legacy-code rows are uniformly zero/NA placeholders
# wherever the current-code row has real data -- e.g. Santa Rosalia's "99572" rows are 0 for
# every variable in every year that "99624" has actual (often large) values. Dropping the
# legacy rows loses no information; keeping them would create duplicate MPIO_CDPMP+year keys.
#   23685 -> 23580  Puerto Libertador, Cordoba (df02 only; 23580 confirmed as current DIVIPOLA
#                    code against geoportal.dane.gov.co/descargas/divipola, 2026-07-03)
#   99572 -> 99624  Santa Rosalia, Vichada (df04 only)
#   99760 -> 99773  Cumaribo, Vichada (df04 only)
# NOTE: z1_merge_all_raw.R went the opposite direction -- it recoded the CURRENT correct codes
# (99624, 99773) onto the legacy ones (99572, 99760) for the entire merged panel (its recode
# runs after all 7 datasets are joined, so it also relabeled df01/03/05/06/07's already-correct
# rows). That's backwards relative to MunYrs.rds (built from DANE's current DIVIPOLA list --
# see 01_source_files/Mun-Year.R) and would silently drop these two municipalities out of every
# downstream join in the pipeline, since master_panel is always built from MunYrs's codes.
legacy_codes <- c("23685", "99572", "99760")
df02 <- df02 %>% filter(!MPIO_CDPMP %in% legacy_codes)
df04 <- df04 %>% filter(!MPIO_CDPMP %in% legacy_codes)

#---- 3. Build the master skeleton from MunYrs.rds ----
# Kept from 03_geocoded_panel/01_merge_geocoded/01_merge_imputed.R's own approach, for
# consistency across the pipeline -- rather than re-deriving a skeleton from crossing() the raw
# datasets' own MPIO codes and manually filtering out department-level aggregates
# (z1_merge_all_raw2.R's approach). MunYrs.rds is the pipeline's single source of truth for
# which 1122 codes are valid; building the skeleton from it here means this script can never
# drift out of sync with what every later stage expects.
MunYrs <- read_rds(paste0(base, "01_source_files/MunYrs.rds"))

# 1998-2023 matches both prior scripts' stated intent (captures the 1998 elections and the
# 2005/2018 Censuses); downstream stages (03_geocoded_panel onward) subset further to 2000-2023.
YEAR_RANGE <- 1998:2023

master_panel <- MunYrs %>%
  distinct(MPIO_CDPMP) %>%
  expand_grid(year = YEAR_RANGE)

cat("Master skeleton:", n_distinct(master_panel$MPIO_CDPMP), "municipalities x",
    length(YEAR_RANGE), "years =", nrow(master_panel),
    "rows (expected", n_distinct(MunYrs$MPIO_CDPMP) * length(YEAR_RANGE), ")\n")

#---- 4. Join panel (year-varying) vs. static (snapshot) datasets separately ----
# Kept from z1_merge_all_raw2.R's approach -- fixes a real risk in z1_merge_all_raw.R's blind
# reduce(full_join) of all 7 datasets by (MPIO_CDPMP, year): a genuinely static/snapshot
# dataset joined on year risks failing to broadcast its values across every year. Here it's
# joined on MPIO_CDPMP only, so its values populate every year for that municipality. Verified
# 2026-07-03 which datasets are actually static: only df07 is (1122 rows, one per
# municipality). df03 looks static (geographic position) but already carries its own
# (MPIO_CDPMP, year) rows upstream -- see the note just above the join call below.
prep_df <- function(df) {
  if ("year" %in% names(df)) df <- df %>% mutate(year = as.numeric(as.character(year)))
  df %>%
    mutate(MPIO_CDPMP = as.character(MPIO_CDPMP)) %>%
    select(MPIO_CDPMP, any_of("year"), everything(),
           -any_of(c("depto", "provincia", "municipio", "DPTO_CCDGO", "MPIO_CNMBR")))
}

# NOTE 2026-07-03: df03 (criteria 5-9: north/south/east/west geographic position) is NOT a
# static/snapshot dataset despite being geography -- it already carries one row per
# (MPIO_CDPMP, year) upstream (26,928 rows, 24 identical yearly copies per municipality,
# 2000-2023), confirmed by inspection. Treating it as static (join by MPIO_CDPMP only, as
# z1_merge_all_raw2.R did) drops its year column and then matches every skeleton year against
# all 24 of its rows per municipality. Only df07 is genuinely static (1122 rows, exactly one per municipality).
df_final <- master_panel %>%
  left_join(prep_df(df01), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df02), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df03), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df04), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df05), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df06), by = c("MPIO_CDPMP", "year")) %>%
  left_join(prep_df(df07) %>% select(-any_of("year")), by = "MPIO_CDPMP")

#---- 5. Attach clean static metadata (name, department) from MunYrs ----
# Kept from z1_merge_all_raw2.R's approach of having one clean static-metadata join -- but
# sourced from MunYrs.rds rather than re-derived from df01/df05, since MunYrs already carries
# an authoritative name/department pairing per code (confirmed 2026-07-03: 33 correct
# departments, properly accented, zero NAs). This replaces both prior scripts' own department-
# name reconciliation logic (v1's manual code->name lookup table; v2's ad hoc accent/typo
# fixes) -- both were working around not having this authoritative source at hand within the
# merge step itself.
static_meta <- MunYrs %>%
  distinct(MPIO_CDPMP, .keep_all = TRUE) %>%
  select(MPIO_CDPMP, municipio, depto = departamento, DPTO_CCDGO)

df_final <- df_final %>%
  left_join(static_meta, by = "MPIO_CDPMP") %>%
  arrange(MPIO_CDPMP, year)

cat("Final merged panel:", n_distinct(df_final$MPIO_CDPMP), "municipalities,",
    nrow(df_final), "rows\n")

#---- 6. Diagnose the merged panel before imputation ----
# Both prior scripts computed similar missingness diagnostics; consolidated here and run on
# df_final itself. 
diagnostic_table <- df_final %>%
  summarise(
    total_obs = n(),
    across(where(is.numeric), ~ sum(is.na(.)), .names = "missing_{.col}")
  ) %>%
  pivot_longer(cols = starts_with("missing_"), names_to = "variable", values_to = "missing_count") %>%
  mutate(
    variable = str_remove(variable, "missing_"),
    missing_pct = round((missing_count / total_obs) * 100, 2)
  ) %>%
  select(variable, missing_count, missing_pct) %>%
  arrange(desc(missing_count))

missing_years <- df_final %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), ~ sum(!is.na(.)), .names = "non_missing_{.col}")) %>%
  pivot_longer(cols = starts_with("non_missing_"), names_to = "variable", values_to = "non_missing_count") %>%
  mutate(variable = str_remove(variable, "non_missing_")) %>%
  filter(non_missing_count == 0) %>%
  group_by(variable) %>%
  summarise(missing_years = paste(unique(year), collapse = ", "), .groups = "drop")

diagnostic_table <- diagnostic_table %>%
  left_join(missing_years, by = "variable") %>%
  mutate(
    missing_type = case_when(
      missing_pct == 0 ~ "No Missingness",
      missing_pct < 10 ~ "Minor Gaps (Random Missingness)",
      missing_pct >= 10 & missing_pct < 50 ~ "Moderate Gaps (Possible MAR)",
      missing_pct >= 50 & !is.na(missing_years) ~ "Structural Gaps (e.g., Census Years)",
      missing_pct >= 50 & is.na(missing_years) ~ "High Missingness (MNAR)",
      TRUE ~ "Unclassified"
    ),
    imputation_method = case_when(
      missing_type == "No Missingness" ~ "None",
      missing_type == "Minor Gaps (Random Missingness)" ~ "Linear Interpolation",
      missing_type == "Moderate Gaps (Possible MAR)" ~ "Multiple Imputation (PMM)",
      missing_type == "Structural Gaps (e.g., Census Years)" ~ "Mixed-Effects Models / Bayesian",
      missing_type == "High Missingness (MNAR)" ~ "Assess Mechanism / Consider Excluding",
      TRUE ~ "Manual Review Needed"
    )
  )

cat("\n---- Missingness diagnostic table (sorted worst first) ----\n")
print(diagnostic_table, n = Inf)

# Municipality-count validation -- should be exact, since the skeleton is built from MunYrs.
cat("\nFinal MPIO count:", n_distinct(df_final$MPIO_CDPMP),
    "(Expected:", n_distinct(MunYrs$MPIO_CDPMP), ")\n")

library(ggplot2)
plot_data <- df_final %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

distribution_plot <- ggplot(plot_data, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(title = "Distribution of Raw Merged Variables",
       subtitle = "Note: Free scales used to compare different units",
       x = "Value", y = "Density")

ggsave(paste0(base, "05_diagnostics/distribution_raw_merged.png"), distribution_plot,
       width = 14, height = 10, dpi = 300, bg = "white")

library(naniar)
miss_plot <- gg_miss_var(df_final, facet = year)
ggsave(paste0(base, "05_diagnostics/naniar_merged.png"), miss_plot,
       width = 10, height = 12, dpi = 300, bg = "white")

#---- 7. Save ----
write_csv(diagnostic_table, paste0(base, "05_diagnostics/diagnostic_table.csv"))
write_rds(df_final, paste0(base, "04_merge_empirical/df_col_clean.rds"))
