#---- Step 4: Subset V-Dem data, calculate expert-weight criteria, national HPD ranges ----

# Pipeline:
# Step 1: Wrangle and clean raw data (Folder: "01_empirical_data")
# Step 2: Impute missing values and merge into one panel ("02_imputation", incl.
#         "02_imputation/03_merge_imputed" for the merge + CDF-standardize sub-stage)
# Step 3: Calculate averages of Empirical CDF data ("03_geocoded_panel")
# Step 4 (this step): Subset V-Dem data, calculate criteria weights, apply national range
#         ("04_vdem_data/02_vdem_weighting")
# Step 5: Weight Averaged CDF data by V-Dem data ("05_weighting")
# Step 6: Benchmark using national V-Dem data ("06_benchmark")
# Step 7: Revise final snvdem index ("07_final_snvdem_data")

# Streamlined replacement for MC's weighting_summer2025.R / weighting_January2026_0726.R
# (both preserved in 04_vdem_data/02_vdem_weighting/MC/).
# Same calculations, collapsed from ~90 repeated blocks into one loop per criterion set.
# Author: MC (original logic); rewritten by PM with Claude Code, July 2026.

library(tidyverse)
library(haven)
library(vdemdata)
library(ggridges)

#---- Paths (all explicit -- do not rely on getwd()) ----
# 04_vdem_data reorganized 2026-07-03 into numbered subfolders (source files / script+MC
# legacy / outputs / images), mirroring 01_empirical_data and 02_imputation's structure.
# out_dir promoted 2026-07-03 out of 02_vdem_weighting/MC/ (legacy tier) into its own
# 03_outputs/ -- ELCLweights_wide.dta is this script's live canonical output, not legacy
# material, so it shouldn't live inside the folder reserved for MC's original scripts.
panel_dir     <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
vdem_dir      <- file.path(panel_dir, "04_vdem_data")
source_dir    <- file.path(vdem_dir, "01_source_files", "V-Dem")
out_dir       <- file.path(vdem_dir, "03_outputs")
img_dir       <- file.path(vdem_dir, "04_images")
benchmark_dir <- file.path(panel_dir, "06_benchmark")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# BUG FIX -- flagged to MC 2026-07-02, see weighting_bug_log_2026-07-02.md and
# HANDOFF_pipeline_revision_june2026.md. The original scripts (weighting_summer2025.R,
# weighting_January2026_0726.R) had two bugs in the criterion-weight calculation:
#   1. pr_2 / pr_3 / pr_4 (Less development, More development, Inside capital) divided by the
#      criterion-1 zero-count instead of their own -- e.g. v2elsnlfc_1_0 instead of v2elsnlfc_2_0.
#   2. pr_5 and pr_6 (Outside capital, North) were computed out of order in the original
#      mutate() call, which combined with sequential colnames() labeling swapped their weights.
# Verified 2026-07-02: with FIX_KNOWN_BUGS <- FALSE this script reproduces the original
# (buggy) ELCLweights_wide.dta bit-for-bit. MC notified; PM confirmed to proceed with the fix.
# Original file backed up as 03_outputs/ELCLweights_wide_ORIGINAL_pre-fix_2026-07-02.dta.
# Set back to FALSE only to reproduce/audit the pre-fix behavior.
# ---------------------------------------------------------------------------
FIX_KNOWN_BUGS <- TRUE

# 0:21 criterion labels, in the sequential order MC's original colnames() calls intended.
criterion_labels <- c(
  "Rural", "Urban", "Less_development", "More_development", "Inside_capital",
  "Outside_capital", "North", "South", "West", "East", "Civil_unrest",
  "Illicit_activity", "Sparse_population", "Remote", "Indigenous",
  "Ruling_party_strong", "Ruling_party_weak", "Longer_foreign_rule",
  "Shorter_foreign_rule", "Recent_foreign_rule", "No_foreign_rule", "None_of_the_above"
)

#---- Load coder-level and country-year V-Dem data ----
# NOTE: the .csv referenced in prior script versions does not exist on disk; only the .rds
# does (confirmed 2026-07-02). Loading it directly is also faster and preserves column types.
v15cl <- readRDS(file.path(source_dir, "Coder-Level-Dataset-v15.rds"))
v15cl$year <- lubridate::year(v15cl$historical_date)

# Load latest V-Dem dataset
v15 <- vdemdata::vdem

#---- Helper: pivot one coder-level 0/1 variable to country-year wide counts ----
# Uses a key-based merge (not positional cbind, which the original relied on and which
# silently misaligns rows if any criterion has different country-year coverage).
count_wide <- function(data, var) {
  data %>%
    filter(year > 1899, !is.na(.data[[var]])) %>%
    count(country_text_id, year, .data[[var]], name = "freq") %>%
    pivot_wider(names_from = all_of(var), values_from = freq, values_fill = 0,
                names_prefix = paste0(var, "_")) %>%
    { if (!paste0(var, "_0") %in% names(.)) mutate(., "{var}_0" := 0) else . } %>%
    { if (!paste0(var, "_1") %in% names(.)) mutate(., "{var}_1" := 0) else . }
}

#---- Elections: subnational unevenness + HPD range ----
# v2elffelr is V-Dem's own subnational summary measure of free-and-fair elections, on the
# same scale/category definitions as the national v2elfrfair. It is used directly below as
# the center of the national range -- no proxy is needed for elections (contrast with civil
# liberties below, where no such direct subnational-average variable exists).
snlsff <- count_wide(v15cl, "v2elsnlsff") %>%
  rename(snlsff_0 = v2elsnlsff_0, snlsff_1 = v2elsnlsff_1, snlsff_2 = v2elsnlsff_2)

HPDs <- v15 %>%
  filter(year > 1899) %>%
  mutate(HPD = v2elffelr_codehigh - v2elffelr_codelow) %>%
  select(country_text_id, year, HPD, v2elffelr)

# v2elsnlsff response scale (confirmed against V-Dem codebook.pdf sec. 3.1.7.11 -- this is
# NOT an increasing "amount of unevenness" scale, it runs the other way):
#   0 = "Yes" -- subnational elections vary SIGNIFICANTLY across areas  -> should get weight 2
#   1 = "Somewhat" -- vary somewhat                                     -> should get weight 1
#   2 = "No" -- subnational elections are equally free/fair everywhere  -> should get weight 0
snlsffHPD <- merge(snlsff, HPDs, by = c("country_text_id", "year"), all.x = TRUE) %>%
  # weighted_range: coder-proportion-weighted average range of subnational variation. Coders
  # coding "significantly different" (snlsff_0) contribute 2*HPD; "somewhat different"
  # (snlsff_1) contribute 1*HPD; "no variation" (snlsff_2) contribute 0. Averaged across all
  # coders, per the operational strategy doc's own description of the design ("if all experts
  # chose answer 2 [No], multiply HPD by 2-2=0 ... if all chose 0 [Yes/significant], multiply
  # by 2-0=2"). Always >= 0, as a range must be.
  #
  # BUG FIX 2026-07-03 (see weighting_bug_log_2026-07-03.md): this line previously read
  # `snlsff_1 * HPD + snlsff_2 * 2 * HPD`, i.e. it weighted the "no variation" category
  # (snlsff_2) by 2 and dropped the "significant variation" category (snlsff_0) entirely --
  # the reverse of the codebook's response scale above. For Colombia, coders pick "significant
  # variation" (0) far more often than "no variation" (2), so the bug systematically
  # understated the range in most years.
  mutate(weighted_range = (snlsff_1 * HPD + snlsff_0 * 2 * HPD) /
           (snlsff_0 + snlsff_1 + snlsff_2))

ggplot(filter(snlsffHPD, country_text_id == "COL"), aes(x = year)) +
  geom_ribbon(fill = "skyblue", aes(ymax = v2elffelr + weighted_range, ymin = v2elffelr - weighted_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(-3.6, 3.3)) +
  theme_light() + labs(x = "", title = "Colombia",
                        y = "maximum national range for free and\nfair subnational elections")
ggsave(file.path(img_dir, "snlsff_colrange.png"), device = "png", height = 4, width = 6, units = "in", dpi = 300)

write_dta(snlsffHPD, file.path(benchmark_dir, "snlsffHPD.dta"))

#---- Civil liberties: SN mean + HPD range ----
# Unlike elections (v2elffelr, used directly above), V-Dem provides no direct subnational
# summary variable for civil liberties -- there is no CL equivalent of v2elffelr. Per the
# "Revised operational strategy" doc (06_benchmark/Revised operational strategy_Jan2026.docx,
# "This range is added to..."), CLSNmean substitutes a proxy built from two variables that
# *do* exist: the national civil liberties index (v2x_civlib) discounted by the percentage of
# the subnational population estimated to have weaker civil-liberties protection than the
# national average (v2clsnlpct). I.e. CLSNmean = v2x_civlib * (1 - v2clsnlpct/100). This is an
# approximation, not a measured subnational average -- worth flagging for anyone replicating
# the elections/CL calculations expecting them to be symmetric.
#
# WHY THIS PUTS ELECTIONS AND CL ON DIFFERENT SCALES (v2elffelr ~ unbounded latent/z-score-like;
# v2x_civlib bounded [0,1]) -- confirmed against 01_source_files/V-Dem/methodology.pdf (V-Dem
# Methodology v15, Coppedge et al., March 2025), sec 2.3, p.6: single-item "component" (C)
# variables like v2elffelr come straight from the coder-level IRT measurement model and stay on
# its raw, unbounded latent scale. Multi-indicator composites go through an extra step: "we run
# a unidimensional Bayesian factor analysis (BFA)... For ease of interpretation, we convert the
# relevant quantities to a zero-one scale using the cumulative distribution function of the
# normal distribution." v2x_civlib is built from exactly this kind of BFA composite (per
# codebook.pdf: v2x_clphy/v2x_clpol/v2x_clpriv are each a BFA over several latent components,
# then v2x_civlib is a plain average of those three already-[0,1] subcomponents) -- so it
# inherits the CDF-bounded scale, while v2elffelr never passes through this step at all. This is
# why 01_benchmark.R's qnorm(CLSNmean) is the *correct* inverse operation to recover a
# z-score-like scale for CL, not an approximation: it's literally inverting the same normal-CDF
# link function V-Dem names explicitly above. No reformulation of the weights below can change
# this -- the bound is baked into v2x_civlib upstream, before this script ever touches the data.
clrgunev <- count_wide(v15cl, "v2clrgunev") %>%
  rename(clrgunev_0 = v2clrgunev_0, clrgunev_1 = v2clrgunev_1, clrgunev_2 = v2clrgunev_2)

CLHPDs <- v15 %>%
  filter(year > 1899) %>%
  mutate(CLSNmean = v2x_civlib * (100 - v2clsnlpct) / 100,
         CLHPD = v2x_civlib_codehigh * (100 - v2clsnlpct) / 100 -
                 v2x_civlib_codelow  * (100 - v2clsnlpct) / 100) %>%
  select(country_text_id, year, CLHPD, CLSNmean, v2x_civlib, v2x_civlib_codelow, v2x_civlib_codehigh)

# NOTE: the original script built SNHPD from snlsffHPD+clrgunev, then immediately overwrote
# it with a CLHPDs+clrgunev-only merge on the next line. In practice this had no downstream
# effect -- 06_benchmark/01_benchmark.R only ever reads CLSNmean/wtdCL_range from SNHPD.dta,
# and gets elections fields separately from snlsffHPD.dta -- but it was dead/confusing code.
# Written directly here as the single intended merge.
#
# v2clrgunev has the same response scale as v2elsnlsff above (codebook.pdf sec. 3.9.2.7):
# 0 = "Yes, significantly" -> weight 2; 1 = "Somewhat" -> weight 1; 2 = "No" -> weight 0.
SNHPD <- merge(CLHPDs, clrgunev, by = c("country_text_id", "year"), all.x = TRUE) %>%
  # wtdCL_range: same coder-proportion-weighted-average construction as weighted_range above.
  # BUG FIX 2026-07-03 (see weighting_bug_log_2026-07-03.md): same category-weight inversion
  # as weighted_range -- previously weighted clrgunev_2 ("no variation") by 2 instead of
  # clrgunev_0 ("significant variation").
  mutate(wtdCL_range = (clrgunev_1 * CLHPD + clrgunev_0 * 2 * CLHPD) /
           (clrgunev_0 + clrgunev_1 + clrgunev_2))

ggplot(filter(SNHPD, country_text_id == "COL"), aes(x = year)) +
  geom_ribbon(fill = "skyblue", aes(ymax = CLSNmean + wtdCL_range, ymin = CLSNmean - wtdCL_range)) +
  scale_x_continuous(breaks = seq(1900, 2024, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_light() + labs(x = "", title = "Colombia",
                        y = "maximum national range for\nsubnational civil liberties")
ggsave(file.path(img_dir, "clx_colrange.png"), device = "png", height = 4, width = 6, units = "in", dpi = 300)

write_dta(SNHPD, file.path(benchmark_dir, "SNHPD.dta"))

#---- Expert relevance-criteria weights (elections more/less, CL stronger/weaker) ----
# One function replaces the ~22-block x 4-prefix repetition in the original scripts.
compute_criterion_weights <- function(v15cl, prefix, snlsffHPD, fix_bugs = FALSE) {
  counts <- purrr::map(0:21, ~ count_wide(v15cl, paste0(prefix, "_", .x)))
  merged <- purrr::reduce(counts, dplyr::full_join, by = c("country_text_id", "year"))
  merged <- merge(snlsffHPD[, c("country_text_id", "year", "snlsff_2")], merged,
                   by = c("country_text_id", "year"), all.x = TRUE)

  pr <- purrr::map(0:21, function(i) {
    one <- merged[[paste0(prefix, "_", i, "_1")]]
    # Reproduce the original denominator bug for criteria 2/3/4 unless fixed.
    zero_idx <- if (!fix_bugs && i %in% c(2, 3, 4)) 1 else i
    zero <- merged[[paste0(prefix, "_", zero_idx, "_0")]]
    one / (one + zero + merged$snlsff_2)
  })

  # Reproduce the original pr_5/pr_6 (Outside capital/North) value swap unless fixed.
  if (!fix_bugs) {
    tmp <- pr[[6]]; pr[[6]] <- pr[[7]]; pr[[7]] <- tmp
  }
  names(pr) <- criterion_labels

  dplyr::bind_cols(merged[, c("country_text_id", "year")], as_tibble(pr))
}

weights_long <- function(wide_df) {
  wide_df %>% pivot_longer(-c(country_text_id, year), names_to = "dimension", values_to = "weight")
}

more_minus_less <- function(more_wide, less_wide, prefix_out) {
  more_long <- weights_long(more_wide) %>% rename(weight_more = weight)
  less_long <- weights_long(less_wide) %>% rename(weight_less = weight)
  merge(more_long, less_long, by = c("country_text_id", "year", "dimension"), all = TRUE) %>%
    mutate(weight_diff = abs(weight_more - weight_less)) %>%
    pivot_wider(id_cols = c(country_text_id, year), names_from = dimension, values_from = weight_diff) %>%
    rename_with(~ paste0(prefix_out, "_", .x), -c(country_text_id, year)) %>%
    arrange(country_text_id, year)
}

el_more <- compute_criterion_weights(v15cl, "v2elsnmrfc", snlsffHPD, FIX_KNOWN_BUGS)
el_less <- compute_criterion_weights(v15cl, "v2elsnlfc",  snlsffHPD, FIX_KNOWN_BUGS)
el_wide <- more_minus_less(el_more, el_less, "el")

cl_more <- compute_criterion_weights(v15cl, "v2clrgstch", snlsffHPD, FIX_KNOWN_BUGS)  # stronger CL
cl_less <- compute_criterion_weights(v15cl, "v2clrgwkch", snlsffHPD, FIX_KNOWN_BUGS)  # weaker CL
cl_wide <- more_minus_less(cl_more, cl_less, "cl")

ELCLweights_wide <- merge(el_wide, cl_wide, by = c("country_text_id", "year"), all = TRUE)

#---- Diagnostic ridge plots (one representative pair; originals had several duplicates) ----
weights_long(el_less) %>%
  filter(!is.nan(weight)) %>%
  ggplot(aes(y = reorder(dimension, weight), x = weight)) +
  geom_density_ridges(scale = 2, fill = "red", alpha = .5) +
  theme_ridges(font_size = 12) + theme(legend.position = "none") +
  labs(title = "Expert weights in full sample", subtitle = "Subnational elections less free and fair",
       y = "", x = "", caption = "Distributions are of country-years. Source: V-Dem v.15")
ggsave(file.path(img_dir, "Ridgelfc.png"), device = "png", height = 9, width = 6, units = "in", dpi = 300)

weights_long(el_wide %>% rename_with(~ sub("^el_", "", .x), -c(country_text_id, year))) %>%
  filter(!is.nan(weight)) %>%
  ggplot(aes(y = reorder(dimension, weight), x = weight)) +
  geom_density_ridges(scale = 2, fill = "purple3", alpha = .7) +
  theme_ridges(font_size = 12) + theme(legend.position = "none") +
  labs(title = "Difference in subnational elections\nmore and less free and fair",
       subtitle = "Expert weights in full sample", y = "", x = "",
       caption = "Distributions are of country-years. Source: V-Dem v.15")
ggsave(file.path(img_dir, "Ridgemoreless.png"), device = "png", height = 9, width = 6, units = "in", dpi = 300)

#---- Write output ----
# With FIX_KNOWN_BUGS <- TRUE (the confirmed default), this writes directly to the live
# path read by 05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R. The
# pre-fix file is preserved as 03_outputs/ELCLweights_wide_ORIGINAL_pre-fix_2026-07-02.dta.
out_file <- file.path(out_dir, if (FIX_KNOWN_BUGS) "ELCLweights_wide.dta" else "ELCLweights_wide_v2_prefix-reproduction.dta")
write_dta(ELCLweights_wide, out_file)
cat("Wrote:", out_file, "\n")
cat("Rows:", nrow(ELCLweights_wide), " Cols:", ncol(ELCLweights_wide), "\n")
