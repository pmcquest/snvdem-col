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
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------------------- #
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
# --------------------------------------------------------------------------- #
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
# Every weighted-range and relevance-weight calculation in this script (elections HPD range,
# CL HPD range, and all 4 criterion-weight families below) starts from the same shape of raw
# data: `v15cl` has one row per individual coder per country-year, with their raw 0/1/2
# response to some question (e.g. "did you see significant subnational variation?"). None of
# the formulas downstream can work on individual coder rows -- they need, per country-year, how
# many coders picked each response option (e.g. "10 coders said 0, 2 said 2"). This function
# makes the conversion, reused everywhere instead of being repeated per variable.
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
# What this section builds: for every country-year, a single number (`weighted_range`)
# representing how far a municipality's free-and-fair-elections score could plausibly sit above
# or below the *national* score (`v2elffelr`). That range is what lets 05_weighting spread the
# national estimate out across municipalities instead of assigning every municipality the
# identical national value.
#
# Where the range comes from: `v2elffelr_codehigh`/`_codelow` are the upper/lower bounds V-Dem's
# own measurement model already reports for its national point estimate (the width of that
# band, `HPD` below, reflects how much the underlying expert coders disagreed with each other).
# This script repurposes that existing width as a stand-in for subnational spread: instead of
# treating it as pure measurement uncertainty around one national number, it treats it as a
# plausible envelope that municipalities could occupy -- but only to the extent coders actually
# reported subnational variation existing at all (`v2elsnlsff` below). A country-year where
# coders unanimously say "no subnational variation" should get ~zero range even if its HPD is
# wide; a country-year with wide reported variation should get close to the full HPD width.
#
# Pipeline:
#   1. snlsff -- per country-year, how many coders picked each of the 3 v2elsnlsff categories
#      (no/somewhat/significant subnational variation).
#   2. HPDs -- per country-year, the national point estimate (v2elffelr) and HPD width.
#   3. snlsffHPD -- merge the two and collapse the 3 coder-count categories into one
#      coder-proportion-weighted `weighted_range`.
#   4. Plot it for Colombia (sanity check) and write `snlsffHPD.dta` for 06_benchmark.
#
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
  # coord_cartesian (not scale_y_continuous(limits=...)) -- the latter converts any point
  # outside the range to NA *before* geom_ribbon draws, silently dropping whole rows and
  # tearing a gap in the ribbon wherever ymin/ymax exceeds the window (confirmed 2026-07-06,
  # see weighting_bug_log_2026-07-06.md -- COL 1948-1953 has ymin = -3.609, just past -3.6).
  # coord_cartesian only zooms the viewport, so it can't drop data this way.
  coord_cartesian(ylim = c(-3.6, 3.3)) +
  theme_light() + labs(x = "", title = "Colombia",
                        y = "maximum national range for free and\nfair subnational elections")
ggsave(file.path(img_dir, "snlsff_colrange.png"), device = "png", height = 4, width = 6, units = "in", dpi = 300)

# Written here, not in 06_benchmark, because this is Step 4's own canonical output -- read
# cross-folder by 06_benchmark/01_benchmark/01_benchmark.R (2026-07-06 reorg: previously written
# directly into 06_benchmark, which blurred which step actually produces this file).
write_dta(snlsffHPD, file.path(out_dir, "snlsffHPD.dta"))

#---- Civil liberties: SN mean + HPD range ----
# Mirrors the elections section immediately above -- same goal (a per-country-year center value
# plus a plausible range around it, for 05_weighting to spread across municipalities) and same
# mechanics (a coder-proportion-weighted range built from an unevenness variable's 3 categories),
# but civil liberties needs one extra step elections didn't: there's no ready-made national
# "subnational CL" variable to use as the center, so CLSNmean below has to be constructed first.
#
# Pipeline:
#   1. clrgunev -- per country-year, how many coders picked each of the 3 v2clrgunev categories
#      (CL's unevenness variable, same response scale as v2elsnlsff above).
#   2. CLHPDs -- build the center (CLSNmean) and range width (CLHPD) from v2x_civlib +
#      v2clsnlpct, since no direct subnational CL variable exists (see note below).
#   3. SNHPD -- merge the two and collapse the 3 coder-count categories into `wtdCL_range`,
#      exactly as weighted_range was built for elections.
#   4. Plot it for Colombia and write `SNHPD.dta` for 06_benchmark.
#
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
# normal distribution." v2x_civlib is built from this kind of BFA composite (per
# codebook.pdf: v2x_clphy/v2x_clpol/v2x_clpriv are each a BFA over several latent components,
# then v2x_civlib is a plain average of those three already-[0,1] subcomponents) -- so it
# inherits the CDF-bounded scale, while v2elffelr never passes through this step at all. This is
# why 01_benchmark.R's qnorm(CLSNmean) is the *correct* inverse operation to recover a
# z-score-like scale for CL: it's inverting the same normal-CDF
# link function V-Dem names explicitly above. No reformulation of the weights below can change
# this; the bound is baked into v2x_civlib upstream, before this script ever touches the data.
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
  # coord_cartesian, not scale_y_continuous(limits=...) -- see identical fix + rationale above
  # for the elections plot (weighting_bug_log_2026-07-06.md).
  coord_cartesian(ylim = c(0, 1)) +
  theme_light() + labs(x = "", title = "Colombia",
                        y = "maximum national range for\nsubnational civil liberties")
ggsave(file.path(img_dir, "clx_colrange.png"), device = "png", height = 4, width = 6, units = "in", dpi = 300)

# See note above snlsffHPD's write_dta() call -- same reasoning, same 2026-07-06 move.
write_dta(SNHPD, file.path(out_dir, "SNHPD.dta"))

#---- Expert relevance-criteria weights (elections more/less, CL stronger/weaker) ----
# What this section answers: when a coder said one area of a country was MORE (or LESS) free
# and fair than the rest -- or, for CL, had STRONGER (or WEAKER) civil liberties -- V-Dem also
# asked them *why*, as a set of 22 yes/no characteristics (criterion_labels above: Rural,
# Urban, Less_development, ... None_of_the_above; one raw 0/1 coder-level column per criterion,
# e.g. v2elsnmrfc_0 .. v2elsnmrfc_21). This section turns those 22 yes/no columns into, per
# country-year and per criterion, a single "relevance weight": how much *more* often that
# criterion was cited to explain a MORE-free area than a LESS-free area (or vice versa).
# A criterion cited equally often on both sides (e.g. "Rural" showing up whether the area was
# freer or less free) gets a low weight -- it isn't actually distinguishing anything. This
# output (`ELCLweights_wide`) is what 05_weighting/01_weighting_geopredictors uses to decide how
# much each geographic predictor (urban/rural, distance to capital, etc.) should count for a
# given country-year.
#
# Pipeline within this section:
#   1. compute_criterion_weights() -- per prefix (one of the 4 more/less/stronger/weaker
#      variable families), turn the 22 raw yes/no columns into 22 proportions ("pr_0".."pr_21"):
#      share of coders, out of everyone who *could* have answered, who cited that criterion.
#   2. weights_long() -- reshape a wide 22-criterion table to one row per (country, year,
#      criterion) so the "more" and "less" (or "stronger"/"weaker") tables can be joined by
#      criterion name rather than by column position.
#   3. more_minus_less() -- join the more-side and less-side proportions per criterion and take
#      |more - less|: the relevance weight described above.
#   4. Do steps 1-3 once for elections (el_more/el_less -> el_wide) and once for civil liberties
#      (cl_more/cl_less -> cl_wide), then merge the two into one output table.

# One function replaces the ~22-block x 4-prefix repetition in the original scripts.
compute_criterion_weights <- function(v15cl, prefix, snlsffHPD, fix_bugs = FALSE) {
  # `prefix` is one of the 4 raw variable families named in the calls below
  # (v2elsnmrfc / v2elsnlfc / v2clrgstch / v2clrgwkch); returns one row per country-year with
  # 22 named columns (criterion_labels), each the proportion pr_i for criterion i.
  counts <- purrr::map(0:21, ~ count_wide(v15cl, paste0(prefix, "_", .x)))
  merged <- purrr::reduce(counts, dplyr::full_join, by = c("country_text_id", "year"))
  # snlsff_2 (elections' own "no subnational variation" coder count, from the snlsffHPD block
  # above) is pulled in here as an extra denominator term -- see note on `pr` below. This is
  # inherited as-is from weighting_January2026_0726.R (e.g. its line 1128 `cnc_proportions`
  # block for CL criteria), which used the *elections* snlsff_2 for all 4 prefixes, including
  # the two civil-liberties ones. Not something introduced by this rewrite -- flagged here
  # because it's non-obvious (CL has its own unevenness variable, clrgunev_2, that one might
  # expect to see used for the CL prefixes instead) and worth a second look if the operational
  # strategy doc is revisited.
  merged <- merge(snlsffHPD[, c("country_text_id", "year", "snlsff_2")], merged,
                   by = c("country_text_id", "year"), all.x = TRUE)

  pr <- purrr::map(0:21, function(i) {
    one <- merged[[paste0(prefix, "_", i, "_1")]]
    # Reproduce the original denominator bug for criteria 2/3/4 unless fixed.
    zero_idx <- if (!fix_bugs && i %in% c(2, 3, 4)) 1 else i
    zero <- merged[[paste0(prefix, "_", zero_idx, "_0")]]
    # pr_i = (coders who cited criterion i) / (coders who cited it + coders who explicitly did
    # not + coders who were never asked because they saw no subnational variation at all,
    # i.e. snlsff_2). Including snlsff_2 in the denominator means "no variation" coders count
    # as part of the eligible pool but never in the numerator -- they pull every criterion's
    # proportion down rather than being excluded from consideration entirely.
    one / (one + zero + merged$snlsff_2)
  })

  # Reproduce the original pr_5/pr_6 (Outside capital/North) value swap unless fixed.
  if (!fix_bugs) {
    tmp <- pr[[6]]; pr[[6]] <- pr[[7]]; pr[[7]] <- tmp
  }
  names(pr) <- criterion_labels

  dplyr::bind_cols(merged[, c("country_text_id", "year")], as_tibble(pr))
}

# Wide (one column per criterion) -> long (one row per country-year-criterion), so two wide
# tables with the same 22 criterion columns (e.g. a "more free/fair" table and a "less
# free/fair" table) can be merged by criterion *name* in more_minus_less() below, instead of by
# column position (fragile -- silently wrong if a future edit reorders criterion_labels).
weights_long <- function(wide_df) {
  wide_df %>% pivot_longer(-c(country_text_id, year), names_to = "dimension", values_to = "weight")
}

# Combines a "more/stronger" criterion-weights table and a "less/weaker" one (both from
# compute_criterion_weights()) into the final per-criterion relevance weight: |more - less|.
# abs() is deliberate -- the original script (weighting_January2026_0726.R)
# first tried the signed difference (its `mvl_el_wide`, commented "Caution: not absolute
# value"), then replaced it with the absolute value a few hundred lines later (its
# `more_vs_less_wide`, commented "New: made this absolute value") once it was clear an
# unsigned magnitude was the intended design: a criterion is "relevant" if it strongly
# distinguishes more-free from less-free areas in *either* direction, not only when it favors
# the "more free" side. This function reproduces that final (absolute-value) version only.
more_minus_less <- function(more_wide, less_wide, prefix_out) {
  more_long <- weights_long(more_wide) %>% rename(weight_more = weight)
  less_long <- weights_long(less_wide) %>% rename(weight_less = weight)
  merge(more_long, less_long, by = c("country_text_id", "year", "dimension"), all = TRUE) %>%
    mutate(weight_diff = abs(weight_more - weight_less)) %>%
    pivot_wider(id_cols = c(country_text_id, year), names_from = dimension, values_from = weight_diff) %>%
    rename_with(~ paste0(prefix_out, "_", .x), -c(country_text_id, year)) %>%
    arrange(country_text_id, year)
}

# Elections: v2elsnmrfc = why a subnational area was named MORE free and fair;
# v2elsnlfc = why an area was named LESS free and fair. el_wide's 22 columns (el_Rural,
# el_Urban, ...) are the |more - less| relevance weight for each criterion.
el_more <- compute_criterion_weights(v15cl, "v2elsnmrfc", snlsffHPD, FIX_KNOWN_BUGS)
el_less <- compute_criterion_weights(v15cl, "v2elsnlfc",  snlsffHPD, FIX_KNOWN_BUGS)
el_wide <- more_minus_less(el_more, el_less, "el")

# Civil liberties: v2clrgstch = why an area had STRONGER civil liberties; v2clrgwkch = why an
# area had WEAKER civil liberties. Same construction as elections, just the CL variable pair.
cl_more <- compute_criterion_weights(v15cl, "v2clrgstch", snlsffHPD, FIX_KNOWN_BUGS)  # stronger CL
cl_less <- compute_criterion_weights(v15cl, "v2clrgwkch", snlsffHPD, FIX_KNOWN_BUGS)  # weaker CL
cl_wide <- more_minus_less(cl_more, cl_less, "cl")

# Final output of this section: one row per country-year, 44 relevance-weight columns
# (el_Rural..el_None_of_the_above, cl_Rural..cl_None_of_the_above) -- written out below and
# consumed by 05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R.
ELCLweights_wide <- merge(el_wide, cl_wide, by = c("country_text_id", "year"), all = TRUE)

#---- Diagnostic ridge plots (one representative pair; originals had several duplicates) ----
# Why look at these at all: compute_criterion_weights()/more_minus_less() run 22 criteria x 4
# variable families through the same formula, entirely by column name/index -- exactly the kind
# of code where a silent misalignment (wrong criterion getting another's proportion, as the
# pr_5/pr_6 swap bug above did) produces numbers that are still in-range and never crash, but are
# wrong. A ridge plot of every criterion's full country-year distribution, side by side, is a
# fast visual check that nothing looks obviously broken (e.g. a criterion with a suspiciously
# identical distribution to its neighbor, or a distribution pinned at 0 that shouldn't be).
# `filter(!is.nan(weight))` drops country-year-criterion cells where compute_criterion_weights()'s
# denominator (one + zero + snlsff_2) was 0 -- no coders at all answered that question that
# year -- which is an undefined 0/0 proportion, not a real weight of zero.
# `reorder(dimension, weight)` sorts the 22 criteria by their mean weight (ascending); since
# geom_density_ridges stacks the first factor level at the bottom, this puts the
# highest-average-weight criteria at the top of the plot, so the most commonly-cited /
# most-distinguishing characteristics are the most visually prominent.
#
# The two plots below are diagnostics on two *different* quantities, not two views of the same
# one -- easy to conflate since both are elections-only, one-sided-looking ridge plots:

# Ridgelfc.png -- el_less itself (an *input*, before more_minus_less() touches it): for each
# criterion, the raw proportion of coders (per country-year) who cited it as a reason an area
# was LESS free and fair. Purely one-sided -- says nothing yet about whether that criterion also
# gets cited just as often for MORE-free areas (in which case it wouldn't be a good
# discriminator; see the second plot).
weights_long(el_less) %>%
  filter(!is.nan(weight)) %>%
  ggplot(aes(y = reorder(dimension, weight), x = weight)) +
  geom_density_ridges(scale = 2, fill = "red", alpha = .5) +
  theme_ridges(font_size = 12) + theme(legend.position = "none") +
  labs(title = "Expert weights in full sample", subtitle = "Subnational elections less free and fair",
       y = "", x = "", caption = "Distributions are of country-years. Source: V-Dem v.15")
ggsave(file.path(img_dir, "Ridgelfc.png"), device = "png", height = 9, width = 6, units = "in", dpi = 300)

# Ridgemoreless.png -- el_wide, the *output* of more_minus_less(el_more, el_less, "el"): for each
# criterion, |more - less|, i.e. the actual relevance weight written into ELCLweights_wide and
# consumed downstream by 05_weighting. rename_with() strips the "el_" prefix purely so the
# criterion names on the y-axis match the first plot's labels one-for-one, making the two
# ridge plots directly comparable row by row (e.g. does a criterion that ranked high in
# "cited for less-free areas" above still rank high here, or does it wash out because it's
# cited just as often for more-free areas?).
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
