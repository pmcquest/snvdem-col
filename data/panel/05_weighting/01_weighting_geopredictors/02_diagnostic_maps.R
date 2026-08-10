#---- Step 5 diagnostics: ruling-party stage-by-stage map + post-weighting facet maps ----

# Purpose: two diagnostic maps that go a step further than 01_weighting_geopredictors.R's own
# raw/hi-lo geopredictor maps.
#   A. Ruling party specifically, across every stage it passes through before it's summed into
#      snelect/sncivlib: raw (post single-flip) -> hi/lo split + strong-side flip -> weighted
#      contribution (using that year's real V-Dem weights, both el_ and cl_ families).
#   B. All 16 predictor criteria, each multiplied by its own V-Dem relevance weight (i.e. the
#      actual numerator terms inside weighted_avg_narm(), one map for el_ weights and one for
#      cl_ weights) -- the stage right before those 16 terms get summed and normalized into a
#      single snelect/sncivlib score.
#
# Self-contained: reloads CDF_averages.rds and ELCLweights_wide.dta and re-derives
# predictors_flip/predict_hilo/weights_col rather than depending on
# 01_weighting_geopredictors.R's run state, since that script has its own plotting/side effects
# and doesn't save these intermediate (pre-aggregation) objects to disk. Formulas below mirror
# that script exactly -- see it for the reasoning behind each flip/split.

library(tidyverse)
library(haven)
library(sf)
library(patchwork)

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
img_dir   <- file.path(panel_dir, "05_weighting", "02_images", "panels")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

REF_YEAR <- 2020  # matches the reference year used in 01_weighting_geopredictors.R's own maps

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

#---- Reload inputs and re-derive predict_hilo (mirrors 01_weighting_geopredictors.R) ----
weights <- read_dta(file.path(panel_dir, "04_vdem_data", "03_outputs", "ELCLweights_wide.dta"))
weights_col <- filter(weights, country_text_id == "COL" & year > 1999) %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity) / 2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity) / 2)

predictors <- readRDS(file.path(panel_dir, "03_geocoded_panel", "01_clean_geocoded", "CDF_averages.rds"))

predictors_flip <- predictors %>%
  mutate(
    avg0t1   = 1 - avg0t1,
    avg4t5   = 1 - avg4t5,
    avg10t11 = 1 - avg10t11,
    avg14    = 1 - avg14,
    avg15t16 = 1 - avg15t16,
    avg13    = (lAR_13pkm + (1 - DisMer_13)) / 2
  ) %>%
  select(-lAR_13pkm, -DisMer_13)

predict_hilo <- predictors_flip %>%
  mutate(avg0t1hi  = ifelse(avg0t1   > .5, avg0t1,   0),
         avg0t1lo  = ifelse(avg0t1   <= .5, avg0t1,  0),
         avg2t3hi  = ifelse(avg2t3   > .5, avg2t3,   0),
         avg2t3lo  = ifelse(avg2t3   <= .5, avg2t3,  0),
         avg4t5hi  = ifelse(avg4t5   > .5, avg4t5,   0),
         avg4t5lo  = ifelse(avg4t5   <= .5, avg4t5,  0),
         avg10t11hi = ifelse(avg10t11 > .5, avg10t11, 0),
         avg10t11lo = ifelse(avg10t11 <= .5, avg10t11, 0),
         avg15t16hi = ifelse(avg15t16 > .5, avg15t16, 0),
         avg15t16lo = ifelse(avg15t16 <= .5, avg15t16, 0),
         avg15t16hi_flip = 1 - avg15t16hi)

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp",
                     quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

year_data <- predict_hilo %>%
  left_join(weights_col, by = "year") %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP)) %>%
  filter(year == REF_YEAR)


##==== A. Ruling party: raw -> hi/lo split+flip -> weighted contribution ====================
# "Weighted contribution" = (strong_side * strong_weight + weak_side * weak_weight) /
# (strong_weight + weak_weight) -- the properly-weighted single number using that year's real
# V-Dem weights (not an unweighted 50/50 assumption). Shown separately for el_ and cl_, since
# the two dimensions' relevance weights differ and needn't peak at the same support level.
# NOTE (raw panel is NOT competitiveness): avg15t16 = 1 - RulPar_15t16, and RulPar_15t16 is a
# rescaled margin-of-victory between the (eventual) ruling-party candidate and the runner-up
# (01_empirical_data/.../15-16_wrangle-ts.R), not a symmetric competitiveness score -- checked
# against real municipalities (2020, LOCF'd from the 2018 Duque-vs-Petro race): Bogota/Cali/
# Barranquilla (all real Petro landslides) come out ~0.74-0.74 on RAW avg15t16, i.e. near the
# YELLOW end, while Medellin (a real Duque landslide) comes out ~0.33, near BLUE -- so raw
# avg15t16 is the mirror image of ruling-party margin (high = opposition landslide, low =
# ruling-party landslide), NOT "more competitive = higher." A genuinely close/tied race would
# sit at ~0.5 (mid-scale), not at either end. Renamed accordingly, and the actual
# competitiveness-shaped construct (pmin(avg15t16, 1 - avg15t16), peaks at 0.5, decays toward
# both extremes) is added as its own panel so the two aren't conflated.
# Sequential prefixes, not shared ones ("3."/"3.", "4."/"4.") -- facet_wrap sorts panels
# alphabetically by this label, and a shared prefix falls back to sorting by the label text
# itself. That happened to work out for strong/weak-side ("s" < "w") but NOT for the weighted
# pair: "civil liberties" < "elections" alphabetically, so despite being written EL-then-CL
# below, it was actually RENDERING CL-then-EL. Distinct numbers avoid relying on that coincidence.
ruling_party_year <- year_data %>%
  transmute(
    MPIO_CDPMP,
    `1. opposition margin (raw avg15t16)` = avg15t16,
    # x2: pmin() alone tops out at 0.5 (only a perfect 50/50 tie reaches it), so on this row's
    # shared 0-1 scale a tied race would only ever show as mid-scale teal, never true yellow.
    # Doubling stretches the tent to the full 0-1 range so a genuine tie reads as bright yellow.
    `2. competitiveness (tent)`           = 2 * pmin(avg15t16, 1 - avg15t16),
    `3. strong-side (avg15t16hi_flip)`    = avg15t16hi_flip,
    `4. weak-side (avg15t16lo)`           = avg15t16lo,
    `5. weighted (elections)`             = (avg15t16hi_flip * el_Ruling_party_strong +
                                                avg15t16lo * el_Ruling_party_weak) /
                                               (el_Ruling_party_strong + el_Ruling_party_weak),
    `6. weighted (civil liberties)`        = (avg15t16hi_flip * cl_Ruling_party_strong +
                                                avg15t16lo * cl_Ruling_party_weak) /
                                               (cl_Ruling_party_strong + cl_Ruling_party_weak)
  )

ruling_party_map <- muni_geo %>%
  left_join(ruling_party_year, by = "MPIO_CDPMP") %>%
  pivot_longer(cols = `1. opposition margin (raw avg15t16)`:`6. weighted (civil liberties)`,
               names_to = "stage", values_to = "value")

ruling_party_facet <- ggplot(ruling_party_map) +
  geom_sf(aes(fill = value), color = NA) +
  facet_wrap(~stage, nrow = 1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = paste("Ruling party across pipeline stages,", REF_YEAR),
       subtitle = "Raw (post-flip) -> hi/lo split+flip -> weighted contribution (real V-Dem weights)") +
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 8))
ruling_party_facet

ggsave(ruling_party_facet, filename = file.path(img_dir, paste0("ruling_party_stages_", REF_YEAR, ".png")),
       device = "png", width = 19, height = 5, units = "in", dpi = 300)


##==== B. All 16 criteria, weighted (predictor_k * weight_k), before aggregation ============
# The literal numerator terms inside weighted_avg_narm() (01_weighting_geopredictors.R), before
# rowSums() sums them and divides by the sum of weights. One facet map per weight family (el_/
# cl_) rather than one combined figure -- a shared scale across 32 criteria with very different
# native ranges would wash most of them out, the same issue flagged for north/south/east/west
# in the raw geopredictor map.
pred_cols <- c("avg0t1hi", "avg0t1lo", "avg10t11", "avg12", "avg13", "avg14",
               "avg15t16hi_flip", "avg15t16lo", "avg2t3hi", "avg2t3lo",
               "avg4t5hi", "avg4t5lo", "avg6", "avg7", "avg8", "avg9")

# Human-readable labels, same order as pred_cols -- matches the naming used in
# 01_weighting_geopredictors.R's raw/hi-lo geopredictor maps where these are the same criteria.
pred_labels <- c("urban (hi)", "rural (lo)", "unrest/illicit", "sparse population", "remote",
                  "indigenous", "ruling party (strong)", "ruling party (weak)",
                  "development (hi)", "development (lo)", "dist. capital (hi)",
                  "dist. capital (lo)", "north", "south", "west", "east")

el_weight_cols <- c("el_Urban", "el_Rural", "wt_el_1011", "el_Sparse_population", "el_Remote",
                     "el_Indigenous", "el_Ruling_party_strong", "el_Ruling_party_weak",
                     "el_More_development", "el_Less_development", "el_Inside_capital",
                     "el_Outside_capital", "el_North", "el_South", "el_West", "el_East")

cl_weight_cols <- c("cl_Urban", "cl_Rural", "wt_cl_1011", "cl_Sparse_population", "cl_Remote",
                     "cl_Indigenous", "cl_Ruling_party_strong", "cl_Ruling_party_weak",
                     "cl_More_development", "cl_Less_development", "cl_Inside_capital",
                     "cl_Outside_capital", "cl_North", "cl_South", "cl_West", "cl_East")

# Multiplies each predictor column by its matched weight column (position-for-position, same
# convention as pred_mat/el_wt_mat/cl_wt_mat in 01_weighting_geopredictors.R), relabels to the
# human-readable names above, and returns one long data frame ready for pivot_longer-style
# faceting.
weighted_long <- function(df, pred_cols, weight_cols, labels) {
  weighted <- mapply(function(p, w) df[[p]] * df[[w]], pred_cols, weight_cols)
  colnames(weighted) <- labels
  as_tibble(weighted) %>%
    mutate(MPIO_CDPMP = df$MPIO_CDPMP) %>%
    pivot_longer(-MPIO_CDPMP, names_to = "criterion", values_to = "value")
}

el_weighted_map <- muni_geo %>%
  left_join(weighted_long(year_data, pred_cols, el_weight_cols, pred_labels), by = "MPIO_CDPMP")

cl_weighted_map <- muni_geo %>%
  left_join(weighted_long(year_data, pred_cols, cl_weight_cols, pred_labels), by = "MPIO_CDPMP")

make_weighted_facet <- function(map_df, family_name) {
  ggplot(map_df) +
    geom_sf(aes(fill = value), color = NA) +
    facet_wrap(~criterion, nrow = 4) +
    scale_fill_viridis_c(na.value = "grey90") +
    labs(title = paste0("Weighted geopredictors (", family_name, "), ", REF_YEAR),
         subtitle = "predictor_k * weight_k -- before rowSums()/normalization into snelect or sncivlib") +
    theme_void() +
    theme(strip.text = element_text(face = "bold", size = 8))
}

el_weighted_facet <- make_weighted_facet(el_weighted_map, "elections")
cl_weighted_facet <- make_weighted_facet(cl_weighted_map, "civil liberties")
el_weighted_facet
cl_weighted_facet

ggsave(el_weighted_facet, filename = file.path(img_dir, paste0("geopredictors_weighted_el_", REF_YEAR, ".png")),
       device = "png", width = 18, height = 11.25, units = "in", dpi = 300)
ggsave(cl_weighted_facet, filename = file.path(img_dir, paste0("geopredictors_weighted_cl_", REF_YEAR, ".png")),
       device = "png", width = 18, height = 11.25, units = "in", dpi = 300)


##==== C. All 12 human-facing criteria, each across its own pipeline stages =================
# Generalizes Section A's ruling-party approach to the other 11 criteria. Four criteria are
# genuinely split (hi/lo pair with two different weights, same asymmetric-weighting pattern as
# ruling party): urban, development, dist. capital. The other 8 (remote, sparse population,
# indigenous, unrest/illicit, north/south/west/east) are single pass-through predictors with one
# weight each -- no split stage, and their "weighted" stage is plain predictor*weight (same
# convention as Section B), not a normalized combination (there's nothing to normalize with only
# one weight).
#
# Each criterion gets its OWN color scale (one scale per row, shared across that criterion's own
# stages) -- comparing a criterion's raw value against its weighted value on the same scale is
# informative (shows how much the weight shrinks it that year), but forcing one scale ACROSS
# different criteria would wash most of them out, per the north/south/east/west finding above.
criteria <- list(
  list(name = "ruling party",     split = TRUE,
       raw = "avg15t16", raw_label = "opposition margin", tent = TRUE,
       hi = "avg15t16hi_flip", lo = "avg15t16lo",
       hi_label = "strong-side", lo_label = "weak-side",
       el_hi = "el_Ruling_party_strong", el_lo = "el_Ruling_party_weak",
       cl_hi = "cl_Ruling_party_strong", cl_lo = "cl_Ruling_party_weak"),
  list(name = "urban / rural",    split = TRUE,
       raw = "avg0t1", raw_label = "raw (urban)", tent = FALSE,
       hi = "avg0t1hi", lo = "avg0t1lo",
       hi_label = "urban-dominant", lo_label = "rural-dominant",
       el_hi = "el_Urban", el_lo = "el_Rural",
       cl_hi = "cl_Urban", cl_lo = "cl_Rural"),
  list(name = "development",      split = TRUE,
       raw = "avg2t3", raw_label = "raw", tent = FALSE,
       hi = "avg2t3hi", lo = "avg2t3lo",
       hi_label = "more-developed", lo_label = "less-developed",
       el_hi = "el_More_development", el_lo = "el_Less_development",
       cl_hi = "cl_More_development", cl_lo = "cl_Less_development"),
  list(name = "dist. capital",    split = TRUE,
       raw = "avg4t5", raw_label = "raw (proximity)", tent = FALSE,
       hi = "avg4t5hi", lo = "avg4t5lo",
       hi_label = "inside capital", lo_label = "outside capital",
       el_hi = "el_Inside_capital", el_lo = "el_Outside_capital",
       cl_hi = "cl_Inside_capital", cl_lo = "cl_Outside_capital"),
  list(name = "remote",           split = FALSE, raw = "avg13", raw_label = "raw",
       el_w = "el_Remote", cl_w = "cl_Remote"),
  list(name = "sparse population", split = FALSE, raw = "avg12", raw_label = "raw",
       el_w = "el_Sparse_population", cl_w = "cl_Sparse_population"),
  list(name = "indigenous",       split = FALSE, raw = "avg14", raw_label = "raw",
       el_w = "el_Indigenous", cl_w = "cl_Indigenous"),
  list(name = "unrest/illicit",   split = FALSE, raw = "avg10t11", raw_label = "raw",
       el_w = "wt_el_1011", cl_w = "wt_cl_1011"),
  list(name = "north",            split = FALSE, raw = "avg6", raw_label = "raw (unflipped)",
       el_w = "el_North", cl_w = "cl_North"),
  list(name = "south",            split = FALSE, raw = "avg7", raw_label = "raw (unflipped)",
       el_w = "el_South", cl_w = "cl_South"),
  list(name = "west",             split = FALSE, raw = "avg8", raw_label = "raw (unflipped)",
       el_w = "el_West", cl_w = "cl_West"),
  list(name = "east",             split = FALSE, raw = "avg9", raw_label = "raw (unflipped)",
       el_w = "el_East", cl_w = "cl_East")
)

# Builds one criterion's stage row: weighted(CL) -> raw [+ tent] [+ hi/lo] -> weighted(EL), as a
# single facet_wrap ggplot sharing one color scale across its own stages. 
# No benchmarked stage: benchmarking (06_benchmark/01_benchmark/01_benchmark.R) is a linear rescale of the fully-summed
# snelect/sncivlib against that year's whole municipal distribution -- it doesn't decompose to
# individual criteria (the additive anchor to the national V-Dem estimate isn't a per-criterion
# quantity), so there's no well-defined "benchmarked development score" to show here.
build_criterion_row <- function(df, spec) {
  raw_val <- df[[spec$raw]]

  if (isTRUE(spec$split)) {
    hi_val <- df[[spec$hi]]
    lo_val <- df[[spec$lo]]
    w_el <- (hi_val * df[[spec$el_hi]] + lo_val * df[[spec$el_lo]]) /
      (df[[spec$el_hi]] + df[[spec$el_lo]])
    w_cl <- (hi_val * df[[spec$cl_hi]] + lo_val * df[[spec$cl_lo]]) /
      (df[[spec$cl_hi]] + df[[spec$cl_lo]])
  } else {
    w_el <- raw_val * df[[spec$el_w]]
    w_cl <- raw_val * df[[spec$cl_w]]
  }

  # Sequential prefixes (not shared ones like "3."/"3.") -- facet_wrap sorts panels
  # alphabetically by this label, and relying on hi_label/lo_label's own text to sort correctly
  # within a shared prefix is NOT safe (e.g. "rural-dominant" < "urban-dominant" alphabetically,
  # which would put the LO side before the HI side despite sharing a prefix meant to keep them
  # together in hi-then-lo order). Giving every stage its own number avoids that entirely.
  # Order (PM, 2026-07-31): raw [+ tent] -> hi/lo split -> weighted results, left to right.
  cols <- list()
  cols[[paste0("1. ", spec$raw_label)]] <- raw_val
  n <- 1

  if (isTRUE(spec$tent)) {
    n <- n + 1
    # x2: pmin() alone tops out at 0.5 (a 50/50 tie); doubling stretches it to the full 0-1
    # range so a genuine tie reads as true yellow on this row's shared scale, not mid-scale teal.
    cols[[paste0(n, ". competitiveness (tent)")]] <- 2 * pmin(raw_val, 1 - raw_val)
  }

  if (isTRUE(spec$split)) {
    n <- n + 1
    cols[[paste0(n, ". ", spec$hi_label)]] <- hi_val
    n <- n + 1
    cols[[paste0(n, ". ", spec$lo_label)]] <- lo_val
  }

  cols[[paste0(n + 1, ". weighted (elections)")]] <- w_el
  cols[[paste0(n + 2, ". weighted (civil liberties)")]] <- w_cl

  long_df <- as_tibble(cols) %>%
    mutate(MPIO_CDPMP = df$MPIO_CDPMP) %>%
    pivot_longer(-MPIO_CDPMP, names_to = "stage", values_to = "value")

  map_df <- muni_geo %>% left_join(long_df, by = "MPIO_CDPMP")

  ggplot(map_df) +
    geom_sf(aes(fill = value), color = NA) +
    facet_wrap(~stage, nrow = 1) +
    scale_fill_viridis_c(na.value = "grey90") +
    labs(title = str_to_sentence(spec$name)) +
    theme_void() +
    theme(strip.text = element_text(size = 7, face = "bold"),
          plot.title = element_text(size = 10, face = "bold"))
}

criterion_rows <- lapply(criteria, function(spec) build_criterion_row(year_data, spec))

# Saved individually first -- if the combined wrap_plots() below hits the same patchwork/
# ggplot2 compatibility issue as Section B's 16-panel facet, these still give you every
# criterion's row on its own.
for (i in seq_along(criteria)) {
  fname <- paste0("criterion_stages_", gsub("[^a-z0-9]+", "_", tolower(criteria[[i]]$name)), "_", REF_YEAR, ".png")
  ggsave(criterion_rows[[i]], filename = file.path(img_dir, fname),
         device = "png", width = 14, height = 3.2, units = "in", dpi = 250)
}

all_criteria_facet <- wrap_plots(criterion_rows, ncol = 1) +
  plot_annotation(title = paste("All 12 geopredictor criteria across pipeline stages,", REF_YEAR),
                  subtitle = "raw [+ tent] [+ hi/lo split] -> weighted contribution (real V-Dem weights, el_/cl_)")
all_criteria_facet

ggsave(all_criteria_facet, filename = file.path(img_dir, paste0("all_criteria_stages_", REF_YEAR, ".png")),
       device = "png", width = 16, height = 40, units = "in", dpi = 150)


##==== D. Stack the same 12 per-criterion PNGs into one markdown file =======================
# The composite all_criteria_facet above renders everything into one oversized ggplot object,
# which is hard to read at a glance. This just re-lists the SAME 12 PNGs already saved by the
# loop above (one image per criterion, each already at its own legible size), stacked in one .md
# file instead -- no re-rendering, no patchwork/facet risk, opens cleanly in any viewer that
# renders embedded images (VS Code preview, GitHub, etc.). Filenames must match the loop above
# exactly -- both build them the same way from `criteria[[i]]$name`.
md_lines <- c(
  paste0("# All 12 geopredictor criteria across pipeline stages, ", REF_YEAR),
  "",
  "Column order per row: weighted (civil liberties) -> raw [+ tent] [+ hi/lo split] -> weighted (elections).",
  ""
)

for (i in seq_along(criteria)) {
  fname <- paste0("criterion_stages_", gsub("[^a-z0-9]+", "_", tolower(criteria[[i]]$name)), "_", REF_YEAR, ".png")
  # width=100% scales each image to the PDF's text width (via pandoc -> \includegraphics[width=
  # \linewidth]) -- these PNGs are saved at 14in wide (width = 14, height = 3.2 in the ggsave()
  # loop above), well past a Letter/A4 page's ~6.5in usable width at 1in margins, so without
  # this they'd overflow and LaTeX would push them to a separate float page, detaching each
  # header from its image (exactly the drift PM found in the first knit). Ruling party (6
  # panels, not 5 -- the extra tent column) is left at natural size per PM's call: shrinking it
  # to match would make it the least legible panel instead, so it's exempted here.
  img_attr <- if (criteria[[i]]$name == "ruling party") "" else "{width=100%}"
  md_lines <- c(md_lines,
                paste0("## ", str_to_sentence(criteria[[i]]$name)),
                "",
                paste0("![", criteria[[i]]$name, "](", fname, ")", img_attr),
                "")
}

md_path <- file.path(img_dir, paste0("all_criteria_stages_", REF_YEAR, ".md"))
writeLines(md_lines, md_path)

# Knit straight to PDF via pandoc (standalone call, not rmarkdown::render() -- this is a plain
# .md with no R code chunks to execute, so there's nothing for knitr to run; pandoc alone is
# enough). Requires pandoc + a PDF engine (xelatex here, via MiKTeX) on PATH.
# -f markdown-implicit_figures: without this, pandoc wraps every standalone image in a floating
# LaTeX figure environment; combined with the width=100% attributes above this keeps images
# inline in reading order instead of letting LaTeX defer oversized ones to a separate float page
# (which is what caused the header/image drift in the first knit).
# --resource-path: the .md references each PNG by bare filename, which pandoc otherwise resolves
# relative to R's current working directory (wherever the script happens to be run from), NOT
# relative to the .md file's own location (img_dir) -- without this, pandoc can't find any of the
# images and silently falls back to alt-text ("Could not fetch resource" warnings).
pdf_path <- file.path(img_dir, paste0("all_criteria_stages_", REF_YEAR, ".pdf"))
pandoc_status <- system2("pandoc",
                          c(shQuote(md_path), "-o", shQuote(pdf_path),
                            "--pdf-engine=xelatex", "-f", "markdown-implicit_figures",
                            "--resource-path", shQuote(img_dir),
                            "-V", "geometry:margin=1in"))
if (pandoc_status != 0) {
  warning("pandoc failed to knit ", md_path, " to PDF (exit status ", pandoc_status, "). ",
          "Check that pandoc and a PDF engine (xelatex/pdflatex) are on PATH.")
}
