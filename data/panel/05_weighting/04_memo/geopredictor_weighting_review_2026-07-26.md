# Geopredictor Weighting Review: MC's Scripts vs. the Live Pipeline

**Date:** 2026-07-26
**Scope:** Steps 3-5 of the snvdem-col pipeline (CDF geopredictor data + expert relevance weights -> combining weights with geopredictors), plus the diagnostic mapping that checks both.
**Files reviewed:**

- MC's scripts (`drafts/1025-v3/Coppedge/geopredictors/`): `weighting_July2026corrected.R`,
  `wts_predictors_v2.R`, `ColMaps.R`, and `MemoCalculations.docx` (MC's own written reasoning
  for the "July" formula in `wts_predictors_v2.R`, discussed in Section B).
- Live pipeline counterparts: `04_vdem_data/02_vdem_weighting/02_vdem_weighting.R`,
  `05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R`,
  `06_benchmark/02_diagnostics/01_scripts/01_validate_map.R`
- Also updated: `03_geocoded_panel/01_clean_geocoded/01_clean_geocode.R` (Step 3), source of
  the geopredictor direction-flip relocated into Step 5.

**Section A** (expert relevance weights): the live pipeline matches MC's own formula. **Section B** (combining geopredictors with
weights): the live pipeline has been **updated to implement MC's "July" formula**
(`MemoCalculations.docx`/`wts_predictors_v2.R`'s `Indices_JulyMC`), replacing the prior formula
(what his script calls "MPSA version"). Everything below reflects the
pipeline as it stands after this update. **Section C:** Summary of changes to pipeline in recent weeks.

---

## A. V-Dem Expert Relevance Weights (Step 4)

### MC's script: `weighting_July2026corrected.R` (1,693 lines)

**Produces** `ELCLweights_wide.dta` — one row per country-year, with 44 columns
(`el_Rural`...`el_None_of_the_above`, `cl_Rural`...`cl_None_of_the_above`). Each column is a
"relevance weight": how much more often V-Dem coders cited that criterion (e.g. "Rural",
"Indigenous", "Remote") to explain why a subnational area was MORE vs. LESS free-and-fair (or
stronger/weaker on civil liberties). This is the file for describing how much each geographic
predictor should count for a given country-year.

**Structure:**

- **Lines 1-16:** loads V-Dem's coder-level dataset from MC's personal Dropbox
  (`C:/Users/mcoppedg/Dropbox/...`), computes `year` from `historical_date`, and de-duplicates
  to Dec-31-only rows (coders' year-end stamping; Jan-1 rows are duplicates of the prior year's
  coding).
- **Lines 18-61.** This block is uncommented prose describing indicator names, concept notes,
  and a design sketch for the weighting formula. (No leading `#`, so the script can't be run
  top-to-bottom as a single batch job.)
- **Lines 62-165:** builds the *elections* HPD-range calculation (`snlsffHPD`) — per
  country-year, how far a municipality's score could plausibly sit above/below the national
  V-Dem estimate, weighted by how many coders reported subnational variation. Includes a recent
  fix (2026-07-03) to the category-weight direction.
- **Lines 168-247:** the same range calculation for *civil liberties* (`SNHPD`/`wtdCL_range`),
  built from a proxy mean (`CLSNmean = v2x_civlib * (100 - v2clsnlpct)/100`) since V-Dem has no
  direct subnational CL summary variable.
- **Lines 249-1560:** the relevance-weight calculation itself, repeated by hand once per
  criterion (0-21) per variable family (`v2elsnmrfc`/`v2elsnlfc` for elections,
  `v2clrgstch`/`v2clrgwkch` for civil liberties) — roughly 90 nearly-identical copy-pasted
  blocks (e.g. `snlfc_0` through `snlfc_21`, then the same shape again for `snmrfc_*`,
  `snwkcc_*`, and a fourth family for "stronger CL" not shown above but structurally identical).
  Each block counts how many coders picked each 0/1 answer for that criterion, per country-year.
  These get cbind'd together (by column position, not by key) and turned into proportions
  (`pr_0`...`pr_21`), then the "more/stronger" and "less/weaker" proportion tables are merged
  and differenced (`abs(more - less)`) to get the final relevance weight per criterion.
- **Line 1560-1563:** merges the elections and CL weight tables into `ELCLweights_wide` and
  writes it out.

**Fixes present in this version:** the criterion-weight zero-count bug (criteria 2/3/4
dividing by criterion 1's zero-count instead of their own) and the pr_5/pr_6
(Outside_capital/North) column-order swap are both corrected here, applied consistently across
all 4 variable families.

**Caveats:** personal Dropbox paths throughout (not portable), and per above, not runnable as a
single batch script.

### Live pipeline: `04_vdem_data/02_vdem_weighting/02_vdem_weighting.R` (417 lines)

Same target output (`ELCLweights_wide.dta`, in the `03_outputs/` folder) and same underlying
formulas as MC's script, restructured as reusable functions instead of ~90 hand-copied blocks:

- **`count_wide()` (lines 81-89):** the one function that replaces every `snlfc_i`/`snmrfc_i`/
  `snwkcc_i`/`snstcc_i`-style block in MC's script — pivots one coder-level 0/1 column to
  country-year wide counts, using a proper `count()`/`pivot_wider()` instead of manual grouping.
- **`compute_criterion_weights()` (lines 275-312):** replaces the whole "22 blocks x 4 prefixes"
  section — takes one of the 4 raw variable-family prefixes and returns all 22 criterion
  proportions per country-year in one call. Includes the `FIX_KNOWN_BUGS` switch (line 51,
  default `TRUE`) that reproduces MC's two confirmed bugs on demand (denominator using criterion
  1's zero-count for criteria 2/3/4; the pr_5/pr_6 swap) for audit purposes.
- **`more_minus_less()` (lines 331-339):** joins the more/less (or stronger/weaker) tables **by
  criterion name**, not column position.
- **Lines 91-247:** the elections/CL HPD-range sections — same fixed formula as MC's script
  (category-weight direction corrected 2026-07-03), plus a plotting fix: `coord_cartesian()`
  instead of `scale_y_continuous(limits=...)` so out-of-range points get clipped visually rather
  than silently dropped as missing rows.
- **Lines 341-357:** builds `el_wide`/`cl_wide`/`ELCLweights_wide` with both fixes applied.
- **Lines 409-416:** writes `ELCLweights_wide.dta` to `03_outputs/` (the pre-fix version is
  preserved separately as `ELCLweights_wide_ORIGINAL_pre-fix_2026-07-02.dta` for audit).

### Comparison: Expert Relevance Weights

| Aspect | MC's script | Live pipeline |
| --- | --- | --- |
| Formula correctness | Both known bugs fixed | Same fixes, via `FIX_KNOWN_BUGS` switch |
| Result | — | **Identical** for COL: 125 country-years x 44 columns, max abs diff = 0.0 |
| Code structure | ~90 hand-copied blocks | 2 reusable functions |
| Merge method | Positional `cbind()` (fragile) | Key-based `full_join`/`merge` |
| Portability | Personal Dropbox paths, not batch-runnable | Shared-drive paths, runs via `Rscript` |

---

## B. Combining Geopredictors with Weights (Step 5)

### MC's script: `wts_predictors_v2.R` (539 lines)

**Produces:** per-municipality-year `snelect` (electoral) and `sncivlib`
(civil liberties) index scores for Colombia, by taking the 16 geocoded predictor columns
(urbanization, distance to capital, remoteness, etc.) and computing a weighted average using
the relevance weights from Section A.

**Structure:**

- **Lines 1-16:** loads `ELCLweights_wide.dta` and `CDF_averages_v1.rds` (both read from local
  copies MC downloaded, not the shared drive), builds `wt_el_1011`/`wt_cl_1011` by averaging the
  Civil_unrest and Illicit_activity weight columns into one.
- **Lines 29-49:** checks the median of each predictor (mostly ~0.5, except South/East, which
  are single directional predictors rather than symmetric pairs) and builds hi/lo split columns
  for the five paired predictors (urban/rural, development, distance-to-capital, unrest/illicit,
  ruling-party) at the 0.5 threshold.
- **Lines 51-165:** exploratory analysis — correlation scatterplots between predictor pairs, a
  Spearman-correlation check on an "extreme rural" subset against a second, differently-named
  input file (`imputed_cdf_panel07_26.rds`), and quadrant-identification of extreme
  municipalities. None of this feeds the final index; it's scratch work.
- **Line 170-171:** the actual join used downstream — `Indices <- predict_hilo %>%
  left_join(weights_col, by = "year")`.
- **Lines 173-333:** more diagnostics — correlation matrices and 12 scatterplots checking that
  each predictor's spatial pattern looks sensible.
- **Lines 335-372 — "Indices_JulyMC":** computes
  `snelect`/`sncivlib` by flipping *geopredictor values*, not weights, for Ruling_party and
  North -- `(1-avg15t16hi)` for Ruling_party_strong (`avg15t16lo` unflipped for
  Ruling_party_weak; see `MemoCalculations.docx`), and `(1-avg6)` for North -- while leaving Indigenous, Ruling_party, and
  Inside/Outside_capital *weights* unflipped (raw).
- **Lines 374-409** — "Indices_both"/"MPSA version" (the pipeline's prior formula, now
  superseded): computed `snelect`/`sncivlib` by instead flipping the *weight* for exactly 5
  criteria (Indigenous, Ruling_party_strong, Ruling_party_weak, Inside_capital,
  Outside_capital) via `1 - el_Indigenous` etc.
- **Line 412:** `write_rds(Indices_JulyMC, "Indices_both.rds")` — the "JulyMC" result is saved
  under the filename "Indices_both.rds," even though a differently-computed object literally
  named `Indices_both` (the MPSA version) exists later in the same script — a naming trap for
  anyone loading `Indices_both.rds` expecting the MPSA formula.
- **Lines 414-525:** more diagnostics and choropleth/bubble maps.
- **Lines 529-533:** the MPSA-version `Indices_both` gets `sndem = 0.5*(snelect+sncivlib)`
  added and is written to `08_final_snvdem_data/MC/SNDEM_tentative.rds` — MC's own script's
  final output uses the MPSA version, not the JulyMC version saved to `Indices_both.rds` at
  line 412.

### `MemoCalculations.docx`

MC offered the "July"/"Indices_JulyMC" formula in his latest memo:

- **States plainly that the prior approach was wrong:** *"This code corrects errors in the
  previous version that a) subtracted weights rather than geopredictors from 1 and b) was
  based on a different understanding of how some of the geopredictor variables were aligned
  with democracy."* 
- **Urban, More_development, Inside_capital** ("the most straightforward geopredictors"): the
  variable is already aligned with democracy in the right direction once the single Step
  3/Step 5 geopredictor flip is applied, so no further weight or geopredictor adjustment is
  needed.
- **Civil_unrest/Illicit_activity, Sparse_population, Remote, Indigenous:** labeled as if
  negatively related to democracy, but *the underlying variables* are actually already
  positively related once flipped -- e.g. "Indigenous population in effect actually measures
  White or Mestizo population." So these also need no further adjustment beyond the one
  geopredictor flip.
- **Ruling_party (strong/weak):** explicitly designed as a *non-linear, inverted-V*
  relationship -- below 50% ruling-party vote share, more support helps democracy (genuine
  popularity); above 50%, more support hurts it (overwhelming dominance). This is *why* only
  the "strong" (>50%) side gets its geopredictor flipped and the "weak" side doesn't.
- **North/South/East/West:** the memo describes a two-part correction, but flags its own
  premise as unverified — the memo's derivation ends with "`[Check this with
  CDF_averages7_20_26.rds]`." Using the updated data, the reversal MC described is not
  present, which leaves only his theoretical claim about North (independent negative
  relationship with democracy) unconfirmed either way. Live pipeline leaves North, South,
  East, and West all unflipped, same as before this review, pending MC's direct confirmation
  on North specifically -- the earlier same-day version of this pipeline flipped North on a
  re-derivation from his claim, but that was reverted per PM rather than act on a claim MC
  hasn't separately confirmed post-data-fix.
- **Cumaribo note:** the memo separately flags a likely miscoding of Cumaribo
  ("the largest municipality in the country") based on its facet maps. PM addressed this: it was a DIVIPOLA geocoding error that has since been corrected.

### Updated live pipeline

`05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R` was updated to implement
the above. The prior version is preserved as
`01_weighting_geopredictors_ORIGINAL_pre-julyformula_2026-07-26.R`, and its output as
`03_output/snvdem_col_weighted_ORIGINAL_pre-julyformula_2026-07-26.rds`.

- **Lines 111-143 (geopredictor direction-flip block):** North, South, East, and West are all
  left unflipped, same as before this update. A same-day version briefly added `avg6 = 1 -
  avg6` (North), re-derived from MC's theoretical claim now that the data-reversal premise
  behind his memo's literal derivation no longer holds -- reverted per PM: implementing a
  re-derivation rather than something MC stated directly isn't the right call without his
  confirmation. North remains an open question, exactly like South/East/West.
- **Lines 159-168 (hi/lo split):** added `avg15t16hi_flip = 1 - avg15t16hi`, implementing the
  Ruling_party inverted-V. This column is always defined (equal to 1 when a row is outside the
  "strong" regime), reproducing MC's `(1-avg15t16hi)` term exactly -- it is the other half of
  one continuous tent-shaped function together with `avg15t16lo`.
- **Line 232 (`pred_cols`):** Ruling_party_strong's slot now points at `avg15t16hi_flip`
  instead of raw `avg15t16hi`.
- **Lines 253-261 (`el_wt_mat`/`cl_wt_mat`):** removed the `1 - weight` flip for Indigenous,
  Ruling_party_strong, Ruling_party_weak, Inside_capital, and Outside_capital -- these now use
  the raw relevance weight, matching MC's July formula. Direction for these criteria is now
  carried entirely by the geopredictor side (already handled above), not the weight.
- **`weighted_avg_narm()` and its 50%-weight-mass floor are unchanged** -- this renormalization
  logic (MC's formula doesn't have an equivalent) is layered on top of the new formula. NAs remain at 0 (previously 682 with a naive sum, 48 before this
  renormalization was added on 2026-07-03).

### Connection to the paper's theoretical claims

Two of the criteria this update touches already have a stated position in the paper, and MC's
memo lines up with both:

- **Ruling_party dominance:** the paper argues the *machine-politics* interpretation — a
  dominant ruling party signals capture/patronage, not popularity, and should count against the
  democracy score. MC's memo's inverted-V (dominance above 50% hurts democracy) is consistent
  with this.
- **Capital proximity:** the paper argues that
  proximity to Bogota signals more state capacity, and should count toward the democracy score.
  MC's memo groups "inside capital" among the "most straightforward" geopredictors, positively
  associated with democracy with no further adjustment needed — also consistent.
- **Indigenous population:** no stated paper position, and the memo's own justification (avg14
  "in effect actually measures White or Mestizo population," assumed positive) is a substantive
  claim that should be agreed to by the team (secondary literature available?).

### By the numbers: what changed when the live pipeline adopted MC's July formula

Compared the live pipeline's actual output before and after the formula update, using the same live inputs
(`ELCLweights_wide.dta`, `CDF_averages.rds`) in both cases, so every difference below comes from
the formula change alone. n = 26,928 municipality-years (COL, 2000-2023).

**Overall distribution:**

| Statistic | `snelect` — prior (MPSA) | `snelect` — current (July, North unflipped) | `sncivlib` — prior | `sncivlib` — current | `sndem` — prior | `sndem` — current |
| --- | --- | --- | --- | --- | --- | --- |
| Min | 0.111 | 0.077 | 0.126 | 0.112 | 0.121 | 0.099 |
| Median | 0.333 | 0.385 | 0.354 | 0.380 | 0.344 | 0.382 |
| Mean | 0.331 | 0.379 | 0.353 | 0.373 | 0.342 | 0.376 |
| Max | 0.535 | 0.615 | 0.584 | 0.590 | 0.556 | 0.580 |

The current (July) formula runs uniformly higher — every quartile shifted up.
`weighted_avg_narm()`'s renormalization means neither version has any NAs (0 in both), even
though MC's own formula alone (without that renormalization) would reintroduce 634.

**Row-level difference (current minus prior), same municipality-year matched 1:1:**

| Index | Mean diff | Mean abs diff | Max abs diff | Correlation |
| --- | --- | --- | --- | --- |
| `snelect` | +0.048 | 0.066 | 0.216 | 0.747 |
| `sncivlib` | +0.021 | 0.042 | 0.144 | 0.823 |
| `sndem` | +0.034 | 0.052 | 0.147 | 0.797 |

A correlation of 0.71-0.82 means this is a visibly different measure of subnational democracy. Municipality-years reorder, not just shift.

**Rank stability** (Spearman correlation of `sndem` within each year — i.e. if only relative
ranking of municipalities mattered, not absolute level):

| Year | Spearman rho |
| --- | --- |
| 2000 | 0.777 |
| 2008 | 0.889 |
| 2016 | 0.895 |
| 2023 | 0.879 |

Even in the best year (2016), roughly 1 - 0.895^2, about 20%, of rank variance differs between
the two formulas — non-trivial for anything that compares municipalities to each other rather
than tracking one municipality over time.

**National year-means for `sndem`** (the number that actually appears in a published trend
line):

| Year | Prior (MPSA) mean | Current (July, North unflipped) mean | Diff |
| --- | --- | --- | --- |
| 2000 | 0.359 | 0.356 | -0.004 |
| 2005 | 0.326 | 0.346 | +0.020 |
| 2008 | 0.313 | 0.374 | +0.060 |
| 2010 | 0.314 | 0.390 | +0.076 |
| 2013 | 0.319 | 0.397 | +0.078 |
| 2016 | 0.356 | 0.386 | +0.029 |
| 2020 | 0.357 | 0.389 | +0.032 |
| 2023 | 0.371 | 0.380 | +0.009 |

The gap is not constant over time: near-zero in 2000, widening to roughly +0.08 in 2010-2013,
then narrowing again toward 2023. The formula change also changes the shape of the time trend. 

**Where the change concentrates** (top single-row differences, `snelect`):

| MPIO_CDPMP | Municipality | Year | Prior (MPSA) | Current (July) | Diff |
| --- | --- | --- | --- | --- | --- |
| 25489 | Nimaima (Cundinamarca) | 2010 | 0.323 | 0.539 | +0.216 |
| 25489 | Nimaima (Cundinamarca) | 2009 | 0.315 | 0.530 | +0.216 |
| 25489 | Nimaima (Cundinamarca) | 2011 | 0.316 | 0.529 | +0.213 |
| 25489 | Nimaima (Cundinamarca) | 2008 | 0.308 | 0.520 | +0.212 |
| 25489 | Nimaima (Cundinamarca) | 2012 | 0.318 | 0.530 | +0.212 |

The largest single-row changes (~0.21, on a [0,1]-ish scale) land on the same handful of
municipalities repeated across years -- Nimaima especially -- consistent with those
municipalities having unusually extreme values on exactly the criteria the formula change
touches (Indigenous, Ruling_party, Capital), rather than a diffuse shift spread evenly
everywhere. North is no longer part of what changed -- see below.

**North/South/East/West: reconsidered, reverted to unflipped.** A same-day version of this
update briefly flipped North (re-derived from MC's theoretical claim, now that the data
reversal behind his memo's literal derivation is verified gone). That was reverted per PM:
implementing a re-derivation of MC's claim isn't the same as implementing something he stated
directly, and shouldn't go live without his confirmation. North, South, East, and West are all
currently unflipped -- identical to the pipeline's treatment before today. The isolated effect
of the North question alone (measured before the revert, flipping North only vs. leaving it
unflipped, holding everything else fixed) was correlation 0.98, mean abs diff ~0.014 -- a real
but secondary effect next to the Indigenous/Ruling_party/Capital change, and one now absent
from the live pipeline until MC confirms.

### Facet map: pre-benchmarked `sndem`, 2000-2023

![Subnational democracy (pre-benchmark), 2000-2023 -- July formula](../../06_benchmark/02_diagnostics/02_outputs/sndem_prebenchmark_facet_allyears_2026-07-26.png){width=100%}

The central-highlands "hotspot" pattern MC's memo describes ("better democracy in the central
highlands, worse outcomes on the borders and coastlines, especially in the south") is visible
here. Cumaribo still sticks out but this is probably reflective of its position in the index.

### Comparison: Combining Geopredictors with Weights

| Aspect | MC's script | Live pipeline |
| --- | --- | --- |
| Which formula | "MPSA version" (superseded) vs. "Indices_JulyMC" (current) — MC's own script's tail end still uses MPSA, per `MemoCalculations.docx` this is inertia, not intent | **Now implements "Indices_JulyMC"**, per `MemoCalculations.docx` |
| 5-of-16 weight-flip (Indigenous, Ruling_party x2, Capital x2) | Present in "MPSA" only | **Removed 2026-07-26** — per MC's memo, this was the error his July formula corrects |
| Ruling_party inverted-V | Present in "Indices_JulyMC" via geopredictor flip | **Implemented 2026-07-26** via `avg15t16hi_flip` |
| North vs. South/East/West flip | Shipped code flips North; memo's literal derivation flips South/East/West | **Unflipped** (all four) -- briefly implemented a North-only flip same-day, reverted per PM pending MC's direct confirmation |
| Missing-predictor handling | None — one missing criterion blanks the whole row | `weighted_avg_narm()`: renormalizes over available criteria, floors at 50% weight mass |

---

## C. Key Changes to the Live Pipeline (last few weeks)

- **2026-07-02:** Criterion-weight zero-count bug found and fixed (criteria 2/3/4 dividing by
  criterion 1's zero-count instead of their own).
- **2026-07-03:**
  - HPD range category-weight inversion fixed (elections and CL both previously weighted "no
    variation" coders by 2x instead of "significant variation" coders — understated the range
    in most country-years).
  - `01_weighting_geopredictors.R`'s plain weighted-sum found to blank whole rows on any single
    missing predictor (a side effect of Step 2's `na.last="keep"` fix letting real NAs flow
    through); decided and implemented `weighted_avg_narm()` with a 50%-weight-mass floor — NAs
    682 -> 48 (see `narm_decision_log_2026-07-03.md`).
  - `04_vdem_data` and `05_weighting` reorganized into numbered subfolders
    (source-files/script+MC-legacy/outputs/images), mirroring `01_empirical_data`'s structure.
  - Positional `cbind()` merges replaced with key-based joins in the criterion-weight
    calculation (protects against silent misalignment if criteria gain uneven country-year
    coverage).
- **2026-07-06:**
  - `scale_y_continuous(limits=...)` (silently drops out-of-range rows before plotting) replaced
    with `coord_cartesian(ylim=...)` (zooms only, never drops data) in both HPD-range ribbon
    plots, after a visible gap in the Colombia 1948-1953 range confirmed the bug.
  - Confirmed directly (not just by inference) that MC's legacy range formula produces
    impossible negative ranges for 40% of all country-year observations globally — documented so
    old figures built from that formula aren't mistaken for a correctness reference.
  - Output-file locations cleaned up: canonical outputs (`ELCLweights_wide.dta`,
    `snvdem_col_weighted.rds`) promoted out of legacy `MC/` folders into each step's own
    `03_outputs`/`03_output`; unbenchmarked and benchmarked data no longer share one folder.
  - Removed a since-obsolete manual NA-fill hardcode for `cl_Less_development` (2000-2002/2004)
    once the underlying merge/weight fixes made it a no-op.
  - Persisted the civil-unrest/illicit-activity scatter plot via `ggsave()` (previously only
    displayed in interactive sessions, silently dropped in batch runs).
- **2026-07-21:** Documented and finalized the missing-predictor renormalization design
  (`weighted_avg_narm()` + `MIN_WEIGHT_FRACTION`) as the pipeline's committed approach.
- **2026-07-26:** Relocated the geopredictor direction-flip (avg0t1, avg4t5, avg10t11, avg14,
  avg15t16, and the avg13 blend) from Step 3 (`01_clean_geocode.R`'s "Ensure correct score
  directions" section) to Step 5 (`01_weighting_geopredictors.R`, immediately after loading
  `CDF_averages.rds`), so direction is decided in exactly one place instead of two.
  `CDF_averages.rds` now stores raw (pre-flip) values for those columns; `avg13`'s two raw
  components (`lAR_13pkm`/`DisMer_13`) are no longer merged in Step 3 either, since flipping
  after averaging would incorrectly flip both components instead of just the one that needs it.
  Caught and fixed one regression along the way: `ViolInd_1011`'s raw name lacks the "t" its
  siblings have, so the generic rename-to-`avg<N>` step would have silently produced `avg1011`
  instead of `avg10t11` — added an explicit rename. Validated end-to-end (0.0 max abs diff,
  26,928/26,928 rows matched, identical NA pattern) before applying to the live paths.
- **2026-07-26:** Found and read MC's `MemoCalculations.docx`, his written reasoning for the
  "July" formula in `wts_predictors_v2.R`. Verified his cardinal-direction reasoning
  (North/South/East/West) against current data: the raw-data reversal his memo describes is not
  present in the current `CDF_averages.rds` (spot-checked Apartado, Cucuta, Cali, Puerto
  Leguizamo against known geography; national `cor(avg6,avg7) = -0.71`, `cor(avg8,avg9) =
  -0.65`, both consistent with correct, non-reversed coding).
- **2026-07-26:** Updated `01_weighting_geopredictors.R` to implement MC's July formula:
  added a geopredictor flip for North (re-derived from his theoretical claim against verified
  current data); added the Ruling_party inverted-V via a new `avg15t16hi_flip` column; removed
  the `1 - weight` flip for Indigenous, Ruling_party_strong/weak, and Inside/Outside_capital in
  `el_wt_mat`/`cl_wt_mat`. Validated against independently-computed versions of MC's formula
  before applying to the live paths. Prior script version preserved as
  `01_weighting_geopredictors_ORIGINAL_pre-julyformula_2026-07-26.R`; prior output preserved as
  `snvdem_col_weighted_ORIGINAL_pre-julyformula_2026-07-26.rds`. `weighted_avg_narm()` and its
  missing-data renormalization are unchanged by this update.
- **2026-07-26:** Reverted the North flip added earlier the same day. Per PM: implementing a
  re-derivation of MC's theoretical claim about North (rather than something he stated
  directly, now that the data-reversal premise behind his literal memo derivation is verified
  gone) shouldn't go live without his confirmation. North, South, East, and West are all
  unflipped again, matching the pipeline's treatment before this review started. Intermediate
  (North-flipped) output preserved as
  `snvdem_col_weighted_ORIGINAL_pre-northrevert_2026-07-26.rds`; live output and the
  pre-benchmark facet map regenerated to match.

---

*Prepared for internal review and for sharing with MC. Questions on any specific line range
above should reference the file and line numbers cited, which correspond to the versions of
each script as reviewed on 2026-07-26.*
