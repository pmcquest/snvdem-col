# SNVDEM-COL Pipeline Revision — Handoff Document
**Date:** June 2026  
**Author:** Patrick McQuestion  
**Co-author:** Michael Coppedge  

---

## Overview

This document records the work completed during the June 2026 revision of the SNVDEM-COL data pipeline. The pipeline measures subnational democracy across 1,125 Colombian municipalities for the years 2000–2023. The revision reorganized the folder structure, confirmed the canonical pipeline, fixed two substantive bugs, and ran the pipeline end-to-end to produce a final index file.

---

## Folder Structure (after revision)

```
snvdem-col/
├── data/
│   └── panel/
│       ├── 01_raw/
│       ├── 02_cleaned/
│       ├── 03_imputed/
│       ├── 04_geocoded/
│       ├── 05_geocoded_panel/          ← CDF averages of geocoded predictors
│       ├── 06_vdem_data/               ← V-Dem coder-level data and weights
│       ├── 07_weighting/               ← Asymmetric weighting of predictors
│       ├── 08_benchmark/               ← Benchmarking to V-Dem national scale
│       └── 09_final_snvdem_data/       ← Final index outputs
├── scripts/                            ← Analysis and visualization (reorganized)
│   ├── Validation/
│   ├── visualization/
│   │   ├── Maps/
│   │   ├── ZoomIn/
│   │   └── Dimensions/
│   └── Exploratory/
│       ├── Outcomes/
│       ├── DiD/
│       └── PanelMatch/
└── HANDOFF_pipeline_revision_june2026.md  ← this file
```

---

## The Canonical Pipeline

The pipeline runs in four steps. Run the numbered scripts in order; each step's output feeds the next.

### Step 1–4: Data wrangling, cleaning, imputation, geocoding (Folders 01–04)
Pre-existing. Not modified in this revision. Produces cleaned, imputed, geocoded municipal-level data.

### Step 5: CDF averages of geocoded predictors
**Script:** `05_geocoded_panel/CDF_averages.R`  
**Output:** `05_geocoded_panel/CDF_averages.rds`  
**Key detail:** Geocoded directional predictors (North, South, West, East) are stored as **separate columns** (`avg6`, `avg7`, `avg8`, `avg9`) — not averaged together. These are used independently in the weighting step.

### Step 6: V-Dem coder-level weights
**Script:** `06_vdem_data/01_weighting.R` (as of 2026-07-02)  
**Key file:** `06_vdem_data/coder-level/MC/ELCLweights_wide.dta`  
This file contains per-country-year V-Dem coder weights (proportions) for each predictor criterion, read as-is by Step 7. `01_weighting.R` is a streamlined rewrite of MC's `weighting_summer2025.R` / `weighting_January2026_0726.R` (collapses ~90 near-identical blocks into one loop-based function; same calculations). **See "Update — July 2, 2026" below: two bugs in the original scripts were found, confirmed, and fixed; the live `ELCLweights_wide.dta` was regenerated.**

Scripts in `06_vdem_data/misc/` (`vdem_extract.R`, `vdem_extract_v2.R`) are exploratory/diagnostic only and do not feed the canonical pipeline. `vdem_coder_extract.R` / `ELCLweights_wide-v2.dta` are superseded by `01_weighting.R`.

### Step 7: Asymmetric weighting
**Script:** `07_weighting/01_weight_predictors.R`  
**Inputs:** `05_geocoded_panel/CDF_averages.rds`, `06_vdem_data/coder-level/MC/ELCLweights_wide.dta`  
**Output:** `09_final_snvdem_data/snvdem_col_weighted.rds`  
**Key detail:** One NA in `cl_Less_development` (filled with the observed mean, 0.2443505) is handled inline. Produces `snelect`, `sncivlib`, and `sndem` for 27,000 municipality-year rows (1,125 × 24 years). All values confirmed in 0.10–0.57 range, zero NAs.

The asymmetric weighting formula:
```
snelect = Σ(predictor_k × weight_k) / Σ(weight_k)
```
where predictors above the country median (0.5) use the "hi" weight and those below use the "lo" weight. Denominators vary by year.

### Step 8: Benchmarking to V-Dem national scale
**Script:** `08_benchmark/01_benchmark.R`  
**Inputs:** `09_final_snvdem_data/snvdem_col_weighted.rds`, `08_benchmark/snlsffHPD.dta`, `08_benchmark/SNHPD.dta`  
**Output:** `09_final_snvdem_data/snvdem_col_final.rds`

Benchmarking formulas:
```
EL_col_mt  = v2elffelr + (snelect - country_mean_snelect) × weighted_range / ELrange_975_025
CL_col_mt  = CLSNmean  + (sncivlib - CLSNyrmean) × wtdCL_range / CLrange_975_025
```
`EL_col_mt` and `CL_col_mt` are raw benchmarked scores on different V-Dem scales (electoral: roughly −4 to 4; civil liberties: 0–1). These are retained in the output for inspection.

**Combination method (as of 2026-07-02, corrected same day — see "Update" section below):** an `ecdf()`-based normalization and later a cross-panel linear normalization were both tried and superseded — both distorted or flattened the temporal trend (see `08_benchmark/memo/`). MC's reply (email 2026-07-02) suggested putting both dimensions on the same latent (Z-score) scale via `qnorm()` on `CL_col_mt`, then averaging.

A first implementation used `scale()` (Colombia's own 2000-2023 panel mean/sd) on both dimensions to force mean 0/sd 1 before averaging. **This was wrong and was corrected the same day** (caught in PM's review): `scale()` is ipsative — it centers Colombia to *Colombia's own* distribution, so a future country run through this pipeline would be centered to *its own* distribution, and the two countries' "z-scores" would not be on the same axis despite both nominally having mean 0/sd 1. This also silently assumed `EL_col_mt` (`v2elffelr`) was "already latent/Z-score-like" (i.e. globally mean≈0/sd≈1) — checked against the full V-Dem panel (2000-2023, all countries) and that's false: global `v2elffelr` has mean 0.789, sd 1.593, not (0,1).

**Current (correct) method:** standardize both dimensions using FIXED constants computed once from the full V-Dem country-year panel (2000-2023, all countries) — not from Colombia's own data — so the same constants apply to any country this pipeline is later extended to:
```r
v15_global <- vdemdata::vdem %>% filter(year >= 2000, year <= 2023)
EL_global_mean <- mean(v15_global$v2elffelr, na.rm = TRUE)   # 0.789
EL_global_sd   <- sd(v15_global$v2elffelr,   na.rm = TRUE)   # 1.593
CLz_global     <- qnorm(v15_global$v2x_civlib[v15_global$v2x_civlib > 0 & v15_global$v2x_civlib < 1])
CL_global_mean <- mean(CLz_global, na.rm = TRUE)             # 0.602
CL_global_sd   <- sd(CLz_global,   na.rm = TRUE)             # 0.843

CL_col_z    = qnorm(CL_col_mt)
EL_col_gz   = (EL_col_mt - EL_global_mean) / EL_global_sd
CL_col_gz   = (CL_col_z  - CL_global_mean) / CL_global_sd
sndem_final = 0.5 × (EL_col_gz + CL_col_gz)
```
Result: `sndem_final` for Colombia now ranges roughly −0.96 to 0.33 (not an artificially stretched −2.65 to 1.83) — correctly reflecting that Colombia occupies only ~22% of the global range on both dimensions, and preserving 0 = "full V-Dem 2000-2023 country-year average" as a globally meaningful reference point. Validated with `08_benchmark/02_validate_map.R` (single fixed-color-scale facet map, all 24 years — clean gradual improvement 2000→2018, plausible dip 2019-2021, partial recovery by 2023) and `08_benchmark/03_trend_diagnostics.R` (municipal-level means track the raw national anchors almost exactly, as expected from a benchmarking step).

---

## Bugs Fixed

### Bug 1: Scale mismatch in `sndem_final`
**Where:** `08_benchmark/01_benchmark.R`  
**Problem:** `EL_col_mt` (V-Dem electoral scale, −4 to 4) and `CL_col_mt` (V-Dem civil liberties scale, 0–1) were averaged directly. This produced `sndem_final` values from −0.016 to 1.538.  
**Fix history:** an `ecdf()`-within-year fix was tried first (see below, now superseded), then a cross-panel linear normalization, then a `qnorm()` + Colombia-only `scale()` combination (also superseded — ipsative, not comparable across countries), then (2026-07-02, current) `qnorm()` + standardization against fixed global V-Dem constants. See "Update — July 2, 2026" and the Step 8 description above for the current method and why the earlier ones were abandoned (documented in `08_benchmark/memo/benchmarking_memo.md`).

### Bug 2: Denominator error + column-swap in V-Dem coder weights (fixed 2026-07-02)
**Where:** MC's `weighting_summer2025.R` / `weighting_January2026_0726.R` (original weight-generation scripts)  
**Problem 1:** For criteria 2 (Less development), 3 (More development), and 4 (Inside capital), the denominator in the proportion calculation used the count of criterion-1 coders (`v2elsnlfc_1_0`) instead of the criterion-specific count.  
**Problem 2 (found 2026-07-02):** `pr_5`/`pr_6` ("Outside capital"/"North") were computed out of declaration order, which combined with sequential `colnames()` labeling swapped their weights.  
**Fix:** `06_vdem_data/01_weighting.R` (rewrite) fixes both, verified to reproduce the original buggy output bit-for-bit with a `FIX_KNOWN_BUGS` toggle before being trusted. MC notified by email 2026-07-02; PM confirmed to proceed. Live `coder-level/MC/ELCLweights_wide.dta` regenerated with the fix; original preserved as `ELCLweights_wide_ORIGINAL_pre-fix_2026-07-02.dta`. Full writeup: `06_vdem_data/weighting_bug_log_2026-07-02.md`. `07_weighting/01_weight_predictors.R` and `08_benchmark/01_benchmark.R` re-run against the corrected weights.

---

## Final Output Diagnostics

`09_final_snvdem_data/snvdem_col_final.rds` — 27,000 rows (1,125 municipalities × 24 years)

**Superseded** — table reflected the `ecdf()`-normalized version of `sndem_final`. Current diagnostics (2026-07-02, corrected weights + `qnorm()` + global-standardized combination) are in the "Update — July 2, 2026" section below.

---

## Pending Items

1. ~~ELCLweights_wide-v2.dta (wait for MC)~~ **Resolved 2026-07-02** — see Bug 2 above and the Update section below.

2. **Run analysis scripts**  
   Scripts in `snvdem-col/scripts/` (validation, visualization, DiD, PanelMatch) have not been re-run against the revised `snvdem_col_final.rds`. Results from prior runs reflect the pre-2026-07-02 pipeline (both the old weights and the old ecdf/linear benchmarking).

3. ~~Check: CDF normalization design choice~~ **Resolved 2026-07-02** — confirmed with MC by email: benchmarked values are the intended final measure for all uses (including within-country maps), and the combination method is now `qnorm()` + `scale()` rather than within-year CDF. See Step 8 above.

---

## Update — July 2, 2026

**Weighting script errors found and fixed (see Bug 2 above).** MC sent `weighting_January2026_0726.R` (the civil-liberties completion of `weighting_summer2025.R`, saved to `06_vdem_data/`). Review surfaced the denominator bug already flagged as "pending" in this doc, plus a new column-swap bug. Rewrote as `06_vdem_data/01_weighting.R` (loop-based, ~150 lines vs. ~1700 across the two originals combined), verified bit-for-bit against the live weights file before trusting it, then regenerated `ELCLweights_wide.dta` with both bugs fixed. Full writeup in `06_vdem_data/weighting_bug_log_2026-07-02.md`. MC notified by email; PM confirmed to proceed with the fix rather than wait for a reply.

**Benchmarking combination resolved.** PM's memo to MC (`08_benchmark/memo/`) laid out the scale-mismatch problem between `EL_col_mt` (latent) and `CL_col_mt` ([0,1]) and the four failed combination approaches tried. MC's reply (2026-07-02): benchmarked values are the intended final measure for all uses; convert `CL_col_mt` to a Z-score with `qnorm()` before combining. First implementation added a `scale()` step (Colombia's own mean/sd) on both dimensions to balance their weight in the average — **caught in PM's review as a mistake**: it's ipsative (ties Colombia to its own distribution, not comparable to other countries) and rested on an unverified/false assumption that `EL_col_mt` was already globally mean≈0/sd≈1. Corrected same day to standardize against fixed global V-Dem constants (full panel, 2000-2023, all countries) instead — see Step 8 description above for the exact formula and reasoning. Validated in `08_benchmark/02_validate_map.R` (single fixed-color-scale facet map, all 24 years — clean gradual improvement 2000→2018, plausible dip 2019-2021, partial recovery by 2023) and `08_benchmark/03_trend_diagnostics.R` (municipal means vs. raw national anchors, and global- vs. Colombia-only standardization compared directly).

**Full pipeline re-run** with corrected weights + new benchmarking: `07_weighting/01_weight_predictors.R` → `08_benchmark/01_benchmark.R` → `snvdem_col_final.rds`. Downstream analysis scripts in `snvdem-col/scripts/` still need re-running (Pending Item 2).

**RESOLVED — `weighted_range`/`wtdCL_range` sign error, found and fixed same day.** While investigating why benchmarked and unbenchmarked maps showed opposite patterns for some municipality groupings, found that `weighted_range` and `wtdCL_range` (national constants from `06_vdem_data/01_weighting.R`, MC's original formula, unchanged since `weighting_summer2025.R`) were **negative in all 24 years (2000-2023)**, when they're meant to represent a positive magnitude of within-country variation — this inverted the within-year correlation between unbenchmarked (`snelect`/`sncivlib`) and benchmarked (`EL_col_mt`/`CL_col_mt`) municipal scores to exactly r = -1.0000 in every year, both dimensions. Root cause: `((2-snlsff_1)*HPD + (2-snlsff_0)*HPD*2)/(snlsff_0+snlsff_1+snlsff_2)` assumed `snlsff_0`/`snlsff_1` were proportions bounded to at most 2, but they're raw V-Dem coder counts (commonly 5+ coders, so counts routinely exceed 2). Rewrote as a coder-proportion-weighted average (`(snlsff_1*HPD + snlsff_2*2*HPD)/(snlsff_0+snlsff_1+snlsff_2)`), grounded in the operational strategy document's own description of the intended design ("% of coders * range of variation"). Re-ran the full pipeline (`06_vdem_data/01_weighting.R` → `07_weighting/01_weight_predictors.R` → `08_benchmark/01_benchmark.R`); rank correlation is now exactly +1.0000 in every year, both dimensions (benchmarking now preserves spatial ranking, as intended, while still repositioning to the true national anchor). Did not affect the national year-over-year trend (anchored directly to `v2elffelr`/`CLSNmean`), which was correct throughout. Maps and Memo 2 regenerated with corrected data.

**Files added:**
- `06_vdem_data/01_weighting.R` — streamlined weighting script (canonical, replaces the two MC originals for pipeline purposes; originals kept for reference)
- `06_vdem_data/weighting_bug_log_2026-07-02.md` — bug writeup for MC's record
- `08_benchmark/02_validate_map.R` — validation maps for the benchmarking combination
- `08_benchmark/03_trend_diagnostics.R` — municipal-mean-vs-national-anchor and global-vs-ipsative standardization comparison
- `08_benchmark/sndem_final_facet_allyears.png`, `08_benchmark/sndem_dims_milestone_years.png`, `08_benchmark/trend_vs_national_anchors.png`
- `08_benchmark/diagnostics/` (`04_spatial_rank_check.R`, `weighted_range_negative_by_year.png`, `rank_inversion_scatter_2023.png`, `global_vs_ipsative_standardization.png`, `cl_qnorm_sensitivity.png`, `README.md`) — sign-error investigation and fix verification (internal record, not part of the shared memos)
- `08_benchmark/memo/benchmarking_memo2_2026-07-02.md` (+ .pdf/.html) — Memo 2, the direct follow-up to MC's email; presents both measures (unbenchmarked and benchmarked) with corrected data, for MC and the team

**Two more issues found and fixed the same day, while preparing Memo 2's figures.**

1. **CL global reference asymmetry.** `CL_global_mean`/`CL_global_sd` (used to standardize `CL_col_gz`) were built from raw, unadjusted `v2x_civlib`, while Colombia's municipalities are anchored to `CLSNmean` (the same index discounted by `v2clsnlpct`, the % of population under weaker-than-national civil liberties). Since `v2clsnlpct > 0` for virtually every country (global mean ~37%), this mismatch alone pulled Colombia's `CL_col_gz` down by ~0.94 global SD, independent of any real difference in civil liberties -- enough to flip the substantive story from "Colombia's civil liberties lag badly" to "roughly typical." Fixed in `08_benchmark/01_benchmark.R` by building the global reference the same `v2clsnlpct`-discounted way as the municipal anchor. `CL_col_gz` mean moved from -0.81 to +0.12 global SD.
2. **Stale reference in `03_trend_diagnostics.R`.** This script independently recomputes the global reference constants (documented as "do not hardcode") but was not updated when fix #1 above was made, so its civil-liberties validation plot compared `CL_col_gz` (built with the corrected reference) against a national-anchor line still standardized with the old, unadjusted reference -- producing a large, misleading gap between the two lines. Fixed to match `01_benchmark.R` exactly; the residual gap is now small, matching the genuine Jensen's-inequality effect expected from applying `qnorm()` municipality-by-municipality before averaging vs. once to the national aggregate.

Also switched `08_benchmark/02_validate_map.R`'s maps from an auto-scaled sequential palette (`scale_fill_viridis_c`, limits = Colombia's own observed min/max) to a diverging palette with **fixed** limits (+/-1 global SD, white = global average). The auto-scaled version always rendered Colombia's lowest observed year as maximally dark regardless of true magnitude (2000-2003 are only ~0.3-0.4 SD below average, not extreme) -- visually overstating how far Colombia sits from the global average. All figures and Memo 2 regenerated with both fixes and the new color scale.

---

## Git / GitHub Sync (June 26, 2026)

The local repository had 4 commits that had never been pushed to GitHub (`pmcquest/snvdem24`). Pushing was blocked because five files in the commit history exceeded GitHub's 100 MB hard limit:

| File | Size |
|---|---|
| `data/geospatial/2018pmq/Col_Wrangle.pdf` | 119 MB |
| `data/geospatial/2018pmq/12_SparsePop/PERSONAS_DEMOGRAFICO_Cuadros_CNPV_2018.xlsx` | 140 MB |
| `data/panel/15-16_RulingParty/source_data/MOE/elecciones_municipios.csv` | 130 MB |
| `data/panel/CEDE_PM/2020/Microdatos/PANEL_DE_EDUCACION(2019).dta` | 116 MB |
| `data/panel/CEDE_PM/2020/Microdatos/PANEL_AGRICULTURA_Y_TIERRA(2020).dta` | 101 MB |

These files were removed from the entire git history using `git filter-repo --invert-paths`, which rewrote all commit hashes. The rewritten history was then force-pushed (`git push --force`) to GitHub. The files still exist on disk but are no longer tracked by git.

**Note:** Several other files between 50–100 MB remain in the repo (GitHub warns but does not block these). If future pushes are blocked by new large files, the same `git filter-repo` approach applies.

---

## Future Tasks

- [ ] **Pipeline manual and codebook**: Create a comprehensive manual describing the entire `snvdem-col/` folder — folder purposes, file inventories, variable definitions, and a step-by-step walkthrough of the 01–09 panel pipeline. Should include: (a) a codebook for all variables in `snvdem_col_final.rds`; (b) descriptions of each input dataset and where it comes from; (c) documentation of methodological choices (asymmetric weighting, CDF normalization, benchmarking formulas); (d) a data provenance section tracing each output back to its source files.

- [ ] Resolve Bug 2 (denominator fix) once MC provides his original script.

- [ ] Re-run and update all visualization and analysis scripts against revised final index.

- [ ] Validate index against external benchmarks (see `scripts/Validation/`).
