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

### Step 6: V-Dem coder-level weights (no script to run)
**Folder:** `06_vdem_data/`  
**Key file:** `06_vdem_data/coder-level/MC/ELCLweights_wide.dta`  
This file contains per-country-year V-Dem coder weights (proportions) for each predictor criterion. It is the product of MC's weighting script (`weighting_summer2025.R`). **This file is read as-is by Step 7; there is no script to run in this folder.**

Scripts in `06_vdem_data/misc/` (`vdem_extract.R`, `vdem_extract_v2.R`) are exploratory/diagnostic only and do not feed the canonical pipeline.

Patrick's revised coder extraction script (`vdem_coder_extract.R`) is at `06_vdem_data/coder-level/vdem_coder_extract.R`. It produces `ELCLweights_wide-v2.dta` — **but see Pending Items below before using it.**

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

**CDF normalization (fix applied this revision):** Before averaging, each component is normalized to [0,1] within each year using the empirical CDF:
```r
EL_col_cdf  = ecdf(EL_col_mt)(EL_col_mt)   # per year
CL_col_cdf  = ecdf(CL_col_mt)(CL_col_mt)   # per year
sndem_final = 0.5 × (EL_col_cdf + CL_col_cdf)
```
This preserves within-year ordinal rankings while placing both dimensions on a common scale. Final output: `sndem_final` ranges from 0.001 to 1.000, mean = 0.500, zero NAs.

---

## Bugs Fixed

### Bug 1: Scale mismatch in `sndem_final`
**Where:** `08_benchmark/01_benchmark.R`  
**Problem:** `EL_col_mt` (V-Dem electoral scale, −4 to 4) and `CL_col_mt` (V-Dem civil liberties scale, 0–1) were averaged directly. This produced `sndem_final` values from −0.016 to 1.538.  
**Fix:** CDF normalization of both components within each year before averaging. `sndem_final` now correctly bounded in [0, 1].

### Bug 2: Denominator error in V-Dem coder weights (pending confirmation)
**Where:** MC's `weighting_summer2025.R` (original weight-generation script)  
**Problem:** For criteria 2 (Less development), 3 (More development), and 4 (Inside capital), the denominator in the proportion calculation used the count of criterion-1 coders (`v2elsnlfc_1_0`) instead of the criterion-specific count. This affects both electoral and civil liberties weights.  
**Fix drafted:** Patrick's `vdem_coder_extract.R` corrects this and generates `ELCLweights_wide-v2.dta`. However, a comparison revealed that the v2 file stores `weight_diff` (more − less, can be negative), while the v1 file stores raw proportions (always positive). These are different quantities — the v2 file is **not a drop-in replacement** for the v1 file without changes to the weighting step.  
**Status: Waiting on MC.** See Pending Items.

---

## Final Output Diagnostics

`09_final_snvdem_data/snvdem_col_final.rds` — 27,000 rows (1,125 municipalities × 24 years)

| Variable | Min | Median | Mean | Max | NAs |
|---|---|---|---|---|---|
| `EL_col_mt` (raw) | −0.373 | 1.265 | 1.120 | 2.459 | 0 |
| `CL_col_mt` (raw) | 0.339 | 0.515 | 0.523 | 0.695 | 0 |
| `EL_col_cdf` | 0.001 | 0.500 | 0.500 | 1.000 | 0 |
| `CL_col_cdf` | 0.001 | 0.500 | 0.500 | 1.000 | 0 |
| `sndem_final` | 0.001 | 0.500 | 0.500 | 1.000 | 0 |

---

## Pending Items

1. **ELCLweights_wide-v2.dta (wait for MC)**  
   MC needs to share his original script that generated `ELCLweights_wide.dta` (or confirm what quantity each column stores — raw proportions vs. weight differences). Once confirmed, either validate the v2 file or revise `vdem_coder_extract.R` to produce a true drop-in replacement. Then update `07_weighting/01_weight_predictors.R` to use the corrected weights.

2. **Run analysis scripts**  
   Scripts in `snvdem-col/scripts/` (validation, visualization, DiD, PanelMatch) have not been re-run against the revised `snvdem_col_final.rds`. Results from prior runs reflect the uncorrected pipeline.

3. **Check: CDF normalization design choice**  
   The within-year CDF normalization means `sndem_final` reflects relative rankings within each year, not absolute levels. Confirm with MC that this is the intended interpretation for cross-municipal comparison (vs. cross-temporal comparison).

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
