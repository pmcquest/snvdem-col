# SNVDEM-COL Panel Pipeline

This is the data pipeline behind Colombia's subnational democracy index (SNVDEM), a panel dataset capturing municipal-level data for 1,122 municipalities for years 2000-2023.
It runs in seven numbered stages, folder `0N` = Step N. Steps 1 and 2 are each
further split into numbered subfolders mirroring their own internal flow (see
below). Each stage (with rare noted exceptions) has one canonical script that
reads the previous stage's output and writes its own. Run them in order, 01
through 07, to reproduce the final index from raw source data.

For the detailed history of recent bug fixes and methodology decisions, see
`claude-md/HANDOFF_pipeline_revision_june2026.md` (moved here 2026-07-07 from
the repo root, alongside `NEXT_SESSION_2026-07-03.md` and this folder's own
`HANDOFF_2026-07-03.md`/`HANDOFF_2026-07-04.md`) and the memos in
`06_benchmark/04_memo/`. This file describes what the pipeline does today, not how
it got here. Folders were last renumbered 2026-07-03 (from an earlier 01-09
scheme) to read more like the actual conceptual flow: wrangle raw data → impute
→ build the working panel → V-Dem weighting → benchmark. `06_benchmark/` and
`07_final_snvdem_data/` (now `07_snvdem-col_diagnostics/`) were further
restructured 2026-07-06 -- see Steps 6 and 7 below.

---

## Pipeline at a glance

| Step | Folder | Canonical script(s) | Produces |
|---|---|---|---|
| 1 | `01_empirical_data/` (5 numbered subfolders, see below) | `df01...df07-*.R`, `01_merge_empirical.R`, `Mun-Year.R` | `df01_clean.rds`...`df07_clean.rds`, `df_col_clean.rds`, `MunYrs.rds` |
| 2 | `02_imputation/` (4 numbered subfolders, see below) | 8 per-variable imputation scripts + `01_merge_imputed.R` | `impStatic.rds` ... `imp13.rds`, `imputed_master_panel.rds`, `imputed_cdf_panel.rds` |
| 3 | `03_geocoded_panel/01_clean_geocoded/` | `01_clean_geocode.R` | `CDF_averages.rds` |
| 4 | `04_vdem_data/` (4 numbered subfolders, see below) | `02_vdem_weighting/02_vdem_weighting.R` | `04_vdem_data/03_outputs/{ELCLweights_wide.dta, SNHPD.dta, snlsffHPD.dta}` |
| 5 | `05_weighting/` (3 numbered subfolders, see below) | `01_weighting_geopredictors/01_weighting_geopredictors.R` | `05_weighting/03_output/snvdem_col_weighted.rds` (`snelect`, `sncivlib`, `sndem`) |
| 6 | `06_benchmark/` (4 numbered subfolders, see below) | `01_benchmark/01_benchmark.R` (+ `02_diagnostics/01_scripts/{01_validate_map.R, 02_trend_diagnostics.R, 03_spatial_rank_check.R}`) | `06_benchmark/03_output/snvdem_col_benchmarked.rds` (`sndem_final`) |
| 7 | `07_snvdem-col_diagnostics/` | *(no canonical script -- comparison workspace, see note below)* | Legacy final outputs kept for comparison; consumed by downstream analysis in `../scripts/` |

---

## Step 1 -- `01_empirical_data/`

Wrangles raw source data (Censos, DANE, HRDAG, RNEC, OpenStreetMap, etc.) into
per-criterion cleaned data frames, and separately builds the municipality-year
skeleton every later stage joins onto. Merged 2026-07-03 from what were
previously two separate top-level folders (`01_raw_data/`, `02_cleaned_data/`),
then split into 5 numbered subfolders mirroring its own internal wrangle -> clean
-> merge flow:

| Subfolder | Contents |
|---|---|
| `01_source_files/` | `Mun-Year.R` and its output `MunYrs.rds` (the municipality-year skeleton), plus `source_files/` (raw inputs by data source) and `datasources.png`. |
| `02_cleaning_scripts/` | `df01-01234513-clean.R` ... `df07-13-clean.R`, one cleaning script per criterion group, plus `colvdem0020.R`. `v2/` holds older/superseded versions -- not canonical. |
| `03_clean_outputs/` | `df01_clean.rds` ... `df07_clean.rds` (outputs of the `02_cleaning_scripts/` above) and `codebook_02.gdoc`. |
| `04_merge_empirical/` | `01_merge_empirical.R` -- the canonical final-merge script (written 2026-07-03, synthesizing the two prior candidates, see below), reading `03_clean_outputs/df01_clean.rds`...`df07_clean.rds` and writing `df_col_clean.rds` here. `z1_merge_all_raw.R`/`z1_merge_all_raw2.R` are kept alongside as historical reference, not run. |
| `05_diagnostics/` | Missingness plots and `diagnostic_table.csv` from the merge step, plus an older archived snapshot (`diagnostic_table_archive.csv`) and `imputed-data.png`. |

`Mun-Year.R`'s municipality-year skeleton (`01_source_files/MunYrs.rds`, currently
1122 municipalities, matching DANE's current DIVIPOLA list) is the base every
later join in the pipeline uses. Includes an integrity check (pinned expected
count + diff against the previous save) added 2026-07-03 after a silent
1125-to-1122 drift went undetected for months.

**`04_merge_empirical/01_merge_empirical.R`** (canonical, written 2026-07-03)
builds the master skeleton from `MunYrs.rds` (not re-derived from the raw
datasets), joins panel-varying datasets by municipality+year and the one
genuinely static/snapshot dataset (`df07`) by municipality only, attaches clean
name/department metadata from `MunYrs.rds`, and diagnoses the merged panel
before it heads into Step 2. Synthesizes the two prior candidate scripts:
- From `z1_merge_all_raw.R`: the `HHomix_1011` column rename.
- From `z1_merge_all_raw2.R`: joining panel vs. static datasets separately
  (fixes a real risk in `z1_merge_all_raw.R`'s blind `reduce(full_join)` of all
  7 datasets by year, which could fail to broadcast static values across years).
- New: three municipalities (Puerto Libertador, Santa Rosalía, Cumaribo) appear
  under two different DIVIPOLA codes across the raw cleaned data. Verified
  2026-07-03 that the legacy-code rows (`23685`, `99572`, `99760`) are uniformly
  zero/NA placeholders wherever they overlap with the current-code rows (`23580`,
  `99624`, `99773`) -- dropped, losing no data. `z1_merge_all_raw.R` had instead
  recoded the *current* correct codes onto the legacy ones for the whole merged
  panel, which would have silently dropped these two municipalities from every
  downstream join (all of which key off `MunYrs.rds`'s current codes).
- New: static metadata (name, department) sourced directly from `MunYrs.rds`
  instead of either prior script's own ad hoc reconciliation logic.
- Fixes `z1_merge_all_raw2.R`'s diagnostics, which accidentally ran on its bare
  `MPIO_CDPMP` x `year` skeleton (`df_all`) instead of the actual joined data
  (`df_final`) -- an unfinished-refactor bug in that script, not fixed there.

**Bug found and fixed 2026-07-03 (this script's first version):** `df03`
(criteria 5-9, geographic north/south/east/west position) looks static but
actually already carries one row per `(MPIO_CDPMP, year)` upstream (26,928 rows,
24 identical yearly copies per municipality) -- unlike `df07`, which is
genuinely one row per municipality (1,122 rows). Treating `df03` as static
(inherited from `z1_merge_all_raw2.R`'s assumption, never verified) dropped its
year column and matched every skeleton year against all 24 of its rows per
municipality -- a 24x row explosion (700,128 rows instead of 29,172) that caused
an 8GB out-of-memory failure three stages downstream, in Step 2's merge
sub-stage, before being traced back here. Fixed by joining `df03` on
`(MPIO_CDPMP, year)` like the other panel datasets.

## Step 2 -- `02_imputation/`

One imputation script per variable/criterion group (different missing-data
methods per variable: MI, PMM, LOCF, growth-based interpolation), plus the
merge-into-one-panel sub-stage that used to be a separate top-level step. Split
2026-07-03 into 4 numbered subfolders mirroring its own internal flow:

| Subfolder | Contents |
|---|---|
| `01_imputation_scripts/` | The 9 imputation scripts (below). `v2/` holds older/superseded versions -- not canonical. |
| `02_imputation_outputs/` | The `.rds` output of each script. |
| `03_merge_imputed/` | `01_merge_imputed.R` (renamed from `z2_merge_imputed_data.R` 2026-07-03) -- joins all 9 outputs onto `MunYrs.rds`, validates the municipality count, then CDF-standardizes every predictor. Outputs `imputed_master_panel.rds` (pre-CDF) and `imputed_cdf_panel.rds` (CDF-standardized) here. |
| `04_diagnostics/` | Missingness plots and `diagnostic_table_imputed.csv`, run immediately after the merge, plus `imputation_methodology_memo_2026-07-05.md` -- criterion-by-criterion missingness/method/mechanism writeup, written in response to an MPSA reviewer comment asking how missingness is actually handled. Read this before touching any imputation script. `02_observed_vs_imputed.R` (2026-07-06) builds a correctly-flagged (real pre-imputation `is.na()` status, not a hardcoded/stale list) observed-vs-imputed density plot for every criterion, saved to `01_criteria-png/`; these are embedded inline in the memo. Run after `03_merge_imputed/01_merge_imputed.R`. |

| Script | Method | Output |
|---|---|---|
| `impStatic_LOCF.R` | LOCF | `impStatic.rds` |
| `imp01_RuralMI.R` | Multiple Imputation (PMM) | `imp01.rds` |
| `imp23_EconGrowth_v3b.R` | Growth model + Random Forest | `imp23.rds` |
| `imp23_FiscalCART_v3.R` | CART (via `mice`; renamed 2026-07-06 from `imp23_FiscalPMM_v3.R` -- the method has actually been CART since at least 2026-07-03, filename/header were stale -- see memo) | `imp23b.rds` |
| `imp1011_CrimeMI-PMM_v3.R` | MI + PMM | `imp1011.rds` |
| `imp1011_FAviolence_pmq.R` | Factor analysis (reads `imp1011.rds`) | `imp1011FA.rds` |
| `imp1214_PopGBIv2.R` | Growth-Based Interpolation | `imp1214.rds` |
| `imp1516_ElectionsLOCF.R` | LOCF (forward-only) | `imp1516.rds` |
| `imp13_RoadsLOCF.R` | LOCF | `imp13.rds` |

`01_merge_imputed.R`'s CDF step uses `rank(., na.last = "keep") / length(.)`. The
`na.last = "keep"` (fixed 2026-07-03) is important: the default `rank()`
behavior silently promotes missing values to the *highest* percentile instead of
preserving `NA` -- this project deliberately keeps missingness visible instead.

**Known data quality note:** `PIB_2t3` (GDP) is the most-missing variable after
imputation (1.87%, 504/26,928), concentrated in four of Colombia's smallest/most
remote departments (San Andrés, Amazonas, Guainía, Vaupés -- Vichada's own gap
closed as of the 2026-07-04 data refresh), where DANE doesn't consistently
publish municipal-level GDP. Expected structural gap, not a pipeline bug -- see
`imputation_methodology_memo_2026-07-05.md` for the full missingness-mechanism
discussion (this is not simply "remote municipalities," it's specifically
DANE's administrative non-coverage of a handful of peripheral departments).

**2026-07-05 re-run, post-trash-recovery:** all 9 imputation scripts and the merge
step were re-run against the current `df_col_clean.rds` (see Step 1's
replicability-audit notes above). Found and fixed: (1) `impStatic_LOCF.R`
referenced a `provincia` column dropped by the empirical-merge rewrite, blocking
the entire imputation stage; (2) `imp1516_ElectionsLOCF.R` called `ggplot()`
before its `write_rds()` without loading `ggplot2`, halting before saving; (3)
`imp01_RuralMI.R` and `imp1011_CrimeMI-PMM_v3.R` passed the ~1,122-level
municipality code into `mice()` as a raw predictor, which did not finish in
25+ CPU-minutes for one of the two -- replaced with per-municipality group means
(the trick already used in `imp23_FiscalCART_v3.R`). Full writeup, including a
diff of the merged panel against the previously-committed version, in
`04_diagnostics/imputation_methodology_memo_2026-07-05.md`.

**Environment gaps found and fixed 2026-07-03:** three scripts in
`01_imputation_scripts/` failed on a fresh run for reasons unrelated to the data
itself: `imp1214_PopGBIv2.R` and `imp1011_CrimeMI-PMM_v3.R` call `str_pad()`
without ever loading `stringr` (added); `imp1011_CrimeMI-PMM_v3.R` also needs
the `mice` package and `imp1011_FAviolence_pmq.R` needs `Hmisc`, neither of
which was installed in this R environment (installed via
`install.packages()`). None of these affected the actual imputation logic --
each script's `write_rds()` call happens before the point of failure -- but they
did stop the scripts from completing.

## Step 3 -- `03_geocoded_panel/01_clean_geocoded/`

`01_clean_geocode.R` (renamed from `CDF_averages.R` 2026-07-03) reads Step 2's
CDF'd panel, subsets to the 16 predictor criteria, flips sign where needed so
high values always mean *more* democratic, collapses two redundant pairs, and
writes `CDF_averages.rds` -- one column per criterion, ready to be weighted.

## Step 4 -- `04_vdem_data/`

Reorganized 2026-07-03 into numbered subfolders, mirroring Steps 1-2:

- `01_source_files/V-Dem/` -- raw V-Dem downloads read by the canonical script:
  `codebook.pdf`, `methodology.pdf`, and `Coder-Level-Dataset-v15.rds` (the
  country-year `v15` panel itself comes from the `vdemdata` package, not a file).
- `02_vdem_weighting/` -- the canonical script (`02_vdem_weighting.R`, renamed
  from `01_weighting.R`) plus `MC/`, which holds MC's original scripts and
  non-canonical legacy material (`coder_level_extraction/`, `misc/`, `v1/`,
  `legacy_imgs/`) that predates this pipeline and isn't read by any canonical
  script.
- `03_outputs/` -- this step's live canonical outputs: `ELCLweights_wide.dta`
  (consumed by Step 5), promoted out of `02_vdem_weighting/MC/` on 2026-07-03
  since it's a live output, not legacy material; and, as of 2026-07-06,
  `SNHPD.dta`/`snlsffHPD.dta` too (moved here from `06_benchmark/`, which used
  to be where this script wrote them -- see Step 6).
- `04_images/` -- diagnostic plots written by `02_vdem_weighting.R`
  (`snlsff_colrange.png`, `clx_colrange.png`, `Ridgelfc.png`, `Ridgemoreless.png`).

`02_vdem_weighting.R` computes V-Dem expert coder-derived relevance weights for
elections and civil liberties separately, and the national HPD-based ranges
(`weighted_range`, `wtdCL_range`) used later to scale municipal deviations onto
the V-Dem national scale. Writes `ELCLweights_wide.dta` (consumed by Step 5) and
`SNHPD.dta`/`snlsffHPD.dta` (consumed by Step 6), all three now into this step's
own `03_outputs/`.

**Fixed 2026-07-07:** `SNHPD.dta` and `snlsffHPD.dta` had fallen under the repo's
blanket `*.dta` gitignore rule and were untracked at this path -- the same
silent-swallow failure mode already documented for scripts/memos elsewhere in
this repo's history. Exempted alongside `ELCLweights_wide.dta` and committed,
so all three of this step's canonical outputs are now tracked consistently.

**Note:** `weighted_range`/`wtdCL_range` had the coder-response category weights
inverted relative to the V-Dem codebook until 2026-07-03 (see
`weighting_bug_log_2026-07-03.md`), which systematically understated both
ranges. Fixed the same day, propagated through Step 6's `sndem_final`.

## Step 5 -- `05_weighting/`

Reorganized 2026-07-03 into numbered subfolders:

- `01_weighting_geopredictors/` -- the canonical script
  (`01_weighting_geopredictors.R`, renamed from `01_weight_predictors.R`) plus
  `MC/`, holding MC's original scripts and non-canonical legacy material
  (`misc/`, `legacy_imgs/`, legacy `.dta` outputs). No `01_source_files` here --
  unlike Steps 1 and 4, this step has no raw *external* source files of its
  own; both of its inputs (`ELCLweights_wide.dta` from Step 4,
  `CDF_averages.rds` from Step 3) are already-canonical outputs of earlier
  steps, read cross-folder like every other inter-step dependency in this
  pipeline.
- `02_images/` -- diagnostic plots written by `01_weighting_geopredictors.R`.
  Previously these plots only displayed in an interactive session and were
  silently discarded in batch runs; `ggsave()` calls were added 2026-07-03.
- `03_output/` -- this step's own live canonical output, `snvdem_col_weighted.rds`,
  added 2026-07-06 (moved out of the shared `07_final_snvdem_data` folder; see
  Step 7).

`01_weighting_geopredictors.R` joins Step 3's per-criterion averages to Step 4's
coder-derived weights and combines them into the two dimensions and composite:

```r
snelect  = weighted_avg_narm(predictors, el_weights)  # renormalized weighted mean, with a floor
sncivlib = weighted_avg_narm(predictors, cl_weights)
sndem    = 0.5 * (snelect + sncivlib)
```

This is the **unbenchmarked** measure -- each municipality's position relative to
*other Colombian municipalities*, not anchored to any external reference. Writes
`05_weighting/03_output/snvdem_col_weighted.rds`.

**Resolved 2026-07-03 (previously an open decision):** since the Step 2
`na.last = "keep"` fix, real `NA`s flow into the 16 per-criterion predictor
columns for 682 of ~26,928 municipality-years (60 municipalities, mostly small/
remote departments -- San Andres `88`, Amazonas `91`, Guainia `94`). A plain
`sum()` blanked the whole row whenever *any* one criterion was missing.
Investigated: none of the 682 are missing all 16 criteria, so `01_weighting_geopredictors.R`
now drops missing (predictor, weight) pairs together and renormalizes over what's
left -- but only when at least 50% of the original weight mass survives; below
that, a handful of criteria isn't a reliable basis for a score, so it stays `NA`.
This rescues 634 of the 682 rows; the remaining 48 are concentrated in Santa
Rosalia (`99624`) and Cumaribo (`99773`), which are missing 12 of 16 predictors in
*every* year -- the same two municipalities flagged for legacy DIVIPOLA codes in
the Step 1 empirical-merge fix, suggesting a residual Step 3 geocoding gap still
worth a separate look, not something to paper over by lowering the floor.

## Step 6 -- `06_benchmark/`

Restructured 2026-07-06 into 4 numbered subfolders, consolidating what were
previously two separate `memo/` and `diagnostics/` locations:

| Subfolder | Contents |
|---|---|
| `01_benchmark/` | The canonical script (`01_benchmark.R`) plus `MC/`, holding MC's original benchmarking script -- non-canonical legacy material. |
| `02_diagnostics/` | `01_scripts/` -- the three live validation scripts (`01_validate_map.R`, `02_trend_diagnostics.R`, `03_spatial_rank_check.R`, the last renamed from `04_spatial_rank_check.R`); `02_outputs/` -- every PNG they produce, live or historical; `README.md` -- internal record of the diagnostics history, not part of the shared memos. |
| `03_output/` | This step's own live canonical output, `snvdem_col_benchmarked.rds` (renamed from `snvdem_col_final.rds`), moved out of the shared `07_final_snvdem_data` folder; see Step 7. |
| `04_memo/` | The current memos (`benchmarking_memo.md` = Memo 1, `benchmarking_memo2_2026-07-06.md` = Memo 2) plus `prior_versions/` for superseded drafts (the 2026-07-02 memo 2/3 iterations). |

`01_benchmark.R` maps each municipality's unbenchmarked score onto the V-Dem
national scale (deviation from `v2elffelr`/`CLSNmean`, scaled by
`weighted_range`/`wtdCL_range`), converts civil liberties to a Z-score via
`qnorm()`, and standardizes both dimensions against fixed global constants (full
V-Dem 2000-2023 panel, all countries) into `sndem_final`. This is the
**benchmarked** measure -- anchored to Colombia's real V-Dem trajectory and
comparable across countries. Writes `06_benchmark/03_output/snvdem_col_benchmarked.rds`.

Supporting scripts (all read `snvdem_col_benchmarked.rds`), now under
`02_diagnostics/01_scripts/`:
- `01_validate_map.R` -- faceted map across all 24 years, fixed color scale.
- `02_trend_diagnostics.R` -- validates municipal means track the raw V-Dem
  national anchors.
- `03_spatial_rank_check.R` -- regression check that benchmarking
  preserves (doesn't invert) spatial rank; kept after a real sign-error bug was
  found and fixed here 2026-07-02.

See `06_benchmark/04_memo/benchmarking_memo2_2026-07-06.md` for the full
methodology writeup, validation, and guidance on which measure (unbenchmarked vs.
benchmarked) to use for a given analytical question -- this supersedes the
2026-07-02 draft (now in `04_memo/prior_versions/`), and correctly cites the
current post-2026-07-06 folder structure throughout.

## Step 7 -- `07_snvdem-col_diagnostics/`

Renamed from `07_final_snvdem_data/` 2026-07-06. **No longer a storage
location** -- Steps 5 and 6 now write their live canonical outputs directly
into their own `03_output/` folders (see above), so this folder holds only:
legacy copies of `snvdem_col_weighted.rds`/`snvdem_col_final.rds` (plus their
`_ORIGINAL_pre-{narmfix,rangefix}_2026-07-03` backups) kept for
comparison against the current per-step outputs; `imgs/`, `v1/` -- older
visualization scripts and plots (`imgs/visuals-snvdem.R` is downstream
analysis, not a pipeline processing step); `MC/` -- MC's original
visualization script (`visuals-snvdemMC.R`) and its plots, using an older,
differently-schema'd version (`emel_index`/`cscw_index`/`sndem_index` instead
of `snelect`/`sncivlib`/`sndem`), stale, not part of the current pipeline; and
`prior/` -- two superseded final-assembly scripts
(`z3_assemble_final_data_v1.R`, `v2.R`), kept for historical reference, not run.
**This folder has no canonical "Step 7" script of its own** and never did.

---

## Running the full pipeline

Each script uses hardcoded absolute paths (`G:/Shared drives/snvdem/...`), so they
can be run individually and in any R session, in this order:

1. `01_empirical_data/01_source_files/Mun-Year.R` → `02_cleaning_scripts/df0*-clean.R` (any order) → `04_merge_empirical/01_merge_empirical.R`
2. `02_imputation/01_imputation_scripts/imp*.R` (any order, `imp1011_FAviolence_pmq.R` last -- depends on `imp1011_CrimeMI-PMM_v3.R`'s output) → `03_merge_imputed/01_merge_imputed.R`
3. `03_geocoded_panel/01_clean_geocoded/01_clean_geocode.R`
4. `04_vdem_data/02_vdem_weighting/02_vdem_weighting.R`
5. `05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R`
6. `06_benchmark/01_benchmark/01_benchmark.R`

## Non-canonical subfolders

Every pipeline folder may contain `v1/`, `v2/`, `MC/`, or `misc/` subfolders --
these hold superseded drafts or exploratory work by other team members, not part
of the canonical run. They're intentionally left with old path references when
folders get renamed/restructured.

## Open questions

- Whether Step 7 should get a thin canonical script of its own, or stay
  script-less with `visuals-snvdem.R` reclassified as pure downstream analysis.
- `06_benchmark/04_memo/prior_versions/` file citations still reference the old
  01-09 folder numbering -- left as-is since they're superseded drafts, not the
  current memo.
- The Santa Rosalia (`99624`)/Cumaribo (`99773`) Step 3 geocoding gap (see Step 5
  above) -- not investigated yet.

## Replicability audit (2026-07-04, post-trash-recovery)

Systematic check of every canonical script's file I/O against what actually
exists on disk, prompted by discovering `15-16_wrangle-ts.R` couldn't find its
input CSV. Steps 2-6 checked out clean. Step 1 had real problems, now split
into two categories:

**Fixed (pure path bugs, mechanical):** `01_source_files/source_files/15-16_RulingParty/
15-16_wrangle-ts.R`, `.../10-11_HRDAG/extractHRDAGv2.R`, and `.../10-11_Osorio/
ViPAA-Col/ViPAA-analysis.R` all hardcoded paths from one or two folder-renames
ago (`01_raw_data/source_files/...` or the even older `data_raw/...`) instead
of the current `01_empirical_data/01_source_files/source_files/...`. Rewritten
2026-07-04; also fixed a bare relative path and a `ViPAA`/`VIPAA` filename-case
mismatch in `ViPAA-analysis.R`. These three scripts feed `df06-1516-clean.R`
and `df04-1011-clean_v3.R` respectively -- they need to actually be *run* now
(they haven't been executed since the fix) to produce the `.rds`/`.csv` outputs
those two cleaning scripts expect.

**Resolved since (2026-07-04):**

- `a1-Censos/{2005TerriData_Dim25_Sub4_poburb.xlsx, 2018TerriData_Dim2_dem.xlsx}`
  and `14_Indigenous/TerriData_Dim25_Sub5_pobetn.xlsx` -- re-sourced directly
  from DNP's TerriData portal (`terridata.dnp.gov.co`), which exposes a plain
  REST endpoint (`/tdtservice/FITService.svc/rest/busqueda/dimensiones`) behind
  the JS app, serving full-dimension bulk exports. Downloaded Dim2/Dim25 bulk
  files and filtered each to the exact `Indicador` values these scripts need
  (urban/rural population; ethnic population). `df01-01234513-clean.R` was
  also fixed to read `2018TerriData_Dim2_dem.xlsx` from `a1-Censos/` instead of
  a stray `Terridata/` reference (inconsistent with its own `2005...` sibling
  read two lines above).
- `2-3_EconDevt/IDF/*.xlsx` (5 files) -- manually re-sourced by the user from
  DNP's fiscal-performance pages; `df02-23-clean.R` now resolves cleanly.
- `10-11_HRDAG` municipal-year panel (`td_HRDAG_ym.csv`) -- HRDAG's `verdata`
  replicate files are **versioned** (v1 = original JEP-CEV-HRDAG project data;
  v2 = for independent new analyses); `extractHRDAGv2.R` was written against
  v2, but v2 isn't reachable anywhere we could find (see to-do below). DANE's
  microdata catalog (`microdatos.dane.gov.co`, study 795) turned out to host
  the **v1** replicates instead, openly downloadable (no login/captcha gate on
  the actual file URLs, just a decorative one on the page). Used the sibling,
  previously-dormant `extractHRDAG.R` (written against v1) instead of
  `extractHRDAGv2.R` -- fixed its stale `data_raw/10-11_HRDAG` paths and
  removed two dead-end `combine_replicates()` attempts the original author had
  already flagged as broken (comment: "it's not working for me"; they renamed
  `replicas_secu`'s strata columns before use elsewhere, and referenced a
  `verdata-examples` aggregate lacking the needed columns). Ran clean after
  those fixes; output lives at both `10-11_HRDAG/v1/td_HRDAG_ym.csv` (script's
  own path) and `10-11_HRDAG/td_HRDAG_ym.csv` (copy, matching what
  `df04-1011-clean_v3.R` actually reads). **`extractHRDAGv2.R` itself was left
  untouched** (still targets v2) so it's ready to run as-is if/when the v2
  replicates become reachable -- see to-do.
- `colvdem0020.R` -- confirmed non-canonical (referenced a pre-`01_raw_data`
  folder scheme entirely); moved to `02_cleaning_scripts/prior_versions/`.

**To-do:**

- **HRDAG v2 replicates.** The package's own GitHub README links an IPFS
  distribution for v2 (one shared directory CID; per-violation files at
  `https://<CID>.ipfs.w3s.link/{homicidio,desaparicion,reclutamiento,secuestro}-v2.parquet.zip`).
  All three public gateways tried (`w3s.link`, `ipfs.io`, `dweb.link`)
  returned `504 Gateway Timeout` on 2026-07-04 -- the content doesn't seem to
  be well-pinned/seeded right now. Worth retrying later, or asking HRDAG to
  re-pin. `extractHRDAGv2.R` is ready to run unmodified once the data is
  reachable -- just needs `10-11_HRDAG/v2/verdata-parquet/{violation}-v2.parquet/`
  populated with the 100 replicate files each (same layout already used for v1).
- `15-16_RulingParty/Presidencia/MOE_resultados2022.csv` -- the 2022 runoff
  election file. Every other year (1958-2018) came back in the trash recovery;
  this one didn't. Script comment says it was originally sourced from
  datoselectorales.org (MOE) rather than RNEC directly. **Update:** user
  re-sourced and placed this file 2026-07-04; verified it matches the schema
  `15-16_wrangle-ts.R` expects.
- `10-11_HRDAG/verdata-examples/Resultados-CEV/Estimacion/output-estimacion/
  yy_hecho-is_conflict-perpetrador-homicidio.rds` -- one specific file missing
  from an otherwise-intact `verdata-examples/` subfolder. Confirmed this is
  HRDAG's own public tutorial repo (has its own embedded `.git/`), not a data
  delivery -- everything else in it is pre-aggregated by año/departamento/
  etnia/sexo/etc., never by municipio, so it can't substitute for the
  `extractHRDAG*.R` output even if found.

**Also fixed while auditing:** `.gitignore`'s folder-level ignore rules for
`01_raw_data/source_files/**` and `04_imputed_intermediate/**` were stale --
both predated the June/July pipeline renumbering and no longer matched
anything, meaning the current `01_empirical_data/01_source_files/source_files/`
folder (hundreds of MB of raw data) sat untracked-but-**not**-ignored. Fixed
to reference the current paths (`01_empirical_data/01_source_files/source_files/**`
and `02_imputation/**`), preserving the `.R`/`.Rmd` exemption. This was a real
risk of another oversized-file incident via a careless `git add -A`.

## First full end-to-end run of df01-df07, post-audit (2026-07-04)

With all the data gaps above resolved, ran `df01` through `df07` for the first
time since the reorg/recovery. Two upstream scripts had to run first to
produce inputs these depend on: `15-16_wrangle-ts.R` (needed `library(stringr)`
added -- `str_starts()` wasn't loaded) and `10-11_Osorio/ViPAA-Col/ViPAA-analysis.R`
(ran clean; its trailing animation section errors on a missing `gganimate`
package, but that's after the needed `write_rds()`, so harmless).

**Bugs found and fixed:**

- `df01-01234513-clean.R`, `df05-1214-clean.R`: DANE's TerriData exports store
  `Dato Numérico` as Spanish-formatted text (`"1.139,00"` = 1139.00, period as
  thousands separator, comma as decimal), not a native number -- arithmetic on
  it failed with "non-numeric argument to binary operator". Added
  `as.numeric(gsub(",", ".", gsub("\\.", "", DatoN)))` right after each read.
  This isn't a bug in the re-sourced files -- it's how DANE always exports this
  field; nobody had hit it before because this data was missing.
- `df01-01234513-clean.R`: `R18` (2018 census demographic projections) had no
  upper-year filter, so it picked up DANE's full projection horizon through
  2042. Capped at `year <= 2023` per team decision, matching the script's own
  comment ("contains from 2018-2023") and keeping this criterion aligned with
  what the rest of the panel covers.
- `df04-1011-clean_v3.R`: `CEDE03`'s `homicidios` column gets renamed to
  `Homi_1011` early on, but three later references (a correlation sanity check
  and the `HHomi_combined = coalesce(...)` patch step) still used the old name
  `homicidios`, which no longer existed post-rename. Fixed to reference
  `Homi_1011`. Also added a missing `library(ggplot2)`.

**Verified against the previously-committed `03_clean_outputs/*.rds`** (diffed
old vs. new row-aligned by `MPIO_CDPMP`/`year`) to catch any regression from
today's fixes:

- `df02`, `df03`, `df06`, `df07`: byte-for-byte identical. No change.
- `df01`: row count now matches old (29,110) after the year cap; ~3,300 rows
  (2021-2023 only) have different population figures because DANE has revised
  its projections since the old committed file was built (e.g. Bogotá 2023
  total population: 7,968,095 old vs 7,905,102 new, ~0.8% difference).
  2018-2020 (census-anchored years) are untouched. Team decided to accept
  DANE's current figures.
- `df04`: `HHomi_combined` differs in 2,879 of 32,994 rows, all in 2000-2013,
  shrinking toward zero by 2016-2017, and always new > old -- the signature of
  HRDAG revising/refining its own `verdata` replicate data over time, same
  pattern as the DANE revision above, not a processing bug (both old and new
  are fully non-NA, so it isn't newly-filled missingness either).

**Second bug found and fixed, upgraded from an initial "flag only" call:**

- `df05-1214-clean.R`'s `SP_12` (from `01_source_files/ColOpenData/
  population_projections.xlsx`, built by the original author via the
  ColOpenData R package + a manual Excel export -- see the script's own
  comment) has its `codigo_municipio` and `municipio` columns **swapped for
  every row in years 2005-2019** (the 5-digit DANE code ends up in the
  `municipio` column, e.g. `codigo_municipio="Medellín", municipio="05001"`
  for those years; 1985-2004 and 2020-2030 are fine). First diagnosed as
  "947 of 2,162 distinct values are municipality names" (looked like scattered
  bad rows) and initially just flagged rather than fixed. Re-investigated after
  running the master merge (`01_merge_empirical.R`) and comparing against the
  previously-committed `04_merge_empirical/df_col_clean.rds`: `PobTot_12`,
  `AREA`, `AREAkm`, `DenPob_12` had gone from **0% missing to 57.7% missing**
  (a real regression, not a data revision) -- the swapped rows weren't just
  inflating a count, they were silently hiding real data under the wrong key
  for every one of those 15 years (e.g. Medellín's 2010 row existed only under
  the key `"Medellín"`, never `"05001"`, so it could never join back to the
  panel). Confirmed the swap is clean and total (not partial) by checking
  per-year valid-code counts (0 or 1,122 valid rows per year, no partial years)
  and that `municipio`'s swapped-in values are always valid 5-digit codes.
  Fixed in `df05-1214-clean.R` by detecting the swap and un-swapping before the
  rename, rather than dropping the malformed rows -- this recovers all 15
  years' data instead of losing it. Verified: after the fix, `PobTot_12` has
  0 NA across all 46 years in `df05_clean.rds`, and the rerun master merge is
  now identical to the previously-committed `df_col_clean.rds` for
  `PobTot_12`/`AREA`/`AREAkm`/`DenPob_12`/`PobInd_14p`/`PobEtn_14p` -- these
  columns are no longer in the diff at all, only `df01`'s and `df04`'s
  DANE/HRDAG revisions remain (see above).

## Merge-level validation (2026-07-04)

Re-ran `01_merge_empirical.R` with the fixed `df01`/`df04`/`df05` outputs and
diffed the resulting `df_col_clean.rds` against the previously-committed
version (row-aligned by `MPIO_CDPMP`/`year`) -- this is the actual "raw" input
`02_imputation` reads, one level downstream of the per-criterion `df0N_clean`
files. Municipality count and row count both match exactly (1,122 x 26 years =
29,172). The only differences are the same DANE (`df01`, `IndRur_0t1`/
`PobRur_0t1`/`PobUrb_0t1`/`PobTot_0t1`) and HRDAG (`df04`, `HHomix_1011`)
source-data revisions documented above -- everything else is byte-identical
to what `02_imputation`'s existing outputs (`imp01.rds`, `imp23.rds`, etc.,
still committed from before this audit) were built on. Those imputation
outputs are now one merge-cycle stale relative to `df_col_clean.rds` and
should be re-run to pick up the `df01`/`df04` revisions and the `df05` fix,
but nothing here indicates they were built on bad data -- just slightly
older data for two specific criteria.
