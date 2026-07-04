# SNVDEM-COL Panel Pipeline

This is the data pipeline behind Colombia's subnational democracy index (SNVDEM).
It runs in seven numbered stages, folder `0N` = Step N. Steps 1 and 2 are each
further split into numbered subfolders mirroring their own internal flow (see
below). Each stage (with rare noted exceptions) has one canonical script that
reads the previous stage's output and writes its own. Run them in order, 01
through 07, to reproduce the final index from raw source data.

For the detailed history of recent bug fixes and methodology decisions, see
`HANDOFF_pipeline_revision_june2026.md` (repo root) and the memos in
`06_benchmark/memo/`. This file describes what the pipeline does today, not how
it got here. Folders were last renumbered 2026-07-03 (from an earlier 01-09
scheme) to read more like the actual conceptual flow: wrangle raw data → impute
→ build the working panel → V-Dem weighting → benchmark.

---

## Pipeline at a glance

| Step | Folder | Canonical script(s) | Produces |
|---|---|---|---|
| 1 | `01_empirical_data/` (5 numbered subfolders, see below) | `df01...df07-*.R`, `01_merge_empirical.R`, `Mun-Year.R` | `df01_clean.rds`...`df07_clean.rds`, `df_col_clean.rds`, `MunYrs.rds` |
| 2 | `02_imputation/` (4 numbered subfolders, see below) | 8 per-variable imputation scripts + `01_merge_imputed.R` | `impStatic.rds` ... `imp13.rds`, `imputed_master_panel.rds`, `imputed_cdf_panel.rds` |
| 3 | `03_geocoded_panel/01_clean_geocoded/` | `01_clean_geocode.R` | `CDF_averages.rds` |
| 4 | `04_vdem_data/` (4 numbered subfolders, see below) | `02_vdem_weighting/02_vdem_weighting.R` | `04_vdem_data/03_outputs/ELCLweights_wide.dta`, `06_benchmark/SNHPD.dta`, `06_benchmark/snlsffHPD.dta` |
| 5 | `05_weighting/` (2 numbered subfolders, see below) | `01_weighting_geopredictors/01_weighting_geopredictors.R` | `07_final_snvdem_data/snvdem_col_weighted.rds` (`snelect`, `sncivlib`, `sndem`) |
| 6 | `06_benchmark/` | `01_benchmark.R` (+ `02_validate_map.R`, `03_trend_diagnostics.R`, `diagnostics/04_spatial_rank_check.R`) | `07_final_snvdem_data/snvdem_col_final.rds` (`sndem_final`) |
| 7 | `07_final_snvdem_data/` | *(no canonical script -- see note below)* | Final `.rds` outputs, consumed by downstream analysis in `../scripts/` |

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
| `04_diagnostics/` | Missingness plots and `diagnostic_table_imputed.csv`, run immediately after the merge. |

| Script | Method | Output |
|---|---|---|
| `impStatic_LOCF.R` | LOCF | `impStatic.rds` |
| `imp01_RuralMI.R` | Multiple Imputation | `imp01.rds` |
| `imp23_EconGrowth_v3b.R` | Growth model + Random Forest | `imp23.rds` |
| `imp23_FiscalPMM_v3.R` | Predictive Mean Matching | `imp23b.rds` |
| `imp1011_CrimeMI-PMM_v3.R` | MI + PMM | `imp1011.rds` |
| `imp1011_FAviolence_pmq.R` | Factor analysis (reads `imp1011.rds`) | `imp1011FA.rds` |
| `imp1214_PopGBIv2.R` | Growth-Based Interpolation | `imp1214.rds` |
| `imp1516_ElectionsLOCF.R` | LOCF | `imp1516.rds` |
| `imp13_RoadsLOCF.R` | LOCF | `imp13.rds` |

`01_merge_imputed.R`'s CDF step uses `rank(., na.last = "keep") / length(.)`. The
`na.last = "keep"` (fixed 2026-07-03) is important: the default `rank()`
behavior silently promotes missing values to the *highest* percentile instead of
preserving `NA` -- this project deliberately keeps missingness visible instead.

**Known data quality note:** `PIB_2t3` (GDP) is the most-missing variable (~2%),
concentrated in Colombia's five smallest/most remote departments (San Andrés,
Amazonas, Guainía, Vaupés, Vichada), where DANE doesn't consistently publish
municipal-level GDP. Expected structural gap, not a pipeline bug.

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
- `03_outputs/` -- this step's live canonical output, `ELCLweights_wide.dta`
  (consumed by Step 5), promoted out of `02_vdem_weighting/MC/` on 2026-07-03
  since it's a live output, not legacy material.
- `04_images/` -- diagnostic plots written by `02_vdem_weighting.R`
  (`snlsff_colrange.png`, `clx_colrange.png`, `Ridgelfc.png`, `Ridgemoreless.png`).

`02_vdem_weighting.R` computes V-Dem expert coder-derived relevance weights for
elections and civil liberties separately, and the national HPD-based ranges
(`weighted_range`, `wtdCL_range`) used later to scale municipal deviations onto
the V-Dem national scale. Writes `ELCLweights_wide.dta` (consumed by Step 5) and
`SNHPD.dta`/`snlsffHPD.dta` (consumed by Step 6).

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

`01_weighting_geopredictors.R` joins Step 3's per-criterion averages to Step 4's
coder-derived weights and combines them into the two dimensions and composite:

```r
snelect  = weighted_avg_narm(predictors, el_weights)  # renormalized weighted mean, with a floor
sncivlib = weighted_avg_narm(predictors, cl_weights)
sndem    = 0.5 * (snelect + sncivlib)
```

This is the **unbenchmarked** measure -- each municipality's position relative to
*other Colombian municipalities*, not anchored to any external reference. Writes
`07_final_snvdem_data/snvdem_col_weighted.rds`.

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

`01_benchmark.R` maps each municipality's unbenchmarked score onto the V-Dem
national scale (deviation from `v2elffelr`/`CLSNmean`, scaled by
`weighted_range`/`wtdCL_range`), converts civil liberties to a Z-score via
`qnorm()`, and standardizes both dimensions against fixed global constants (full
V-Dem 2000-2023 panel, all countries) into `sndem_final`. This is the
**benchmarked** measure -- anchored to Colombia's real V-Dem trajectory and
comparable across countries. Writes `07_final_snvdem_data/snvdem_col_final.rds`.

Supporting scripts (all read `snvdem_col_final.rds`):
- `02_validate_map.R` -- faceted map across all 24 years, fixed color scale.
- `03_trend_diagnostics.R` -- validates municipal means track the raw V-Dem
  national anchors.
- `diagnostics/04_spatial_rank_check.R` -- regression check that benchmarking
  preserves (doesn't invert) spatial rank; kept after a real sign-error bug was
  found and fixed here 2026-07-02.

See `06_benchmark/memo/benchmarking_memo2_2026-07-02.md` for the full
methodology writeup, validation, and guidance on which measure (unbenchmarked vs.
benchmarked) to use for a given analytical question. Note the memo's own internal
file citations still reference the pre-2026-07-03 folder names (`07_weighting`,
`08_benchmark`, etc.) -- it was written before this reorg and hasn't been
re-pointed.

## Step 7 -- `07_final_snvdem_data/`

Holds the final outputs (`snvdem_col_weighted.rds`, `snvdem_col_final.rds`) --
both written directly by Steps 5 and 6, not by a script living in this folder.
**This folder has no canonical "Step 7" script of its own** -- the one script
here, `imgs/visuals-snvdem.R`, is downstream analysis/visualization, not a
pipeline processing step. `master_snvdem_col.rds` and `MC/` hold an older,
differently-schema'd version (`emel_index`/`cscw_index`/`sndem_index` instead of
`snelect`/`sncivlib`/`sndem`) -- stale, not part of the current pipeline.

---

## Running the full pipeline

Each script uses hardcoded absolute paths (`G:/Shared drives/snvdem/...`), so they
can be run individually and in any R session, in this order:

1. `01_empirical_data/01_source_files/Mun-Year.R` → `02_cleaning_scripts/df0*-clean.R` (any order) → `04_merge_empirical/01_merge_empirical.R`
2. `02_imputation/01_imputation_scripts/imp*.R` (any order, `imp1011_FAviolence_pmq.R` last -- depends on `imp1011_CrimeMI-PMM_v3.R`'s output) → `03_merge_imputed/01_merge_imputed.R`
3. `03_geocoded_panel/01_clean_geocoded/01_clean_geocode.R`
4. `04_vdem_data/02_vdem_weighting/02_vdem_weighting.R`
5. `05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R`
6. `06_benchmark/01_benchmark.R`

## Non-canonical subfolders

Every pipeline folder may contain `v1/`, `v2/`, `MC/`, or `misc/` subfolders --
these hold superseded drafts or exploratory work by other team members, not part
of the canonical run. They're intentionally left with old path references when
folders get renamed/restructured.

## Open questions

- Whether Step 7 should get a thin canonical script of its own, or stay
  script-less with `visuals-snvdem.R` reclassified as pure downstream analysis.
- `06_benchmark/memo/` file citations still reference the old 01-09 folder
  numbering -- not yet updated since the memo is an external-facing deliverable.
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

**Not fixed -- genuinely missing raw source data, needs re-sourcing, not a path fix:**

- `15-16_RulingParty/Presidencia/MOE_resultados2022.csv` -- the 2022 runoff
  election file. Every other year (1958-2018) came back in the trash recovery;
  this one didn't. Script comment says it was originally sourced from
  datoselectorales.org (MOE) rather than RNEC directly.
- `01_source_files/source_files/a1-Censos/{2005TerriData_Dim25_Sub4_poburb.xlsx,
  2018TerriData_Dim2_dem.xlsx}` -- no `a1-Censos` folder exists at all.
  Similarly-named `TerriData_DimN_*` files exist under `data/2018pmq/`
  in differently-numbered subfolders -- unclear if these are the same source
  data under old naming or genuinely different extracts. Needs a human check.
- `01_source_files/source_files/2-3_EconDevt/IDF/*.xlsx` (5 files, DNP's
  Índice de Desempeño Fiscal) -- no trace anywhere in the repo or the recovered
  trash list.
- `01_source_files/source_files/14_Indigenous/TerriData_Dim25_Sub5_pobetn.xlsx`
  -- likely candidate at `data/2018pmq/14_Indigenous/
  TerriData_Dim2_Sub5_etnica.xlsx`, but different filename -- needs verification
  it's actually the same data before pointing `df05-1214-clean.R` at it.
- `10-11_HRDAG/v2/verdata-parquet/{homicidio,desaparicion,reclutamiento,
  secuestro}-v2.parquet` -- the actual raw HRDAG data delivery folder
  (`extractHRDAGv2.R`'s real input, distinct from the `verdata-examples/`
  worked-example subfolder which is HRDAG's own demo package). Not found
  anywhere.
- `10-11_HRDAG/verdata-examples/Resultados-CEV/Estimacion/output-estimacion/
  yy_hecho-is_conflict-perpetrador-homicidio.rds` -- one specific file missing
  from an otherwise-intact `verdata-examples/` subfolder.
- `colvdem0020.R` (sits directly in `02_cleaning_scripts/`, not a `v1/v2/MC`
  subfolder, but *not* listed as canonical in the pipeline-at-a-glance table
  above) references a pre-01-09-numbering folder scheme
  (`data/panel/validation/`, `data/panel/final_data/Weighted/`) that predates
  even `01_raw_data`. Likely dead/superseded -- flagging rather than assuming.
