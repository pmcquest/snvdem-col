# Missing-predictor handling decision -- 2026-07-03

**Context:** `README.md` (Step 5) had flagged an open decision since at least 2026-07-02: once
Step 2's `na.last = "keep"` fix let real `NA`s flow through the pipeline, `01_weighting_geopredictors.R`'s
plain `sum()` formula blanked a municipality-year's entire `snelect`/`sncivlib`/`sndem` whenever
*any one* of the 16 weighted predictor criteria was missing. This log records the investigation
and the decision.

## Where the 682 NAs came from

Traced to the predictor side only (`03_geocoded_panel/01_clean_geocoded/CDF_averages.rds`) --
zero came from the V-Dem weight columns (the pre-existing `cl_Less_development` patch already
handles that one). 60 municipalities affected, concentrated in Colombia's smallest/most remote
departments (San Andres `88`, Amazonas `91`, Guainia `94`); 24 of the 60 are missing at least one
criterion in *every* year.

Two municipalities stood out: `99624` (Santa Rosalia) and `99773` (Cumaribo) are missing 12 of 16
predictor terms (Urban/Rural, Ruling-party, Development, Capital, and all four directional
predictors) in every single year, 2000-2023. These are the same two municipalities flagged in the
2026-07-03 empirical-merge fix for legacy DIVIPOLA codes -- suggesting a residual geocoding /
spatial-join gap in Step 3 that predates and is separate from this decision. Not investigated
further here; worth a dedicated look at `03_geocoded_panel/01_clean_geocode.R`'s spatial join for
these two codes specifically.

## What na.rm + renormalize would do (before deciding on a floor)

Tested dropping only the missing (predictor, weight) pairs and renormalizing over what's left:

- **All 682 rows are rescuable in principle** -- no municipality-year is missing all 16 criteria
  (no true 0/0 case).
- **Most retain most of the weight mass**: median 14/16 criteria available (~88% of the weight
  denominator); 1st quartile still ~73%.
- **A ~44-row tail retains very little**: as low as 4/16 criteria (~18% of weight mass) --
  entirely the Santa Rosalia/Cumaribo rows above.
- **Rescued rows skew lower-scoring**: median rescued `snelect` = 0.22 vs. 0.34 for the
  originally-valid population. Partly plausible (remote territories), partly a composition
  artifact -- 552 of the 682 rows are missing the "development" criterion specifically, which
  changes what's being averaged, not just how much.

## Decision

Per PM: na.rm + renormalize, with a 50% weight-mass floor. Rows retaining at least half the
original weight denominator get a renormalized score over the available criteria; rows below
that threshold stay `NA` rather than resting on a handful of criteria. Implemented as
`weighted_avg_narm()` in `01_weighting_geopredictors.R`.

## Result

- `snelect`/`sncivlib` NAs: 682 -> 48 (the Santa Rosalia/Cumaribo tail, i.e. the floor is doing
  exactly what it was designed to do).
- Full pipeline re-run through Step 6: `CL_col_mt` stayed inside (0,1) (required for `qnorm()`),
  spatial-rank check held (r = +1.0000, all 24 years), all diagnostic plots regenerated.
- National year-level means (`EL_mean`/`CL_mean`/`sndem_mean` in `06_benchmark/01_benchmark.R`'s
  diagnostic table) shifted only in the 3rd decimal place from the pre-fix run -- expected, since
  only 682 of ~26,928 rows were touched and municipal deviations average to ~0 within a year
  regardless.

## Status

Pre-fix files backed up as `07_final_snvdem_data/snvdem_col_weighted_ORIGINAL_pre-narmfix_2026-07-03.rds`
and `snvdem_col_final_ORIGINAL_pre-narmfix_2026-07-03.rds`. Live files reflect the fix.
Follow-up not done here: investigate the Santa Rosalia/Cumaribo Step 3 geocoding gap directly,
which would let those 48 rows (and potentially their neighbors) get a real score rather than a
renormalized/floored one.

**Superseded later the same day:** `05_weighting` was reorganized into numbered subfolders
right after this fix (`01_weighting_geopredictors/`, `02_images/`), so the canonical script now
lives at `05_weighting/01_weighting_geopredictors/01_weighting_geopredictors.R`. The history
above is accurate for what was true at the time this decision was made.
