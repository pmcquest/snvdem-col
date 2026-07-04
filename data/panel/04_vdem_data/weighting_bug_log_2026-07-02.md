# Weighting script bug log — 2026-07-02

**Context:** MC sent `weighting_January2026_0726.R` (saved to `06_vdem_data/`) to complete the
civil-liberties half of the criterion-weight calculation that produces `ELCLweights_wide.dta`.
Reviewing it against the original `weighting_summer2025.R` (`06_vdem_data/coder-level/MC/`)
surfaced two bugs present in **both** versions — i.e. present in the live
`coder-level/MC/ELCLweights_wide.dta` since it was first generated, not something MC introduced
in January. Flagged to MC by email 2026-07-02. Recorded here per his reply so the fix and its
impact are on record before the live weights file is corrected.

Rewritten, streamlined version of the script: `06_vdem_data/01_weighting.R`. It reproduces the
two bugs behind a `FIX_KNOWN_BUGS` toggle and was verified to match the live
`ELCLweights_wide.dta` bit-for-bit (max |diff| = 0 across all 44 weight columns, Colombia
2000-2023) before being trusted to compute the corrected version.

## Bug 1 — wrong denominator for 3 of 22 criteria

In each of the 4 repeated relevance-criteria blocks (elections-more, elections-less,
CL-stronger, CL-weaker), the proportion calculation for criteria 2, 3, and 4 ("Less
development", "More development", "Inside capital") divides by the criterion-1 zero-count
instead of its own:

```r
pr_2 = v2elsnlfc_2_1/(v2elsnlfc_2_1 + v2elsnlfc_1_0 + snlsff_2),   # should be v2elsnlfc_2_0
pr_3 = v2elsnlfc_3_1/(v2elsnlfc_3_1 + v2elsnlfc_1_0 + snlsff_2),   # should be v2elsnlfc_3_0
pr_4 = v2elsnlfc_4_1/(v2elsnlfc_4_1 + v2elsnlfc_1_0 + snlsff_2),   # should be v2elsnlfc_4_0
```
(`weighting_January2026_0726.R:438-440`, and the equivalent lines in the mrfc/wkch/stch blocks.)

## Bug 2 — "Outside capital" and "North" weights are swapped

In each block, `pr_6` is computed before `pr_5` inside the `mutate()` call:
```r
pr_6 = v2elsnlfc_6_1/(v2elsnlfc_6_1 + v2elsnlfc_6_0 + snlsff_2),
pr_5 = v2elsnlfc_5_1/(v2elsnlfc_5_1 + v2elsnlfc_5_0 + snlsff_2),
```
The resulting data frame's 6th/7th criterion columns are therefore in the order `pr_6, pr_5`,
but the subsequent `colnames()` call assumes strict sequential order `pr_0...pr_21` and labels
those positions "Outside capital", "North". The values end up swapped between the two labels.

## Verification

- `01_weighting.R` with `FIX_KNOWN_BUGS <- FALSE` reproduces the live `ELCLweights_wide.dta`
  exactly (Colombia 2000-2023, all 44 `el_`/`cl_` weight columns, max |diff| = 0.00000000).
- `01_weighting.R` with `FIX_KNOWN_BUGS <- TRUE` isolates the fix to exactly the 10 affected
  columns; all other columns are unchanged (max |diff| = 0 across unaffected columns).

## Magnitude of the fix (Colombia, 2000-2023 means)

| Column | Buggy mean | Corrected mean | Mean \|Δ\| | Max \|Δ\| |
|---|---|---|---|---|
| el_Less_development | 0.136 | 0.443 | 0.371 | 0.603 |
| el_More_development | 0.666 | 0.447 | 0.219 | 0.667 |
| el_Inside_capital    | 0.706 | 0.589 | 0.117 | 0.333 |
| el_Outside_capital   | 0.126 | 0.241 | 0.164 | 0.600 |
| el_North             | 0.241 | 0.126 | 0.164 | 0.600 |
| cl_Less_development  | 0.384 | 0.678 | 0.319 | 0.589 |
| cl_More_development  | 0.806 | 0.723 | 0.097 | 0.500 |
| cl_Inside_capital    | 0.747 | 0.630 | 0.128 | 0.250 |
| cl_Outside_capital   | 0.097 | 0.580 | 0.482 | 0.750 |
| cl_North             | 0.580 | 0.097 | 0.482 | 0.750 |

Note `el_Outside_capital`/`el_North` (and `cl_` counterparts) exactly trade values — confirms
Bug 2 is a pure swap, not a broader computational error.

## Status

MC notified 2026-07-02. Per PM's direction, proceeding to promote the corrected weights as the
live `ELCLweights_wide.dta` and propagate through `07_weighting` and `08_benchmark`. Original
(buggy) file preserved as `coder-level/MC/ELCLweights_wide_ORIGINAL_pre-fix_2026-07-02.dta` for
reference / rollback.
