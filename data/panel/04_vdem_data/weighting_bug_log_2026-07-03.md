# Weighting script bug log — 2026-07-03

**Context:** Patrick asked for code comments explaining why the civil-liberties calculation
substitutes `v2clsnlpct` where elections uses `v2elffelr` directly (no CL equivalent of
`v2elffelr` exists in V-Dem), and separately flagged that the `colrangeCL.png`-style plot
(now `clx_colrange.png`) looked narrower than the example in `06_benchmark/Revised
operational strategy_Jan2026.docx`. Investigating the second question surfaced a real bug in
`01_weighting.R`, independent of the two bugs fixed 2026-07-02
(`weighting_bug_log_2026-07-02.md`).

## Bug — inverted category weights in `weighted_range` / `wtdCL_range`

Both `v2elsnlsff` and `v2clrgunev` are 3-level ordinal variables. Per the V-Dem codebook
(`coder-level/V-Dem/codebook.pdf`, sec. 3.1.7.11 and 3.9.2.7) and the operational strategy
doc's own worked example, the scale runs the *opposite* direction from what you'd guess by
the number alone:

| Code | Meaning | Correct weight |
|---|---|---|
| 0 | "Yes" — significantly different across areas | 2 |
| 1 | "Somewhat" different | 1 |
| 2 | "No" — equally free/fair (or equally protected) everywhere | 0 |

The operational strategy doc states this explicitly: *"If all experts chose answer 2, we
would multiply the HPD by 2 − v2elsnlsff = 0 ... If all the experts chose answer 0, we would
multiply the HPD by 2 − v2elsnlsff = 2."*

`01_weighting.R` had this backwards in both formulas:

```r
# elections (was):
weighted_range = (snlsff_1 * HPD + snlsff_2 * 2 * HPD) / (snlsff_0 + snlsff_1 + snlsff_2)
# civil liberties (was):
wtdCL_range = (clrgunev_1 * CLHPD + clrgunev_2 * 2 * CLHPD) / (clrgunev_0 + clrgunev_1 + clrgunev_2)
```

This weighted the "no variation" category (`_2`) by 2 and dropped the "significant variation"
category (`_0`) from the numerator entirely — i.e. exactly inverted. For Colombia, coders pick
"significant variation" (0) far more often than "no variation" (2) in most years (confirmed by
direct inspection of `Coder-Level-Dataset-v15.rds`), so the bug systematically *understated*
the true range — which is what produced the visibly narrower ribbon plot Patrick noticed.

This was not carried over cleanly from either prior script version — MC's
`weighting_January2026_0726.R` used yet a third, dimensionally broken formula
(`(2-snlsff_1)*HPD + (2-snlsff_0)*HPD*2`, applied to raw aggregate counts rather than
per-coder 0/1 indicators, which produces meaningless/negative values for any country-year with
more than 2 coders). It looks like this bug was introduced fresh during the July 2026
streamlining rewrite, when the category semantics got mapped in the wrong direction.

**Fix:**

```r
weighted_range = (snlsff_1 * HPD + snlsff_0 * 2 * HPD) / (snlsff_0 + snlsff_1 + snlsff_2)
wtdCL_range = (clrgunev_1 * CLHPD + clrgunev_0 * 2 * CLHPD) / (clrgunev_0 + clrgunev_1 + clrgunev_2)
```

## Secondary bug found while fixing the above: wrong output path

`out_dir <- file.path(base_dir, "coder-level", "MC")` pointed at a directory
(`04_vdem_data/coder-level/MC/`) that has never existed on disk. The live
`ELCLweights_wide.dta` has always lived directly in `04_vdem_data/MC/` (since Jan 2026). This
path (and the matching read path in `05_weighting/01_weight_predictors.R`) is inherited
verbatim from MC's original scripts and this repo's own README table, so the mismatch predates
today's session — it only surfaced now because `write_dta()` fails outright when the target
directory doesn't exist (previously the file may have been written under `coder-level/MC/`
during some earlier run and then the folder got swept up in a later "stale empty folder"
cleanup — unclear, but not worth reconstructing). Fixed both paths to point at `04_vdem_data/MC/`.

**Superseded later the same day:** `04_vdem_data` was reorganized into numbered subfolders
(`01_source_files/`, `02_vdem_weighting/`, `03_images/`) right after this fix, so the live
path is now `04_vdem_data/02_vdem_weighting/MC/ELCLweights_wide.dta`. The history above is
accurate for what was true at the time the bug was found.

## Magnitude of the fix (Colombia, 2000-2023)

| Quantity | Buggy mean | Corrected mean | Mean \|Δ\| | Max \|Δ\| |
|---|---|---|---|---|
| `weighted_range` (elections range) | 0.541 | 0.810 | 0.269 | 0.831 |
| `wtdCL_range` (civil liberties range) | 0.0361 | 0.0688 | 0.0328 | 0.0751 |

| Final output (all municipality-years, 2000-2023) | Mean \|Δ\| | Max \|Δ\| |
|---|---|---|
| `EL_col_gz` | 0.037 | 0.433 |
| `CL_col_gz` | 0.059 | 0.318 |
| `sndem_final` | 0.039 | 0.339 |

Correlation between old and new `sndem_final`: 0.981 — rankings are largely preserved, but
individual municipality-year values shift meaningfully (max |Δ| = 0.34 on a global-SD scale).

## Verification

- Plots regenerated: `04_vdem_data/snlsff_colrange.png`, `04_vdem_data/clx_colrange.png` — both
  visibly wider now, consistent with the range in the operational strategy doc's Figure 1/2/3.
- Full pipeline re-run 2026-07-03: `04_vdem_data/01_weighting.R` →
  `05_weighting/01_weight_predictors.R` → `06_benchmark/01_benchmark.R`. `CL_col_mt` stayed
  inside (0,1) throughout (required for `qnorm()`), 682 NAs in `snelect`/`sncivlib` (expected,
  unchanged from before — from the Step 2 `na.last = "keep"` decision, not from this fix).

## Status

Pre-fix files backed up as `*_ORIGINAL_pre-rangefix_2026-07-03.{dta,rds,png}` next to each live
file (`06_benchmark/snlsffHPD.dta` and `SNHPD.dta`, `07_final_snvdem_data/snvdem_col_weighted.rds`
and `snvdem_col_final.rds`, `04_vdem_data/snlsff_colrange.png` and `clx_colrange.png`). Live
files now reflect the corrected formula. Not yet re-run: `06_benchmark/02_validate_map.R`,
`03_trend_diagnostics.R`, `diagnostics/04_spatial_rank_check.R` — these consume
`snvdem_col_final.rds` and should be re-checked against the corrected values before treating
those diagnostics (e.g. the `r = +1.0000` spatial-rank check) as still valid.
