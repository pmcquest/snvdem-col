# Diagnostics (internal record, not part of the shared memos)

Reorganized 2026-07-06: this folder used to be `06_benchmark/diagnostics/`, holding only the
historical record below. It's now `06_benchmark/02_diagnostics/`, folding in the two live
validation scripts too -- `01_scripts/` has all three (`01_validate_map.R`,
`02_trend_diagnostics.R`, `03_spatial_rank_check.R`, the last renamed from
`04_spatial_rank_check.R`), and `02_outputs/` has every PNG they produce, live or historical.

Files here document the investigation and fix of the `weighted_range`/`wtdCL_range` sign
error found and resolved on 2026-07-02 (see `HANDOFF_pipeline_revision_june2026.md` at the
repo root for the full account). Kept for the record and as a regression check, not intended
for circulation with the memos in `04_memo/`.

- `01_scripts/03_spatial_rank_check.R` — checks whether benchmarking preserves or inverts
  within-year municipal ranking. Re-running it against the current pipeline should show
  r = +1.0000 (fixed); it originally surfaced r = -1.0000 (the bug).
- `02_outputs/weighted_range_negative_by_year.png`, `rank_inversion_scatter_2023.png` —
  outputs of the above, from before the fix. Historical.
- `02_outputs/global_vs_ipsative_standardization.png` — from `01_scripts/02_trend_diagnostics.R`
  (still the live, canonical script); compares the corrected global-standardization approach to
  the earlier, superseded Colombia-only (`scale()`) version. Historical/explanatory, not needed
  to use the current pipeline.
- `02_outputs/cl_qnorm_sensitivity.png` — from an earlier version of `02_trend_diagnostics.R`
  (since rewritten); checked whether `qnorm()` matters empirically for civil liberties. No live
  script currently regenerates this file; static artifact only.
