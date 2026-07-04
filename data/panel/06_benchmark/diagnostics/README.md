# Diagnostics (internal record, not part of the shared memos)

Files here document the investigation and fix of the `weighted_range`/`wtdCL_range` sign
error found and resolved on 2026-07-02 (see `HANDOFF_pipeline_revision_june2026.md` at the
repo root for the full account). Kept for the record and as a regression check, not intended
for circulation with Memo 1/Memo 2.

- `04_spatial_rank_check.R` — checks whether benchmarking preserves or inverts within-year
  municipal ranking. Re-running it against the current pipeline should show r = +1.0000
  (fixed); it originally surfaced r = -1.0000 (the bug).
- `weighted_range_negative_by_year.png`, `rank_inversion_scatter_2023.png` — outputs of the
  above, from before the fix. Historical.
- `global_vs_ipsative_standardization.png` — from `03_trend_diagnostics.R` (still the live,
  canonical script at `08_benchmark/03_trend_diagnostics.R`); compares the corrected
  global-standardization approach to the earlier, superseded Colombia-only (`scale()`)
  version. Historical/explanatory, not needed to use the current pipeline.
- `cl_qnorm_sensitivity.png` — from an earlier version of `03_trend_diagnostics.R` (since
  rewritten); checked whether `qnorm()` matters empirically for civil liberties. No live
  script currently regenerates this file; static artifact only.
