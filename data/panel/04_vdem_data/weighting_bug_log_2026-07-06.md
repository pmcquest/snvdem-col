# Weighting script bug log — 2026-07-06

**Context:** Patrick noticed a gap in `snlsff_colrange.png` around 1950 (`ggsave` warning:
"Removed 6 rows containing missing values or values outside the scale range
(`geom_ribbon()`)"), and separately asked why the new plot looks so different from MC's
`weighting_January2026_0726.R` output, since line 115 of that script "appears to properly
calculate the weighted ranges."

## Bug — `scale_y_continuous(limits=...)` silently drops out-of-range rows

Traced the 6 missing rows directly: both `snlsff` (coder counts) and `HPDs` (`v2elffelr`) are
complete for Colombia, 1900-2024/2025 — no NAs, no gaps, no duplicate merge keys. The 6 rows
are COL 1948-1953, where (with the 2026-07-03 corrected formula) `ymin = v2elffelr -
weighted_range = -1.596 - 2.013 = -3.609`, just past the plot's hardcoded
`scale_y_continuous(limits = c(-3.6, 3.3))`.

`scale_y_continuous(limits=...)` converts any value outside the window to `NA` before
`geom_ribbon()` draws — it doesn't just zoom, it deletes data. That's what tore the gap.
Before the 2026-07-03 range-formula fix, `weighted_range` was smaller for COL (the bug
understated it), so `ymin` never crossed -3.6 and the gap didn't appear — i.e. the gap is a
side effect of the *previous* fix becoming more visible, not a new data problem.

**Fix:** replaced `scale_y_continuous(limits=...)` with `coord_cartesian(ylim=...)` in both the
elections (`snlsff_colrange.png`) and civil-liberties (`clx_colrange.png`) plots in
`02_vdem_weighting.R`. `coord_cartesian` only sets the viewport/zoom — it never converts
in-range data to `NA`, so no rows get dropped regardless of how the weighting formula evolves.

## Confirmed: MC's legacy formula is invalid, not a valid alternate calculation

This was already flagged in `weighting_bug_log_2026-07-03.md` (see "This was not carried over
cleanly from either prior script version"), but Patrick's question today prompted a direct,
fuller check rather than relying on that note secondhand. Computed both formulas across the
whole merged `snlsffHPD` table (all countries, all years):

| Check | Legacy (`weighting_January2026_0726.R` line 115) | Current (`02_vdem_weighting.R`) |
|---|---|---|
| Share of rows with `weighted_range < 0` (impossible for a range) | **40.0%** | 0% |
| Share of rows with `weighted_range > 2×HPD` (theoretical max) | 1.4% | 0% |

Worked example, COL 1950 (`snlsff_0=10` sig.-variation coders, `snlsff_1=0`, `snlsff_2=2`
no-variation coders, `HPD=1.208`):
- Legacy: `((2-0)*1.208 + (2-10)*1.208*2) / 12 = -1.409` — negative, i.e. `ymax < ymin`.
- Current: `(0*1.208 + 10*2*1.208) / 12 = 2.013` — sensible, ≤ `2*HPD`.

The legacy code applied `(2 - x)` to the **aggregated coder count** (`snlsff_0`, `snlsff_1` —
how many coders picked that category), not to each coder's own category code. The intended
design (operational strategy doc: "if all experts chose answer 2, multiply HPD by 2-2=0 ... if
all chose 0, multiply by 2-0=2") is a per-coder weight of `2 - category`, summed across coders
and averaged. That collapses to fixed per-category coefficients — `snlsff_0 * 2 + snlsff_1 * 1
+ snlsff_2 * 0` — which is exactly what the current script computes directly, without a visible
"2 -" anywhere, because the subtraction was already carried out at the coefficient level. The
current script isn't missing a term relative to the legacy one; the legacy script's `(2 -
snlsff_1)` / `(2 - snlsff_0)` was a dimensional error (subtracting a *count* from 2, not a
*category code*), and it just happens to look similar syntactically.

**Practical implication:** MC's original `colrange.png`-style plots (and by extension any
figure in the operational strategy doc or older memos built from that script) should not be
treated as a correctness reference for these two ribbon plots — they're drawn from a formula
that returns negative "ranges" for 40% of all country-year observations globally.

**Follow-up (not yet done):** Patrick wants a side-by-side of the original MC graphic vs. the
current corrected graphic (for both elections and civil liberties) included in the benchmarking
memo, to make this visible rather than just documented in this log.

## Status

- `02_vdem_weighting.R` updated (coord_cartesian fix), re-run to confirm `snlsff_colrange.png`
  and `clx_colrange.png` render without gaps.
- No underlying `.dta` output changed by this fix — only the plot rendering. The 2026-07-03
  corrected `weighted_range`/`wtdCL_range` values were already correct; only their visualization
  was affected.
