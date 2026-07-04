---
output:
  pdf_document: default
  html_document: default
---
# Benchmarking Memo 3: Team Follow-Up  --  Spatial Variation and a Sign Error in the Benchmarking Formula

**Date:** July 2, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The full SNVDEM-COL team, following the 2026-07-02 meeting

---

## How this memo fits with what came before

This is the third memo in a chain, and each one only makes sense in light of the last:

1. **Memo 1** (`benchmarking_memo.md`, June 26) laid out the problem: our two dimensions  --  `EL_col_mt` (elections, V-Dem's latent scale) and `CL_col_mt` (civil liberties, [0,1] scale)  --  couldn't be combined into a single `sndem_final` without distorting or flattening the trend. Every method tried produced a bad result.
2. **MC's email reply** (July 2) resolved the conceptual question: benchmarked scores are the intended final measure for all uses, and the two dimensions should be combined by converting `CL_col_mt` to a Z-score with `qnorm()`.
3. **Memo 2** (`benchmarking_memo2_2026-07-02.md`) implemented that, caught and fixed a mistake in the first attempt (an ipsative  --  Colombia-only  --  standardization that would have broken comparability with other countries), and validated the result against the raw V-Dem national anchors.
4. **This memo** follows up on the 2026-07-02 team meeting, where Patrick presented the Memo 2 maps. It answers Patrick's own follow-up question about how the Z-scores are calculated, and reports something found while digging into why the benchmarked maps show less visible spatial variation than the unbenchmarked ones  --  a sign error in the benchmarking formula, present since the original scripts and unrelated to anything in Memos 1-2.

**If you only read one section, read Section 2.**

---

## 1. Recap: what "benchmarked" and "unbenchmarked" each show

For readers who weren't in the weeds of Memos 1-2:

- **Unbenchmarked (`sndem`, `snelect`, `sncivlib`):** each municipality's geographic/structural predictors (rurality, distance from capital, violence, etc.), weighted by V-Dem expert judgments about what matters for elections and civil liberties, then standardized against the empirical distribution of *other Colombian municipalities* (a CDF, 2000-2023 pooled). This is a **within-Colombia, relative** measure  --  it tells you how a municipality compares to the rest of Colombia, but not how Colombia as a whole compares to any other country, and only weakly reflects absolute national-level change over time (Memo 1, Sections 3-4).

- **Benchmarked (`EL_col_mt`, `CL_col_mt`, `sndem_final`):** each municipality's unbenchmarked score is re-expressed as a deviation from the actual V-Dem national estimate for that year (`v2elffelr` for elections, `v2x_civlib`-derived `CLSNmean` for civil liberties), scaled by how much the country is estimated to vary internally (`weighted_range`/`wtdCL_range`, built from V-Dem's own uncertainty and "unevenness" codings). This is meant to be a measure that's **anchored to something real and comparable across countries**  --  which is the entire reason the benchmarking step exists (per the operational strategy document quoted in Memo 1: "we conceive of free and fair elections at the municipal level as a set of deviations centered around this summary subnational measure").

Michael's point at the meeting  --  that both are useful for different things, and we should keep both  --  is right and isn't in question here. The question this memo investigates is whether the benchmarked version is currently computing what it's supposed to.

---

## 2. Finding: benchmarking is currently inverting the spatial pattern, not just muting it

At the meeting, the working explanation for why the new maps show less visible spatial (municipality-to-municipality) variation than the old ones was that national-level, year-to-year change is large enough to visually dominate the color scale, "overwhelming" the subtler spatial differences. That's a real effect and part of the picture  --  but checking it turned up something bigger.

**Within every single year from 2000-2023, a municipality's benchmarked score is currently the exact mathematical mirror image of its unbenchmarked score.** Not "compressed" or "less visible"  --  inverted. The correlation between `snelect` (unbenchmarked) and `EL_col_mt` (benchmarked) is **r = -1.0000** in all 24 years. Same for civil liberties: `sncivlib` vs. `CL_col_mt`, r = -1.0000 in all 24 years. The scatter plot below (2023, elections) isn't noise or a weak relationship  --  it's a perfectly straight line with negative slope:

![Rank inversion](../rank_inversion_scatter_2023.png)

Concretely: sorting Colombia's municipalities into deciles by their unbenchmarked elections score, the *top* decile has the *lowest* mean benchmarked score (1.43) and the *bottom* decile has the *highest* (2.03)  --  a clean, monotonic reversal from decile 1 through decile 10. This isn't specific to any one region or grouping; it will show up in any comparison between high- and low-scoring municipalities.

### Root cause

The benchmarking formula (`08_benchmark/01_benchmark.R`, unchanged from MC's original design):

```
EL_col_mt = v2elffelr + (snelect - snelectyrmean) * weighted_range / ELrange_975_025
CL_col_mt = CLSNmean  + (sncivlib - CLSNyrmean)   * wtdCL_range   / CLrange_975_025
```

`weighted_range` and `wtdCL_range` are national constants (same value for every municipality in a given year) meant to represent *how much* Colombia's subnational units are estimated to vary  --  a magnitude, which should be positive. Checked across the full panel, **`weighted_range` is negative in all 24 years, and `wtdCL_range` is negative in all 24 years**:

![Negative range multiplier](../weighted_range_negative_by_year.png)

Since `(snelect - snelectyrmean)` is positive for above-average municipalities and negative for below-average ones, multiplying by a *negative* `weighted_range` flips the sign of every municipality's deviation before it's added back to the national anchor. That's the entire mechanism  --  nothing more exotic than a sign error propagating through the formula.

**Why is the multiplier negative?** Tracing it back to `06_vdem_data/01_weighting.R` (streamlined rewrite of MC's original weighting script, same formula preserved unchanged):

```r
weighted_range = ((2 - snlsff_1) * HPD + (2 - snlsff_0) * HPD * 2) / (snlsff_0 + snlsff_1 + snlsff_2)
```

`snlsff_0`, `snlsff_1`, `snlsff_2` are **counts of V-Dem coders** who selected "same" (0), "somewhat different" (1), or "significantly different" (2) when asked how uneven Colombia's subnational elections are. For Colombia in a typical year, 5+ coders are surveyed, so it's common for `snlsff_0` or `snlsff_1` to exceed 2  --  at which point `(2 - snlsff_1)` or `(2 - snlsff_0)` goes negative. Example, 2010: `snlsff_0 = 12`, `snlsff_1 = 16`, giving `weighted_range = -0.631`. The same mechanic affects `wtdCL_range` via `clrgunev_0/1/2`.

The `(2 - count)` construction only produces a sensible (positive) result if `snlsff_0`/`snlsff_1` are bounded to at most 2  --  i.e., if they were meant to be **proportions or weights on a 0-2 scale**, not raw coder counts. The operational strategy document's own description of the intended design ("subnational level +/- (% of coders * range of variation...)") talks in terms of *percent of coders*, which supports this reading. Whether the fix is to convert counts to proportions before this formula, cap the multiplier at zero, or something else structural is MC's call  --  this is his original formula design, not something introduced in Memos 1-2 or in Patrick's June/July revisions (confirmed unchanged since `weighting_summer2025.R`).

### What this does and doesn't affect

- **Affected:** the within-year *spatial* pattern in every benchmarked map produced so far, including the maps just presented to the team (`sndem_final_facet_allyears.png`, `sndem_dims_milestone_years.png`)  --  municipality-to-municipality comparisons within a given year are backwards.
- **Not affected:** the *national* year-over-year trend. `sndem_final`'s national mean by year is anchored directly to `v2elffelr`/`CLSNmean` (validated against the raw V-Dem anchors in Memo 2, `trend_vs_national_anchors.png`)  --  that validation only checked the national-level series, which doesn't depend on the sign of `weighted_range`. So the overall "Colombia improved 2000-2018, dipped 2019-2021" story likely still holds; it's specifically the *within-country geography* that's currently backwards.
- **Not affected:** the `qnorm()`/global-standardization work in Memo 2  --  that's a separate, later step in the pipeline and is correct on its own terms. It's just operating on `EL_col_mt`/`CL_col_mt` inputs that are already sign-flipped upstream.

---

## 3. Answering Patrick's question from the meeting about Z-scores

At the meeting, Patrick asked for clarification on how the Z-scores were calculated and whether they properly represent global benchmarks. Short answer: yes, as of the fix described in Memo 2  --  `EL_col_gz` and `CL_col_gz` are standardized against fixed constants from the full V-Dem country-year panel (2000-2023, all countries), not from Colombia's own data, so they are comparable across countries in principle. That part is independent of, and unaffected by, the sign-error finding in Section 2 above  --  the global-standardization fix operates downstream of `EL_col_mt`/`CL_col_mt` and would apply equally correctly once those are fixed.

---

## 4. Recommendation before further sharing

**Hold off on presenting the benchmarked spatial maps further until the `weighted_range`/`wtdCL_range` sign issue is resolved with MC.** The national trend line (the improving-over-time story) is likely fine to keep using. The within-Colombia geography shown in the benchmarked maps is currently backwards and shouldn't be presented as-is. The unbenchmarked maps do not have this problem and remain valid for showing subnational spatial variation, consistent with what Michael and Kelly suggested  --  presenting both versions with a clear explanation of what each is for is still the right end state; we just need the benchmarked version corrected first.

---

## 5. Next steps

- [ ] Confirm the `weighted_range`/`wtdCL_range` sign issue with MC and agree on a fix (proportions vs. counts, or another approach to the formula).
- [ ] Re-run `06_vdem_data/01_weighting.R`  ->  `07_weighting/01_weight_predictors.R`  ->  `08_benchmark/01_benchmark.R` once fixed, and re-validate (rank correlation should be strongly *positive*, not -1, after the fix).
- [ ] Regenerate the benchmarked maps once corrected.
- [ ] Per Michael's suggestion: draft the explanatory language distinguishing benchmarked (cross-national, smoother, anchored to V-Dem) from unbenchmarked (within-Colombia, more spatial detail) for the paper, once the benchmarked maps are corrected.
- [ ] Revisit Memo 2's six open questions (reference window, reference population, pooled vs. year-specific global constants, `qnorm()` order of operations, etc.)  --  those remain relevant once this more urgent issue is resolved.
- [ ] Explore how the corrected spatial-variation data could support cross-national comparison hypotheses (per Michael's example of testing democratic influence across national borders) once the sign issue is fixed.

---

## Files

- `08_benchmark/04_spatial_rank_check.R`  --  this investigation (rank-inversion check, plots)
- `08_benchmark/weighted_range_negative_by_year.png`, `rank_inversion_scatter_2023.png`
- `08_benchmark/memo/benchmarking_memo.md`  --  Memo 1
- `08_benchmark/memo/benchmarking_memo2_2026-07-02.md`  --  Memo 2
- `HANDOFF_pipeline_revision_june2026.md`  --  running pipeline log, to be updated once this is resolved
