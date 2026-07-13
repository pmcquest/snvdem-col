---
output:
  pdf_document:
    toc: true
    toc_depth: 2
  html_document:
    toc: true
    toc_depth: 2
header-includes:
  - \usepackage{float}
  - \floatplacement{figure}{H}
---
# Leave-One-Out Sensitivity Analysis: How Much Does Any Single Characteristic Drive the SNVDEM Index?

**Date:** July 9, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The SNVDEM-COL team
**Ties to:** `v3_ToDo_REORGANIZED.md` items A1 ("which characteristics drive the index") and A2 ("leave-one-out recalculation")

---

## 1. The question this memo answers

CIPS, Rosas (MPSA), and LASA discussants converged on one question about the
index: **how much does any single geopredictor -- especially development and
distance/remoteness -- drive the index, and is the index more than repackaged
modernization theory?**

MC's guidance on how to answer it (email, referenced in the ToDo):

> "We don't need to refute this, as we make no claims that the experts have
> specific knowledge of each subnational unit; they are simply stating their
> intuitive understanding of the correlates of subnational democracy. Of
> course modernization theory influences those prior beliefs! It's not a
> problem if it does. All we have to show is that modernization variables are
> not the only variables they take into account. The leave-one-out analysis
> does that. We can mention that one implication is that it's not just
> modernization variables that matter."

Our task is not to prove the experts are innocent of
modernization-theory priors (they probably aren't, and that's fine), but to
show empirically that those priors aren't *doing all the work* in the index.
Leave-one-out (LOO) analysis can respond to this concern.

## 2. What kind of analysis this is

LOO asks how much our SNVDEM index moves when a criterion is left out. Variation depends on both the weight and how that criterion's real data compares to the rest of the formula. 
Mechanically, LOO takes each characteristic, removes each one at a time, and recalculates the SNVDEM index to understand the relative influence it has on the output.
We can visualize each characteristic's influence using a **tornado diagram** -- essentially a sensitivity-analysis chart that shows variation when one input factor is removed at a time, measuring how much the output changes, and sorting the bars largest-to-smallest so the biggest effect is on top.

## 3. Data at a glance

| | |
|---|---|
| Municipalities | 1,122 |
| Years | 24 (2000-2023) |
| Municipality-years | 26,928 |
| `sndem` (composite index) | mean 0.342, SD 0.068, range [0.121, 0.556] |
| `snelect` (elections) | mean 0.331, SD 0.071 |
| `sncivlib` (civil liberties) | mean 0.352, SD 0.068 |

## 4. Data used

This analysis uses the **pre-benchmarked** index -- `snelect`/`sncivlib`/`sndem`
from `05_weighting/03_output/snvdem_col_weighted.rds` -- not the benchmarked
`sndem_final`. Two short reasons: (1) the pre-benchmarked measure is the one for within-Colombia relative comparison; (2) LOO drops a term from the
weighted-average numerator/denominator built in Step 5
(`01_weighting_geopredictors.R`), a decomposition that doesn't survive into
the benchmarked measure's further transformation (deviation from a V-Dem national
anchor, `qnorm()`, global standardization).

## 5. Step by step: how the calculation works

**Step 1 -- group the 16 V-Dem relevance criteria into 12 interpretable
criteria.** Four of the 16 (Urban/Rural, More/Less development, Inside/Outside
capital, Ruling-party strong/weak) are opposite ends of the same underlying
predictor, split at the median into "hi" and "lo" versions in Step 3 of the
pipeline. This analysis groups each hi/lo pair back into one criterion (e.g.
`urban` = Urban + Rural) for reporting, after confirming empirically that each
pair shares an *identical* missingness pattern -- so grouping before or after
the missingness check gives the same answer.

**Step 2 -- reconstruct each criterion's numerator and denominator terms**,
using the same predictor x weight products as the production formula
(`01_weighting_geopredictors.R`):

```r
num_emel$urban <- avg0t1hi * el_Urban + avg0t1lo * el_Rural
den_emel$urban <- el_Urban + el_Rural
# ... one such pair per criterion, x2 for EMEL (elections) and CSCW (civil liberties)
```

**Step 3 -- recompute the index with and without each criterion**, using the
same drop-missing/renormalize/floor logic as the production
`weighted_avg_narm()` function (not a plain sum -- see Section 7 for why that
distinction matters):

```r
weighted_avg_narm_grouped <- function(num_df, den_df, min_weight_fraction = 0.5) {
  num_mat <- as.matrix(num_df); den_mat <- as.matrix(den_df)
  den_avail <- den_mat
  den_avail[is.na(num_mat)] <- NA          # drop a missing criterion's weight too, not just its score
  num <- rowSums(num_mat, na.rm = TRUE)
  den <- rowSums(den_avail, na.rm = TRUE)
  full_den <- rowSums(den_mat)             # weight mass if nothing were missing
  result <- num / den                      # renormalized average over what's left
  result[(den / full_den) < min_weight_fraction] <- NA  # too little weight mass left -> NA
  result
}

full_index   <- weighted_avg_narm_grouped(num_emel, den_emel)             # all 12 criteria
loo_index_cn <- weighted_avg_narm_grouped(num_emel[-cn], den_emel[-cn])   # criterion cn dropped entirely
```

This is run once per criterion (12 times), for both `snelect` (elections) and
`sncivlib` (civil liberties); `sndem` is `0.5 * (snelect + sncivlib)` in both
the full and LOO versions, matching the production formula exactly.

**Step 4 -- the impact is the difference:**

```r
Impact_sndem <- loo_sndem - sndem_recon   # (index without this criterion) - (index with it)
```

computed for every one of the 1,122 municipalities x 24 years x 12 criteria
(323,136 rows).

## 6. Two ways a criterion can be "influential"

Before looking at results, it's worth separating two properties that a LOO
impact can capture, since they answer different questions and don't always
agree:

- **Level (mean impact):** how much, *on average*, does removing this
  criterion shift the index? This is what the tornado ranking (Section 8)
  sorts by.
- **Variation (spread -- SD, IQR, P05-P95):** does the criterion's impact
  *differ from one municipality-year to the next*, or does it shift nearly
  everyone by about the same amount?

These aren't the same question, and a criterion can score high on one and low
on the other. The clean way to see the difference: imagine a company gives
every employee a flat $500 raise, versus gives some employees $2,000 and
others nothing. Both changes move the *average* salary by a similar amount
(both score high on level). But only the second one changes who out-earns
whom (only the second scores high on variation). A metric that only checks
"did anyone's relative position change" -- e.g. a correlation between the
index with and without a criterion -- would call the flat raise close to a
non-event, even though the same total amount of money moved.

In this data, `compete` and `urban` score high on *both* -- they have the
largest average effect on the index and the widest spread across
municipalities (Section 8). `prox_cap` is the interesting split case: third
by average effect, but one of the smallest spreads (SD 0.015 vs. 0.030 for
`compete`/`urban`) -- nearly every municipality gets nudged by about the same
amount when it's removed, regardless of how close or far it actually is from
the capital. It moves the index a lot; it barely discriminates between
municipalities. Section 8 reports both.

## 7. Validation

- **Reconstructing the full index from these terms matches the saved
  `snelect`/`sncivlib`/`sndem` columns to floating-point precision**
  (max abs diff = 1.11e-16) -- confirming the numerator/denominator terms
  above are the actual production formula, not an approximation.

## 8. Results: which characteristics move the index the most

![Composite index (sndem), magnitude only](imgs/leave_one_out_sndem.png)

**Interpretation.** Each box summarizes one criterion's
`Impact_sndem` across all 26,928 municipality-years: the box spans the 25th to
75th percentile (the middle half of observations), the line inside is the
median, the whiskers extend to the most extreme value within 1.5x the box's
width, and the dots beyond the whiskers are individual municipality-years
further out than that. Criteria are sorted by `|mean impact|`, largest at the
top -- the tornado ordering.

`compete`, `prox_cap`, and `econ_dev` have boxes entirely above
zero (excluding them raises the index for the middle 50% of municipality-years
without exception), and `east` -- the smallest-magnitude criterion in the
whole set -- has its box entirely *below* zero, just easy to miss at this
scale since the box is so short. Every other criterion's box straddles zero:
even for a criterion like `nonviolent` with a clearly negative *average*
impact, a meaningful share of individual municipality-years show the opposite
sign. Concretely, dropping `compete` raises the index in 76.5% of
municipality-years (not literally all), dropping `urban` in 67.5%, and
dropping `east` lowers it in 85.5%. The average is a real pattern, but it's a
tendency across thousands of municipality-years, not a uniform rule.

![Elections (EMEL) vs. civil liberties (CSCW), mean |impact|](imgs/leave_one_out_pillars.png)

| Criterion | Mean impact (sndem) | Median | SD | P05 | P95 |
|---|---|---|---|---|---|
| compete | +0.0237 | +0.0239 | 0.0300 | -0.0265 | +0.0698 |
| urban | +0.0180 | +0.0182 | 0.0310 | -0.0310 | +0.0666 |
| prox_cap | +0.0165 | +0.0150 | 0.0150 | -0.0062 | +0.0430 |
| econ_dev | +0.0125 | +0.0129 | 0.0151 | -0.0129 | +0.0362 |
| nonviolent | -0.0136 | -0.0136 | 0.0241 | -0.0512 | +0.0241 |
| nonindig | -0.0126 | -0.0101 | 0.0203 | -0.0469 | +0.0163 |
| nonremote | -0.0117 | -0.0121 | 0.0169 | -0.0397 | +0.0154 |
| south | -0.0076 | -0.0028 | 0.0120 | -0.0323 | +0.0031 |
| pop_density | -0.0075 | -0.0075 | 0.0127 | -0.0268 | +0.0121 |
| east | -0.0044 | -0.0021 | 0.0073 | -0.0169 | +0.0008 |
| west | -0.0032 | -0.0024 | 0.0077 | -0.0184 | +0.0062 |
| north | -0.0023 | -0.0013 | 0.0054 | -0.0138 | +0.0043 |

(Full table: `leave_one_out_summary.csv`.)

**What a mean impact of +0.0237 (`compete`) actually looks like in the
data.** It's an average, not a uniform shift -- concretely, for 2023:

- A municipality (`19845`) with a full-index score of 0.409 becomes 0.502
  without `compete` (+0.093, one of the largest single moves that year); a
  municipality (`94884`) with a full-index score of 0.240 becomes 0.174
  (-0.066, one of the largest drops). A third (`88001`, San Andrés) moves
  from 0.382 to 0.405 -- almost exactly the +0.0237 mean.
- 76.5% of municipality-years move up when `compete` is excluded, 23.5% move
  down -- most, not all, and the ones that move down aren't rare noise (see
  Section 8's box-and-whisker discussion above).
- In SD terms, +0.0237 is about a third of `sndem`'s own cross-sectional
  spread (SD 0.068, Section 3) -- for a municipality sitting exactly at the
  median, a shift that size moves it from the 50th percentile to roughly the
  **62nd-63rd percentile** of the full distribution. Real, but not
  dramatic -- nobody jumps from bottom decile to top decile on the strength
  of one criterion.
- Splitting the two dimensions: `compete`'s effect is similar in size on
  both (Mean_EMEL +0.026, Mean_CSCW +0.022) -- not concentrated in elections
  or civil liberties specifically.
- **Does the relative ranking of municipalities change, not just the
  average?** Yes, more than the aggregate correlation suggests. Within 2023
  alone, the Spearman rank correlation between the full and `compete`-excluded
  ranking is 0.96 -- high, as expected for dropping 1 of 12 terms (Section 11
  above) -- but the *average* municipality still moves 68 rank positions (out
  of 1,122), and 581 of 1,122 municipalities (52%) move by more than 50
  positions. A correlation of 0.96 and "half the panel reshuffles by 50+
  ranks" are both true at once -- the aggregate correlation summarizes away
  a lot of real movement underneath it.

`compete` and `urban` lead on both level
*and* variation (largest mean impact and largest SD, ~0.030 each) -- they move
the index's overall level the most and discriminate the most between
municipalities. `prox_cap` is influential in level but comparatively uniform
(SD 0.015, narrow P05-P95) -- it nudges the index a similar amount almost
everywhere. The four regional criteria (north/south/east/west) are modest on
both counts.

The signed value behind each box above (does
excluding a criterion raise or lower the index, *on average*) is driven
mostly by which of two predictor-encoding schemes a criterion belongs to --
eight criteria are single rank-standardized predictors that average ~0.50 by
construction, while the other four (`urban`, `econ_dev`, `prox_cap`,
`compete`) are hi/lo pairs that zero-fill the inactive half of each row,
structurally capping their own average well below 0.50 regardless of the
data. Every one of the 12 underlying predictors correlates *positively* with
`sndem` with no exceptions, so this average-level sign shouldn't be read as
"this criterion is good or bad for the index." Both figures above show
magnitude only for this reason; see Section 12 for detail and
`leave_one_out_standardized_check.csv` for the supporting check.

## 9. Mechanics in practice: three time-trend stories

Ranking criteria across the full 2000-2023 panel is useful. Three
criteria's year-by-year trajectories provide a clear illustration of *how*
LOO impact actually gets generated.

![Mean LOO impact over time, three criteria](imgs/leave_one_out_time_trend.png)

For any criterion that isn't a hi/lo split (8 of the 12), the V-Dem weight
cancels out of its own num/den ratio algebraically -- e.g. `nonviolent`'s own
score is `avg10t11 * wt_el_1011 / wt_el_1011`, which reduces to `avg10t11`
exactly. So its "own score" is just the Step-3 CDF geopredictor value,
independent of the Step-4 V-Dem weight, and a LOO trend for one of these
criteria can come from the geopredictor changing over time, the weight
changing, or both. Both need checking before crediting either one alone --
the `nonindig` story below is a case where the first version of this analysis
credited the wrong one.

**`nonviolent` -- impact grows alongside both its weight and the underlying
violence data, moving together.** Its mean impact is close to zero in
2000-2002, becomes increasingly negative through 2018 (-0.003 to -0.026), then
eases back through 2023 (-0.015). Two things move in step with it: `wt_el_1011`
(the relevance weight) rises from 0.25-0.33 in 2000-2004 to 0.54 by 2005-2017
and spikes to 0.77 in 2018 before easing to 0.59 by 2023; and `avg10t11` (the
criterion's own geopredictor -- Colombia's municipalities scoring steadily
more "nonviolent," on average, over the same stretch) rises from 0.41 in 2000
to 0.61 by 2017. Both correlate strongly with the yearly impact (weight:
r = -0.91; own predictor: r = -0.95), and the two are correlated with each
other too, so this isn't purely a weight story -- a real trend in the
underlying violence data and a rising relevance weight are reinforcing each
other.

**`nonindig` -- impact shrinks in two distinct phases, and the first is a
data trend, not a weight effect.** Mean impact shrinks steadily from -0.032
in 2000 to about -0.006 by 2017, then continues shrinking more slowly to
-0.003 to -0.005 by 2018-2023. Its own score is `avg14` exactly (same
cancellation as above) -- the CDF geopredictor itself, matching the declining
trend in `03_geocoded_panel/01_clean_geocoded/imgs/CDF_avgs.png`. `avg14`
falls from 0.68 in 2000 to 0.44 by 2017, while the weight (`1 - el_Indigenous`)
stays flat over that same stretch (0.67-0.79, no trend) -- so the 2000-2017
shrinkage is a **data effect**: the geopredictor is converging toward the rest
of the formula's typical level, not a change in how much weight it carries.
From 2018 on, `avg14` flattens (0.44-0.44, no further movement) while
`el_Indigenous` jumps from 0.33 to 0.54 -- and because the production formula
uses `1 - el_Indigenous` for this criterion's weight (`benchmarking_memo2_2026-07-06.md`
Section 6.1, an already-flagged open question), the effective weight now falls
sharply (0.67 to 0.46) and drives the smaller, residual fluctuation in this
later period. The first 18 years of the trend are a geopredictor effect; only
the last 6 show the weight mechanism. This ranking should be treated as
provisional for `nonindig` until Section 6.1 is resolved.

**`compete` -- its swings track its own average score, which is
driven by the underlying data, not by its weight.** Unlike `nonviolent`,
`compete`'s relevance weight (`el_Ruling_party_strong`/`weak`) stays
comparatively flat across 2005-2021 (0.25-0.33), so it doesn't explain the
impact's much larger swings (0.009 to 0.044, peaking in 2010-2013, dipping in
2014-2017, rising again in 2018-2021). Tracing it one level deeper: `compete`'s
own average score (its own numerator/denominator ratio, before comparing to
the rest of the formula) moves substantially over the period -- from ~0.30 in
2000-2004, down to ~0.135 in 2010-2013 (its lowest point, coinciding exactly
with the impact's peak), back up to ~0.29-0.30 in 2014-2017, down again to
~0.23 in 2018-2021, and up to ~0.31-0.34 by 2022-2023. Since LOO impact is
driven by how far a criterion's own score sits from the rest of the formula,
a lower own-score mechanically produces a larger positive impact -- which is
exactly the inverse relationship observed here. Digging into *why* the own
score moves that much: the underlying predictor's cross-municipal average
(`avg15t16hi`, the "ruling party strong" side of the split) itself swings from
~0.52 (2000-2001) down to ~0.09 (2010-2013) and back up to ~0.64 (2022-2023) --
a much larger swing than the relevance weight ever shows. So `compete`'s
story is data-driven rather than weight-driven: the mix of municipalities
V-Dem coders would currently code as having a strong/dominant ruling party
changed substantially across the period, and that change (not a change in how
much coders say the criterion matters) is what moves `compete`'s LOO impact.
This coincides plausibly with Colombia's Uribe-Santos-Duque transitions, but
that's a hypothesis worth testing further.

## 10. Validation check: is this just modernization theory?

Section 1 posed the question directly: is the index just repackaged
modernization theory, with expert weights doing little more than encoding
"democracy is weak where it's rural, poor, and remote"? A pooled PCA on the 12
criteria's own predictor scores (recombining each hi/lo split back into one
continuous variable first) gives a one-number-style answer, following the
same logic the ToDo doc proposed for item A4.

- **No single dominant factor.** PC1 explains only 23.0% of variance across
  the 12 criteria (PC2: 19.0%, PC3: 14.7%, PC4: 9.9%). If the 12 criteria were
  mostly one underlying "development" dimension, PC1 would explain much more.
  Instead the variation is genuinely multidimensional.
- **PC1 isn't a "development" axis -- it's closer to a regional-geography
  axis.** The strongest loadings on PC1 are the regional criteria (`south`
  0.48, `west` 0.45, `north` 0.31) plus `nonindig` (0.35) and `nonviolent`
  (0.28). The criteria one might expect to dominate a "modernization"
  component -- `econ_dev` (0.25), `prox_cap` (0.23), `compete` (0.09), `urban`
  (0.07) -- load comparatively weakly.
- **PC1 correlates moderately, not overwhelmingly, with the index.**
  `cor(PC1, sndem) = -0.50` (r-squared ~0.25) -- real, but far from the index
  being a restatement of this one shared dimension.
- **The direct test MC's guidance calls for: does LOO-influence rank line up
  with PC1-loading rank?** It doesn't -- if anything, it runs the opposite
  way (Spearman correlation = **-0.50**). The criteria that matter *most* by
  LOO (`compete`, `urban`, `prox_cap` -- ranks 1-3) have among the *weakest*
  PC1 loadings (ranks 11, 12, 8). The criteria that dominate the shared latent
  dimension (`south`, `west`, `nonindig`, `north` -- PCA ranks 1-4) are
  middling-to-low in LOO influence (ranks 8, 11, 5, 12).

| Criterion | LOO rank | PC1 \|loading\| rank |
|---|---|---|
| compete | 1 | 11 |
| urban | 2 | 12 |
| prox_cap | 3 | 8 |
| nonviolent | 4 | 5 |
| nonindig | 5 | 3 |
| econ_dev | 6 | 7 |
| nonremote | 7 | 6 |
| south | 8 | 1 |
| pop_density | 9 | 10 |
| east | 10 | 9 |
| west | 11 | 2 |
| north | 12 | 4 |

The criteria driving the
actual index score are not simply the ones that would dominate a naive
shared-covariation ("modernization") composite -- if anything, the two
rankings are inversely related.

**Robustness check: do the four time-invariant regional dummies distort
this?** `north`/`south`/`east`/`west` have zero within-municipality variation
across the whole panel (confirmed: max within-municipality SD = 0 for all
four) -- a municipality's region never changes year to year, unlike the other
8 criteria, which vary both between *and* within municipalities over time.
That structural difference could pull a pooled PCA somewhere unrelated to
development. Re-running the PCA on the remaining 8 criteria: PC1 explains
*more* variance without them (28.1% vs. 23.0%), and its loadings now read as a
much more recognizable development/remoteness axis (`nonremote` 0.59,
`pop_density` 0.48, `econ_dev` 0.39, `prox_cap` 0.38 dominate; `compete` 0.03,
effectively zero). `cor(PC1, sndem)` strengthens from -0.50 to -0.81. And the
LOO-rank-vs-PCA-rank comparison gets *more* inverse, not less: Spearman
correlation drops from -0.50 to **-0.86** -- `compete` (LOO rank 1) is now
dead last (PCA rank 8 of 8) on the cleaner development axis.

| Criterion | LOO rank | PC1 \|loading\| rank (no NSEW) |
|---|---|---|
| compete | 1 | 8 |
| urban | 2 | 6 |
| prox_cap | 3 | 4 |
| nonviolent | 4 | 7 |
| nonindig | 5 | 5 |
| econ_dev | 6 | 3 |
| nonremote | 7 | 1 |
| pop_density | 8 | 2 |

Excluding NSEW doesn't undermine the Section 10 finding -- it sharpens it.
Read as a single exploratory check (one method choice for recombining the
hi/lo predictors) worth confirming with the team before it goes in the paper,
not a settled result -- but it's a promising, ready-made answer to the
CIPS/Rosas critique.

## 11. Comparison with a parallel analysis

A co-author independently ran a LOO analysis (`2026_04_LOO_tests.qmd`) using
the same 12-criterion formula lineage as this one, but a different metric:
correlation between the excluded-index and the full index (does the relative
ranking of municipalities survive), plus mean absolute deviation, rather than
this memo's signed mean-level shift. Running both approaches on the current
production data side by side (full detail and results table in
`loo_comparison_memo.md`):

- **Strong agreement on the top criteria.** Both approaches independently
  rank `compete`/`ruling_party` and `urban`/`rural_urban` as the two most
  consequential criteria to exclude -- two genuinely different metrics
  landing on the same answer.
- **A real, informative disagreement on `prox_cap`/`capital`.** This memo
  ranks it 3rd by level impact; the correlation-based approach ranks it much
  lower (middle of the pack). This is the Section 6 level-vs-variation split
  in action: `prox_cap` shifts nearly every municipality by a similar amount
  (low SD), which barely moves a correlation-based metric (invariant to
  near-uniform shifts) while still producing a real, sizeable level change.
  Neither ranking is wrong -- they're answering different questions.
- **A finding worth flagging: `south`'s weighted contribution correlates
  *negatively* with the index** on current data (the co-author's variance-
  decomposition check). Traced to source: the municipalities scoring highest
  on the `south` geopredictor cluster almost entirely in Amazonas, Putumayo,
  Vaupés, Caquetá, Nariño, Guaviare, and Guainía -- the same departments that
  are simultaneously the most remote, the most Indigenous-population-dense,
  among the least economically developed, and historically the most
  conflict-affected in the data. `south` is highly confounded with that whole
  cluster geographically, which is consistent with its high PC1 loading in
  Section 10's PCA (0.48, among the highest of all 12) -- not evidence that
  "south" carries independent information beyond what `nonremote`, `nonindig`,
  and `econ_dev` already capture, but a reminder that Colombia's regional
  criteria aren't geographically independent of its development/remoteness
  ones.

## 12. Limitations and open dependencies

- This entire analysis reflects the weighting formula **as it currently
  stands**, including the unresolved double-inversion question
  (`benchmarking_memo2_2026-07-06.md` Section 6.1) and two other open items
  in that memo's Section 6 (`CLSNyrmean` centering, `snlsff_2` reuse) -- none
  of the latter two affect the pre-benchmarked measure used here, but the
  double-inversion one does, directly, for 5 of the 12 criteria (`nonindig`,
  `Ruling_party_strong`/`weak` inside `compete`, `Inside`/`Outside_capital`
  inside `prox_cap`). Any resolution of that question will move those
  criteria's rank and trend in this memo.
- Mean impact ranks criteria by their effect on the index's level; it is not
  a variance-explained or discriminating-power measure -- Section 6/8 use
  SD/IQR/P05-P95 for that second sense.
- The composite- and pillar-level mean impact's *sign* is not a reliable
  signal for "is this criterion good or bad for the index" (Section 8's
  technical note) -- a standardized check (each criterion rescaled to a
  common footing before recomputing LOO) confirms the raw-scale sign doesn't
  hold up once that scale difference is removed; see
  `leave_one_out_standardized_check.csv` for the full check. Both figures
  show magnitude only for this reason. If a direction claim is needed
  anywhere, use a simple correlation-with-`sndem` check instead (every
  predictor is positive, no exceptions).
- Per the original ToDo note, a post-hoc regression add-on is still deferred;
  this memo implements the core leave-one-out recalculation plus the PCA
  diagnostic explicitly requested for the modernization-theory question.
- The Section 10 PCA check is exploratory and hasn't been reviewed by the
  team -- treat the ranking comparison as suggestive, not final, until
  discussed.

## 13. Files

- `leave_one_out.R` -- the analysis script (Sections 5-10 of this memo)
- `tornado.R` -- the separate weight-share diagnostic (Section 2)
- `leave_one_out_summary.csv` -- full results table (Section 8)
- `leave_one_out_standardized_check.csv` -- the sign/scale check referenced in
  Sections 8 and 12
- `leave_one_out_pca_check.csv`, `leave_one_out_pca_check_noNSEW.csv` -- the
  PCA validation check and its NSEW-excluded robustness check (Section 10)
- `loo_comparison_memo.md`, `coauthor_LOO_Table.csv`, `2026_04_LOO_tests.qmd`
  -- the parallel analysis referenced in Section 11
- `../../../../data/panel/03_geocoded_panel/01_clean_geocoded/imgs/CDF_avgs.png`
  -- geopredictor trends over time referenced in Section 9
- `imgs/leave_one_out_sndem.png`, `imgs/leave_one_out_pillars.png`,
  `imgs/leave_one_out_time_trend.png` -- figures embedded above
- `../../../../data/panel/05_weighting/03_output/snvdem_col_weighted.rds` --
  input data (pre-benchmarked index)
- `../../../../data/panel/06_benchmark/04_memo/benchmarking_memo2_2026-07-06.md`
  -- pre- vs. post-benchmark methodology and the open questions referenced in
  Sections 9 and 11
