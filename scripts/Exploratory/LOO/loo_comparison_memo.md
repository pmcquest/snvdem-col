---
output:
  pdf_document: default
  html_document: default
---
# Comparing Two LOO Analyses: `leave_one_out.R` vs. `2026_04_LOO_tests.qmd`

**Date:** July 9, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The SNVDEM-COL team

---

## What this checks

The co-author's `2026_04_LOO_tests.qmd` runs a parallel leave-one-out
analysis using the same 12-criterion formula lineage as `leave_one_out.R`,
but a different metric: correlation between the excluded-index and the full
index (does the relative ranking of municipalities survive), plus mean
absolute deviation, rather than this memo's signed mean-level shift. Both
scripts are now run against the same current production data
(`05_weighting/03_output/snvdem_col_weighted.rds`), so any difference below
is a metric difference, not a data difference.

## Results comparison

| Criterion | My magnitude rank | Their LOO-correlation rank | Their "explanatory" rank |
|---|---|---|---|
| compete / ruling_party | 1 | 2 | 6 |
| urban / rural_urban | 2 | 1 | 3 |
| prox_cap / capital | 3 | 7 | 7 |
| nonviolent / violence | 4 | 3 | 9 |
| nonindig / indigenous | 5 | 4 | 5 |
| econ_dev | 6 | 6 | 4 |
| nonremote / remote | 7 | 5 | 1 |
| south | 8 | 9 | **8 (negative)** |
| pop_density / sparse | 9 | 8 | 2 |
| east | 10 | 11 | 11 |
| west | 11 | 10 | **12 (negative)** |
| north | 12 | 12 | 10 |

("LOO-correlation rank" = ascending `cor_sndem`, i.e. rank 1 is most
disruptive when removed, matching the qmd's own sort order. "Explanatory
rank" = descending `|cor_with_sndem|` from the variance-decomposition
section -- this asks a third question, "does this criterion's own weighted
contribution track the final index," separate from either LOO approach.)

## Why the correlation metric understates how much actually moves

`cor_sndem` ranges 0.907-0.997 across all 12 criteria -- "all above .9,
nothing collapses" is a real, correctly-computed result. But it's worth being
precise about what it does and doesn't rule out. Correlation is invariant to
any change that doesn't reorder municipalities relative to each other --
including a uniform shift applied to everyone. And when you drop 1 of 12
comparably-weighted terms from a weighted average, the other 11 still
dominate the pattern, so a high correlation is close to the *expected*
outcome for this kind of exercise almost regardless of which one is dropped
-- it takes an unusually discordant criterion to move it much. That makes
"correlation stayed above 0.9" a weaker signal of robustness than it sounds:
it mostly tells you the relative ranking of municipalities survived, not
that the index's actual values didn't move.

The co-author's own script already computes the more direct answer to "how
much moved" -- `mad_sndem` (mean absolute deviation), a magnitude measure
much closer in spirit to this memo's approach. It's worth leading with that
number alongside (or instead of) the correlation when reporting "how
impactful" a criterion is, since it answers the question a reader is
actually asking.

## `compete`/`urban` -- strong agreement across every version of this check

Both `leave_one_out.R` (level-shift) and the co-author's script
(rank-preservation) independently put `compete`/`ruling_party` and
`urban`/`rural_urban` at or near the top. Two different metrics agreeing on
the same two criteria, using two different questions, is a solid convergent
finding -- these two are influential in *both* the level sense and the
municipality-discriminating sense (Section 6 of `leave_one_out_memo.md`
walks through why both `compete` and `urban` also have the largest
municipality-to-municipality *spread*, not just the largest average effect).

## `capital` -- a genuine metric disagreement, not a data issue

This is the clearest case of the two approaches measuring different things.
`prox_cap`/`capital` is my 3rd-most-impactful criterion by level shift, but
ranks 7th (middle of the pack) by the co-author's correlation metric.
Reason: `prox_cap` has one of the smallest municipality-to-municipality
spreads in the whole set (SD 0.015, versus 0.030 for `compete`/`urban`) --
removing it shifts nearly every municipality's score by about the same
amount, whether that municipality sits next to Bogotá or 800km away. A
near-uniform shift barely moves a correlation (which only tracks whether
relative rankings change) while still producing a real, sizeable move in the
index's actual level -- exactly what the magnitude metric is built to catch.
Both rankings are correct; they're answering different questions, and
`capital` is the one criterion where that difference is large enough to
flip its apparent importance depending which question you ask.

## `south` -- real, and traceable to a geographic confound, not a sign error

The co-author's variance-decomposition check finds `south`'s weighted
contribution correlates *negatively* with the index (`cor_with_sndem` =
-0.19). Traced to source: the municipalities that score highest on the
`south` geopredictor cluster almost entirely in Amazonas, Putumayo, Vaupés,
Caquetá, Nariño, Guaviare, and Guainía -- Colombia's Amazon-basin and
southern-border departments. These same departments are, independently,
among the most remote, the most Indigenous-population-dense, the least
economically developed, and historically the most conflict-affected
municipalities in the whole panel (department-level check: mean `south`
score above 0.85 in all seven departments, versus 0.39 in Medellín's
department, Antioquia, at the other extreme).

That's the mechanism: `south` isn't carrying independent information about
some normatively distinct "southern-ness" -- geographically, it's a proxy
for the *same* cluster of municipalities that `nonremote`, `nonindig`, and
`econ_dev` already flag as less democratic under the current expert
weighting, because in Colombia those characteristics happen to be
concentrated in the same region. This is consistent with `south` having one
of the highest loadings on the shared PC1 dimension in
`leave_one_out_memo.md` Section 10 (0.48, tied for the highest of all 12) --
not because "south" is doing independent conceptual work, but because it's
geographically collinear with the criteria that are. Worth a sentence in the
paper's geopredictor discussion (the regional criteria aren't independent of
the development/remoteness ones in Colombia specifically), not a red flag
about the formula.

## Files

- `leave_one_out.R`, `leave_one_out_memo.md` -- this project's own LOO analysis
- `2026_04_LOO_tests.qmd` -- co-author's parallel analysis (original script)
- `coauthor_method_current_data.R` -- the co-author's `calc_loco`/`vd_df` logic,
  unmodified, run against current production data for this comparison
- `coauthor_LOO_Table.csv` -- that run's `final_table` output
