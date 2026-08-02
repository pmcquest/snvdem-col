# Memo: Why does the Colombia LISA map show so few outlier clusters?

**To:** Michael, Kelly
**From:** Patrick (drafted with Claude Code)
**Re:** Interpreting the sparse High-Low/Low-High outlier count in the sndem LISA map, and what it might mean for how we describe the method

## Background

We built a LISA cluster map of `sndem` (the subnational democracy index) for Colombia,
2011, at the municipio level (queen contiguity weights, Benjamini-Hochberg FDR-corrected
p < 0.05 — see `03_lisa_map_2011.R`). The map shows a clean, compact High-High cluster
around Bogota/Cundinamarca/Boyaca and a large Low-Low cluster across the periphery, with
almost no High-Low or Low-High outliers: only 3 HL and 2 LH municipios out of 1,122
(`output/lisa_clusters_2011.csv`).

Comparing this against a canonical LISA illustration Patrick found (a London map of the
local proportion of part-time workers, from a Stack Exchange post on interpreting LISA
maps) raised a question: that map shows pink (HL) and dark-blue (LL) outlier pixels
scattered liberally throughout and along the edges of both of its main clusters —
visually maybe 15-20% of the colored area — versus under 0.5% in ours. Is our LISA
analysis missing something, or is this a real difference?

## What we checked

**The two maps are structurally more similar than the outlier density suggests.** In
both, the two main clusters never touch directly — they're separated by a broad band of
statistically insignificant units, not by a ring of HL/LH outliers. We confirmed this
directly for our data: 0 of 919 High-High neighbor-relationships are with a Low-Low
municipio (`04_lisa_adjacency_check.R`). So the "buffer" in both maps is real, it's just
made of non-significant territory rather than outlier categories.

**Significance threshold explains some, but not most, of the density gap.** The London
map looks like a standard tutorial treatment reporting raw p < 0.05 without correcting
for running ~1,000+ simultaneous local tests. We can measure what that costs us: our own
raw-p version of the 2011 map still only has 6 HL + 5 LH out of 1,122
(`02_lisa_raw_vs_fdr_comparison.R`, `output/lisa_raw_vs_fdr_comparison.csv`) — nowhere
near London's apparent density. Threshold choice is a real factor but doesn't fully
account for the gap.

## The more compelling explanation: sndem may be smoother by construction

"Proportion of part-time workers" is a raw, directly-tabulated administrative statistic.
It can swing sharply between adjacent small areas because of one large local employer,
one housing estate, one transit catchment — genuine, idiosyncratic local noise, which is
exactly the kind of pattern that shows up as HL/LH outliers in a LISA test.

`sndem` is not tabulated directly — it's a **model-based estimate**, built from expert
survey assessments combined with geopredictors (rurality, distance/access to the
capital, indigenous-territory status, presence of illicit activity, etc. — the `el_*`
and `cl_*` columns in `snvdem_col_weighted.rds`). Several of those geopredictors are
themselves spatially patterned (e.g., "remote," "rural," "north/south/east/west,"
"inside/outside capital"). If the weighting procedure leans on predictors that are
spatially correlated, the resulting `sndem` estimates could inherit smoothness across
neighboring municipios as a byproduct of the estimation method — independent of how
much genuine local variation exists in Colombia's actual subnational democracy.

As such, the sparse LISA outlier count partly reflects a property of *how we
measure* subnational democracy, not solely a property of Colombia's underlying
democratic landscape. That's a meaningfully different claim from "Colombia's spatial
structure is unusually clean," and it changes how we'd want to caveat the LISA map in
the paper.

## What this doesn't resolve

We have not verified how much smoothing the weighting procedure actually induces, or
distinguished that from genuine spatial regularity in Colombian subnational democracy.
That requires looking at the `05_weighting` methodology directly — how geopredictors
enter the estimation, whether spatial autocorrelation is explicit or incidental, and
whether a comparable check exists (e.g., comparing the smoothness of `sndem` against a
component that's closer to a raw tabulation, if one exists in the pipeline). This is
really a question for Michael, since he built that methodology, rather than something
we should assert from the LISA output alone.

## Suggested next step

Before finalizing how the LISA section describes this pattern (or characterizes it as a
finding about Colombia specifically), it would help to have Michael weigh in on whether
and how much the weighting procedure's use of geopredictors would be expected to smooth
`sndem` spatially, independent of the underlying democracy data.
