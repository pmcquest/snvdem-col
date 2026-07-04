---
output:
  pdf_document: default
  html_document: default
---
# Benchmarking Memo: `sndem` vs. `sndem_benchmark` for Within-Country Analysis

**Date:** June 2026  
**Prepared by:** PM, with assistance from Claude Code Sonnet 4.6  
**For:** Michael Coppedge and co-authors  

---

## 1. What the method produces and why

The snvdem index is built from two sources of information that serve different roles:

**Expert data (weights):** Four V-Dem survey questions ask country experts what characteristics are associated with (1) more free and fair elections, (2) less free and fair elections, (3) stronger civil liberties, and (4) weaker civil liberties. The proportions of coder agreement around each characteristic and question are synthesized into relevance weights for each dimension — elections (`snelect`) and civil liberties (`sncivlib`).

**Objective data (geopredictors):** Geocoded structural variables measured at the municipal level in Colombia (rurality, distance from capital, population density, violence indicators, etc.) are standardized using an empirical CDF applied across the full 2000–2023 panel. This maps each predictor to [0,1] representing its percentile within the full Colombian distribution.

**The intermediate index:** Expert weights are applied to the CDF-standardized geopredictors, producing a weighted average for each dimension. The paper's Equation 1 then defines:

```
sndem = 0.5 * (snelect + sncivlib)
```

Both subindices naturally fall on [0,1] because the inputs are CDF-standardized. The benchmark step (folder 08) is then applied to each dimension separately.

---

## 2. A critical reading of the operational strategy document

Two excerpts from the revised operational strategy (Jan 2026) are in tension with each other, and that tension is creating confusion around our data and benchmarking.

### Excerpt 1 — a conceptual definition (p. 1)

> "We conceive of free and fair elections at the municipal level as **a set of deviations centered around this summary subnational measure** [v2elffelr]. In order to benchmark the many municipal-level values, we need to determine (a) the maximum range of variation around the summary measure in the entire country in a given year and (b) how high or low each municipality is within that range."

This is making a **theoretical claim** about what municipal election quality *is*: a deviation from the national V-Dem subnational average. If taken at face value, this means `EL_col_mt` — each municipality's position expressed as a deviation from `v2elffelr` within the expert-estimated national range — is the conceptually intended final measure, not an optional calibration.

This matters for temporal interpretation: two municipalities with the same unbenchmarked `snelect = 0.6` in different years are not equivalent if the national V-Dem anchor (`v2elffelr`) changed between those years. The benchmark anchors each year's scores to the current national estimate, making scores comparable over time within Colombia as well as across countries.

### Excerpt 2 — a stated purpose (p. 3)

> "To complete the calibration of the data **for cross-national comparisons**, the subnational estimates such as those we have generated for Colombia can be centered around the means of their respective series and rescaled to fit within their ranges."

This phrase appears to restrict the purpose of benchmarking to cross-national comparison only. But in context, "for cross-national comparisons" may be describing an additional payoff rather than the exclusive purpose. The preceding sentence reads: "For *both* civil liberties and free and fair elections, the characteristics of each location determine how much it varies within that maximum national range" — a general statement about the measure, not a cross-national-only claim.

### The tension

| Excerpt | Implication |
|---|---|
| "We conceive of free and fair elections as deviations centered around v2elffelr" | Benchmarked scores ARE the intended final measure — for any analytical use |
| "To complete the calibration for cross-national comparisons" | Benchmarking is supplementary, needed only for cross-national comparison |

These two readings lead to different practical conclusions about which variable to use for within-country mapping and regression.

---

## 3. The unsolved combination problem

Both excerpts describe the benchmark in terms of **individual dimensions**: `EL_col_mt` (elections on the V-Dem latent scale, ~[-3.5, 3.5]) and `CL_col_mt` (civil liberties on [0,1], anchored to `v2x_civlib`). The operational strategy document shows these as separate figures (Figures 4 and 5). It does not describe how to combine them into a single `sndem_benchmark`.

This is the implementation gap. Every approach Patrick tried in June 2026 produced distortions:

| Approach | Problem |
|---|---|
| Within-year `ecdf()` (original script) | V-Dem anchor mathematically cancels; national average fixed at 0.5 every year; temporal trend removed |
| Within-bounds normalization | Moving reference frame: V-Dem anchors rise with Colombia's improvement; CDF-based geopredictors do not; inverted trend |
| `pnorm(EL_col_mt)` | Trend direction preserved but non-linearly amplified (pnorm is steepest near 0, where Colombia's EL scores cluster) |
| Cross-panel linear normalization | Still exaggerated because EL and CL capture different magnitudes of absolute change |

**Root cause:** The geopredictors' CDF is computed over the full 2000–2023 panel, capturing cross-sectional relative position but not absolute temporal improvement. The V-Dem anchors (`v2elffelr`, `CLSNmean`) do track absolute improvement over time. Every normalization that tries to merge these two types of information either removes the temporal trend or distorts its shape.

---

## 4. A structural note on the geopredictors and time

Because the CDF is applied across the full panel (all municipality-years from 2000–2023), each predictor represents a municipality's *percentile rank within the full Colombian distribution*, not its absolute level. This design choice enables replication across countries with very different absolute levels — but it means `sndem` is primarily a cross-sectional measure. Apparent trends over time reflect changes in which municipalities rank higher or lower on the weighted predictors, not absolute democratic improvement.

This is arguably acceptable for within-country regression (which asks: "what predicts relative democratic quality across municipalities?") but creates difficulty when combined with a time-varying V-Dem anchor in the benchmark step. The benchmark is designed to capture absolute level, while the geopredictors capture relative rank — these are not the same thing.

---

## 5. Core questions requiring clarification

The following questions cannot be resolved by implementation choices alone. They require a methodological decision by the authors:

**Q1 — Is Excerpt 1 a theoretical commitment or a motivating framework?**

Does "we conceive of free and fair elections as deviations centered around v2elffelr" mean that `EL_col_mt` (the benchmarked score) is the *intended final measure* for all uses including within-country mapping and regression? Or is it describing the theoretical logic that motivates a calibration step whose outputs (`EL_col_mt`, `CL_col_mt`) are only needed when comparing across countries?

*This determines whether prior descriptive maps and regressions using `sndem` are using the correct variable.*

**Q2 — How should the two benchmarked dimensions be combined?**

The methodology documents describe `EL_col_mt` and `CL_col_mt` as outputs per dimension (separate figures) but do not specify how to combine them into a single `sndem_benchmark`. They are on incompatible scales by design (latent elections scale vs. [0,1] civil liberties scale). Any normalization step introduces either scale distortion or trend distortion. Is there a principled combination rule the authors intend?

*This is a calculation bottleneck.*

**Q3 — Does the CDF standardization limit temporal interpretation?**

The full-panel CDF makes `sndem` a cross-sectional measure. Is it the intent of the methodology that the index captures relative democratic quality at each point in time (cross-sectional validity), with temporal trends being a secondary and potentially noisy feature? Or should a different standardization approach be used to better preserve temporal information?

*This affects how time-series maps and trend figures in the paper should be framed.*

---

## 6. Practical recommendations pending clarification

**If Excerpt 1 is a theoretical commitment (benchmarking is required for all uses):**  
The combination problem (Q2) must be resolved before any maps or regressions proceed. The current `01_benchmark.R` outputs (`EL_col_mt`, `CL_col_mt`) are the right intermediate products; a principled combination rule is needed.

**If Excerpt 1 is a motivating framework (benchmarking is for cross-national use only):**  
Use `sndem = 0.5 * (snelect + sncivlib)` from the weighting script for all within-country analysis. Prior descriptive maps and regressions using this variable are methodologically sound. `EL_col_mt` and `CL_col_mt` are supplementary outputs for cross-national comparison.

**Either way**, the paper's current language should be made internally consistent: the theoretical framing in Excerpt 1 and the stated purpose in Excerpt 2 should not point in different directions.
