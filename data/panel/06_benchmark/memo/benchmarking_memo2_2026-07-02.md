---
output:
  pdf_document: default
  html_document: default
---
# Benchmarking Memo 2: Two Measures of Subnational Democracy  --  Calculation, Interpretation, and Use

**Date:** July 2, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** Michael Coppedge and the SNVDEM-COL team

---

This memo follows directly from Michael's reply to the June 26 benchmarking memo. That memo (attached) laid out a scale-mismatch problem: `EL_col_mt` (elections, V-Dem's latent scale) and `CL_col_mt` (civil liberties, [0,1] scale) couldn't be averaged into `sndem_final` without distorting the trend. Michael's reply resolved the open questions  --  benchmarked scores are the intended final measure for all uses, and the two dimensions should be combined by converting `CL_col_mt` to a Z-score with `qnorm()`.

This memo implements that and lays out where things stand: we now have two complete, validated measures of Colombian subnational democracy  --  an **unbenchmarked** measure and a **benchmarked** measure  --  built for different purposes. Below: how each is calculated, what each tells us, and how I suggest presenting both in the paper.

---

## 1. The unbenchmarked measure (`sndem`)

Built in two steps:

**Step 1 -- CDF and reorient (Folders 03 and 05).** Each municipality's geographic and structural predictors (rurality, distance from the capital, violence indicators, population density, etc.) are standardized against the empirical distribution of *other Colombian municipalities* -- an empirical CDF, `rank(.) / length(.)` computed once across the full pooled 2000-2023 panel (`03_imputation_scripts/z2_merge_imputed_data.R:88`). Folder 05 (`CDF_averages.R`) then subsets to the 16 criteria, flips sign where needed so high values always mean *more* democratic, and collapses two redundant pairs -- still one column per predictor dimension (`avg0t1`, `avg2t3`, ... `avg15t16`), not yet combined into elections/civil liberties.

**Step 2 -- weight and combine (Folder 07).** `07_weighting/01_weight_predictors.R` joins those per-criterion averages to V-Dem's coder-derived relevance weights and combines them into the two dimensions, then the composite:

```r
# 07_weighting/01_weight_predictors.R
snelect  = sum(predictor_k * weight_k) / sum(weight_k)   # per municipality-year
sncivlib = sum(predictor_k * weight_k) / sum(weight_k)   # civil liberties analog
sndem    = 0.5 * (snelect + sncivlib)
```

**What it represents:** each municipality's position *relative to the rest of Colombia*. It is a within-country, cross-sectional measure  --  well suited to answering "which kinds of municipalities look more or less democratic than others in Colombia, and why." It reflects real temporal change to the extent the underlying geopredictors and expert weights shift year to year, but it is not anchored to any external, absolute reference point. 

(Because the percentile ranks are computed once against the full pooled 2000–2023 panel rather than recomputed within each year, the national average is free to rise or fall over time, unlike a within-year ranking, which would pin it at 0.5 by construction every year.)

![National mean unbenchmarked scores, min-max normalized to 0-1](../avgtrends_unbenchmarked.png)

The national mean of each index (min-max normalized across all municipality-years for readability) falls from about 0.58-0.60 in 2000 to a low around 0.47-0.49 in 2006-2007, recovers through the 2010s, and reaches a peak around 0.65-0.68 in 2022 before a slight pullback in 2023 -- elections and civil liberties track each other closely throughout, with elections modestly ahead from 2016 on. Main finding here: more municipalities' weighted predictor values have moved toward the higher-percentile end of the pooled 2000-2023 distribution over time, i.e. relative standing has shifted upward nationally. It is not evidence that Colombia has moved toward a higher external absolute score of democracy -- that is what Section 2's benchmarked measure is for.

---

## 2. The benchmarked measure (`sndem_final`)

**Step 1  --  map each municipality onto the V-Dem scale.** Each municipality's unbenchmarked score is re-expressed as a deviation from the actual V-Dem national estimate of two key variables (v2elffelr and v2x_civlib) for that year, scaled by how much the country is estimated to vary internally:

```r
# 08_benchmark/01_benchmark.R
EL_col_mt = v2elffelr + (snelect  - snelectyrmean) * weighted_range / ELrange_975_025
CL_col_mt = CLSNmean  + (sncivlib - CLSNyrmean)   * wtdCL_range   / CLrange_975_025
```

Precisely, the two municipal-level anchors are built differently, which matters for how to read them:

| Dimension | Municipal anchor | Built from |
|---|---|---|
| Elections | `v2elffelr` | V-Dem's "Subnational elections free and fair" -- latent scale, ~[-3.5, 3.5] |
| Civil liberties | `CLSNmean = v2x_civlib * (100 - v2clsnlpct) / 100` | V-Dem's Civil Liberties Index (`v2x_civlib`, [0,1]) adjusted downward by `v2clsnlpct` -- V-Dem's own estimate of the percent of the population living under weaker civil liberties than the national figure |

`v2elffelr` is used as-is. `CLSNmean` is not the raw civil liberties index -- it's `v2x_civlib` discounted by how much of the population V-Dem's own coders judge to be experiencing weaker-than-national civil liberties.

**Why the two anchors are on different native scales.** `v2elffelr` is a V-Dem "component" (C) variable -- a single expert-coded question processed directly by the coder-level IRT measurement model, which outputs a continuous, unbounded, roughly-standardized latent scale (~[-3.5, 3.5]). Civil liberties has no equivalent single-question summary; the closest available variable, `v2x_civlib`, is a composite built through Bayesian factor analysis (BFA) over several underlying latent components. V-Dem's own methodology document is explicit that BFA composites get converted onto a [0,1] scale "using the cumulative distribution function of the normal distribution" (V-Dem Methodology v15, Coppedge et al., March 2025, sec. 2.3, p.6). `v2x_civlib` inherits that bound because it is a plain average of three such CDF-transformed subcomponents (`v2x_clphy`, `v2x_clpol`, `v2x_clpriv`), while `v2elffelr` never passes through a BFA step at all.

This matters for replication: the scale mismatch is not an artifact of our weighting code and can't be removed by reformulating the civil-liberties weights -- it's baked into which source variable is available, upstream of anything this pipeline does. `qnorm(CLSNmean)` below is therefore the *correct* way to place civil liberties on a scale comparable to elections, not an approximation: it inverts the same normal-CDF link function V-Dem names explicitly in their methodology.

`weighted_range`/`wtdCL_range` are coder-proportion-weighted estimates of how unevenly V-Dem experts judge Colombia's subnational units to vary:

```r
# 06_vdem_data/01_weighting.R
weighted_range = (snlsff_1 * HPD + snlsff_2 * 2 * HPD) / (snlsff_0 + snlsff_1 + snlsff_2)
```

(`snlsff_0/1/2` = counts of V-Dem coders judging Colombia's subnational elections "the same," "somewhat different," or "significantly different"; `HPD` = the width of V-Dem's uncertainty interval around `v2elffelr`. `wtdCL_range` is constructed the same way for civil liberties.)

**Step 2  --  combine elections and civil liberties on a common, globally comparable scale.** Per Michael's guidance, `CL_col_mt` is converted to a Z-score with `qnorm()`. Both dimensions are then standardized against fixed constants computed once from the full V-Dem country-year panel (all countries, 2000-2023)  --  not from Colombia's own data  --  so the scale means the same thing regardless of which country is being scored, which matters once this pipeline is extended beyond Colombia:

```r
v15_global      <- vdemdata::vdem %>% filter(year >= 2000, year <= 2023)
EL_global_mean  <- mean(v15_global$v2elffelr, na.rm = TRUE)
EL_global_sd    <- sd(v15_global$v2elffelr,   na.rm = TRUE)

# CL global reference must be built the same way as the CL municipal anchor (CLSNmean, i.e.
# v2x_civlib discounted by v2clsnlpct) or the two aren't comparable -- see note below.
v15_global      <- v15_global %>% mutate(CLSNmean_global = v2x_civlib * (100 - v2clsnlpct) / 100)
CLSNmean_valid  <- v15_global$CLSNmean_global[!is.na(v15_global$CLSNmean_global) &
                                                 v15_global$CLSNmean_global > 0 &
                                                 v15_global$CLSNmean_global < 1]
CLz_global      <- qnorm(CLSNmean_valid)
CL_global_mean  <- mean(CLz_global, na.rm = TRUE)
CL_global_sd    <- sd(CLz_global,   na.rm = TRUE)

CL_col_z    <- qnorm(CL_col_mt)
EL_col_gz   <- (EL_col_mt - EL_global_mean) / EL_global_sd
CL_col_gz   <- (CL_col_z  - CL_global_mean) / CL_global_sd
sndem_final <- 0.5 * (EL_col_gz + CL_col_gz)
```

**A scale asymmetry, found and fixed 2026-07-02.** An earlier version built `CL_global_mean`/`CL_global_sd` from the *raw, unadjusted* `v2x_civlib`, while Colombia's municipalities are anchored to `CLSNmean` -- the same index discounted by `v2clsnlpct` (Section 2 above). Elections was apples-to-apples (`v2elffelr` used identically at both the municipal and global level); civil liberties was not. Since `v2clsnlpct > 0` for virtually every country (global mean ~37%), that mismatch alone pulled Colombia's `CL_col_gz` down by about **0.94 global standard deviations**, independent of any real difference in civil liberties -- enough to flip the substantive story from "Colombia's civil liberties lag badly behind the global average" to "roughly typical." Fixed by building the global reference the same discounted way as the municipal anchor (code above). All figures and numbers in this memo reflect the corrected version.

**What it represents:** each municipality's position relative to the *entire V-Dem world*, not just the rest of Colombia. On this scale, 0 = the average country-year in the full 2000-2023 V-Dem panel, and units are standard deviations of that global distribution. It is directly anchored to Colombia's real national trajectory (`v2elffelr`, `v2x_civlib`) and  --  because the standardization constants are fixed rather than recomputed per country  --  is built to support comparison with other countries once they're added to the pipeline.

---

## 3. Validation

Three checks confirm the benchmarked measure is doing what it's supposed to:

**It tracks Colombia's real national trajectory.** The year-by-year municipal mean of `EL_col_gz` tracks the (globally standardized) raw `v2elffelr` exactly -- by construction, since `EL_col_mt`'s municipal deviations average to zero within any year regardless of `weighted_range`. `CL_col_gz` tracks `qnorm(CLSNmean)` closely, with a small residual gap (expected: `mean(qnorm(x)) != qnorm(mean(x))` for a nonlinear function applied municipality-by-municipality before averaging, vs. once to the national aggregate -- not a data issue).

![Municipal means vs. national anchors](../trend_vs_national_anchors.png)

**It shows real, meaningful spatial and temporal variation.** A single fixed-color-scale map across all 24 years (one ggplot object, so the scale doesn't reset year to year), using the same palette as the unbenchmarked facet maps (viridis "magma"), with limits fixed to the full panel's own range:

![Subnational democracy, 2000-2023](../sndem_final_facet_allyears.png)

Colombia's national trajectory moves from modestly below the global average in 2000-2003 (under 0.4 global SD), through roughly the global average around 2004-2009, to modestly above it from 2010 on (peaking around 0.35-0.4 SD in 2015-2018), with a dip around 2019-2021 that stays close to average rather than reversing, and partial recovery by 2023. Within every year, municipalities visibly differ from one another, not just over time.

**Elections and civil liberties, compared directly.** Same palette applied to both dimensions individually (limits fixed across all three columns so neither is clipped), so they're comparable to each other and to the combined measure, at milestone years:

![Electoral vs. civil liberties vs. combined](../sndem_dims_milestone_years.png)

Elections moves further above the global average by 2016/2023 than civil liberties does -- a real, modest difference between the two dimensions, not an artifact of the color scale.

**Civil liberties specifically, in context.** Because civil liberties reads visibly darker than elections in the maps above, it's worth showing both what's actually happening to Colombia's civil liberties on their own terms, and how that translates once placed on the global scale:

![Civil liberties in context](../civil_liberties_in_context.png)

Colombia's own civil liberties level (`CLSNmean`, top panel) rises fairly steadily from about 0.38 in 2000 to a peak around 0.59 in 2016-2017, dips after 2018, and partially recovers by 2023 -- a real, moderate improvement over the period, not a flat or declining line. Once placed on the global scale (bottom panel), the same trajectory moves from modestly below the global average (2000-2002) to modestly above it (2010-2018), matching the elections story in shape, just at a smaller magnitude. The darker map coloring in early years and the generally cooler tone of the civil-liberties column relative to elections both reflect this real, moderate gap -- Colombia's civil liberties have consistently tracked a bit closer to the world average than its elections have, not a data or rendering problem.

---

## 4. What each measure is for

**Unbenchmarked (`sndem`, `snelect`, `sncivlib`):** the right choice for within-Colombia analysis  --  regressions or maps asking what predicts *relative* democratic quality across Colombian municipalities, since it isolates spatial variation from any national-level shift.

**Benchmarked (`sndem_final`, `EL_col_mt`, `CL_col_mt`, and the global-standardized `EL_col_gz`/`CL_col_gz`):** the right choice whenever the question involves an absolute or externally anchored reference point  --  Colombia's trajectory against its own historical V-Dem trend, its position relative to the rest of the world, or (once extended) direct comparison with another country's benchmarked scores. This is also the version to use for anything framed around the paper's calibration to V-Dem, since it's anchored to the same national-level estimates V-Dem itself reports.

Both are legitimate, validated measures of the same underlying phenomenon, viewed through different reference frames  --  using the wrong one for a given question is the main risk, not that either is unreliable.

---

## 5. Suggested presentation in the paper

- Lead with the **unbenchmarked** maps/analysis for the paper's core contribution  --  fine-grained subnational spatial variation within Colombia, which V-Dem's national-level estimates cannot show. This is the paper's unique empirical contribution.
- Use the **benchmarked** measure to situate Colombia's overall trajectory against the V-Dem global scale, and as the basis for any cross-national framing or future extension of the pipeline to other countries.
- Include a short methods note distinguishing the two explicitly for readers, e.g.: *"We report two related measures. The unbenchmarked index (`sndem`) captures each municipality's democratic quality relative to other Colombian municipalities. The benchmarked index (`sndem_final`) anchors these same municipal estimates to V-Dem's national-level estimates for Colombia, placing them on a scale comparable across countries. The two are not competing estimates  --  they answer different questions."*

---

## Files

- `06_vdem_data/01_weighting.R`  --  geopredictor weighting and national range calculation
- `07_weighting/01_weight_predictors.R`  --  unbenchmarked index (`sndem`)
- `08_benchmark/01_benchmark.R`  --  benchmarked index (`sndem_final`)
- `08_benchmark/02_validate_map.R`  --  maps in this memo
- `08_benchmark/03_trend_diagnostics.R`  --  national-anchor validation
- `08_benchmark/memo/benchmarking_memo.md`  --  the June 26 memo this one follows up on
