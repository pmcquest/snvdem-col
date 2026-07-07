---
output:
  pdf_document: default
  html_document: default
---
# Imputation Methodology Memo: Missing Data, Method, and Justification by Criterion

**Date:** July 5, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** SNVDEM-COL team
**Prompted by:** MPSA reviewer (Rosas) comment: *"You are almost cavalier about interpolation and
multiple imputation. You say you do it, but you don't tell us how."*

---

## Purpose and scope

This memo is the direct response to that comment. It documents, for every criterion that feeds `sndem_final`, three things: (1) how much data was missing and what that missingness looks like (concentrated by year? by municipality? by region?), (2) which method was used and why it fits that pattern, and (3) how much missingness remains after imputation and where it lives. Section 8 translates this into suggested paper/appendix language. The underlying diagnostics (density plots, LOESS/linear sensitivity checks, FE-regression comparisons, KMO/factor loadings) already exist *inside* the individual imputation scripts in `01_imputation_scripts/` — this memo pulls the results together in one place rather than duplicating the analysis.

**Context for this pass:** This memo reflects a full re-run of all 9 imputation scripts against the latest (2026-07-04) `df_col_clean.rds`, done 2026-07-05 and 2026-07-06. Three real bugs in the imputation scripts themselves were found and fixed in the course of that re-run (Section 7) — worth knowing before trusting any imputation output committed before this date.

All missingness percentages below are computed on `df_col_clean.rds` (29,172 rows: 1,122 municipalities × 1998-2023, before the panel is trimmed to 2000-2023) unless noted otherwise. "Residual" percentages are post-imputation, on the final merged panel (26,928 rows: 1,122 × 2000-2023).

**Note on the figures below vs. the plots:** the observed-vs-imputed plots (added 2026-07-06, `04_diagnostics/02_observed_vs_imputed.R`, saved to `01_criteria-png/`) were generated after a same-day revision to the Step 1 cleaning scripts, so a few exact raw counts in the prose below are from a slightly earlier data snapshot than the plots (e.g. `IndRur_0t1` raw missingness has since dropped from 142 to 85; `DisBog_4t5` from 62 to 2 — same story, smaller gap, nothing changes substantively). `diagnostic_table_imputed.csv` and the PNGs are regenerated from source each run and are always the current numbers; treat the prose counts as indicative rather than exact if the two ever disagree.

---

## 1. Criterion 0-1 (Rurality): `IndRur_0t1`, `PobRur_0t1`, `PobUrb_0t1`

**Missingness:** low — 142/29,172 (0.49%) for `IndRur_0t1`; 62/29,172 (0.21%) each for `PobRur_0t1`/`PobUrb_0t1`. Concentrated in a small set of municipalities entirely missing for a run of early years (e.g. Norosí, Guachené, San José de Uré, Tuchín — municipalities created after 2000 by subdivision of a parent municipio under Colombia's decentralization-era municipal-creation laws, which explains why their own historical rural/urban split doesn't exist before their creation). This is **Missing at Random (MAR) conditional on municipality age**, not random.

**Method (`imp01_RuralMI.R`):** multiple imputation via `mice` (predictive mean matching, m = 5), followed by a LOCF/NOCF finishing pass for any municipality-years mice still can't reach (e.g. a municipality missing in *every* year up to its creation, which has no informative donor within its own series). PMM is a sound choice here — non-negative, bounded, non-normal proportions matched to real donor observations rather than model-extrapolated.

**Fixed this session:** the script previously passed the ~1,122-level municipality-code column into `mice()` as a raw factor predictor, left in the default predictor matrix for every target variable. This is a real design problem: PMM builds a linear donor-matching model per iteration/dataset, and a ~1,122-column dummy expansion made a fresh run fail to finish in 25+ minutes of CPU time for <0.5% missingness. Replaced with the same "group mean carries municipality identity" trick already used in `imp23_FiscalCART_v3.R`'s `IDF_mean` — a per-municipality mean of each target variable, with the raw ID excluded from the predictor matrix. Re-run completed in under two minutes with identical logic otherwise.

**Residual:** 0 after the LOCF finishing pass.

![Observed vs. imputed: rurality index](01_criteria-png/diag_02_imp01_rurality.png)

**Reading the plot:** the imputed density is taller and narrower than observed (SD 0.16 vs. 0.24) and never reaches the near-0 (fully urban) tail that observed data has. This is expected: PMM matches missing rows to real donors based on the model's fitted value, and with only `year` and each municipality's own mean as predictors, the fitted values for these ~19 small, newly-created municipios cluster in the moderate-to-high rurality range rather than spanning the full national range — so the imputed values inherit that same, somewhat-compressed spread.

---

## 2. Criterion 2-3 (Economic development): `PIB_2t3` (GDP), `IDF_2t3` (fiscal performance)

**This is the criterion most worth reading carefully** — it has the most severe raw missingness of any variable retained in the index, and the residual gap is not random.

### 2a. `PIB_2t3` (municipal GDP)

**Missingness:** 18,202/29,172 (62.4%) raw. This is **not** primarily a remote-municipality problem — it is mostly a *temporal* structural gap: DANE's municipal GDP series simply doesn't exist after roughly 2009 for most of the country (baseline missing rate is ~62% in ordinary departments like Antioquia, Bolívar, Cauca, Atlántico). On top of that baseline, four departments run substantially higher: Guainía (95.7%), Amazonas (93.0%), San Andrés (80.8%), Vaupés (80.8%) — Colombia's smallest and most remote departments, where DANE does not consistently publish municipal-level GDP at all, in any year. A direct check found municipality-years missing `PIB_2t3` have somewhat higher `DisMer_13` (distance to market: 76.1 vs. 58.8 for observed) but essentially identical road density and rurality — so the *residual* (post-imputation) gap is better described as **DANE administrative non-coverage of structurally peripheral departments** than a smooth remoteness gradient.

**Method (`imp23_EconGrowth_v3b.R`):** a three-tier hybrid, in priority order: (1) each municipality's own 2000-2009 growth rate, chained forward from its last known value; (2) where a municipality has no own trend, its department's growth rate over the same window, applied to department-level GDP (`PIBd_2t3`, from DNP's TerriData); (3) a random forest (`PIB_2t3 ~ department_code + year + PIBd_2t3`) trained on complete cases, blended 50/50 with the growth-based estimate. Observed values are always kept via `coalesce()` — nothing here overwrites real data. This tiered design is defensible: it prefers a municipality's own trajectory, falls back to its department's macro trend, and only resorts to a cross-sectional model where neither time-series anchor exists.

**Caveat worth stating explicitly in the paper:** because the residual missingness is concentrated in specific, structurally peripheral departments rather than scattered randomly, any remaining gap is not missing-at-random with respect to the *construct* the index measures (state reach/capacity). Imputing over it with a department-level proxy is a reasonable practical choice, but it should be flagged as a limitation, not silently smoothed over — this is the kind of disclosure Rosas's comment is asking for.

**Residual:** 504/26,928 (1.87%), still concentrated in the four departments named above.

![Observed vs. imputed: municipal GDP](01_criteria-png/diag_03_imp23_gdp.png)

### 2b. `IDF_2t3` (fiscal performance index)

**Missingness:** 4,292/29,172 (14.7%) raw — much more modest than GDP, but with three distinct sources rather than one. By year: 
- (1) **1998-1999, 2,244 rows (52% of the gap)** — the index itself is constructed for 2000-2023 only (`df02-23-clean.R`'s own section header: "Municipal fiscal performance (2000-2023)"), so these two years are missing by construction, not a data- quality problem. 
- (2) **2023, 1,122 rows (26% of the gap)** — this is *not* an unpublished-data issue: DNP's 2023 results are loaded in `df02-23-clean.R` (`ResultadosIDF_Nueva_MetodologIa_2023_Act.xlsx`), but the script combines only the "Anterior" (old-methodology) 2021/2022 files into the object that actually gets saved (`IDF_combined <- bind_rows(IDF21a, IDF22a)`) — the 2023 file is used in two in-script diagnostic plots comparing old vs. new methodology, then never merged in, because no "Anterior"-methodology reconciliation of 2023 exists yet to match the older series on the same scale. This is a real, addressable-in-the-future gap (see note left in `df02-23-clean.R`), not administrative non-publication — reconciling the two methodologies is a judgment call for the team to make later, not something to paper over silently here. 
- (3) **2000-2002, 926 rows (22% of the gap)**, declining from 14.9% missing in 2000 to ~3-5% by 2004 — consistent with the underlying CEDE panel not yet having full municipal coverage in its earliest years.

**Method (`imp23_FiscalCART_v3.R`):** single imputation via `mice` with **CART** (classification and regression trees), using a per-municipality mean (`IDF_mean`) as a fixed-effect-style predictor rather than the raw municipality code (the group-mean trick this file originated, later copied into 0-1 and 10-11 above/below). CART captures the municipality-level trend via `IDF_mean` and the time trend via `year` without needing a distributional assumption.

**Residual:** 0.

![Observed vs. imputed: fiscal performance index](01_criteria-png/diag_04_imp23b_fiscal.png)

**Reading the plot:** the imputed density is shifted left of observed (mean 58.9 vs. 63.8) with a tall, narrow sub-peak around 50-55. This reflects how the national average fiscal performance rises steadily from 53.7 in 2000 to 70.5 in 2022. CART tracks this trend well wherever it has same-year information to anchor to (e.g. imputed rows in 2010 average 64.6, close to that year's true 66.7; 2022's imputed rows average 70.3 vs. the true 70.5). The two years with *zero* observed data, **1998 and 1999 — which alone are 52% of everything imputed for this variable**, have nowhere to anchor to, and get predicted at ~53.8. Essentially CART is falling back to the earliest cross-section it has ever seen (2000's ~53.7), since a tree-based method has no way to extrapolate a trend backward past its training window. We have no actual evidence whether the true 1998-1999 level was higher, the same, or lower, but it means over half the "imputed" mass for this variable sits at the low end of the historical range by construction, which is what pulls the pooled imputed distribution's mean down and produces that narrow low-value sub-peak. 2023 (26% of the imputed mass) is predicted at 69.6, just below the true 2022 value of 70.5, a small and reasonable forward extrapolation.

---

## 3. Criteria 4-5, 6-9 (static geography): `DisBog_4t5`, `north6`/`south7`/`west8`/`east9`, `axis_ns`/`axis_we`, `disp_ns`/`disp_we`, `LATITUD`/`LONGITUD`

**Missingness:** low (62-2,268 rows, 0.2-7.8%), and **genuinely time-invariant** — these are fixed geographic facts about a municipality (distance to Bogotá, position within the country), so any missingness is a data-linkage gap, not a real temporal absence.

**Method (`impStatic_LOCF.R`):** `fill(..., .direction = "downup")` per municipality — carries the single known value to every year for that municipality. This is the correct method for a genuinely-static variable; there is no meaningful alternative (interpolation/MI would imply the value could plausibly differ by year, which contradicts the variable's definition).

[**Notable finding:** two municipalities — Santa Rosalía (`99624`) and Cumaribo (`99773`) — had *every* static-geography column NA in *every* year in the previously-committed imputation output (these are the same two municipalities flagged in the 2026-07-04 benchmarking handoff for a "residual Step 3 geocoding gap," tied to a legacy-vs-current DIVIPOLA code issue found in the empirical-merge fix). Comparing the freshly re-run output against the previously-committed version confirms this is now **resolved for the static-geography columns** — both municipalities have real values in the new run, evidently as a side effect of the DIVIPOLA-code consolidation done in the 2026-07-03 merge rewrite. This is only partial progress, though: Step 5's own diagnostics (`05_weighting/narm_decision_log_2026-07-03.md`) still show these same two municipalities missing 12 of 16 predictor criteria in every year downstream, so the underlying geocoding gap is not fully closed — worth a dedicated follow-up, not treated as resolved everywhere.]

**Residual:** 0 (static columns); the `LATITUD`/`LONGITUD` pair is intentionally dropped before the final merge (coordinates aren't used past Step 2).

![Observed vs. imputed: static geography variables](01_criteria-png/diag_01_impStatic.png)

---

## 4. Criteria 10-11 (civil unrest | illicit activity): `Desp_1011` (displacement), `VDays_1011` (violent days), `HHomix_1011` (homicides), combined into `ViolInd_1011`

**Missingness:** `Desp_1011` 4,698/29,172 (16.1%); `VDays_1011`/`HHomix_1011` 2,284/29,172 (7.8%) each. This is conflict/administrative-reporting data (sourced from HRDAG's `verdata` replicates and CEDE). The by-year breakdown matters more here than the overall rate suggests: 1998-1999 run elevated (4.5-5.3%), 2000-2008 is low (~1-1.3%), then missingness *rises* steadily from 2009 (3.1%) to 2020 (17%), and **2021-2023 are 100% missing for every municipality** — HRDAG's `verdata` replicate files (see `01_empirical_data/README.md`'s replicability-audit notes on the v1/v2 versioning issue) simply don't extend that far yet. So the largest single component of this gap is a hard temporal edge in the source data, mechanically similar to the `PIB_2t3`/`IDF_2t3` edge-of-series gaps in Section 2 — not scattered missingness. On top of that structural edge, missingness is plausibly **Missing Not At Random (MNAR) adjacent** within the years that *are* covered: the years/places with the weakest state presence to report violence are not unrelated to the years/places with the most violence. Both are limitations we acknowledge, but they're distinct and worth naming separately.

**Method (`imp1011_CrimeMI-PMM_v3.R` then `imp1011_FAviolence_pmq.R`):** (1) multiple imputation via `mice` PMM (m = 5) using year, total population, and the other two crime variables as predictors; (2) the three completed variables are log-transformed and combined into a single factor score (`ViolInd_1011`) via principal-axis factor analysis (`fa(..., fm = "pa", scores = "tenBerge")`), chosen over the default ML/normal-theory factor method because the inputs are heavily right-skewed. Validated in-script: KMO = 0.68 (adequate; item-level MSA 0.65-0.76), loadings 0.618-0.812, ~54% variance explained by the single factor — reasonable evidence these three indicators share one latent "violence" dimension worth collapsing to.

**The script also runs its own sensitivity comparison** (MICE-PMM vs. mean imputation vs. LOCF vs. a two-way fixed-effects regression) directly on `Desp_1011` and reports the resulting coefficients side by side. Worth lifting into the paper's data appendix directly (see Section 8).

**Fixed this session:** missingness is higher (~16% vs. ~0.5%). Replaced with per-municipality group means (`Desp_mean`, `VDays_mean`, `HHomix_mean`), `MPIO_CDPMP` excluded from the predictor matrix. A fresh run of the unfixed script would very likely not have finished in reasonable time at all (imp01's smaller version of this problem took 25+ CPU-minutes without finishing).

**Residual:** 0 for all four columns (mice fully resolves all missingness before the FA step runs, so nothing gets silently dropped by the FA script's own `filter(!is.na(...))` step. If a future data update leaves any residual NA after the MICE step, the FA script will silently drop those municipality-years rather than erroring, and they'll re-enter the final panel as NA. Script can include a one-line assertion (`stopifnot(sum(is.na(viol)) == 0)`) before the FA step if this is revisited.

![Observed vs. imputed: crime/violence inputs](01_criteria-png/diag_05_imp1011_crime.png)

**Reading the plot:** observed and imputed look essentially identical: `Desp_1011` mean 15.9 (imputed) vs. 16.1 (observed), median 3 vs. 3, SD 87 vs. 82. But the plot itself is a weak instrument for checking this: with `Desp_1011` ranging up to 60,847 on a linear scale, a handful of extreme outliers compress the entire comparison into a sliver near zero regardless of whether the two distributions actually match — you genuinely can't see a meaningful difference here even if one existed. The crime script's own internal diagnostics (`log1p()`-transformed density plots, further down in `imp1011_CrimeMI-PMM_v3.R`) handle this correctly; a future revision of `02_observed_vs_imputed.R` should log-transform these three
variables rather than rely on the numbers alone to make the case.

![Violence factor score, by whether any input was imputed](01_criteria-png/diag_06_imp1011FA_violence.png)

**Reading the plot:** rows relying on ≥ 1 imputed crime input average -0.34 on the factor score vs. +0.07 for fully-observed rows; a real, visible leftward shift. But this tracks a genuine national trend rather than a modeling artifact: mean `ViolInd_1011` declines steadily across the whole panel, from around +0.4 in the early 2000s to around -0.3 by the 2020s — consistent with Colombia's well-documented drop in recorded conflict violence after the Uribe-era security campaigns and the 2016 peace accord. And the years needing imputation are disproportionately the *later* less violent years: 2021-2023 (100% imputed) plus a rising share through 2016-2020 (8%→17%), against a low imputed share (~1-5%) in the higher-violence 2000-2010 years. So the red curve being shifted left is mostly a compositional effect — the same mechanism as `IDF_2t3` in Section 2b — not evidence that imputation is artificially suppressing the violence measure for the municipality-years that need it.



---

## 5. Criterion 12 (population density): `DenPob_12`

**Missingness:** none. `DenPob_12`, `PobTot_12`, `AREA`, `AREAkm` are all fully observed — no imputation was needed or performed.

---

## 6. Criterion 13 (remoteness): `DisMer_13` (distance to market), road variables

**Missingness:** `DisMer_13` 3,368/29,172 (11.5%); all road-count/length variables (from Open Street Maps (OSM), via M. Sisk) fully observed.

**Method (`imp13_RoadsLOCF.R`):** `fill(..., .direction = "downup")`. Distance to market is slow-moving at the municipal level (it doesn't meaningfully change year to year absent major infrastructure shifts), so LOCF is an appropriate, low-risk choice — equivalent to assuming no change over the gap, which is a safe assumption for this specific variable given its economic-geography nature.

**Residual:** 0.

![Observed vs. imputed: distance to market](01_criteria-png/diag_08_imp13_roads.png)

---

## 7. Criterion 14 (indigenous/ethnic population): `PobInd_14`, `PobEtn_14` → `PropInd_14`, `PropEtn_14`

**Missingness:** 21,339/29,172 (73.1%) raw for the levels. These are **census-sourced** variables, but the actual by-year pattern is 1998-2004 100% missing (before this panel's first census), **2005 98.1% observed** (census year), 2006-2017 100% missing (the inter-censal gap), and **2018-2023 100% observed** — i.e. DANE's raw source already carries population-projection estimates for every year from the 2018 census forward, not just the census year itself. This is still close to **MCAR conditional on year** (a fixed collection/projection schedule, not municipality characteristics) — one of the cleanest missingness mechanisms in this pipeline, despite having the highest raw missing rate.

**Method (`imp1214_PopGBIv2.R`):** a tiered cascade from most-specific to least-specific: (1) linear growth interpolation between each municipality's own first and last observed value (i.e. the 2005→2018 census anchors, extrapolated where needed); (2) constant imputation for municipalities with only one observed data point (no trend computable); (3) cross-sectional yearly median for the ~52 municipalities with *zero* observed data across all years; (4) a logical cap (subpopulation ≤ total population) applied last. (Could be treated as the template for how the other tiered scripts (2-3, 15-16) document their own fallback logic; shows method-to-mechanism fit.

**Residual:** 40/26,928 (0.15%).

![Observed vs. imputed: indigenous/ethnic population share](01_criteria-png/diag_07_imp1214_population.png)

**Reading the plot:** both variables are extremely right-skewed — most municipalities have a near-zero indigenous/ethnic share, with a small cluster of resguardo/majority-ethnic municipalities out near 1.0 — so, like the crime plot in Section 4, the huge near-0 spike (density up to ~120 for `PropInd_14`) visually swamps any difference in the tail. Imputed `PropInd_14` has a lower mean (0.076 vs. 0.110 observed), is more concentrated near zero (76.6% below 0.02 vs. 69.2%), and reaches the indigenous-majority range (>50%) less often (5.7% vs. 8.6%) — `PropEtn_14` shows the same pattern more mildly (mean 0.163 vs. 0.202; >50% share 12.5% vs. 17.1%), while the near-zero mode and median are nearly identical across both. The likely source is the cascade's third (least-specific) tier: the ~52 municipalities with *zero* observed data in either census year fall back to that year's cross-sectional median — a low value, since most municipalities are low-indigenous. If any of those 52 are actually indigenous-majority areas where the census itself failed to enumerate anyone (plausible: the same remote/weak-state-capacity conditions that produce a total census gap are not unrelated to where indigenous territories are), the median fallback would understate them specifically, which is the same MNAR-adjacent shape as the GDP and violence criteria. Worth checking in the future which of the 52 zero-data municipalities this tier actually applies to.

---

## 8. Criteria 15-16 (ruling party/elections): `RulPar_15t16`, `RulParD_15t16`

**Missingness:** 21,388/29,172 (73.3%) raw. Election results are only observed in election years; the value for intervening years is *defined* as "who currently holds power," not a separately-measured quantity that happens to be missing. This is not really "missing data" in the conventional sense so much as a construction problem (carry the last election's winner forward until the next election).

**Method (`imp1516_ElectionsLOCF.R`):** forward-only LOCF (`.direction = "down"`, deliberately not "downup"), anchored with a 1998 baseline patch from `df06_clean.rds` so the very first years in the panel have a starting value to carry forward from. The forward-only direction is the correct choice; backward-filling would fabricate a "ruling party" for years before a municipality's first recorded election, i.e. before it existed as an independent electoral unit in many cases.

**Residual:** 138/26,928 (0.51%), concentrated in 41 municipalities missing all years from 2000 through the year of their first recorded election (matches the "municipalities created after 2000" mechanism from Section 1) — correctly left `NA` rather than backward-filled, per the reasoning above.

![Observed vs. imputed: ruling party](01_criteria-png/diag_09_imp1516_elections.png)

---

## 9. Excluded from the index entirely: `Pobre_2t3`, `NBI_2t3`/`NBIu_2t3`/`NBIr_2t3`, `IPM_2t3`/`IPMu_2t3`/`IPMr_2t3`

**Missingness:** 92.3-96.4% raw — by a wide margin the most-missing variables in the cleaned data.

**These are never imputed, and never enter the final panel at all.** `NBI` (unsatisfied basic needs) is only available for 1993/1995/2000/2005/2018; `IPM` (multidimensional poverty index) only for 2005/2018 — both are census/large-survey products with no annual update, so >90% missingness is intrinsic to how they're collected. `03_geocoded_panel/01_clean_geocoded/01_clean_geocode.R`'s own variable-selection step documents the choice made instead: `PIB_2t3` (GDP) and `IDF_2t3` (fiscal performance) were selected as the two indicators for criterion 2-3 (economic development), and the poverty-rate alternatives were not carried forward. 

---

## 10. Summary observation

> *"Missing data varies substantially by criterion, from 0% (population density, most road variables) to over 90% (poverty variables collected only in census years, which we exclude in favor of GDP and fiscal-performance measures with better annual coverage — see [Appendix table]).
> We use interpolation and imputation methods matched to each variable's missingness mechanism rather than a single blanket approach: last-observation-carried-forward for time-invariant or event-anchored variables (geography, election outcomes between elections); growth/trend-based interpolation anchored to census years for slow-moving demographic shares; multiple imputation (predictive mean matching or CART, implemented via the `mice` R package) for variables best predicted by other observed covariates; and a hybrid growth-model/random-forest approach for municipal GDP, which has the most severe genuine missingness of any retained variable. 
> [Appendix table] reports, for every criterion: pre-imputation missingness, method used, post-imputation residual missingness, and where that residual concentrates. 
> The residual gap in municipal GDP (1.9% of municipality-years, concentrated in Colombia's smallest/most remote departments, where the national statistical agency does not consistently publish municipal estimates) is a genuine limitation we flag rather than paper over."*

The `diagnostic_table_imputed.csv` in this folder already has the residual numbers in machine-readable form; this memo has the raw/pre-imputation numbers and the method/mechanism narrative to go with it.

---

## 11. Bugs found and fixed this session (2026-07-05), for the record

1. **`impStatic_LOCF.R`** referenced a `provincia` column dropped by the 2026-07-03 merge rewrite — blocked the entire imputation stage from running at all. Fixed by removing it from the fill list (unused downstream).
2. **`imp1516_ElectionsLOCF.R`** called `ggplot()` before `write_rds()` without loading `ggplot2` — halted before saving on a fresh run. Fixed by adding the missing `library()` call.
3. **`imp01_RuralMI.R`** and **`imp1011_CrimeMI-PMM_v3.R`** passed the ~1,122-level municipality code into `mice()` as a raw factor predictor — computationally very expensive (one script did not finish in 25+ CPU-minutes) and a poor way to represent municipality identity in a PMM model. Fixed by replacing it with per-municipality group means (the trick already used in `imp23_FiscalPMM_v3.R`), consistent with the existing pattern in this codebase.

All 9 imputation scripts plus the merge step (`01_merge_imputed.R`) were re-run end to end after these fixes; the merged panel validates cleanly (1,122 municipalities at every join). Diffing the new merged panel against the previously-committed version shows differences consistent with (a) upstream source-data revisions from the 2026-07-04 trash recovery (DANE/HRDAG re-sourced files), (b) the Santa Rosalía/Cumaribo static-geography fix noted in Section 3, and (c) expected stochastic differences from re-running `mice`/random-forest with a corrected (and in two cases, different) predictor specification — no unexplained discrepancies found.
