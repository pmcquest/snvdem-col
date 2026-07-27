---
output:
  pdf_document: default
  html_document: default
geometry: margin=1in
header-includes:
  - \usepackage{float}
  - \floatplacement{figure}{H}
---
# Colombia Deforestation Analysis: Team Update (v2, condensed)

**Date:** July 20, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The SNVDEM-COL team

*This is a short companion to the full methodology memo (`deforestation_memo_v2.md`), pulling out just the variable definitions, summary statistics, model specifications, and headline regression results. Background (literature, data-source comparison, robustness batteries, open questions) is in the full memo.*

---

## 1. Dependent variable: forest-cover change

We test four constructions of the DV, all built the same way -- `d_forest_pct(t) = forest_pct(t) - forest_pct(t-1)`, where `forest_pct(t) = forest_ha(t) / municipal_area_ha * 100` -- but each with its own source-specific definition of what counts as "forest."

**TMF.** The Tropical Moist Forest dataset (Vancutsem et al. 2021, JRC/European Commission) reclassifies every 30m Landsat pixel independently every year, 2000-2023, into one of six classes (undisturbed, degraded, deforested, regrowth, water, other). Our DV sums undisturbed + degraded + regrowth pixels as "forest," so a pixel can move into or out of that total in either direction each year. This is the only one of the four sources that is genuinely bidirectional and applies a persistence rule (roughly 2.5-3 years of stable evidence before confirming a transition, applied the same way for loss and regrowth). It is also the only source we could externally validate: our extraction lands within 1.7% of an independently published department-level (Guaviare) deforestation total (Katz-Asprilla et al. 2024).

**TMF-lossonly.** The same TMF classification, but counting only transitions into the "deforested" class -- loss only, like GFC. Because it's a loss, we flip the sign so a positive value always means a better outcome, matching the other three DVs. This construction isolates whether it's TMF's *bidirectionality* or its *classifier/persistence rule* that drives its result, by holding the classifier fixed and varying only whether gain counts.

**GFC.** Hansen Global Forest Change (Hansen et al. 2013) applies a one-time >=30% canopy-density threshold to a 2000 baseline (`treecover2000`), then flags whichever year each pixel was cleared (`lossyear`). It is loss-only by construction -- a cleared pixel is never reclassified back to forest even if it visibly regrows -- and it is a popular measure (it underlies Global Forest Watch for example). It also has a documented weakness for Colombia specifically: Fergusson, Saavedra, and Vargas (2020) show GFC substantially misclassifies oil-palm plantations as forest here.

**MapBiomas.** MapBiomas Colombia Collection 3.0 is a 30m annual product built from a trained Random Forest land-use classifier rather than a canopy threshold. Our DV sums the pixels it assigns to its "Formación Boscosa" (forest-formation) category, which is bidirectional in principle. It applies a post-classification temporal-smoothing filter that may treat gain and loss asymmetrically (the methodology document's own worked example of a transition considered too fast to smooth is loss, not gain), and it excludes roughly 16.4M ha of supra-municipal indigenous reserve territory that doesn't map to a single municipio. We keep it in the comparison because it approaches "forest" from a completely different methodological starting point (a trained statistical classifier) than GFC's or TMF's threshold/reclassification approach.

![National forest cover by source, 2000-2023](forest_stock_by_source.png)

![Cumulative forest-cover change by municipality, 2000-2023, all four sources](map_cumulative_change_facet_4sources.png)

The first figure tracks each source's national forest-cover total over the panel using that source's own `forest_ha` column -- the three lines sit at different heights because the four sources define "forest" differently. The second figure maps the same underlying pattern at the municipal level: net forest-cover change from 2000 to 2023, by source. TMF is the only one of the four with a substantial blue (net-gain) region, concentrated in the central Andean highlands; GFC, MapBiomas, and TMF-lossonly are dominated by red (net loss) essentially everywhere, which for GFC and TMF-lossonly is partly mechanical -- both are loss-only by construction, so they cannot register a municipality as a net gainer even where real regrowth occurred.

## 2. Control variable: forest cover level (t-1)

Every model includes `forest_pct_lag1`: the share of a municipality's land area classified as forest the year *before* the DV's change window, using that same DV source's own forest definition. It tests whether the *rate* of forest-cover change depends on the *starting level* -- the same logic as a beta-convergence regression in growth economics. Both papers this analysis follows treat this as a deliberate choice: Sanford (2023) expects "deforestation rates might be higher in places that are partially forested than places that have 100% forest cover" (p. 754), and Boehmelt and Bernauer (2025) include it "to address unit-specific temporal path dependencies in deforestation" (p. 4).

## 3. Independent variable: sndem (composite democracy index)

`sndem` is the composite subnational democracy index -- combining the elections (`snelect`) and civil-liberties (`sncivlib`) sub-indices -- pre-benchmarked at the municipality-year level in the panel's weighting-stage output file, `snvdem_col_weighted.rds`. It measures relative democratic quality *across* municipalities, with national-level shifts isolated out. Because it genuinely varies at the municipal level (unlike the national democracy scores used in the cross-national literature this analysis builds on), the municipality + year fixed-effects panel does not have the "artificial power" problem Boehmelt and Bernauer identify in Sanford's grid-cell design. `sndem` is a composite of institutional quality, security/state-capacity conditions, and other characteristics which is why we don't pair it with conflict-intensity or state-capacity controls (doing so would risk circularity, since those conditions are already embedded in how the index itself is built). In every regression below, `sndem` is entered on its normalized 0-1 scale (0 = least democratic municipality-year observed in the sample, 1 = most), matching how Boehmelt and Bernauer report their own turning points.

## 4. Summary statistics

**Democracy index, raw scale (`sndem`), 2000-2023:**

| N | Mean | SD | Min | Median | Max |
|---|---|---|---|---|---|
| 26,928 | 0.342 | 0.068 | 0.121 | 0.344 | 0.556 |

**Dependent variable, annual forest-cover change in percentage points (`d_forest_pct`):**

| Source | N | Mean | SD | Min | Median | Max |
|---|---|---|---|---|---|---|
| TMF | 26,928 | 0.162 | 1.043 | -10.88 | -0.00 | 12.46 |
| TMF-lossonly | 26,928 | -0.241 | 0.453 | -11.01 | -0.08 | -0.00 |
| GFC | 24,150 | -0.232 | 0.343 | -5.84 | -0.11 | 0.00 |
| MapBiomas | 25,369 | -0.075 | 0.755 | -14.66 | -0.02 | 9.32 |

**Control variable, forest cover level the prior year in percent (`forest_pct_lag1`):**

| Source | N | Mean | SD | Min | Median | Max |
|---|---|---|---|---|---|---|
| TMF / TMF-lossonly | 26,928 | 32.85 | 28.78 | 0.00 | 25.78 | 99.53 |
| GFC | 24,150 | 70.48 | 29.94 | 0.00 | 75.88 | 116.50 |
| MapBiomas | 25,369 | 32.44 | 24.18 | 0.00 | 27.60 | 120.51 |

*Note: GFC's and MapBiomas's controls exceed 100% for a small number of municipality-years -- a boundary/raster-area artifact in those two sources, not present in TMF. Regression sample sizes below are slightly smaller than these N's because rows with a missing lag (each municipality's first observed year) are dropped.*

## 5. Model specification

Following Boehmelt and Bernauer (2025), who replicate Sanford's (2023) design, we estimate a two-way (municipality + year) fixed-effects panel regression, separately for each of the four DV sources above:

**Linear model:**

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{sndem}_{it} + \beta_2\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

**Quadratic model** (adds a squared democracy term to test curvature):

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{sndem}_{it} + \beta_2\,\text{sndem}_{it}^2 + \beta_3\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

| Term | Definition |
|---|---|
| $\Delta\text{Forest}_{it}$ | The DV: `d_forest_pct`, percentage-point change in forest cover, municipality $i$, year $t$ |
| $\text{sndem}_{it}$ | The IV: composite democracy index, normalized 0-1 |
| $\text{sndem}_{it}^2$ | Squared democracy term (quadratic model only), testing curvature |
| $\text{Forest}_{i,t-1}$ | The control: `forest_pct_lag1`, forest-cover level the year before |
| $\mu_i$ | Municipality fixed effect -- absorbs each municipality's time-invariant baseline (geography, history, etc.) |
| $\tau_t$ | Year fixed effect -- absorbs national shocks common to all municipalities in a given year |
| $\varepsilon_{it}$ | Error term; standard errors clustered by municipality |

For the quadratic model, the turning point of the fitted parabola is $-\beta_1/(2\beta_2)$: a positive $\beta_2$ implies a U-shape (the turning point is a minimum -- deforestation is worst at intermediate democracy levels), a negative $\beta_2$ an inverted U (the turning point is a maximum).

## 6. Results

### Linear model

| | TMF (primary) | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| sndem | 1.251$^{***}$ (0.1136) | 0.641$^{***}$ (0.0582) | 0.077$^{**}$ (0.0309) | 0.024 (0.1104) |
| Forest$_{t-1}$ | -0.0057 (0.0055) | -0.0285$^{***}$ (0.0032) | 0.0185$^{***}$ (0.0053) | -0.0895$^{***}$ (0.0106) |
| Municipality FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| Clustered SE (municipality) | Yes | Yes | Yes | Yes |
| Observations | 26,928 | 26,928 | 24,150 | 24,725 |
| R$^2$ | 0.459 | 0.049 | 0.566 | 0.175 |
| Within R$^2$ | 0.010 | 0.012 | 0.019 | 0.058 |

Except for MapBiomas, all data sources (TMF the strongest among them) show a positive, significant linear relationship between `sndem` and forest-cover change. Within R$^2$ nets out the fixed effects and isolates what `sndem` and the lagged control explain on their own: GFC (0.019) and TMF (0.010) are close together, and MapBiomas (0.058) is actually the highest of the four. The `sndem` coefficient being null in this specification suggests that the lagged forest-cover control and municipality/year effects are driving this result.

### Quadratic model

| | TMF (primary) | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| sndem | -0.087 (0.2640) | 0.139 (0.1177) | -0.589$^{***}$ (0.1024) | -1.450$^{***}$ (0.3266) |
| sndem$^2$ | 1.369$^{***}$ (0.2832) | 0.514$^{***}$ (0.1243) | 0.688$^{***}$ (0.1032) | 1.489$^{***}$ (0.2873) |
| Forest$_{t-1}$ | -0.0077 (0.0055) | -0.0293$^{***}$ (0.0032) | 0.0177$^{***}$ (0.0053) | -0.0905$^{***}$ (0.0106) |
| Turning point | 0.032 | -0.135 | 0.428 | 0.487 |
| Municipality FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| Clustered SE (municipality) | Yes | Yes | Yes | Yes |
| Observations | 26,928 | 26,928 | 24,150 | 24,725 |
| R$^2$ | 0.460 | 0.050 | 0.568 | 0.177 |
| Within R$^2$ | 0.012 | 0.014 | 0.023 | 0.060 |

*Significance: $^{***}$ p<0.01, $^{**}$ p<0.05, $^{*}$ p<0.1. Standard errors in parentheses.*

Once the quadratic term is added, TMF's linear term ($\beta_1=-0.087$) is no longer significant on its own, and while the quadratic term is still significant, the fitted turning point (0.032) sits at the extreme low end of the observed democracy range. The fitted curve is still effectively rising across nearly the whole distribution but not really in a U-shape.

GFC and MapBiomas however show a different, sharper pattern: a **negative and significant** linear term ($\beta_1=-0.589$ for GFC, $-1.450$ for MapBiomas) paired with a positive, significant quadratic term, and turning points close to the middle of the 0-1 range (0.428 and 0.487). This resembles the U-shape (negative-linear/positive-quadratic) result Boehmelt and Bernauer report at the national level using FAO, V-Dem, and Polity data. (Our version is more tightly estimated, with t-stats in the 5-6 range vs. their low single digits, which is plausible given the municipal panel has roughly 24,000+ observations and far more within-sample variation in `sndem` than a ~30-country, multi-decade national panel does.) TMF-lossonly sits apart from both patterns: its linear term is positive (not negative) but not significant, and its quadratic term is significant with a turning point (-0.135) outside the observed range entirely.

![Fitted quadratic curve, forest-cover change across the sndem range, all four sources](quadratic_curves_facet_4sources.png)

Plotting the fitted quadratic curve itself (predicted change relative to the least democratic municipality-year, with a 95% cluster-robust confidence band), we see TMF and TMF-lossonly rise smoothly across the whole range with no real dip, while GFC and MapBiomas trace a U-shape, bottoming out around the middle of the distribution before recovering toward the high-democracy end.

### Supporting specifications: year-only FE and random effects

Following Boehmelt and Bernauer, we also estimate two supporting specifications alongside the two-way FE model above, for both the linear and quadratic models: a **year-only-FE** model (drops the municipality fixed effect, keeping only year FE, so it retains cross-municipality comparisons that two-way FE absorbs into $\mu_i$) and a **random-effects (RE)** model (municipality enters as a random rather than fixed effect, so municipalities that never change much on `sndem` over time still contribute to the estimate). Boehmelt and Bernauer report these because together they "ensure that all forms of government and democracy scores are considered for the analysis, including states that never change their score over time" (p. 4).

**Linear model, sndem coefficient (t-stat):**

| | TMF (primary) | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Two-way FE | 1.251$^{***}$ (11.0) | 0.641$^{***}$ (11.0) | 0.077$^{**}$ (2.5) | 0.024 (0.2, n.s.) |
| Year-only FE | 0.815$^{***}$ (14.9) | 0.008 (0.2, n.s.) | 0.156$^{***}$ (3.1) | 0.375$^{***}$ (7.4) |
| Random effects | 0.905$^{***}$ (15.1) | 0.356$^{***}$ (10.6) | 0.107$^{***}$ (4.5) | 0.154$^{***}$ (3.2) |

**Quadratic model, sndem / sndem$^2$ coefficients (t-stat / t-stat):**

| | TMF (primary) | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Two-way FE | -0.087 (-0.3) / 1.369$^{***}$ (4.8) | 0.139 (1.2) / 0.514$^{***}$ (4.1) | -0.589$^{***}$ (-5.8) / 0.688$^{***}$ (6.7) | -1.450$^{***}$ (-4.4) / 1.489$^{***}$ (5.2) |
| Year-only FE | 0.490$^{**}$ (2.2) / 0.325 (1.4) | -1.055$^{***}$ (-6.0) / 1.062$^{***}$ (6.6) | -0.656$^{***}$ (-3.4) / 0.825$^{***}$ (4.7) | -0.330 (-1.6) / 0.713$^{***}$ (3.5) |
| Random effects | 0.219 (1.4) / 0.690$^{***}$ (4.0) | 0.103 (1.2) / 0.253$^{***}$ (2.8) | -0.689$^{***}$ (-9.0) / 0.819$^{***}$ (11.3) | -0.753$^{***}$ (-4.3) / 0.907$^{***}$ (5.3) |

The pattern is hardly uniform.

**Linear model:** TMF and GFC are the only two sources significant, and positive, under *all three* specifications (two-way FE, year-only FE, RE). TMF-lossonly loses significance under year-only FE specifically. MapBiomas is not significant in the two-way-FE model, only in year-only FE or RE.

**Quadratic model, linear term ($\beta_1$):** GFC is consistent (negative linear, positive quadratic term) and significant under all three specifications. MapBiomas matches it under two-way FE and RE but loses significance under year-only FE. TMF's linear term is essentially never significant on its own; it's only significant, and positive, under year-only FE. TMF-lossonly is the least stable of the four: not significant under two-way FE or RE, and significant but *negative* under year-only FE, flipping sign from its own two-way FE point estimate.

**Quadratic model, curvature term ($\beta_2$):** this is consistently positive and significant under all three specifications for TMF-lossonly, GFC, and MapBiomas. Using TMF, the curvature term is significant under two-way FE and RE, but not under year-only FE.

To summarize, GFC is the only source whose full quadratic story (negative linear term, positive quadratic term, both significant) survives every specification we ran. 

### Non-parametric check

A quadratic term can return a "significant" curvilinear result even when the true relationship isn't a parabola, because it's flexible enough to partially fit almost any mild non-monotonicity. As a check that doesn't depend on that functional-form assumption, we sort municipality-years into deciles by `sndem` (1 = least democratic, 10 = most) and report the mean of the DV within each decile directly, no regression at all:

| `sndem` decile | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|
| TMF | -0.088 | -0.047 | -0.020 | 0.037 | 0.116 | 0.178 | 0.251 | 0.293 | 0.380 | 0.516 |
| TMF-lossonly | -0.229 | -0.260 | -0.281 | **-0.297** | -0.263 | -0.247 | -0.232 | -0.218 | -0.203 | -0.161 |
| GFC | -0.263 | **-0.282** | -0.281 | -0.274 | -0.253 | -0.224 | -0.207 | -0.206 | -0.188 | -0.138 |
| MapBiomas | -0.131 | -0.130 | **-0.152** | -0.104 | -0.082 | -0.062 | -0.073 | -0.053 | -0.032 | 0.071 |

TMF is the only source with no dip: every decile rises monotonically from the least to the most democratic municipality-years, matching its linear regression result. TMF-lossonly, GFC, and MapBiomas all bottom out in deciles 2-4 (bolded) before rising -- the same shape their quadratic terms are picking up. TMF-lossonly's dip is real but shallow enough that the strong rise across the top deciles pulls the fitted parabola's turning point past the edge of the observed range (which is why the quadratic model reports a turning point (-0.135) that looks meaningless on its own).

### Spatial dependence

Municipality and year fixed effects don't absorb a shock that hits a cluster of neighboring municipalities in a single year but not the whole country (a regional conflict flare-up, a local drought). Moran's I on the quadratic model's residuals confirms real spatial autocorrelation for all four sources (TMF: I = 0.131, p < 1e-8; GFC and MapBiomas cluster even more strongly, I = 0.257-0.597 across years). As a check, we re-cluster standard errors by department (33 clusters) instead of municipality (1,122 clusters) and see whether `sndem` holds up. The point estimate itself doesn't move -- clustering only changes the standard error, not the coefficient -- so what changes row to row below is the SE (and t-stat) alone:

**Linear model, sndem coefficient (t-stat):**

| | TMF | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Municipality-clustered | 1.251$^{***}$ (10.8) | 0.641$^{***}$ (11.0) | 0.077$^{**}$ (2.5) | 0.024 (0.2, n.s.) |
| Department-clustered | 1.251$^{***}$ (4.8) | 0.641$^{***}$ (6.6) | 0.077 (1.3, n.s.) | 0.024 (0.1, n.s.) |

**Quadratic model, sndem / sndem$^2$ coefficients (t-stat / t-stat):**

| | TMF | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Municipality-clustered | -0.087 (-0.3) / 1.369$^{***}$ (4.7) | 0.139 (1.2) / 0.514$^{***}$ (4.1) | -0.589$^{***}$ (-5.6) / 0.688$^{***}$ (6.5) | -1.450$^{***}$ (-4.3) / 1.489$^{***}$ (5.1) |
| Department-clustered | -0.087 (-0.2, n.s.) / 1.369$^{**}$ (2.5) | 0.139 (0.6, n.s.) / 0.514$^{**}$ (2.4) | -0.589$^{***}$ (-2.6) / 0.688$^{***}$ (2.9) | -1.450$^{**}$ (-2.1) / 1.489$^{***}$ (2.6) |

TMF's and TMF-lossonly's linear results survive department-clustering, and hold up better than their own quadratic terms do (t $\approx$ 4.8-6.6 for the linear term vs. 2.4-2.5 for the curvature term). This suggests a positive linear relationship between democracy and forest change for TMF-derived regressions. But for GFC and MapBiomas, their linear coefficients (already weak or null under municipality-clustering) lose significance entirely once department-clustered (t = 1.3 and 0.1), while their *quadratic* terms stay significant throughout (t = 2.6-2.9 for the curvature term, department-clustered). Spatial dependence appears to favor whichever model was already stronger for that particular source. For GFC and MapBiomas specifically, this is a reason to trust their curvilinear result more than their linear one. In the following section, we show that MapBiomas's quadratic pattern turns out to be concentrated in two regions (Andes, Pacífico) rather than spread evenly across the country, which is the kind of structure a spatial-clustering check would be expected to pick up on.

## 7. Quadratic result by region: TMF, GFC, MapBiomas

MapBiomas defines "forest" separately for Colombia's five natural regions -- Andes, Caribe, Orinoquía, Pacífico, Amazonía -- each with its own region-specific legend. But it's worth asking the same question of TMF and GFC too: is each source's pooled national quadratic result evenly spread across the country, or really just the largest region's pattern showing up in the national average? And is it the *same* region driving it for every source? We re-ran the same two-way-FE quadratic specification (municipality-clustered SEs) separately within each region, for TMF, GFC, and MapBiomas (TMF-lossonly omitted):

![Colombia's five natural regions](colombia_regions_map.png){width=55%}

![Fitted quadratic curve by natural region and source](quadratic_by_region_3sources.png)

**Linear term ($\beta_1$, t-stat):**

| Region | TMF | GFC | MapBiomas |
|---|---|---|---|
| Andes | 0.776 (1.4, n.s.) | -0.550$^{***}$ (-3.0) | -1.568$^{***}$ (-3.7) |
| Pacífico | 0.243 (0.6, n.s.) | -0.101 (-0.9, n.s.) | -1.093$^{**}$ (-2.0) |
| Caribe | 0.701 (1.5, n.s.) | -0.465$^{*}$ (-1.6) | -0.978 (-0.8, n.s.) |
| Orinoquía | -1.892$^{**}$ (-2.4) | -1.283$^{***}$ (-2.7) | -0.950 (-1.6, n.s.) |
| Amazonía | 0.942 (1.2, n.s.) | 0.644 (1.6, n.s.) | -2.127 (-1.4, n.s.) |

**Quadratic term ($\beta_2$, t-stat):**

| Region | TMF | GFC | MapBiomas |
|---|---|---|---|
| Andes | -0.063 (-0.1, n.s.) | 0.473$^{***}$ (3.0) | 1.518$^{***}$ (4.3) |
| Pacífico | 1.348$^{**}$ (2.5) | 0.381$^{***}$ (3.0) | 1.211$^{**}$ (2.1) |
| Caribe | -0.285 (-0.6, n.s.) | 0.411 (1.3, n.s.) | -0.312 (-0.3, n.s.) |
| Orinoquía | 2.957$^{***}$ (3.7) | 1.359$^{***}$ (3.3) | 0.821 (1.4, n.s.) |
| Amazonía | 1.244 (1.1, n.s.) | -0.314 (-0.5, n.s.) | 1.947 (1.1, n.s.) |

**Sample size by region and source (municipalities / departments / municipality-years):**

| Region | TMF | GFC | MapBiomas |
|---|---|---|---|
| Andes | 629 / 11 / 15,096 | 629 / 11 / 14,467 | 629 / 11 / 14,467 |
| Pacífico | 178 / 4 / 4,272 | 178 / 4 / 4,094 | 177 / 4 / 4,071 |
| Caribe | 197 / 8 / 4,728 | 125 / 5 / 2,875 | 193 / 7 / 4,439 |
| Orinoquía | 59 / 4 / 1,416 | 59 / 4 / 1,357 | 59 / 4 / 1,357 |
| Amazonía | 59 / 6 / 1,416 | 59 / 6 / 1,357 | 45 / 6 / 1,035 |

*Sample sizes differ slightly by source within the same region because each source excludes municipalities with zero baseline forest cover under its own definition; TMF excludes none nationally, so its regional counts are each region's true total. GFC drops 72 Caribe municipalities that TMF and MapBiomas both retain, consistent with GFC's 30%-canopy threshold reading a lot of Caribe's dry-forest/savanna baseline as non-forest from the start.*

GFC and MapBiomas agree on which region drives their national quadratic result -- Andes, and to a lesser extent Pacífico -- but TMF does not. TMF shows no significant relationship at all, linear or quadratic, in Andes ($\beta_2$ t = -0.1) -- the region generating essentially all of the net-gain over the time period under study. Pacífico is significant for TMF, but its turning point falls outside the observed range, so it reads as monotonically rising, not a real U. The one region where TMF shows a genuine, strongly significant U-shape is Orinoquía on the eastern flank of the country ($\beta_1$ t = -2.4, $\beta_2$ t = 3.7, the strongest curvature t-stat of any TMF region). This is also the smallest region in the panel (59 municipalities), and not a region GFC or MapBiomas lean on for their own result (GFC's Orinoquía is also significant and even stronger, t = 3.3, so this is one place all three sources actually agree). Caribe and Amazonía show no significant relationship for any of the three sources.

TMF's pooled national quadratic term was significant but with a turning point (0.032) sitting right at the edge of the observed range, which made the national curve look nearly linear. The regional analysis shows that curvature is concentrated in Orinoquía, a regions that's too small to bend the pooled national estimate very far off its otherwise-linear shape.

## 8. Conclusions

The bottom line is that **the result depends on which data source you use, and that dependence has a discernible regional structure, not just a national-level split**:

- **Nationally**, the three primary sources point in different directions. GFC's loss-only methodology supports a **U-curve** (Section 6: $\beta_2 = 0.688^{***}$, turning point 0.428). MapBiomas's bidirectional-but-classifier-based methodology also supports a **U-curve** (Section 6: $\beta_2 = 1.489^{***}$, turning point 0.487, almost dead center). TMF's bidirectional, persistence-filtered methodology supports **linearity** instead -- its quadratic term is technically significant, but the turning point (0.032) sits at the extreme edge of the observed range, so the practical shape is a straight line, not a U.
- **Regionally** (Section 7), the Andean pattern is the clearest illustration of *why* the sources disagree: GFC and MapBiomas both show a strong, significant U-curve in the Andes specifically ($\beta_2$ t = 3.0 and 4.3), but TMF shows nothing there at all (t = -0.1) -- a direct result of the three sources measuring "forest cover change" differently (Section 1), not of Andean municipalities behaving differently depending on which dataset happens to be looking at them.
- In Colombia's **most heavily forested region, Amazonía**, none of the three sources find any significant relationship, linear or quadratic. Whatever is driving the national results, it isn't coming from the deep Amazon.
- **Orinoquía** is the one region where the U-curve shows up consistently **across methodologies** -- both TMF and GFC find a significant U-shape there (t = 3.7 and 3.3), even though the two sources disagree almost everywhere else. That cross-method agreement, in the one region where it happens, is a more credible signal than any single source's national-level result on its own -- though it's worth remembering Orinoquía is also the smallest region (59 municipalities), so it's a real but narrow piece of evidence, not a substitute for resolving the broader source disagreement.

---

*Additional robustness detail -- alternative (stricter) TMF forest definitions, the pre/post-2016 period split, and external validation -- is in the full memo. Happy to add any of it here if the team wants it in this version.*
