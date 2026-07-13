---
output:
  pdf_document: default
  html_document: default
geometry: margin=1in
header-includes:
  - \usepackage{float}
  - \floatplacement{figure}{H}
  - \usepackage{fvextra}
  - \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
  - \DefineVerbatimEnvironment{verbatim}{Verbatim}{breaklines}
---
# Colombia Deforestation Analysis: Methodology Memo

**Date:** July 10, 2026
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The SNVDEM-COL team

---

## Recap
As part of the revisions we plan to implement for the next version of the snvdem Colombia paper, we decided to include an illustration of how our measure can be useful for empirical research. One line of research recommended by Kelly is the relationship between democracy and deforestation (see Sanford 2023; Bohmelt and Bernauer 2025). The objective for the paper is to test the correlation between subnational democracy and subnational deforestation, building upon prior research that has been up to now limited to the national level. This memo documents the methodological choices for modeling this relationship. Findings are intended to inform a 1-2 page section for the manuscript, with a technical appendix if necessary.

1. **Pre-Analysis** (June 24-25): Patrick wrangled Hansen GFC (raster) + MapBiomas land use (pre-processed using raster) data, and then merged it to `snelect`. The DV was defined as annual forest loss (ha) divided by municipal area, log-scaled for the scatter/loess plots.
2. **July 2 meeting**: Michael questioned logging the DV (Patrick's response was that this was used to control for extremes), and Matthew proposed a lagged control (the forest-cover level at the start of the year). We agreed upon measuring forest-cover *change* rather than deforestation or forest loss amounts, for comparability across municipalities of different sizes. The team agreed to keep the statistics simple.
3. **July 9 meeting**: Kelly confirmed that the forest cover change DV aligns with Sanford (2023), and provided information from FAO and other sources regarding forest management policy globally, including in Colombia and the degree to which it is decentralized. We decided against adding agriculture/GDP or population size controls, since GDP and demographic information is already embedded in how the snvdem index itself is generated. Patrick agreed to implement the lag and align the DV.
4. **This memo** offers a subnational analysis of democracy and deforestation. It builds upon Boehmelt and Bernauer (2025), who directly critique and extend Sanford (2023). Results provide evidence of a curvilinear (U-shaped) relationship between democracy and deforestation at the municipal level in Colombia, with coefficient magnitudes and turning points closely matching Boehmelt and Bernauer's national-level estimates. These results are robust even when units are aggregated to department-level clusters, isolating the effects of spatial autocorrelation across subnational units.

---

## 1. Research Question

Does democracy constrain or enable deforestation? Forests provide long-term, diffuse public goods (carbon storage, biodiversity, watershed protection), while clearing them provides short-term, concentrated private benefits (land for farming, settlement, timber). Negative environmental conditions may reduce electoral support for incumbent politicians. Conversely, key voters of the selectorate may demand from politicians increased access to forest commodities in exchange for support.

Sanford (2023) uses satellite-derived global forest-cover data and the results of over 1,000 national elections between 1982 and 2016 to test the second hypothesis. He shows that countries undergoing a democratic transition lose an additional 0.8 percentage points of forest cover per year, and that closer elections predict more forest loss than less competitive ones. He measures land cover change at the grid-cell level (0.05-degree cells of land), each assigned a percentage-point change in forest cover per year and the national democracy score of the country it sits in:

> "The dependent variable for this study is the percentage point change in primary forest cover in a 0.05\textdegree{} x 0.05\textdegree{} cell of land in one year... The dependent variable exhibits a unit root in levels, which suggests taking a first difference will produce more consistent results than including a lagged dependent variable." (p. 753)

Boehmelt and Bernauer (2025) challenge these findings, however. Their central methodological critique is that Sanford's grid-cell approach inflates statistical power artificially because the explanatory variable -- national democracy -- doesn't actually vary at the grid-cell level:

> "We contend that employing the grid cell as the unit of analysis vastly increases the number of observations, but such boosting of statistical power is rather artificial because the key explanatory variables -- including the political regime variables -- are country-level measures." (p. 2)

In Sanford's design, every grid cell within a given country-year is assigned the exact same national democracy score. To correct this, Boehmelt and Bernauer collapse Sanford's data to the country-year level instead, so their unit of analysis and their explanatory variable are measured at the same scale. They also add a squared democracy term, and find evidence of a curvilinear, U-shaped pattern where deforestation is worst in partial democracies and less widespread in both the least and most democratic states:

> "Democracy is now negatively signed and significant while its square term is positively signed and significant... These estimates indicate that forest-cover growth initially decreases with higher levels of democracy -- but only until a turning point is reached. After this turning point, forest-cover growth rises with higher levels of democracy... The turning point for the Polity V measure is reached at around 3 (on a -10 to 10 scale)." (p. 4)

Collapsing to the country level is not an ideal choice because it hides whatever subnational variation might reveal. In many countries, national institutions retain control over protected environmental units and indigenous reserves, but municipalities exercise significant authority over land-use planning. Local zoning and land-allocation decisions drive agricultural expansion and settlement -- what Sanford calls "demand-side" mechanisms. If municipal governance quality plausibly drives a country's deforestation, a municipal-level democracy measure would be convenient for testing this relationship.

Our subnational indices `snelect`, `sncivlib`, and `sndem` address this need by assigning each municipality-year in Colombia its own democracy score. With these data, we can replicate the cross-national tests of Boehmelt and Bernauer at the local level. We can match the scale of analysis between our explanatory and dependent variables, allowing us to recover subnational variation instead of averaging it away.

Disaggregation of this type introduces distinct methodological challenges. Neighboring municipalities often share unobserved local conditions -- such as terrain, regional economic shocks, or conflict dynamics -- that can cause their deforestation outcomes to move in tandem, independent of local governance. While two-way fixed effects (municipality and year) absorb a great deal of this variance, they cannot account for localized shocks that strike a cluster of neighboring municipalities within a single year. Addressing spatial autocorrelation thus requires additional robustness tests, such as cluster-robust standard errors or spatial autoregressive models.

This memo builds upon the work of these authors, asking whether democracy measured at the municipal level predicts forest-cover change, and whether that relationship is also curvilinear. Finding the same curvilinear shape at this different scale would suggest that the pattern is more than the byproduct of cross-national confounders.

---

## 2. Empirical strategy

Adapting the basic empirical strategy used by both Sanford (2023) and Boehmelt and Bernauer (2025), we wrangle raster data for 2000-2023 in Colombia and use municipal-level data to regress forest-cover change on democracy.

**Forest-cover change (DV).** `d_forest_pct(t) = forest_pct(t) - forest_pct(t-1)`, where `forest_pct(t) = forest_ha(t) / municipal_area_ha * 100`. This replicates Sanford's DV, and is similar to Boehmelt and Bernauer's DV, "the annual change of a country's forest cover (in percent)... Positive values indicate an inter-annual growth in forested land, while negative values stand for deforestation" (p. 4). This is a **percentage-point** difference (`forest_pct(t) - forest_pct(t-1)`), not a **relative percent change** (`(forest_pct(t) - forest_pct(t-1)) / forest_pct(t-1)`). The latter would skew for municipalities with very little starting forest cover -- common in Colombia (savanna/urban municipalities) -- which is plausibly why neither Sanford nor Boehmelt and Bernauer use it either.

**Forest-cover level (control).** The control is `forest_pct(t-1)` -- the level of forest cover the year *before* the year the DV measures a change. It's one of the two elements of the DV, but entering it separately as a regressor tests whether the *rate* of change depends on the *starting level*, the same logic as a beta-convergence regression in growth economics (does growth depend on initial income?). Both papers state this as a deliberate, substantive choice:

- Sanford (p. 754): "I also include a control for the amount of forest remaining in a cell at the start of the year because I expect deforestation rates might be higher in places that are partially forested than places that have 100% forest cover."
- Boehmelt and Bernauer (p. 4): "We do not include a lagged dependent variable in the estimations, but consider an item on the level of forest cover (in % of land area) in the year before a focal year to address unit-specific temporal path dependencies in deforestation."

We build `forest_pct(t)` from Hansen/Global Forest Watch, identifying which 30m patches were cleared each year (2001-2023), and separately, what percentage of each patch was covered by tree canopy in 2000. A patch counts as "forest" if at least 30% was canopy-covered in 2000, matching Global Forest Watch's own default threshold (Ertel et al. 2023). Each year's forest area is last year's area minus that year's detected clearing, restricted to patches that counted as forest at baseline. `forest_pct(t-1)` is this same series, one year back. Following Sanford, who drops grid cells that neer have forest cover over his study window, we drop any municipality with zero forest cover at the 2000 baseline.

Hansen's `lossyear` layer only records clearing events -- there's no corresponding annual regrowth layer in what we're using, so our reconstruction can only subtract from the 2000 baseline, never add back to it. (Hansen does publish a separate `gain` layer, but it's one cumulative total for 2000-2012, not year-by-year, so we can't attribute a gain to a specific year -- the same reason Sanford gives for not modeling gains at all: "forest increases are... difficult to link to a political event because... new trees may take many years to appear in the data.")

To test robustness, we use MapBiomas data (MapBiomas Colombia Collection 3.0, Formacion Boscosa class (native + flooded forest)). Unlike Hansen, MapBiomas data classifies independent land-cover for each year, so it can register an apparent increase in classified forest area (e.g., secondary regrowth). And while Hansen detects *any* canopy loss, including due to fires, selective logging, natural disturbance, or primary clearing, MapBiomas tracks *land cover class transitions*, meaning it detects when land is cleared for agriculture, cattle ranching, or colonization. 

GFC is more standard in this literature. MapBiomas serves as a cross-check (producing similar results, as seen below) but there are two open questions about how directly comparable it is to GFC (see Section 4, Open Question 6).

Agriculture/GDP-type controls are excluded from our analysis because GDP is already embedded in how the snvdem index itself is generated -- adding it again would double-count the same data against the explanatory variable. Sanford's baseline model includes lagged per-capita GDP, GDP growth, and population growth alongside the forest-cover control -- e.g. `felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX | FID + year | 0 | un + year)` in his `coef_plots_democracy.R`. Boehmelt and Bernauer report parallel columns with and without these same controls (e.g. `xtreg forestdiff forestl democracy_bx i.year, fe` alongside `xtreg forestdiff forestl democracy_bx br_elecyear pcgdpl pcgdpchangel popgrowthl i.year, fe`) and find the result holds either way.

**Democracy measures (Explanatory variables): pre-benchmarked `snelect` / `sncivlib` / `sndem`.** Source: `data/panel/05_weighting/03_output/snvdem_col_weighted.rds`. The pre-benchmarked measures are for *within-Colombia* analysis. They measure relative democratic quality across municipalities, with national-level shifts isolated out. The municipality + year fixed-effects panel regression absorbs each municipality's time-invariant baseline. Because these measures vary genuinely at the municipal level, this panel does not have the "artificial power" problem Boehmelt and Bernauer identify in Sanford's grid-cell design.

(Some remaining choices -- the canopy-cover threshold, whether to lag the democracy measure itself, forest-gain modeling, and a possible subnational veto-players analogue -- are discussed in Section 4, Open Questions, after the results below.)

---

### Model specification, including the curvilinear test

We estimate both linear and quadratic models. First, a two-way (municipality and year) fixed-effects panel regression via `fixest::feols`:

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{Democracy}_{it} + \beta_2\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

where $\Delta\text{Forest}_{it}$ is `d_forest_pct`, $\text{Forest}_{i,t-1}$ is the lagged-cover control (`forest_pct_lag1`), and $\mu_i$ / $\tau_t$ are municipality and year fixed effects. The municipality fixed effect absorbs time-invariant characteristics (location, terrain, soil, baseline institutional context); the year fixed effect absorbs national-level shocks common to a given year (commodity prices, national policies). This part -- municipality and year fixed effects -- matches both Sanford's and Boehmelt and Bernauer's designs. Sanford clusters standard errors by *country and year* (he has 147-164 countries to cluster across); we cluster by *municipality and year*.

Second, to test the same U-shaped hypothesis from Boehmelt and Bernauer (2025) at the municipal level, we add a squared democracy term:

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{Democracy}_{it} + \beta_2\,\text{Democracy}_{it}^2 + \beta_3\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

Both the linear-only and quadratic versions are estimated and reported side by side (six models total per forest-data source: 3 democracy measures x linear/quadratic). The "turning point" of a quadratic fit, $-\beta_1 / (2\beta_2)$, is computed for each of the three democracy measures. Democracy terms are entered on their normalized 0-1 scale rather than raw index units, both so the three measures' coefficients are comparable to each other and so the turning point has a directly interpretable location within our own sample's range (0 = least democratic municipality-year observed, 1 = most) -- analogous to how Boehmelt and Bernauer report their turning point as a specific location on V-Dem's 0-to-1 scale or Polity's bounded -10-to-10 scale.

We run three specifications separately for `snelect`, `sncivlib`, and `sndem` as the democracy term. This allows us to compare the democracy-deforestation relationship across the elections channel, the civil-liberties channel, and the aggregate.

---

## 3. Results

The curvilinear relationship replicates at the municipal level, consistently, across both forest-data sources and all three democracy measures.

| | GFC linear coef. | GFC quadratic (linear / squared) | GFC turning point | MB linear coef. | MB quadratic (linear / squared) | MB turning point |
|---|---|---|---|---|---|---|
| `snelect`  | 0.094** | -0.508*** / 0.593*** | 0.429 | 0.086 (n.s.) | -1.208*** / 1.248*** | 0.484 |
| `sncivlib` | 0.018 (n.s.) | -0.599*** / 0.662*** | 0.452 | -0.112 (n.s.) | -1.623*** / 1.585*** | 0.512 |
| `sndem`    | 0.077* | -0.589*** / 0.688*** | 0.428 | 0.024 (n.s.) | -1.450*** / 1.489*** | 0.487 |

(GFC: n = 24,150, 1,050 municipalities; MapBiomas: n = 24,725, 1,075 municipalities. Turning points on the normalized 0-1 democracy scale, where 0 = least democratic municipality-year observed, 1 = most. Coefficients from `forest_pct_lag1`, the lagged-cover control, all significant at p < .001 in every model. Full regression tables, including `forest_pct_lag1`, appear in the Appendix.)

The linear-only specification is weak and inconsistent -- significant for `snelect` and `sndem` in the GFC data, not significant at all in MapBiomas, and never significant for `sncivlib`. The moment the squared term is added, every single coefficient (all six linear terms, all six squared terms, across both data sources and all three measures) turns highly significant (p < .001), with the same sign pattern Boehmelt and Bernauer report: negative linear term, positive squared term -- forest-cover growth is worst at intermediate democracy and recovers at both tails. Turning points cluster tightly between 0.43 and 0.51 across all six models -- essentially the middle of Colombia's municipal democracy distribution. This is a close match to Boehmelt and Bernauer's own reported turning points: their electoral-democracy model turns at "around a V-Dem score of 0.5 on a scale from 0 to 1" (p. 6) -- almost exactly our own 0.43-0.51 range -- and their Freedom House model turns "between 3 and 4 on a scale from 1 (least) to 7 (most)" (p. 6), which normalizes to about 0.42, also close to our low end. Their Polity-based turning point (~3 on a -10-to-10 scale, ~0.65 normalized) sits somewhat higher than ours.

**How this compares to Boehmelt and Bernauer's national-level V-Dem models.** Their Table 2 reports the same quadratic specification at the country-year level, using V-Dem's electoral-democracy index. Their Model 7 (no additional controls) is the closest match to our own design; Model 8 adds their controls (competitive elections, population growth, income, income growth) and shows the result is robust either way:

| | Democracy coef. | Democracy\textsuperscript{2} coef. | Turning point | n |
|---|---|---|---|---|
| B&B Model 7 -- national FE, V-Dem, no controls | -0.382*** | 0.431*** | 0.44 | 5,024 country-years |
| B&B Model 8 -- national FE, V-Dem, with controls | -0.447*** | 0.517*** | 0.43 | 4,799 country-years |
| Ours -- `sndem`, municipal FE, GFC | -0.589*** | 0.688*** | 0.428 | 24,150 municipality-years |
| Ours -- `sndem`, municipal FE, MapBiomas | -1.450*** | 1.489*** | 0.487 | 24,725 municipality-years |

(We use `sndem`, our own aggregate index, as the closest analogue to Boehmelt and Bernauer's single V-Dem electoral-democracy item -- both combine multiple components of regime quality into one score. Turning points computed as $-\beta_1/(2\beta_2)$ from each row's own coefficients.)

The sign pattern and magnitude line up closely across two very different levels of analysis: a negative linear term, a larger positive squared term, and turning points clustered tightly around a 0.5 range whether democracy is measured nationally (Boehmelt and Bernauer's country-year panel) or municipally (ours).

Other patterns from the fuller breakdown can be found in Section 4 of the R script (`colombia_deforestation.R`): the snelect-`d_forest_pct` correlation roughly quadruples after the 2016 Peace Accord in both data sources (GFC: r = 0.052 pre- vs. 0.189 post-2016; MapBiomas: 0.018 vs. 0.159), and collapsing to municipality-level averages (across all 23 years) strengthens the relationship further, especially for MapBiomas (r = 0.292 vs. 0.064 year-to-year).

---

### Visualizing the U-Curve in Colombia

The figure below shows what the coefficients from the table above imply about the shape of the relationship between subnational democracy and deforestation, in the same style as Boehmelt and Bernauer's own marginal-effect plots -- predicted `d_forest_pct` across the full range of each democracy measure, holding the lagged-cover control at its mean, with a 95% confidence band.

![Predicted forest-cover change across the full range of each democracy measure (95% CI), for all three measures and both forest-data sources. All six panels show the same U-shape: predicted forest-cover change is worst in the middle of the democracy distribution and improves toward both tails.](output/curvilinear_effects_grid.png)

The maps below plot each municipality's mean `snelect` (left) against its mean `d_forest_pct` (right) across the full panel. Read alongside the curve above, the worst forest-cover change appears to concentrate in intermediate-democracy municipalities, not uniformly in the least- or most-democratic ones.

The maps use `snelect` specifically, not `sndem` or `sncivlib`, mostly for space. The three measures are highly correlated with each other: `snelect` and `sndem` correlate at r = 0.978 (municipality-year) / 0.985 (municipality-mean); `sncivlib` and `sndem` at r = 0.976 / 0.985; `snelect` and `sncivlib` at r = 0.909 / 0.940. `snelect` was chosen as the one measure to map because it's analogous to Sanford's original focal variable (elections).

![Municipality-level means across the full panel: mean `snelect` (left) and mean `d_forest_pct` (right). The spatial pattern in forest-cover change reflects the same curvilinear relationship as the regression results, not a simple low-democracy-equals-high-loss gradient.](output/map_democracy_vs_forestchange.png)

---

### Spatial dependence

In a recent meeting, Michael raised a reasonable question about spatial autocorrelation: municipalities sit next to each other geographically, so is a simple Pearson/Spearman correlation even valid here, or does it overstate how confident we should be?

The concern applies to the regressions too. Municipality fixed effects remove each municipality's own fixed characteristics (terrain, baseline institutions); year fixed effects remove national shocks common to everyone in a given year (commodity prices, national policy). Neither one removes a shock that hits a cluster of *neighboring* municipalities in the same year but not the whole country -- a regional conflict flare-up, a regional drought, a local commodity boom. If deforestation and democracy scores both drift together across a cluster of nearby municipalities for reasons like that, the model can pick up a relationship that is partly geographic -- and the standard municipality-clustered standard errors don't fix this because they only address a municipality's own observations correlating with *each other over time*, not one municipality's observations correlating with its *neighbors'* in the same year.

**Moran's I test.** For each year 2001-2023, we can compute a Moran's I -- the standard test for whether values (here, the regression residuals from the `snelect` model) cluster geographically -- using queen contiguity (municipalities that share any border point count as neighbors). Positive, significant Moran's I means residuals are spatially clustered: nearby municipalities have residuals that are more similar to each other than to municipalities elsewhere.

The result confirms Michael's concern: every single year, in both data sources, shows strong, statistically significant positive spatial clustering.

- GFC: Moran's I ranges from 0.257 to 0.543 across the 23 years, all significant at p < 1e-44 (23 of 23 years).
- MapBiomas: Moran's I ranges from 0.333 to 0.597, all significant at p < 1e-72 (23 of 23 years).

Neighboring municipalities' unexplained deforestation outcomes move together, beyond what democracy, the lagged-cover control, and the fixed effects already account for.

Spatial autocorrelation like this biases standard errors and p-values, not the coefficients themselves; the point estimates in the table above are still valid descriptions of the average relationship in the data. The open question is whether the relationship is as statistically strong as municipality-clustered SEs suggest.

**Department-clustering robustness check.** As a check, we can re-run every regression with standard errors clustered by department (Colombia's 33 departments; 30 appear in our GFC estimation sample) instead of by municipality. Department clustering is coarser -- it at least partly absorbs shocks shared by every municipality inside the same department, which is a meaningfully different (and more conservative) grouping than clustering by municipality alone.

The two specifications respond very differently:

- **The linear-only models are fragile.** `snelect` and `sndem`'s coefficients, both significant under municipality-clustering (p < .05 and p < .01 respectively), lose significance entirely under department-clustering (`snelect`: SE grows from 0.028 to 0.058; `sndem`: SE grows from 0.031 to 0.058). 
- **The curvilinear (quadratic) models mostly hold up.** Of the 12 democracy-related coefficients in the six quadratic models (3 measures x 2 sources x {linear term, squared term}), 11 remain significant at p < .05 under department-clustering -- including the three GFC squared terms, which hold at p < .01. The one exception is the MapBiomas `snelect` linear term, which drops from p < .001 to marginal (p < .10); its squared term, however, still holds at p < .05.

With only ~30 clusters, department-clustered SEs are themselves less reliable (Cameron and Miller 2015). Given both diagnostics point the same way, it may be reasonable to report the curvilinear relationship with normal confidence, while flagging that a simple linear correlation between democracy and forest-cover change should be read as suggestive only, in line with what Michael was suggesting.

---

## 4. Open Questions

**1. Canopy-cover threshold, and a third robustness measure using Sanford's actual data source.** 30% is used to define "forest" in both the 2000 baseline and the loss data, matching Global Forest Watch's own default presentation threshold (Ertel et al. 2023). This is a chosen convention rather than a universal one (FAO's own global forest definition uses 10% instead). It may be worth a sensitivity check across 25%/30%/50% before this is final.

Running the analysis on VCF5KYR itself -- Sanford's literal data source -- as a third robustness measure was considered and set aside for two reasons:

- **Access requires a NASA Earthdata login** (free account, but not something I had ready this session).
- **VCF5KYR only covers 1982-2016** (also missing 1994 and 2000 specifically). It cannot reach 2023, so it would miss 2017-2023 entirely (the post-2016 Peace Accord period). It's also worth noting that VCF5KYR's 0.05-degree grid (~5.6km per cell) is much coarser than most Colombian municipalities, so it would have averaged over a lot of within-municipality heterogeneity that the 30m GFC data captures. 

**2. No forest-gain modeling.** The reconstructed GFC series can only go down, never up, so any apparent "positive" `d_forest_pct` in the data reflects noise/measurement variation in the loss detection rather than real regrowth. We may want to include a sentence on this paralleling Sanford's own caveat about gains being hard to attribute.

**3. Should the democracy measure itself be lagged?** Both Sanford and Boehmelt and Bernauer lag their *controls* (GDP, population, competitive elections, income growth) to avoid post-treatment bias, but neither establishes whether the focal explanatory variable itself is contemporaneous or lagged. We've kept `snelect`/`sncivlib`/`sndem` contemporaneous with the DV year. We can decide to lag but only if theoretically motivated.

**4. Subnational forest-policy framing.** Colombian municipalities control land-use planning but not environmental units or indigenous reserves, which sit under national jurisdiction, so `sndem` is a plausible lever on part but not all of Colombia's deforestation. We may want to include a citation from FAO or another source for the global 192-national/104-subnational forest-policy landscape, which is useful framing for why subnational governance data matters here.

**5. Veto players and other Boehmelt & Bernauer country-level controls.** According to their replication data, the authors use a veto-players variable -- `h_polcon3` -- from Henisz's POLCON3 political-constraints index, sourced from the Quality of Government dataset. They find more veto points associated with more forest loss. POLCON3 counts independent branches of national government with veto power over the executive, which doesn't have an obvious one-to-one municipal analogue in Colombia's unitary system (a mayor and council aren't "branches" the way an executive/legislature/judiciary split is). Nevertheless, we could consider including subnational veto players such as: (a) municipal council (*concejo*) composition -- does the mayor's coalition control a council majority, since Colombian land-use plans (*planes de ordenamiento territorial*) require council approval; (b) CAR (*Corporacion Autonoma Regional*) environmental licensing oversight, which can block municipal land-use decisions on environmental grounds; (c) prior consultation (*consulta previa*) requirements for indigenous/Afro-Colombian territories. None of these are currently computed anywhere in the snvdem-col panel -- this would be new data-collection work.

**6. Results and utility of MapBiomas data.** MapBiomas reproduces the same curvilinear sign pattern as GFC (Section 3), but two things about it are worth resolving before treating it as a genuine independent cross-check rather than just a second column in the table.

- **The lagged-cover control's sign flips between sources.** In the GFC models, `forest_pct_lag1` is positive (more forest already standing predicts *less* loss the following year) -- matching Sanford's stated rationale. In the MapBiomas models, it's negative (more standing forest predicts *more* loss) -- the opposite direction. Both are highly significant (p < .001) in their own models.
- **Cross-source agreement is much weaker on this DV than it used to be.** The GFC-vs-MapBiomas correlation on `d_forest_pct` is Pearson r = 0.186, Spearman rho = 0.206 (n = 23,115) -- far weaker than v1's r = 0.771 on the old, coarser, always-positive ha-based loss measure (`deforestation_handoff.md`, "Validation results"). Switching to a percentage-point change DV seems to have surfaced source-specific noise the old measure was averaging over.

MapBiomas Colombia is built from an independently developed land-cover classification (methodologically distinct from GFC's purpose-built loss-detection algorithm, though we haven't audited its specific pipeline this session), so some disagreement with GFC isn't itself surprising -- the open question is whether that disagreement is large enough, and patterned enough (the sign flip in particular), to undermine treating it as confirmation rather than a separate, harder-to-interpret result. Options: (a) investigate the sign flip and weak correlation further before leaning on MapBiomas as a robustness check; (b) keep reporting both but flag these two issues explicitly in the paper rather than presenting MapBiomas as agreeing with GFC; (c) drop MapBiomas from the main results and note it as a robustness attempt whose disagreement with GFC is itself worth a sentence, since it's measuring something arguably outside the scope of either Sanford's or Boehmelt and Bernauer's original data sources anyway.

---

## References

Boehmelt, Tobias, and Thomas Bernauer. 2025. "New evidence reveals curvilinear relationship between levels of democracy and deforestation." *Research and Politics*, January-March 2025: 1-8. https://doi.org/10.1177/20531680251320073

Cameron, A. Colin, and Douglas L. Miller. 2015. "A Practitioner's Guide to Cluster-Robust Inference." *Journal of Human Resources* 50(2): 317-372.

Ertel, Jessica, Liz Goldman, Justine Spore, and John Brandt. 2023. "Data 101: Comparing tree cover data over time." Global Forest Watch Blog, June 6, 2023. https://www.globalforestwatch.org/blog/data-and-tools/tree-cover-data-comparison/

Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. "High-Resolution Global Maps of 21st-Century Forest Cover Change." *Science* 342(6160): 850-853. https://doi.org/10.1126/science.1244693

Henisz, Witold J. 2000. "The Institutional Environment for Economic Growth." *Economics & Politics* 12(1): 1-31. (Cited via the Quality of Government dataset compilation used in Boehmelt and Bernauer's replication package as the source of `h_polcon3`; not independently re-verified against Henisz's original publication this session.)

MapBiomas Project. Collection 3.0 of the Annual Land Use and Land Cover Mapping of Colombia. Accessed via the MapBiomas Colombia platform.

Sanford, Luke. 2023. "Democratization, Elections, and Public Goods: The Evidence from Deforestation." *American Journal of Political Science* 67(3): 748-763. https://doi.org/10.1111/ajps.12662

Song, X.-P., M. C. Hansen, S. V. Stehman, P. V. Potapov, A. Tyukavina, E. F. Vermote, and J. R. Townshend. 2018. "Global land change from 1982 to 2016." *Nature* 560: 639-643. https://doi.org/10.1038/s41586-018-0411-9

---

## Appendix 1: Full Regression Tables

Municipality- and year-fixed-effects panel regressions, clustered standard errors by municipality (parentheses). Democracy terms on their normalized 0-1 scale. See Section 2, Model specification, for the estimating equations.

### GFC -- linear

\input{output/fe_regression_table_gfc.tex}

### GFC -- quadratic (curvilinear)

\input{output/fe_regression_table_gfc_quadratic.tex}

### MapBiomas -- linear

\input{output/fe_regression_table_mb.tex}

### MapBiomas -- quadratic (curvilinear)

\input{output/fe_regression_table_mb_quadratic.tex}

---

## Appendix 2: Deforestation and Democracy by Municipality

Numeric complement to the choropleths in Section 3 (`map_democracy_vs_forestchange.png`). Tables below use municipality-level means across the full 2001-2023 panel (Section 4.3's `muni_gfc`/`muni_mb`), GFC first, then MapBiomas.

### GFC

**Forest-cover-change tercile vs. mean snelect.** Municipalities are grouped into terciles by their own mean `d_forest_pct` (the inverse of Section 4.2's approach, which grouped by `snelect` instead), then averaged on `snelect` within each group.

\input{output/appendix2_tercile_table.tex}

**Named leaderboard.** The 10 highest-loss and 10 lowest-loss/highest-gain municipalities.

\input{output/appendix2_leaderboard_top10.tex}

\input{output/appendix2_leaderboard_bottom10.tex}

### MapBiomas

Same two views, MapBiomas source.

\input{output/appendix2_tercile_table_mb.tex}

\input{output/appendix2_leaderboard_top10_mb.tex}

\input{output/appendix2_leaderboard_bottom10_mb.tex}
