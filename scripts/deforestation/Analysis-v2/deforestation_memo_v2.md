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
# Colombia Deforestation Analysis: Methodology Memo (v2)

**Date:** July 16, 2026 (v1, circulated to the team July 12, 2026, is preserved as `deforestation_memo.md` in `Analysis-v1/`. `Pre-Analysis/` contains only the earlier presentation materials, not a memo.)
**Prepared by:** PM, with assistance from Claude Code Sonnet 5
**For:** The SNVDEM-COL team

---

## Recap

As part of the revisions we plan to implement for the next version of the snvdem Colombia paper, we decided to include an illustration of how our measure can be useful for empirical research. One line of research recommended by Kelly is the relationship between democracy and deforestation (see Sanford 2023; Boehmelt and Bernauer 2025). The objective for the paper is to test the correlation between subnational democracy and subnational deforestation, building upon prior research that has been up to now limited to the national level. This memo (which extends Memo v1) documents the methodological choices for modeling this relationship. Findings are intended to inform a 1-2 page section for the manuscript, with a technical appendix if necessary.

1. **Pre-Analysis** (June 24-25): Patrick wrangled Hansen Global Forest Cover (GFC, raster) + MapBiomas land use (pre-processed using raster) data, and then merged it to `snelect`. The DV was defined as annual forest loss (ha) divided by municipal area, log-scaled for the scatter/loess plots.
2. **July 2 meeting**: Michael questioned logging the DV (Patrick's response was that this was used to control for extremes), and Matthew proposed a lagged control (the forest-cover level at the start of the year). We agreed upon measuring forest-cover *change* rather than deforestation or forest loss amounts, for comparability across municipalities of different sizes. The team agreed to keep the statistics simple.
3. **July 9 meeting**: Kelly confirmed that the forest cover change DV aligns with Sanford (2023), and provided information from FAO and other sources regarding forest management policy globally, including in Colombia and the degree to which it is decentralized. We decided against adding agriculture/GDP or population size controls, since GDP and demographic information is already embedded in how the snvdem index itself is generated. Patrick agreed to implement the lag and align the DV.
4. **v1 Memo (July 12)** offered a subnational analysis built on Hansen GFC and MapBiomas data, reporting a curvilinear (U-shaped) relationship at the municipal level that closely matched Boehmelt and Bernauer's (B&B) national-level estimates and appeared robust to department-level clustering.
5. **v2 Memo (this memo)**: After the July 15th meeting, Patrick revisited the operationalization of the DV. First, he looked for alternative data sources that could more accurately capture subnational forest cover change and found a dataset by Vancutsem et al (2021) from the Joint Research Centre at the European Commission: Tropical Moist Forest (TMF) v1_2025. This data uses Landsat images at 30m resolution to provide annual reclassification of forest cover between 1990-2023, but unlike GFC's `lossyear` layer, it independently classifies every pixel every year to capture forest-cover change in both directions (loss and gain). And in comparison to MapBiomas, TMF is more transparent in its measurement approach and consistent application over time. Re-running the analysis with this data led to substantially different findings: the curvilinear result no longer holds up cleanly, while a simpler linear relationship -- higher subnational democratic quality predicts better forest-cover outcomes -- appears more robust. The divergences between results in Memos 1 and 2 are likely explained by core conceptual differences in forest cover measurement approaches.

---

## 1. Introduction: Democracy and Deforestation

Does democracy constrain or enable deforestation? Forests provide long-term, diffuse public goods (carbon storage, biodiversity, watershed protection), while clearing them provides short-term, concentrated private benefits (land for farming, settlement, timber). Negative environmental conditions may reduce electoral support for incumbent politicians. Conversely, key voters of the selectorate may demand from politicians increased access to forest commodities in exchange for support.

Sanford (2023) uses satellite-derived global forest-cover data and the results of over 1,000 national elections between 1982 and 2016 to test the second hypothesis. He shows that countries undergoing a democratic transition lose an additional 0.8 percentage points of forest cover per year, and that closer elections predict more forest loss than less competitive ones. He measures land cover change at the grid-cell level (0.05-degree cells of land), each assigned a percentage-point change in forest cover per year and the national democracy score of the country it sits in:

> "The dependent variable for this study is the percentage point change in primary forest cover in a 0.05\textdegree{} x 0.05\textdegree{} cell of land in one year... The dependent variable exhibits a unit root in levels, which suggests taking a first difference will produce more consistent results than including a lagged dependent variable." (p. 753)

Boehmelt and Bernauer (2025) challenge these findings. Their central methodological critique is that Sanford's grid-cell approach inflates statistical power artificially because the explanatory variable -- national democracy -- doesn't actually vary at the grid-cell level:

> "We contend that employing the grid cell as the unit of analysis vastly increases the number of observations, but such boosting of statistical power is rather artificial because the key explanatory variables -- including the political regime variables -- are country-level measures." (p. 2)

In Sanford's design, every grid cell within a given country-year is assigned the exact same national democracy score. To correct this, B&B collapse Sanford's data to the country-year level instead, so their unit of analysis and their explanatory variable are measured at the same scale. They also add a squared democracy term, and find evidence of a curvilinear, U-shaped pattern where deforestation is worst in partial democracies and less widespread in both the least and most democratic states:

> "Democracy is now negatively signed and significant while its square term is positively signed and significant... These estimates indicate that forest-cover growth initially decreases with higher levels of democracy -- but only until a turning point is reached. After this turning point, forest-cover growth rises with higher levels of democracy... The turning point for the Polity V measure is reached at around 3 (on a -10 to 10 scale)." (p. 4)

In their second empirical analysis, they use alternative forest-related data from FAO and -- in addition to the two-way (country + year) fixed-effects models (their Models 7-8) -- report a year-only-FE model (Model 9, which drops country fixed effects, retaining cross-national variation) and a random-effects model (Model 10), stating that this combination "ensures that all forms of government and democracy scores are considered for the analysis, including states that never change their score over time."

---

Collapsing to the country level, as B&B do, is not an ideal operation because it hides whatever subnational variation might reveal. In many countries, national institutions retain control over protected environmental units and indigenous reserves, but municipalities exercise significant authority over land-use planning (Katz-Asprilla et al. 2024). Local zoning and land-allocation decisions drive agricultural expansion and settlement -- what Sanford (2023) calls "demand-side" mechanisms. If municipal governance quality plausibly drives a country's forest cover change, a municipal-level democracy measure lets us test that directly rather than infer it from a national aggregate.

Indeed, Colombia's legal framework involves a complex division of authority between national and subnational governments. According to Katz-Asprilla et al. (2024, p. 3), "'the responsibility for planning and defining land uses belongs to the municipal authorities' (Constitución Política de Colombia 1991, art. 330; Congreso de la República de Colombia 1993, arts. 65, 67; 1997, art. 1)." However, nationally designated environmentally protected areas and indigenous reserves sit outside this municipal authority entirely. These features characterize governance dynamics in the Amazon Forest Reserve Zone, for example, which spans eight departments and dozens of rural municipalities. National-level institutions retain authority over the protected areas and reserves, while municipal-level democratic quality is what can plausibly explain variation in land-use decisions on the remaining land.

Understanding this division of authority on the ground requires disaggregation, and disaggregating a single country into its constituent units involves methodological tradeoffs. On the one hand, "scaling down" multiplies the observations available for controlled comparison, mitigates the small-N problem endemic to cross-national work, and holds country-level factors (culture, national institutions, colonial history) constant while letting the variable of interest vary (Snyder 2001). `snelect`/`sncivlib`/`sndem` genuinely vary at the municipal level in Colombia, letting us replicate Sanford's and B&B's cross-national tests at the local level, matching the scale of our explanatory and dependent variables, and recovering subnational variation instead of averaging it away. On the other hand, disaggregation introduces spatial autocorrelation as a real methodological complication: neighboring municipalities often share unobserved local conditions -- terrain, regional economic shocks, conflict dynamics -- that can move forest cover change outcomes in tandem, independent of local governance. Two-way fixed effects (municipality and year) absorb a great deal of this, but not localized shocks that strike a cluster of neighboring municipalities within a single year. (Section 4 reports a Moran's I diagnostic and department-clustered standard errors to address this directly.)

## 2. Subnational democracy and forest cover

Pivoting to the subnational level requires specifying the core variables and finding suitable data to test the democracy-deforestation relationship accurately. The data underlying Sanford's and B&B's are not suitable for these puroposes:

- **Sanford's data (VCF5KYR; Hansen and Song 2018)** is reported at 0.05-degree grid cells (~5.6km) -- coarser than many Colombian municipalities, some of which are smaller than a single cell. A municipal-level panel needs data finer than the unit of analysis itself, which rules this source out for our purposes.
- **B&B's data (FAO's Land Use domain)** has no subnational breakdown at all -- Colombia is reported as a single national figure -- and only six of its thirty-six annual values for Colombia (1990, 2000, 2010, 2015, 2020, 2025) are real reported figures; the rest are linearly interpolated between them. Neither its spatial nor its temporal resolution supports a municipal-year panel.

To define our dependent variable as bidirectional, generic forest-cover change at the subnational level (following Sanford's and B&B's own designs) we must find alternative sources of data. And the data must be sufficiently granular to match our explanatory variable of interest: subnational democracy.

### 2.1 Subnational data on forest cover change

Changes in forest cover can take many forms and can be measured in many ways. While deforestation often refers to loss of old-growth/primary forest (the narrower definition behind Global Forest Watch statistics, for example), *forest cover change* refers to the broader balance of forest cover of all types (from deciduous to moist tropical) gained and lost over time, including secondary regrowth. Moreover, "cover" itself can mean a wide variety of things, from thick canopies to secondary vegetation. Understandably, cross-national studies like those reviewed above treat forest cover change as a country-level aggregate to be compared across contexts; Sanford's and B&B's papers describe their measures in a sentence or two and move on to the empirical analysis. But how forest cover is defined is consequential for any statistical analysis, especially at the subnational level.

At the municipal level, primary and secondary forest interpenetrate at a fine spatial grain, and short- to medium-term reforestation (5-10+ years for visible canopy recovery, 20+ for a fuller stand) is itself a plausible local-governance outcome -- something municipal land-use policy and environmental licensing decisions can affect. These are locally-attributable dynamics a country-level aggregate cannot show. If the mechanism of interest is local governance's relationship to forest outcomes, a measure that aggregates deforestation rates to the national level would structurally hide evidence of that mechanism.

*(A note on terminology: this memo uses "forest cover" throughout, not "forest stock," to match the term every source cited here already uses -- Sanford, Boehmelt and Bernauer, GFC, GFW, and TMF'. "Forest stock" is a real term in this literature, but it refers to standing biomass or carbon volume, not land area. In Colombia, IDEAM treats "the surface of forests" and "carbon reserves stored in natural forests" as two explicitly separate monitoring objects. Since this memo measures area, not biomass or carbon, "forest cover" is the term of choice.)*

In our first attempt at this analysis (v1), we built the dependent variable from two sources, Hansen Global Forest Change (GFC) and MapBiomas. Both are heavily used, well-cited products in environmental science and remote-sensing research, but each has real limitations for a Colombian municipal-level panel specifically.

#### 2.1.1 GFC and the forest-loss measure

The GFC dataset (Hansen et al. 2013, continuously updated since) was developed by the [Global Land Analysis and Discovery laboratory (GLAD)](https://glad.umd.edu/) at the University of Maryland. It produces a 30m-resolution global dataset built from Landsat imagery, in two separate steps. First, `treecover2000` gives a one-time canopy-cover percentage for every pixel, as of the year 2000. We apply a >=30% cutoff to that snapshot to decide which pixels count as "forest" to begin with; that threshold is only ever applied once, to establish the starting baseline. Second, the `lossyear` band flags whichever single year (if any) that specific pixel's canopy was cleared in a detectable, stand-clearing event -- a change-detection algorithm scanning the full Landsat time series for a clearing signature. So loss occurs if a pixel is cleared and an event flag marks when this happens. These data are the most widely used forest-change product in this literature, and is featured on public websites like Global Forest Watch.

Because GLAD-derived data were used by Sanford (2023), in Memo 1 we treated GFC as a natural extension for subnational analysis. We used the `lossyear` layer alone because GFC's `gain` layer is a single cumulative total for 2000-2012, with no year attribution at all. Unfortunately, `lossyear` by construction can only ever register loss -- a pixel that is cleared and later regrows is never reclassified back to forest. GLAD introduced a second forest gain dataset extending the window to 2000-2020 (Potapov et al. 2022), but their calculation lacks temporal granularity, subtracting one forest-height map in 2000 from another in 2020 to measure net gain and net loss over the two decades. A pixel that gained canopy and later lost it (or the reverse) within the twenty-year window is not registered.

This is not a one-time gap in an early product version -- it persists in the current release. The official GFC-2024-v1.12 tile description, the same data-description format used for every GFC release since 2013 and now extending `lossyear` through 2024, still defines `gain` as "Forest gain during the period 2000-2012, defined as the inverse of loss, or a non-forest to forest change entirely within the study period. Encoded as either 1 (gain) or 0 (no gain)" (GLAD/UMD, GFC-2024-v1.12 Data Description). Twelve years and eight version releases after the original paper, `gain` has never been extended past 2012, while `lossyear` has been extended every year since. Global Forest Watch's own methodology documentation is explicit that the two layers should not be combined into a net figure: "tree cover, tree cover loss, and tree cover gain data sets cannot be compared accurately against each other. Accordingly, 'net' loss cannot be calculated by subtracting figures for tree cover gain from tree cover loss" (World Resources Institute, Global Forest Review, "Data and Methods"). This is a second, independent confirmation -- beyond the definitional/plantation argument below -- of why we build `d_forest_pct` from `lossyear` alone for GFC rather than attempt any gain-inclusive reconstruction (Section 3.1).

**A published case study of this exact pitfall: Harding et al. (2024).** Harding, Prem, Ruiz, and Vargas's APSR article on campaign donations and Colombian deforestation cites Hansen et al. (2013) and describes their measure as follows: "Using the baseline coverage levels and the yearly tree-cover loss and gain for each municipality, we recover the yearly coverage in each municipality" (p. 640), with the reconstruction given as $\text{coverage}_t = \text{coverage}_{2000} + \sum_{i=2001}^{t}(\text{gain}_i - \text{loss}_i)$ (p. 640, fn. 17). This describes `gain` as available on the same annual basis as `loss`, which, per the official data description quoted above, it is not -- the base Hansen product has no `gain_i` term for any individual year.

We checked their posted Harvard Dataverse replication files (`Hansen_V2.R`, `001_data_deforestation.do`, `Hansen_2000_2018.csv`; Harding et al. 2024b, DOI 10.7910/DVN/GYX7GC) to see how this was actually implemented, since the base data cannot supply what the footnote describes. Their extraction script calls the `gfcanalysis` R package's `gfc_stats()` function (Zvoleff), whose own documentation states plainly that "forest gain and combined loss and gain are not available in the GFC product on an annualized basis" -- and, consistent with that, `gfc_stats()` returns loss and gain as two separate objects: an annual `loss_table` and a single non-annual `gain_table` covering the full period. Harding et al.'s script writes only `loss_table` to disk (`Hansen_V2.R`, line 107); `gain_table` is never exported or used. Their Stata cleaning script then drops the `loss` column entirely and builds every deforestation measure as a year-over-year difference in a single `coverage` variable (`001_data_deforestation.do`, lines 16, 36-37, 56-59). We confirmed directly in their posted `Hansen_2000_2018.csv` that this `coverage` variable never increases: across all 22,440 municipality-year rows (1,122 municipalities, 2000-2019), there is not one case where `coverage` in year *t* exceeds `coverage` in year *t-1*. Whatever the published footnote's formula states, the replication data actually posted is loss-only -- monotonically non-increasing by construction, structurally identical to our own GFC reconstruction (Section 3.1).

This is useful to us for two reasons. First, it is a second, independently-written codebase confirming empirically that Hansen's `gain` layer cannot deliver what a "yearly loss and gain" description promises -- reinforcing, not undermining, our own choice to treat GFC as loss-only throughout. Second, it means Harding et al.'s own headline result is not actually compromised by the footnote's overstatement: their primary specification is the 2011 election's 2012-2015 mayoral term, which falls entirely after `gain`'s 2000-2012 validity window, so `gain` could not have contributed to that result regardless of implementation. The gap here is between a published footnote's *description* of the method and the *replication code* actually posted -- not necessarily a flaw in Harding et al.'s substantive finding -- but it is exactly the Hansen-data pitfall this section warns about, now documented in a second team's codebase besides our own.

Additionally, GFC's tree-cover measure carries a significant definitional limitation. GFC's `treecover2000` band defines "tree cover" as any vegetation exceeding 5m in height, irrespective of canopy density, without distinguishing native forest from commercial tree crops. Fergusson, Saavedra, and Vargas (2020) show this causes GFC to substantially misclassify oil-palm plantations as forest in Colombia specifically, concentrated in the Central and North regions with high oil-palm suitability. In their study of conflict and forest cover, correcting for this misclassification using field-validated data from the country's environmental monitoring institute (IDEAM) reverses the sign of their main result: GFC's own numbers say paramilitary violence *increases* "forest cover" (in this case oil-palm growth), while the same violence, measured against IDEAM's plantation-excluded baseline, *decreases* it. (We also considered using IDEAM data, but annual time-series data for 2000-2023 is incomplete. See Section 2.1.4.)

#### 2.1.2 MapBiomas and the land-use-change measure

MapBiomas Colombia (Collection 3.0) is also a 30m-resolution, satellite-derived annual product, but built using a unique machine learning algorithm. Rather than a canopy-cover threshold or a multi-temporal reclassification, MapBiomas uses a trained Random Forest classifier (Fundación Gaia Amazonas 2025, the ATDB methodology document for Collection 3). Its full legend has 26 classes (15 natural cover types, 9 land-use types, 1 water body, 1 no-data), organized under two top-level branches: "Natural" and "Antrópico" (anthropogenic). Forest sits on the *natural* side of that split.

The forest class ("Bosque," ID:3) is defined as natural vegetation dominated by tree or shrub elements forming a "more or less continuous" canopy, generally taller than 5m. MapBiomas Colombia defines this separately for five natural regions (Andes, Caribe, Orinoquía, Pacífico, Amazonía) -- each with its own worded description tailored to that region's forest types, citing IDEAM's own national land-cover legend conventions as the source for several of these regional definitions. Because the underlying forest definition itself varies by region, we test the linear and quadratic democracy-forest relationship separately within each of these five regions in Section 4.1.1, rather than assuming the pooled national result applies uniformly across them. Canopy/height descriptions are used to guide which pixels a human interpreter selects as training samples for the Random Forest classifier; they are not applied as a live per-pixel rule the way GFC's `>=30%` cutoff is. The actual class assigned to any given pixel is whatever the trained model predicts, based on ~156 layers of spectral bands, texture, spectral indices, and geophysical variables from a median Landsat composite for that year.

MapBiomas applies its own temporal smoothing filter, and as a result it may not treat gain and loss symmetrically. Its post-classification "Filtro Temporal" replaces a pixel's classification in a single "sandwiched" year with its neighbors' shared class whenever a 3-5 year window starts and ends in the same class but shows a different class in between (treated as noise, not real change). Relatedly, the model corrects the first and last year of the series if they show an "improbable" abrupt transition relative to the trend around them. Critically, the interpreter chooses which class transitions this filter applies to. It is possible that gain gets smoothed toward "no change" more readily than loss does. If so, that would understate MapBiomas's true bidirectional signal specifically on the gain side.

[MapBiomas](https://mapbiomas.org/) originated as a Brazilian civil-society/government land-use-monitoring initiative in 2015 and was only later extended to Colombia and other South American countries through regional partner initiatives. It is heavily used and well-cited in environmental science generally (several hundred academic papers), but it is not a dataset the cross-national democracy-and-deforestation literature we engage with here -- Sanford, B&B, or related citations -- has used. That is not itself a reason to distrust MapBiomas, but it does mean we have no precedent in this specific literature for how its results should be read, the way we do for GLAD products.

Three limitations to the data exist. First, MapBiomas Colombia is available to us only as a pre-aggregated municipal-level CSV. We cannot independently verify or fully audit MapBiomas's own aggregation method. Second, its temporal-filtering step (above) is documented as configurable and interpreter-judged rather than a fixed rule applied identically to every transition, which is one further step removed from full transparency compared to TMF's explicitly-stated, uniform persistence rule. Third, MapBiomas's own accuracy documentation notes that its classification algorithm has been revised across collection versions to improve accuracy, which can introduce inconsistencies in temporal (year-over-year) analysis specifically.

#### 2.1.3 Tropical Moist Forests

The Tropical Moist Forest (TMF) dataset (v1_2025) developed by Vancutsem et al (2021) for the [Joint Research Centre at the European Commission](https://forobs.jrc.ec.europa.eu/TMF) uses Landsat images at 30m resolution to provide annual reclassification of forest cover between 1990-2023 (`AnnualChanges`). It can be extracted via Google Earth Engine (`projects/JRC/TMF/v1_2025/AnnualChanges`). And it is bidirectional data: each year, JRC independently reclassifies every pixel, allowing it to move into or out of "forest" in either direction, capturing both loss and regrowth in each year.

TMF uses a six-class categorization: undisturbed, degraded, deforested, regrowth, water, other. This already excludes agricultural and plantation land from the forest classes, without our needing to impose that exclusion ourselves. Notably, TMF's "Deforested land" class is itself subdivided, and one of its subcategories is specifically "forest converted to tree plantations" -- identified by JRC using external plantation-boundary data sources and photo-interpretation of high-resolution imagery (Vancutsem et al. 2021; JRC TMF Data Users' Guide, v1_2025). Therefore, unlike GFC, a pixel converted to oil palm is classified as deforested, never as undisturbed, degraded, or regrowth. Separating out the deforested category, we find it tracks GFC's `lossyear` almost perfectly in an ordinary year (2012-2013, municipality-year r > 0.9) but runs to roughly half of GFC's total during the 2016-2020 period, and lower still in 2021-2023. In the highest-instability frontier departments (Caquetá, Putumayo, Meta, Guaviare) during 2016-2020 period, a real share of disturbed land evidently shows some regrowth while GFC's single-year threshold counts it as loss.

JRC's methodology requires roughly 3 years of stable evidence before reclassifying a pixel as *regrowth*, and treats a disturbance as "degraded" rather than "deforested" unless it persists past 2.5 years. The stated purpose is to filter transient, single-year anomalies (such as a bad Landsat image, or storm damage that recovers quickly) that would otherwise flip a pixel's classification. This should make TMF's classifications more policy-relevant (durable change) than a single-year threshold classifier.

The drawback is that a real clearing or regrowth event in year *t* only enters TMF's published record once enough subsequent evidence has accumulated to confirm it -- timestamped to the *confirmation* year rather than the true event year. In this case, a contemporaneous specification (`d_forest_pct(t)` on `Democracy(t)`) would be misaligned with when the underlying land-cover change actually happened. This is improbable however because `AnnualChanges` is a retrospective recomputation of the full 1990-present record built from all Landsat evidence available as of the release date. There is no obvious reason JRC would report the confirmation year in a pixel's history rather than back-assign the true onset year, once both are already known at compute time.

To be safe, we checked whether the 3-year lag actually produces a *smoother* aggregate series in practice, since that would be the most direct evidence the persistence filter is doing its stated job. The result runs the other way: municipality-level year-to-year volatility (SD of `d_forest_pct`) is higher for TMF (median 0.73) than for GFC (0.13) or MapBiomas (0.48), and TMF's national annual series has a wider range (3.05 percentage points) than either (0.31 and 0.36, respectively). We also validated this data using a pre-existing study of Guaviare department, Katz-Asprilla et al. (2024), which reports an independently-published deforestation total for the same JRC product, the same municipios, and almost the same window we use. Our own extraction lands within 1.7% of their reported figure, on both the total and the year-to-year shape. Results are in Appendix A.3.

#### 2.1.4 IDEAM/SMByC (scoped, not yet run)

IDEAM's Sistema de Monitoreo de Bosques y Carbono (SMByC) is Colombia's own government forest-monitoring institute. Structurally, its "Dinámica de cambio de la cobertura de bosque" product sits much closer to TMF than to GFC or MapBiomas: it is genuinely bidirectional (Deforestación and Regeneración are separate, independently-assigned classes, not a loss-only mask), and its forest definition is a real national/legal standard rather than a remote-sensing threshold. Because it is built by a different institution, from a different (though still Landsat-based) processing pipeline, than TMF, a TMF-vs-IDEAM comparison would be a stronger test of whether the linear democracy-forest finding is robust to *whose* classification choices generated the DV, rather than another comparison with the GLAD-related products.

Two feasibility problems surfaced in scoping this data, however:

1. **Temporal resolution mismatch.** The product is only truly annual from 2013 onward. Before that, it exists as four multi-year period totals -- 1990-2000, 2000-2005, 2005-2010, 2010-2012 -- not year-by-year classifications. Forcing these four periods into an annual `d_forest_pct(t)` panel matching TMF/GFC/MapBiomas's full 2000-2023 window would mean interpolating across 5-10 year gaps. The defensible option is restricting any IDEAM comparison to 2013-2023 (11 years of true annual data) rather than the full panel window.
2. **No programmatic or bulk access.** Unlike TMF, GFC, and MapBiomas (Google Earth Engine assets, or in MapBiomas's case a ready municipal-level CSV export), IDEAM's product is not available as a GEE asset. The actual data is distributed as per-year GeoTIFF/shapefile downloads through IDEAM's `visualizador.ideam.gov.co` map gallery, mirrored via the national SIAC map catalog (`siac.gov.co/catalogo-de-mapas`). Building a municipio-year panel from this would require: (a) locating and downloading roughly 11-15 separate year/period rasters by hand, (b) reprojecting and clipping each to the DANE MGN municipio boundaries already used elsewhere in this project, (c) tabulating class area per municipio-year locally (e.g., `terra`/`exactextractr` in R). This would take a few days of work, not weeks.

### 2.2 Summary table of forest cover change data considered

| Source | Role | Resolution | Bidirectional? | Usable for this study? |
|---|---|---|---|---|
| VCF5KYR (Hansen and Song 2018) -- Sanford's actual source | External validation attempt | 0.05deg (~5.6km) cell, annual, 1982-2016 | Yes, but empirically noisy (Appendix A.0) | No -- too coarse for within-municipality analysis; change signal also doesn't correlate with TMF at a usable level (Appendix A.0) |
| FAOSTAT Land Use domain -- BnB's described source | External validation attempt | Country-level only, ~5-10 year real reporting interval | Yes in principle, monotonic in practice for Colombia | No -- no subnational data exists in this domain, and only 6 real (non-interpolated) data points exist for Colombia across 35 years |
| Hansen GFC (`lossyear`) | Memo 1 source | 30m pixel, cumulative mask vs. 2000 baseline | No, loss-only by construction, and `gain` is a non-annual cumulative total (Section 2.1) | Referenced only for comparison with Memo 1's finding (Section 4.2); not re-analyzed here |
| MapBiomas Colombia Collection 3.0 | Memo 1 source | 30m pixel, annual, land-*use* legend | Yes | Yes, as an independent cross-check (Section 4) |
| JRC Tropical Moist Forest (TMF) v1_2025 | Memo 2 source | 30m pixel, annual, 2000-2023 | Yes (independent reclassification each year) | Yes -- see Section 2 |
| IDEAM SMByC ("Bosque No Bosque" / "Dinámica de cambio de la cobertura de bosque") | Candidate validation source, scoped but not yet built (Section 2.5) | 1 ha minimum mapping unit; truly annual only 2013-2023, four multi-year periods (1990-2000, 2000-2005, 2005-2010, 2010-2012) before that | Yes (Deforestación / Regeneración classes, pairwise epoch comparison) | Possibly, for 2013-2023 only -- see Section 2.5 for the resolution mismatch and access-path caveats before committing |

#### 2.2.1 How each source operationalizes "forest cover change" specifically

The table above compares the scope and role of each dataset considered for our subnational analysis. But it is also important to compare basic conceptual differences as well. The table below compares definitions of "forest cover" and measurement approaches to "forest cover change" among the main candidate datasets.

| Source | "Forest cover" defined as | "Change" registered as |
|---|---|---|
| VCF5KYR (Hansen and Song 2018) | A continuous 0-100% tree-canopy-cover estimate per 0.05-degree cell, fit independently year by year from AVHRR/MODIS-era imagery; no fixed threshold applied in the source data itself (Sanford applies his own cutoff for his "primary forest" measure) | Year-over-year change in the continuous canopy-% estimate; bidirectional in principle, but each year's estimate is fit independently with no cross-year consistency requirement -- Appendix A.0 finds this produces national annual swings an order of magnitude noisier than TMF's |
| FAOSTAT Land Use domain | Country-reported "Forest land," aligned to FAO's own global definition (>=10% canopy cover, a lower bar than GFC's, IDEAM's, or MapBiomas's flooded-forest subtype) -- Colombia's specific reporting methodology behind that number is not detailed in the bulk data itself | A national annual total with almost no real temporal resolution: only 6 of 36 Colombia-series years (1990/2000/2010/2015/2020/2025) are actually reported, the rest FAO-interpolated; no subnational breakdown exists at all |
| **GFC (`lossyear`)** | `treecover2000`: any vegetation >=5m tall at >=30% canopy density at baseline year 2000 only; does not distinguish native forest from tree crops | A single, one-time threshold-crossing per pixel; loss-only, no persistence requirement, and a pixel is never reclassified back to forest even if it visibly regrows |
| **MapBiomas** | A trained Random Forest classifier's forest-formation branch ("Bosque"/"Formación Forestal," one of 15 natural-cover classes, structurally separate from the legend's 9 anthropogenic land-use classes) -- defined per natural region (Andes/Caribe/Orinoquía/Pacífico/Amazonía) as tree/shrub vegetation with a "more or less continuous" canopy generally >5m tall; one subtype (flooded/gallery forest) has an explicit >70% canopy / >15m height threshold. Canopy/height criteria guide which pixels get selected as *training* examples, not a live per-pixel rule (Fundación Gaia Amazonas 2025) | Independent annual reclassification across all 26 classes; bidirectional in principle, but passed through a temporal filter afterward that overwrites a single deviant year sandwiched between two matching years -- and the methodology document's own worked example of a transition considered too fast to filter is forest-to-agriculture (loss), not the reverse, raising the possibility that gain is smoothed toward "no change" more readily than loss is (Section 2.1.2) |
| **TMF** | Six-class classification (undisturbed, degraded, deforested, regrowth, water, other); no single fixed canopy-% threshold; "deforested" class has an explicit tree-plantation subcategory identified via external plantation-boundary data + photo-interpretation, excluding tree crops from all forest classes | Independent pixel reclassification every year; a ~2.5-3yr persistence requirement before confirming a degraded-to-regrowth or degraded-to-deforested transition -- applied the same way in both directions -- meant to filter transient single-year anomalies |
| **IDEAM SMByC** (not yet built) | National legal definition (Decreto 1655 de 2017/SNIF framework, FAO-aligned): "land occupied mainly by trees... in which tree cover predominates with a minimum canopy density of 30%, a minimum canopy height of 5 meters at the time of identification, and a minimum area of 1 hectare"; **explicitly and by definition excludes** commercial forestry plantations, oil-palm crops, and trees planted for agricultural production. A secondary "woody vegetation" buffer class exists for 10-30% canopy density / 0.5-5m height / >=0.5 ha areas -- not forest, but not counted as bare non-forest either | Pairwise comparison of two satellite-image epochs, classifying every unit into Bosque Estable (stable forest), No Bosque Estable (stable non-forest), Deforestación (forest -> non-forest), Regeneración (non-forest -> forest), or Sin información (cloud/shadow/relief gaps); annual epochs only from 2013 onward ("Desde 2013, el SMByC cuantifica anualmente la superficie de bosque y los cambios asociados"), four multi-year epochs before that |

### 2.3 Data sources summary

**GFC** stays in the comparison because it is the field's incumbent forest-change product -- the data behind Global Forest Watch's public numbers, and the Hansen-family data underlying Sanford's own study. That last point needs a caveat, not an endorsement: Sanford pairs grid-cell Hansen data with *national*-level democracy scores, exactly the scale mismatch B&B critique (Section 1). We avoid that specific error by pairing GFC with municipal-level democracy scores instead, matching scales throughout. Keeping GFC lets us ask how much of the difference between "what this literature has always used" and our result comes from the DV construction itself, rather than from Colombia being a special case. TMF-lossonly (Section 3.1) sharpens this further, isolating how much of TMF's difference from GFC is about *bidirectionality* specifically, holding the underlying pixel classification fixed.

**MapBiomas** is not a threshold-and-reclassification approach like TMF or GFC. Instead, it applies a trained Random Forest classifier. Agreement or disagreement between MapBiomas and the other two therefore tests whether the substantive finding depends on assumptions shared across canopy-threshold-style classifiers, or survives under a completely different classification philosophy. Section 4.2 shows that MapBiomas's curvilinear pattern is *more pronounced* than GFC's -- its quadratic term ($\beta_2 = 1.237$) is roughly double GFC's (0.59-0.69), and a larger share of its municipality-years sit on the downward-sloping side of the curve (40.6% vs. GFC's 28.5%).

**TMF** is the only source that is both bidirectional *and* applies an explicit persistence rule -- roughly 2.5-3 years of stable evidence before confirming a transition, applied the same way whether the transition is toward loss or toward regrowth. Its plantation exclusion is built into the classification itself, and it is the only source we could check using external validation (Katz-Asprilla et al. 2024's Guaviare total, within 1.7%; Appendix A.3).

Read together, these three sources triangulate the same measurement question from three different methodological starting points -- a canopy-density mask, a trained statistical classifier, and an expert multi-temporal reclassification with an explicit persistence rule -- rather than reproducing the same approach three times. The trade-off is that we cannot decisively conclude whether a linear or curvilinear relationship holds between subnational democracy and subnational forest change without justifying the use of one over the others.

## 3. Empirical Strategy

Following B&B, we replicate both linear and quadratic models of democracy and forest cover change at the subnational level. We present results from four DV constructions: **TMF** (a bidirectional measure), **TMF-lossonly** (the same TMF data, counting loss only), **GFC** (loss only, Memo 1's original source), and **MapBiomas** (bidirectional land-use change). Comparing all four lets us see whether results depend on *what gets counted* (loss only vs. bidirectional) or *which institution produced the data*. Section 4 reports all four side by side to answer this.

The short answer, detailed in Section 4.2, is that it's complicated. TMF-lossonly -- built from TMF's own classifier, but counting loss only -- shows the same curvilinear (U-shaped) pattern as GFC and MapBiomas. That looks like evidence that bidirectionality matters. But MapBiomas is bidirectional and still shows the curvilinear pattern, which is evidence against bidirectionality alone being the explanation. TMF's full bidirectional construction is the only one of the four that produces the clean linear result, and its unclear whether it's because it's bidirectional, because of TMF's specific 3-year persistence rules, or some combination of the two.

### 3.1 Dependent variable: forest-cover change

`d_forest_pct(t) = forest_pct(t) - forest_pct(t-1)`, where `forest_pct(t) = forest_ha(t) / municipal_area_ha * 100`. This is a **percentage-point** difference, not a relative percent change -- the latter would skew badly for municipalities with very little starting forest cover (common in Colombia's savanna and urban municipalities), which is plausibly why neither Sanford nor B&B use it either. We also include a forest-cover level *control*, `forest_pct(t-1)`: the level of forest cover the year before the DV's change window, testing whether the *rate* of change depends on the *starting level* (the same logic as a beta-convergence regression in growth economics). Both source papers treat this as a deliberate choice:

- Sanford (p. 754): "I also include a control for the amount of forest remaining in a cell at the start of the year because I expect deforestation rates might be higher in places that are partially forested than places that have 100% forest cover."
- Boehmelt and Bernauer (p. 4): "We do not include a lagged dependent variable in the estimations, but consider an item on the level of forest cover (in % of land area) in the year before a focal year to address unit-specific temporal path dependencies in deforestation."

Each source's "forest cover" is built differently, and the DV is always this same year-over-year percentage-point difference applied to each source's own definition of forest:

- **GFC**: `forest_ha(t)` = the 2000 baseline (pixels with `treecover2000` >= 30%) minus cumulative `lossyear` loss through year *t*. Loss-only by construction -- once a pixel is lost it cannot re-enter this sum, ever.
- **MapBiomas**: `forest_ha(t)` = the sum of every pixel MapBiomas classifies that year under its "Formacion Boscosa" (forest formation) category -- four named subtypes (Bosque, Manglar, Bosque inundable, Vegetación leñosa sobre arena). Bidirectional, the same way TMF is: a pixel can move into or out of any of these four subtypes every year, independently reclassified.
- **TMF**: `forest_ha(t)` = undisturbed + degraded + regrowth pixels. Bidirectional -- a pixel can move into or out of this sum in either direction, every year.
- **TMF-lossonly**: built from the same TMF data but restricted to loss only. It tracks transitions *into* the "deforested" category only. Because that's a loss, we flip its sign -- TMF-lossonly's DV is $-1 \times$ (deforestation-transition area / municipal area) -- so that, like the other three DVs, a more positive value always means a better forest outcome, and a positive regression coefficient always means more democracy predicts less loss (or more gain).

Following Sanford's convention of dropping cells that never have forest cover, we drop a municipality only if it has zero forest cover in *every* year 2000-2023 (the bidirectional generalization of his "zero at baseline" rule, which only reduces to "zero at baseline" once loss-only is assumed). Zero of 1,122 municipalities are excluded under this rule.

The figure below plots each source's national total, year by year, using exactly this same `forest_ha` column each source's own regression uses (TMF-lossonly isn't included -- it's a loss transition, not a cover level, so there's no comparable line to draw):

![National forest cover by source, 2000-2023](forest_stock_by_source.png)

The three lines sit at different heights because the four sources define "forest" differently (Section 2.1). TMF dips through the mid-2010s and recovers to a net increase; GFC and MapBiomas both decline steadily across the whole window, GFC monotonically by construction, MapBiomas because enough pixels move out of "Formacion Boscosa" each year to outpace the ones moving in.

### 3.2 Descriptive statistics

Colombia's land area covered by this panel is ~110.18M ha. As a share of that, 2000 forest cover ranges from 85% under GFC's definition (93.73M ha) to 48% under MapBiomas's (52.61M ha), with TMF in between at 62% (68.59M ha) -- a reminder that "forest cover" here means three different things by construction, not one quantity three sources happen to disagree about:

| | TMF | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Municipality-years | 26,928 | 26,928 | 25,200 | 26,472 |
| Municipalities (of 1,122) | 1,122 | 1,122 | fewer (baseline-2000 exclusion) | 1,103 (baseline-2000 exclusion; see note) |
| Years | 2000-2023 (24) | 2000-2023 (24) | 2000-2023 (24) | 2000-2023 (24) |
| National forest cover, 2000 | 68.59M ha | n/a (loss-only flow, not a stock) | 93.73M ha (30%-canopy def.) | 52.6M ha ("Formacion Boscosa" def.; see note) |
| National forest cover, 2023 | 69.53M ha (net +1.37%) | n/a (loss-only flow, not a stock) | 87.70M ha (net -6.4%) | 49.55M ha (net -5.8%) |

Note: Amazonian indigenous collective-reserve territories (*Entidad Territorial Indígena* or ETI) are reported by MapBiomas as their own named units rather than folded into a standard DIVIPOLA municipality. This represents close to 29% of the national "Formacion Boscosa" total (about 20.1M of 69.1M ha), concentrated in Amazonas, Guainía, and Vaupés. Roughly 16.4M ha of supra-municipal ETI territories (e.g., "Entidad Territorial Indígena PANI," "...CIMTAR") remain excluded because they don't correspond to any single municipio and would need real ETI polygon geometry, which we don't have, to apportion correctly. This exclusion is a second plausible, and separate, contributor to the MapBiomas curvilinearity discussed in Section 2.1.2 and Section 4.2. The excluded 16.4M ha sits in the most intact and indigenous-governed part of the Amazon (Amazonas, Guainía, Vaupés). National and indigenous authority rather than municipal democratic quality is arguably the main operative governance channel. If MapBiomas's usable sample is systematically missing this high-forest, low-municipal-relevance corner while TMF's isn't, MapBiomas's remaining sample tilts further toward agricultural-frontier and moderately-settled municipios -- plausibly the same municipios most consistent with a partial-democracy-worst-deforestation story. We have not tested this directly (it would require real ETI polygon geometry to re-apportion the missing 16.4M ha to host municipios and re-run the comparison), so it stands alongside the temporal-filter hypothesis as a second candidate mechanism.

**Variability across sources.** The table above describes each source's *level* and *net change* over the panel, but says nothing about how much the year-to-year signal actually moves around -- two sources with the same mean change can look completely different if one is dominated by a few volatile years and the other is smooth. We report two measures of spread: (1) municipality-level year-to-year volatility -- the standard deviation of `d_forest_pct` computed separately for each municipality across its observed years, reported as the median (and interquartile range) across all municipalities; and (2) national annual volatility -- the range (max minus min) and standard deviation of the unweighted across-municipality mean of `d_forest_pct` in each year, i.e., how much that single national number moves from one year to the next:

| | TMF | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Municipality-level SD of `d_forest_pct` (median) | 0.73 | 0.19 | 0.13 | 0.48 |
| Municipality-level SD, interquartile range | 0.32 -- 1.18 | 0.07 -- 0.38 | 0.06 -- 0.22 | 0.30 -- 0.74 |
| National annual series, range across years (pp) | 3.05 | 0.81 | 0.31 | 0.36 |
| National annual series, SD across years | 0.67 | 0.20 | 0.07 | 0.10 |

By both measures, TMF is by far the most volatile of the four series -- both at the municipality level and in its national annual figure -- despite (or *because of* the timing effect of) its multi-year persistence filter. TMF-lossonly and MapBiomas sit in the middle, GFC is the smoothest by construction (a cumulative loss-only mask cannot register the kind of one-year swings a bidirectional reclassification can). This is a caution against reading any single year's `d_forest_pct` for a given municipality too literally under TMF or MapBiomas -- both spreads are wide enough that a single-year value is a noisy signal of that municipality's underlying trend, which is part of why the panel regressions (Section 4) and the multi-year non-parametric checks (Section 3.5) rather than any single-year comparison are the right basis for the paper's claims.

### 3.3 Democracy measures (explanatory variables)

Pre-benchmarked `snelect` / `sncivlib` / `sndem`, source `data/panel/05_weighting/03_output/snvdem_col_weighted.rds`. These measure relative democratic quality across municipalities, with national-level shifts isolated out. The municipality + year fixed-effects panel regression absorbs each municipality's time-invariant baseline. Because these measures vary genuinely at the municipal level, this panel does not have the "artificial power" problem B&B identify in Sanford's grid-cell design.

Our democracy measures are composites of institutional quality *and* security/state-capacity conditions, among other factors, not a "pure" measure of electoral or civil-liberties democracy in isolation. This means we cannot cleanly add theoretically relevant interaction terms or control variables related to conflict intensity or state capacity without risking circularity.

### 3.4 Model specification

Following B&B, we replicate Sanford's two-way (municipality + year) fixed-effects panel regression:

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{Democracy}_{it} + \beta_2\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

Standard errors are clustered by municipality. We also report two supporting specifications used by B&B themselves -- a year-only-FE model (drops the municipality FE, retaining cross-municipality variation) and a random-effects model. Then, to test curvature, a squared democracy term is added in a second model:

$$
\Delta\text{Forest}_{it} = \beta_1\,\text{Democracy}_{it} + \beta_2\,\text{Democracy}_{it}^2 + \beta_3\,\text{Forest}_{i,t-1} + \mu_i + \tau_t + \varepsilon_{it}
$$

The turning point of a quadratic fit, $-\beta_1/(2\beta_2)$, is computed for each model: a positive $\beta_2$ implies a U-shape (turning point is a minimum), a negative $\beta_2$ an inverted U (turning point is a maximum). Democracy terms are entered on their normalized 0-1 scale (0 = least democratic municipality-year observed, 1 = most), matching how B&B report their own turning points. We run separate specifications for `snelect`, `sncivlib`, and `sndem`.

### 3.5 Non-parametric checks

A quadratic regression coefficient answers a narrow question: does *this specific functional form* (a parabola) fit the data better than a straight line? It can return a "significant" curvilinear result even when the true relationship isn't well described by a parabola, because a quadratic term is flexible enough to partially fit almost any mild non-monotonicity. To guard against this, every regression result below is paired with a non-parametric check: municipalities sorted into deciles by democracy score, with mean `d_forest_pct` reported bin by bin, no functional-form assumption at all.

---

## 4. Tests

### 4.1 Linear results

**Full panel, two-way FE, `snelect`:**

| | TMF (primary) | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| $\beta$ (t) | **1.086 (t=11.2)** | 0.551 (t=10.9) | 0.09-0.13 (t=2.8-4.3) | 0.08-0.29 (significant only under year-only FE) |

TMF-lossonly's linear coefficient (0.551) sits roughly midway between TMF-bidirectional's (1.086) and GFC's (0.09-0.13) -- consistent with TMF-lossonly sharing TMF's classifier and persistence-filtering but, like GFC, only measuring half the bidirectional signal. 

Below are the results for each data source:

**TMF-lossonly**

| | Two-way FE $\beta$ (t) | Year-only FE $\beta$ (t) | RE $\beta$ (t) |
|---|---|---|---|
| `snelect` | 0.551 (10.9) | 0.004 (0.1, n.s.) | 0.332 (11.0) |
| `sncivlib` | 0.570 (9.3) | 0.011 (0.2, n.s.) | 0.290 (8.1) |
| `sndem` | 0.641 (11.0) | 0.008 (0.2, n.s.) | 0.356 (10.6) |

TMF-lossonly's year-only-FE coefficient collapses to essentially zero -- unlike TMF-bidirectional, whose year-only-FE result (0.739-0.815) stays strong. That means TMF-lossonly's relationship with democracy lives almost entirely *within* municipalities over time (absorbed by municipality fixed effects), not in *cross-municipality* comparisons of which places are more or less democratic on average. TMF-bidirectional's signal, by contrast, shows up both ways.

**GFC**

| | Two-way FE $\beta$ (t) | Year-only FE $\beta$ (t) | RE $\beta$ (t) |
|---|---|---|---|
| `snelect` | 0.094 (3.2) | 0.125 (2.8) | 0.127 (6.4) |
| `sncivlib` | 0.018 (0.5, n.s.) | 0.177 (3.3) | 0.039 (1.5, n.s.) |
| `sndem` | 0.077 (2.5) | 0.156 (3.1) | 0.107 (4.5) |

**MapBiomas**

| | Two-way FE $\beta$ (t) | Year-only FE $\beta$ (t) | RE $\beta$ (t) |
|---|---|---|---|
| `snelect` | 0.083 (0.9, n.s.) | 0.285 (6.2) | 0.079 (1.8, n.s.) |
| `sncivlib` | -0.111 (-0.9, n.s.) | 0.443 (8.2) | 0.230 (4.5) |
| `sndem` | 0.021 (0.2, n.s.) | 0.375 (7.4) | 0.154 (3.2) |

MapBiomas is significant under year-only FE for all three measures, and this extends partway to random effects too -- RE is significant for `sncivlib` and `sndem` but not `snelect`. Two-way FE is not significant for any of the three. Section 4.3 below breaks this pooled result out by region and finds it is not uniform across the country.

Read together with TMF-lossonly's opposite pattern (strong within-municipality, null between-municipality), this is a further illustration of how much the *specification*, not just the *data source*, shapes what "the result" looks like.

**TMF's** linear relationship is the strongest and most consistent of the four sources. Sorting municipalities into deciles by `snelect` (no regression at all) gives a near-monotonic increase in mean `d_forest_pct` from decile 1 (-0.110) to decile 10 (+0.519). There is no dip in the middle deciles -- and, as Section 4.2's full four-source decile table shows, this dip-free shape is unique to TMF's bidirectional construction specifically, not to TMF's classifier more generally. All three democracy measures, under all three specifications B&B themselves report (two-way FE, year-only FE, random effects), are positive and significant at the municipality level using TMF data:

| | Two-way FE $\beta$ (t) | Year-only FE $\beta$ (t) | RE $\beta$ (t) |
|---|---|---|---|
| `snelect` | 1.086 (11.2) | 0.739 (14.4) | 0.829 (14.5) |
| `sncivlib` | 1.092 (8.7) | 0.810 (14.3) | 0.856 (14.1) |
| `sndem` | 1.251 (11.0) | 0.815 (14.9) | 0.905 (15.1) |

Aggregating to the department-level (n=33 departments) helps us test robustness in the face of spatial autocorrelation. Similar results hold, though less strongly under year-only FE, which drops the within-department comparison the linear result otherwise leans on:

| | Two-way FE $\beta$ (t) | Year-only FE $\beta$ (t) | RE $\beta$ (t) |
|---|---|---|---|
| `snelect` | 1.499 (4.4) | 0.615 (2.1) | 0.949 (3.2) |
| `sncivlib` | 1.847 (6.5) | 0.520 (1.8) | 0.835 (3.1) |
| `sndem` | 1.928 (6.0) | 0.589 (1.9) | 0.960 (3.3) |

Across every specification and level of aggregation, using TMF data **the linear relationship between subnational democratic quality and forest-cover change is positive and holds up consistently.**

MapBiomas provides an independent cross-check on this pattern. Municipality-year correlation between TMF's and MapBiomas's own change measures is weak-to-moderate in raw terms (Pearson r = 0.12, Spearman rho = 0.17-0.26). This is expected given two independently-built measures disagreeing on exact year-to-year timing and boundary pixels. What matters more is agreement on substance: nationally, 2000-2023, MapBiomas's own "Bosque" (forest-formation) subtype fell 6.73% (-3.02M ha) while "Manglar" (mangrove) rose 6.70% (+12K ha). This resembles an underlying statistical trend that a flattened forest/non-forest measure like TMF's cannot register, but fully consistent with TMF's net finding. Agreement between the two sources is stable across the 2016 accord (municipality-year Pearson r = 0.153 pre-2016, 0.132 post-2016), reassuring that this isn't an artifact of the two sources happening to diverge more in one period than the other.

### 4.2 Quadratic results

Memo 1 found a curvilinear relationship using both GFC and MapBiomas data. MapBiomas's quadratic term is highly significant (b1=-1.196, t=-4.10; b2=1.237, t=4.87; turning point=0.483 -- almost exactly the middle of the distribution, closely matching B&B's own ~0.5 and GFC's ~0.42-0.44). TMF tells a different story: the quadratic term does not disappear under bidirectional TMF, but what changes is *where* the turning point sits. In the full pooled panel, two-way FE, TMF's quadratic term remains statistically significant ($\beta_2 = 1.39$, t = 5.21, p < .0001), with turning point = 0.122:

| | Linear $\beta$ (t) | Quadratic $\beta_2$ (t) | Turning point | Share of municipality-years *below* turning point |
|---|---|---|---|---|
| GFC (Memo 1) | weak, t=2.8-4.3 | significant, 0.59-0.69 (t=6.1-6.7) | ~0.42-0.44 | **28.5%** |
| MapBiomas (Memo 1) | not significant (b=0.083, t=0.90) | 1.237 (t=4.87) | **0.483** | **40.6%** |
| TMF (Memo 2) | 1.086 (t=11.2) | 1.39 (t=5.21) | 0.122 | **0.8%** |
| TMF-lossonly (Memo 2) | not significant (b=0.077, t=0.65) | 0.462 (t=4.12) | -0.083 (below the observed 0-1 range) | **0.0%** |

Read on its own, TMF-lossonly's fitted turning point (-0.083) falls entirely outside the observed democracy range. But, because quadratic terms can be misleading, a non-parametric check can be useful. Below are decile-binned means `d_forest_pct`:

| Democracy decile (1=lowest, 10=highest) | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|
| GFC | -0.243 | -0.273 | **-0.287** | -0.274 | -0.260 | -0.234 | -0.222 | -0.205 | -0.181 | -0.136 |
| MapBiomas | -0.106 | -0.139 | **-0.140** | -0.135 | -0.079 | -0.062 | -0.070 | -0.042 | -0.018 | 0.043 |
| TMF-bidirectional | -0.110 | -0.099 | -0.053 | 0.046 | 0.133 | 0.172 | 0.246 | 0.324 | 0.439 | 0.518 |
| TMF-lossonly | -0.243 | **-0.292** | -0.290 | -0.270 | -0.243 | -0.252 | -0.230 | -0.224 | -0.200 | -0.149 |

TMF-bidirectional is the only one of the four with no dip at all (decile 1 is already its minimum, and every subsequent decile rises). TMF-lossonly, GFC, and MapBiomas, however, all bottom out in deciles 2-3 before rising. TMF-lossonly's shape visibly tracks GFC's and MapBiomas's, not TMF-bidirectional. The quadratic fit misses TMF-lossonly's real (if shallow) dip because the strong rise in the top few deciles pulls the fitted parabola's turning point past the left edge of the data.

![Forest-cover change by democracy decile, all four sources](decile_ushape_comparison.png)

TMF-lossonly's dip suggests loss-only construction produces the curvilinear pattern. But MapBiomas is *bidirectional* -- it reclassifies every pixel independently every year and can register a pixel moving from forest to non-forest and back -- and it still shows the same dip as GFC and TMF-lossonly. If bidirectionality alone explained the difference, MapBiomas should look like TMF-bidirectional. It doesn't. The divergence may stem from its persistence-filtering rule, which MapBiomas's independent year-by-year reclassification doesn't have. Distinguishing "bidirectional" from "bidirectional and persistence-filtered" would need a construction that varies one without the other, which we don't have among these four sources. Additional analyses show a period-split behind the TMF result (a pre-2016 U-shape recurs under year-only FE, and the relationship's shape flips post-2016) (see Appendix).

#### 4.3 MapBiomas: a regional breakdown

Section 2.1.2 notes that MapBiomas defines "forest" separately for five natural regions (Andes, Caribe, Orinoquía, Pacífico, Amazonía), each with its own region-specific legend. That is a reason to check whether MapBiomas's pooled national result (Section 4.1's table above, and Section 4.2's quadratic results) actually holds within each region, rather than being an artifact of how the five regions' different-sized samples combine in the pooled panel. We re-ran the same two-way-FE linear and quadratic specifications (municipality-clustered SEs) separately within each region:

| Region | Municipalities | Departments | Linear $\beta$ (t), `snelect` | Quadratic $\beta_2$ (t), `snelect` | Turning point |
|---|---|---|---|---|---|
| Andes | 629 | 11 | 0.058 (0.4, n.s.) | **1.200 (3.8)** | 0.522 |
| Caribe | 193 | 7 | -0.716 (-1.8, n.s.) | 0.107 (0.1, n.s.) | -- (n.s.) |
| Pacífico | 177 | 4 | 0.060 (0.5, n.s.) | 1.031 (2.0) | 0.410 |
| Orinoquía | 59 | 4 | -0.159 (-1.0, n.s.) | 0.810 (1.7, n.s.) | -- (n.s.) |
| Amazonía | 45 | 6 | -0.662 (-1.4, n.s.) | 2.109 (1.4, n.s.) | -- (n.s.) |

The pooled national quadratic result (Section 4.2: $\beta_2=1.237$, t=4.87, turning point=0.483) is not evenly distributed across these five regions. The Andes alone is 57% of MapBiomas's usable sample (629 of 1,103 municipalities), its quadratic term is the only one that matches the national result's significance level, and its turning point (0.522) sits closest to the pooled figure. Pacífico shows a weaker version of the same shape (significant quadratic term, turning point 0.410, similar direction), but Andes and Pacífico combined are still 806 of 1,103 municipalities (73%), so this is closer to two regions reproducing most of a national average than five regions independently confirming it.

The other three regions diverge in different ways. Caribe's quadratic term is not significant in any of the three democracy measures (`sncivlib` t=0.09, `sndem` t=-0.29), but its *linear* term is negative and significant for `sncivlib` (b=-1.895, t=-4.28) and `sndem` (b=-1.270, t=-3.04) -- the opposite sign from the national linear-results table above. Orinoquía and Amazonía show no statistically significant relationship, linear or quadratic, under any of the three democracy measures. This is plausibly a small-sample problem (45-59 municipalities, 4-6 departments each) rather than evidence the relationship is truly absent there. Amazonía in particular overlaps heavily with the ETI-exclusion issue already flagged in Section 3.2: the municipios MapBiomas can score here are disproportionately the agricultural-frontier and moderately-settled ones, with the deepest, most indigenous-governed forest excluded from the municipal panel entirely.

Read together, this cuts against treating MapBiomas's national curvilinear result as a single, geographically uniform phenomenon -- it looks much more like an Andes (and, more weakly, Pacífico) pattern that the pooled panel reports as if it were national.

### 4.4 TMF decomposition: a degraded-forest sensitivity check

Our definition of "forest cover" using TMF categories -- undisturbed + degraded + regrowth -- treats a pixel that has been selectively logged, edge-damaged, or otherwise disturbed but not fully cleared as equivalent to forest. However, if degradation is growing quietly inside a "forest" pixel, the net trend could look far healthier than the underlying forest actually is. Other sources that are more sensitive to degradation (e.g., FAO or GFW) would disagree with TMF for substantive reasons.

And indeed, between 2000 and 2023, undisturbed forest fell from 65.2M ha (95.1% of TMF's "forest" total) to 57.1M ha (82.1%) -- a monotonic decline. Over the same period, degraded forest more than doubled (3.1M ha, 4.5% of forest, to 6.4M ha, 9.3%) and regrowth grew more than twentyfold (0.3M ha, 0.4%, to 6.0M ha, 8.7%).

| Definition | Forest cover, 2000 | Forest cover, 2023 | Net change |
|---|---|---|---|
| Current DV (undisturbed + degraded + regrowth) | 68.59M ha | 69.53M ha | **+1.37%** |
| Strict (undisturbed + regrowth; degraded excluded) | 65.52M ha | 63.09M ha | **-3.71%** |
| Pristine (undisturbed only) | 65.24M ha | 57.06M ha | **-12.54%** |

To test this, we re-ran the full two-way FE panel under stricter definitions and found that the linear democracy relationship does not weaken as the definition tightens. On the contrary, it strengthens, and the curvilinear result weakens even more:

| | Linear $\beta$ (t) | Quadratic $\beta_2$ (t) | Turning point |
|---|---|---|---|
| Current DV | 1.089 (11.1) | 1.395 (5.19) | 0.122 |
| Strict (no degraded) | **1.454 (13.5)** | 1.987 (6.60) | 0.149 |
| Pristine (undisturbed only) | 0.366 (6.5) | **0.096 (0.77, n.s.)** | -1.39 (meaningless) |

Under the pristine-only definition -- the strictest, most conservative reading of "forest" -- the quadratic term is not statistically significant at all, and the turning point is outside the observed range. If anything, removing degraded and regrowth pixels strengthens support for the linear hypothesis between democracy and forest cover change.

We also tested a more pointed question: is weaker democracy associated with faster growth in degradation? Regressing the year-over-year change in a municipality's degraded-forest share on `snelect_norm` (two-way FE, municipality-clustered SEs) gives b = -0.32, t = -8.01: higher-democracy municipalities show significantly *slower* growth in degraded-forest share over time. This suggests that democracy does not predict more degradation.

This claim still holds, and is in fact consistent with Section 4.2's own revised conclusion: GFC and MapBiomas both still show a real, centrally-located curvilinear pattern under every version of TMF we tested here, including the strictest versions which show no curvature at all. If GFC's and MapBiomas's curvature were simply a matter of them registering degradation as loss where TMF's blended definition doesn't, TMF's own pristine-only measure should reproduce that same pattern -- and it does not. That rules out *this specific* candidate explanation (degradation-counting), the same way Section 4.2 ruled out "bidirectional vs. loss-only" as a clean, sufficient explanation once MapBiomas is considered. Both leave the discrepancy between GFC/MapBiomas and TMF open. (MapBiomas's own quadratic result is itself uneven across regions -- see Section 4.1.1.)

### 4.5 Spatial dependence

Municipality and year fixed effects do not remove a shock that hits a cluster of neighboring municipalities in a single year but not the whole country -- a regional conflict flare-up, a regional drought, a local commodity boom. Moran's I on the quadratic model's residuals confirms real spatial autocorrelation (I = 0.131, p < 1e-8 for TMF; GFC and MapBiomas show even stronger clustering, I = 0.257-0.597 across years). Re-clustering standard errors by department (33 clusters) instead of municipality (1,122 clusters) shows that **TMF's and TMF-lossonly's linear coefficients survive comfortably** (t-stats remain well above 4 across all three democracy measures, for both the bidirectional and loss-only constructions), but **GFC's and MapBiomas's linear coefficients do not** -- their department-clustered t-statistics fall to 0.1-1.6, below conventional significance, even where the same coefficient is itself significant under municipality-clustering. 

Quadratic terms weaken more uniformly across all four sources under department-clustering (t-stats fall from a 3.3-6.5 muni-clustered range to roughly 1.6-2.9), with GFC's the most robust of the four and TMF-lossonly's `sncivlib` specification the only one dropping below conventional significance. 

This is a further reason the linear relationship, not the quadratic one, is the more defensible finding for the paper. And TMF/TMF-lossonly may be preferred over GFC/MapBiomas specifically, since only the former survive a spatial-clustering check this basic.

All four data sources, all three democracy measures, two-way FE, full panel (33 department clusters vs. 1,122 municipality clusters):

| | TMF | TMF-lossonly | GFC | MapBiomas |
|---|---|---|---|---|
| Linear $t$ (muni-clustered) | 8.7 to 11.2 | 9.3 to 11.0 | 0.5 to 3.2 | -0.9 to 0.9 |
| Linear $t$ (dept-clustered) | 4.3 to 4.8 | 5.1 to 6.9 | 0.3 to 1.6 | -0.5 to 0.5 |
| Quadratic $\beta_2$ $t$ (muni-clustered) | 3.4 to 5.2 | 3.3 to 4.1 | 5.9 to 6.5 | 4.8 to 5.2 |
| Quadratic $\beta_2$ $t$ (dept-clustered) | 2.2 to 2.6 | 1.6 to 2.4 | 2.8 to 2.9 | 2.5 to 2.7 |

Per-measure detail (`snelect` / `sncivlib` / `sndem`):

| Source | Linear $t$ (muni) | Linear $t$ (dept) | Quad. $\beta_2$ $t$ (muni) | Quad. $\beta_2$ $t$ (dept) |
|---|---|---|---|---|
| TMF | 11.2 / 8.7 / 11.0 | 4.7 / 4.3 / 4.8 | 5.2 / 3.4 / 4.8 | 2.3 / 2.2 / 2.6 |
| TMF-lossonly | 10.9 / 9.3 / 11.0 | 6.9 / 5.1 / 6.6 | 4.1 / 3.3 / 4.1 | 2.1 / 1.6 / 2.4 |
| GFC | 3.2 / 0.5 / 2.5 | 1.6 / 0.3 / 1.3 | 6.3 / 5.9 / 6.5 | 2.8 / 2.8 / 2.9 |
| MapBiomas | 0.9 / -0.9 / 0.2 | 0.5 / -0.5 / 0.1 | 4.8 / 5.2 / 5.2 | 2.5 / 2.7 / 2.7 |

---

## 5. Implications and Limitations

### Implications

- Subnational democratic quality and subnational forest-cover change in Colombia can be measured using different data sources.
1. Using TMF-bidirectional data, we observe a **linearly, positively correlated** pattern: municipalities with better electoral/civil-liberties quality show smaller net forest losses (or larger net gains). Within TMF, this linear reading beats the curvilinear alternative considering non-parametric deciles (Section 3), the degraded-forest sensitivity battery (Section 4.3), and department-clustering (Section 4.4). The magnitude is modest in any single year but consistent across specifications: a 0.1-unit increase in `sndem` (normalized 0-1) predicts roughly a **0.13 percentage-point** increase in annual forest-cover change (0.11 pp for `snelect` and `sncivlib` individually), two-way FE, $t > 8$ throughout (Section 4.1).
2. GFC, MapBiomas, and even TMF-lossonly all show the same centrally-located curvilinear pattern TMF-bidirectional does not (Section 4.2) -- and, in a further asymmetry, GFC's and MapBiomas's *linear* coefficients themselves do not survive department-clustering (t-stats fall to 0.1-1.6), while TMF's and TMF-lossonly's do (Section 4.4). TMF-bidirectional and TMF-lossonly are the only two of the four sources whose central result is robust to both the parametric-vs-non-parametric check and the clustering check simultaneously. That is the actual basis for leading with TMF, not merely that it was the most recently adopted source.

- *[Placeholder -- Kelly to fill in: specific local-governance mechanisms hypothesized to drive this relationship (land-use planning authority, environmental licensing capacity, electoral accountability for visible environmental outcomes, etc.). Flagging this explicitly rather than guessing at mechanisms she has in mind.]*

### Limitations

- **There is no single, agreed definition of "deforestation" across the literature this paper engages with, and it is not a simple loss-only-vs-bidirectional split.** Section 2.3 and Section 4.2 show four genuinely different constructions (a canopy-threshold mask, a persistence-filtered multi-temporal reclassification, a loss-only cut of that same reclassification, and a trained statistical classifier) rather than two. Only one of the four -- TMF's full bidirectional construction -- produces the clean linear result; the other three converge on a similar curvilinear pattern despite differing from each other in construction, institution, and even bidirectionality itself (MapBiomas is bidirectional and still curvilinear). We can name plausible mechanisms (TMF's persistence rule; MapBiomas's temporal filter and ETI-territory exclusion; Section 2.1.2, Open Question 4) but have not proven any of them. Separately, primary/old-growth forest loss is ecologically irreversible on any policy-relevant timescale and carries distinct carbon and biodiversity consequences that secondary regrowth cannot quickly replace; our bidirectional measure cannot distinguish the two, by design and because none of the source data we examined can either.
- **Whether secondary-forest regrowth is a "sufficient" substitute for primary loss is politically ambiguous, and that ambiguity may itself vary by observer.** A municipality that clears old-growth forest and later sees agricultural land revert to secondary growth will look identical to one that never lost old-growth forest at all, under our DV. Whether local politicians, voters, or national policymakers treat these as equivalent outcomes is an open and likely context-dependent question -- and it bears on how confidently this paper can claim political *salience* for the outcome we measure, separate from the outcome's ecological meaning.
- **Index circularity.** `snelect`/`sncivlib`/`sndem` already incorporate violence and fiscal-capacity proxies, which rules out adding those same constructs back in as a separate interaction term without risking circularity (Section 3). A "purer" institutional-only re-weighting (excluding those components, as done for the LOO analysis elsewhere in the project) is the correct non-circular path if we want to probe this further; not yet done (Open Question 5).
- **Spatial dependence is real, confirmed, and source-dependent, not uniform (Section 4.4).** TMF's and TMF-lossonly's linear coefficients survive department-clustering comfortably (t-stats 4.3-6.9 across the three measures); GFC's and MapBiomas's do not (t-stats 0.1-1.6, several losing significance entirely, even though some of those same coefficients are significant under municipality-clustering or a different specification -- Section 4.1's completed three-specification battery). Quadratic terms weaken more uniformly across all four sources under department-clustering. This is a second, independent reason (alongside the non-parametric decile check) to prefer TMF/TMF-lossonly's linear result over any of the four sources' quadratic ones.
- **External validation is now real, though it took finding the right comparison.** Sanford's own data doesn't correlate with TMF on change (Appendix A.0) due to a documented noise problem in VCF5KYR, not a flaw in TMF, and FAO's data has no subnational dimension and almost no real annual variation -- neither offers a usable check, because neither is built at a scale that could agree or disagree with a municipal panel in the first place. What does check out: reproducing Katz-Asprilla et al. (2024)'s independently-published Guaviare deforestation total from their own cited extraction script gets within 1.7% of their reported figure (Appendix A.3), matching not just the total but the year-to-year shape (mid-2000s trough, post-2016 surge) their narrative independently describes. That is a genuine external check against a real, published Colombian number, not just an internal robustness test.

---

## Appendix A: Robustness detail (period-split and specification battery)

*This appendix retains the fuller period-split and three-specification (two-way FE / year-only FE / random effects) evidence behind Section 4.2's headline. It is not required reading for the team's first pass through this memo -- it exists for reviewer-proofing and for anyone who wants to verify the headline claim in more depth.*

### A.0 Supplementary check: TMF vs. VCF5KYR (Sanford 2023) vs. FAO (B&B 2025) data

Section 2.1 notes that Sanford's data (VCF5KYR -- a GLAD-produced measure) is too coarse for a municipal-level panel. We also checked directly whether it agrees with TMF where the two do overlap in scale. We downloaded Sanford's own processed replication data (`GID_2_aggregated.Rdata`, admin-level-2 units, roughly equivalent to Colombian municipios) from the public Harvard Dataverse archive (DOI 10.7910/DVN/EF7R0Z), built a name-based crosswalk to our own municipal boundaries via GADM (matching 1,010 of 1,122 municipalities), and compared his `forest.diff` series against our TMF-based `d_forest_pct` for the 2000-2016 window both sources cover (script: `sanford_correlation.R`, `sanford_correlation2.R`):

- **Change-signal correlation is essentially zero:** municipality-year Pearson r = 0.048, Spearman rho = 0.106 (n = 17,170); net long-run change per municipality (2000 vs. 2016) has r = 0.001.
- **Forest-cover *level* correlation is strong:** r = 0.649, rho = 0.687 (n = 17,170) -- a municipality TMF says is heavily forested is very likely one Sanford's data also says is heavily forested.

VCF5KYR's canopy-percent estimates are fit independently, year by year, from coarse AVHRR/MODIS-era imagery with no requirement of temporal consistency. Sanford's dataset subsetted for Colombia bears this out: national annual means swing from -7.5 to +8.4 percentage points year to year, versus TMF's -0.84 to +0.45. This is consistent with TMF and VCF5KYR targeting the same underlying construct, with TMF's finer, temporally-persistent classification.

We also downloaded and cleaned FAOSTAT's Land Use domain bulk data directly (`faostat_colombia.R`, `faostat_check_subnational.R`) to confirm reporting frequency (only six of Colombia's thirty-six annual "Forest land" values -- 1990/2000/2010/2015/2020/2025 -- are reported by the country's agency; the rest are imputed by FAO). FAO's Colombia series declines monotonically across our study window (62,570 -> 61,432 -> 60,964 -> 60,123 thousand ha, 2000/2010/2015/2020; -3.9% cumulative). This is a fundamentally different trajectory from our own bidirectional TMF measure, which is *non-monotonic*: a mid-2010s trough (68.59M ha in 2000 to 66.19M ha in 2015; Section A.2) followed by a recovery to a net national *increase* by 2023 (69.53M ha, +1.37%; Section 3's descriptive statistics). TMF's net-positive, trough-and-recover trajectory is the outlier among every other source checked here, all of which show some version of ongoing net decline.

### A.1 Period-split test (quadratic model)

It may be that a dramatic national-level intervention -- such as the 2016 Peace Agreement -- created new deforestation trends at the subnational level. Splitting the TMF panel at 2016 shows the pooled full-panel quadratic result reveals two different sub-period relationships:

| | two-way FE | year-only FE | random effects |
|---|---|---|---|
| Pre-2016 $\beta_2$ (t) | -0.026 (n.s.) | **1.356 (t=7.0)** | 0.356 (t=2.2) |
| Pre-2016 turning point | -- | **0.449** | -0.143 |
| Post-2016 $\beta_2$ (t) | -1.165 (t=-2.8) | -2.921 (t=-6.3) | -2.040 (t=-5.2) |
| Post-2016 turning point | 0.290 | 0.864 | 0.781 |

Pre-2016, only year-only FE finds a significant U-shape, with a turning point (0.449) closely matching Boehmelt and Bernauer's own ~0.43-0.51 range and Memo 1's GFC-based estimates. Post-2016, all three specifications agree on an inverted U. Testing alternative break years (2006 when the AUC paramilitaries demobilized, or 2017-2020 after several other policies from the 2016 agreement were implemented) and rolling 6-year windows shows the pre/post shift is gradual, not a step change at the accord's signing. Year-only FE's $\beta_2$ declines steadily from 2000-05 (+2.00) through a null window at 2014-19 (0.07, n.s.), then turns significantly negative through 2018-23 (-4.64).

To address spatial autocorrelation, again we calculated department-clustered standard errors. The pre-2016 year-only-FE U-shape survives (t: 7.01 muni-clustered -> 4.49 department-clustered); the post-2016 version survives barely (t: -6.28 -> -2.44); and the full-panel and post-2016-two-way-FE versions do not survive (t: 3.18 -> 1.39; -2.80 -> -1.06). Linear coefficients survive department-clustering almost everywhere by comparison (t-stats remain 1.1-7.7). Adding department fixed effects to the year-only-FE quadratic model shows department identity absorbs 57% of the pre-2016 curvature and 20% of the post-2016 curvature 

In sum, the pre-2016 U-shape survives under year-only FE (TP=0.562) but the post-2016 inverted U is not significant under any specification at this level of aggregation. A non-parametric quartile check shows a monotonic decline pre-2016 and monotonic increase post-2016 with no hint of a turn in either period. Department-level aggregation does not rescue the curvilinear hypothesis.

### A.2 TMF data case studies

TMF registers the following trend in Colombia's forest cover: 68.59M ha (2000) -> trough of 66.19M ha (2015) -> 69.53M ha (2023). Extreme forest growth concentrates in Antioquia's highland dairy/cattle belt (Entrerríos +31.6pp, Santa Rosa de Osos +29.8pp) and Tolima/Nariño highlands, suggesting post-agricultural secondary regrowth. Extreme deforestation clusters in three area: mining zones (Remedios -24.8pp, Segovia -22.7pp), agro-industrial belts (Urabá, Magdalena banana/palm zones), and Amazon/former-FARC-frontier municipalities (La Macarena, Cartagena del Chairá, Puerto Guzmán). Providencia (San Andrés) shows impacts from Hurricane Iota (Nov. 2020). Guaviare is the worst pre/post-2016-shifting department, consistent with the "arc of deforestation" literature on former-FARC Amazon-frontier territory.

**Post-accord contextual literature.** Prem, Saavedra, and Vargas (2020) find deforestation surged in former-FARC-controlled municipalities after the ceasefire (291,973 ha in the first implementation year, 23% above 2016), attenuated in municipalities with higher state presence/judicial capacity. Clerici et al. (2020) document a parallel surge inside protected areas specifically. We cannot test this mechanism directly as an interaction term in our own model without risking circularity. But these readings provide interpretive context for why the relationship's shape may have shifted around 2014-2020.

### A.3 External validation of TMF data against Katz-Asprilla et al. (2024): the Guaviare deforestation total

*(Moved here from Section 2.1.3, which now only summarizes the result; this appendix carries the full check.)*

We also ran external validation of the data using Katz-Asprilla et al.'s (2024) subnational study of deforestation in the Colombian Amazon. They use TMF's `AnnualChanges` product to estimate legal and illegal deforestation in Guaviare department (2000-2020), one of three departments in the "northwestern arc of deforestation" that has accounted for nearly 60% of Colombia's total deforestation since 2010. Their study is department-level and focuses on forest cover *loss* only (they explicitly scope their analysis to "the deforestation disturbances, defined as a change in land cover (from forest to non-forested land)," p.6). Because the authors report a total deforestation figure for the same JRC product, the same four Guaviare municipios, and almost the same time window we use, we can check our own TMF data (extracted directly from Google Earth Engine) against an independently published number.

Their reported figure: **322,160 ha total deforestation in Guaviare, 2000-2020, an average of 15,341 ha/yr** (Katz-Asprilla et al. 2024, p.8). We used the Google Earth Engine script Katz-Asprilla et al. provide (2024, p.6, fn.1; replicating JRC's own official TMF extraction template) to match their construction directly: TMF version v1_2021 (our own panel uses v1_2025), `ee.Image.pixelArea()`'s true geodesic pixel area rather than a fixed per-pixel constant, and their specific deforestation-transition definition (undisturbed-or-degraded TMF classes transitioning to deforested), applied to the same four Guaviare municipios. (Script: `tmf_guaviare_decompose_gap.py`; output: `output/tmf_guaviare_decompose_gap.json`.)

**Our result: 327,581 ha total, 15,599 ha/yr average -- 1.7% above their reported total.** The annual series:

| Year | ha | Year | ha | Year | ha |
|---|---|---|---|---|---|
| 2000 | 18,797 | 2007 | 15,484 | 2014 | 25,287 |
| 2001 | 13,155 | 2008 | 13,915 | 2015 | 15,568 |
| 2002 | 6,768 | 2009 | 10,381 | 2016 | 20,670 |
| 2003 | 6,131 | 2010 | 12,886 | 2017 | 31,303 |
| 2004 | 15,379 | 2011 | 19,755 | 2018 | 32,431 |
| 2005 | 5,203 | 2012 | 17,011 | 2019 | 14,857 |
| 2006 | 13,135 | 2013 | 13,565 | 2020 | 5,898 |

A 1.7% gap between two independently-built extractions is a close match, on the total and on the *shape* of the series. Both our reconstruction and their narrative independently show a mid-2000s lull followed by a surge in forest cover loss beginning in 2017, the same pattern a wide literature on Amazon-frontier territory documents (Prem, Saavedra, and Vargas 2020; Clerici et al. 2020). The small residual is likely explained by differences in the municipal boundary shapefiles themselves, since Katz-Asprilla et al.'s paper names no source for it and ours is DANE's 2018 `MGN`.

---

## Open Questions

From Memo 2:
### Data validation and comparison
1. **Canopy-cover threshold.** GFC's 30% canopy-density threshold (applied to the continuous `treecover2000` estimate within each 30x30m Landsat pixel) defines "forest" in its baseline (Ertel et al. 2023), but this is a convention, not a universal standard (FAO's own global definition uses 10%). MapBiomas does **not** use a canopy-density threshold at all -- as Section 2.1.2 describes, it's a trained land-*use* classifier (assigning each pixel to "Bosque" or one of ~24 other categories), a conceptually different operationalization of "forest" than a canopy-cover cutoff, not a variant of the same convention. TMF likewise does not use a canopy-cover threshold: it is a six-class multi-temporal Landsat reclassification (undisturbed/degraded/deforested/regrowth/water/other; Section 2.1.3), incorporating external plantation-boundary data and photo-interpretation for specific subclasses (Vancutsem et al. 2021), not a single continuous canopy-density cutoff. Less central now that TMF is primary, but worth keeping the three straight since GFC and MapBiomas remain secondary sources.
2. **MapBiomas biome-boundary caveats.** A few departments show weak/negative TMF-MapBiomas agreement (Amazonas r=-0.227 on a small n=46 sample, Cauca -0.075, La Guajira -0.070), and the flooded-forest subtype ("Bosque inundable") correlates weakly/negatively with TMF (r=-0.046) -- plausibly a wetland/hydrology classification-boundary disagreement, not investigated further. Best-agreeing departments (Guaviare r=0.567, Caldas 0.359) are worth a sentence in the paper as a positive check.
3. **IDEAM/SMByC as a fourth validation source -- scoped, not built.** IDEAM data is bidirectional and government-produced, unlike GFC/MapBiomas/Potapov's shared GLAD lineage. But there are two obstacles: true annual resolution only exists 2013-2023 (four multi-year period totals cover 1990-2012), and there's no GEE asset or REST service, so a municipality-year panel would need to be built from per-year raster downloads reprojected/clipped/tabulated locally. Not yet decided whether to commit to building this.
4. **Why does TMF's full bidirectional construction produce a linear result while GFC, MapBiomas, and even TMF-lossonly all show the same centrally-located curvilinear pattern (Section 4.2)?** Bidirectionality alone doesn't explain it -- MapBiomas is bidirectional and still curvilinear. Reading MapBiomas's actual methodology document (Section 2.1.2) sharpened, but didn't resolve, the leading candidate: MapBiomas's post-classification "Filtro Temporal" smooths out single-year deviations, and the interpreter can choose which class transitions it applies to -- the methodology document's own worked example of a transition considered too fast to smooth away is forest-to-agriculture (loss), not the reverse. If gain gets smoothed toward "no change" more often than loss does, MapBiomas's *effective* signal would behave more like a loss-dominated measure than its nominal bidirectionality suggests, unlike TMF's ~2.5-3-year persistence rule (Section 2.1.3), which is applied the same way in both directions. We cannot confirm from the methodology document alone which specific transitions were actually exempted for Colombia's Collection 3 run -- would need either MapBiomas's own class-transition audit logs (if they exist and are shareable) or a pixel-level reconstruction of gain-vs-loss transition timing to test this directly, neither of which we have. A second, separate candidate sits alongside this one: MapBiomas's usable sample is missing 16.4M ha of supra-municipal ETI (indigenous collective-reserve) territory in the deepest, most intact part of the Amazon (Section 3.2's note), which TMF's extraction does not exclude. If that missing territory is exactly the high-forest, low-municipal-relevance corner of the distribution, its absence could tilt MapBiomas's usable sample toward the agricultural-frontier municipios more consistent with a partial-democracy-worst-deforestation story. Neither candidate has been tested directly; both are plausible and not mutually exclusive.

### Models
5. **Index circularity and a non-circular alternative.** A "purer" institutional-only re-weighting of `snelect`/`sncivlib`/`sndem` (excluding violence/fiscal-capacity components, as in the LOO analysis) is the correct non-circular path to probe how much of the linear/curvilinear finding is "institutions" versus "security and capacity conditions." Computationally heavy; not yet decided as worth doing.


From Memo 1: 
6. **Should the democracy measure itself be lagged?** Unresolved. Neither Sanford nor Boehmelt and Bernauer establishes this; we've kept it contemporaneous. Decide only if theoretically motivated.
7. **Subnational forest-policy framing.** A citation on the global national/subnational forest-policy landscape (which institutions control which decisions) would help frame why subnational governance data matters here, beyond the Snyder (2001) methodological argument in Section 2.
8. **Veto players and other B&B country-level controls** (`h_polcon3` and possible municipal analogues: council composition, CAR environmental licensing, prior consultation requirements). None currently computed in the snvdem-col panel.


**Reproducibility housekeeping.** Scripts behind this version's results (`tmf_extract.py`, `build_tmf_cover.R`, plus `deep_dive.R`, `linear_check.R`, `spatial_robustness.R`, `department_level_test.R`, `tmf_vs_mapbiomas_components.R`, `re_yearonly_test.R`, `turning_point_drift.R`, `turning_point_prepost.R`, `rebuild_mapbiomas_crosswalk.R`, `rebuild_mapbiomas_panel_and_rerun.R`, `tmf_degraded_sensitivity.R`, `curvature_diagnostics.R`, `tmf_guaviare_validation.py` [Appendix A.3], `tmf_guaviare_decompose_gap.py` [Appendix A.3's gap decomposition], `tmf_extract_lossonly.py`, `build_tmf_lossonly.R`, `tmf_gfc_divergence_test.R`, `tmf_lag_test.R` [Section 2.1.3's national loss-only/lag check], `dept_battery.py` [Section 4.4's four-source department-clustering battery], `decile_plot.py` [Section 4.2's decile-comparison figure and table], `stock_breakdown_plot.py` [Section 3.1's forest-cover-composition figure], `three_spec_battery.py` [Section 4.1's four-source, three-specification linear battery]) live in `scripts/deforestation/Analysis-v2/`. None are yet integrated into the main `colombia_deforestation_v3.R` pipeline with proper `etable()`/CSV exports -- the period-split, department-clustering, department-level-aggregation, and cross-source-validation results in this memo still trace to standalone script runs. `colombia_deforestation_v3.R` and `mb_muni_cover.rds` also still need to be updated to use the corrected MapBiomas crosswalk (`mb_muni_cover_corrected.rds`) as the canonical file, rather than each downstream script separately loading the correction. **This session found concrete instances of the risk that housekeeping item describes, twice**: (1) `output/fe_regression_table_mb_dept_cluster.tex` and `output/fe_regression_table_mb_quadratic.tex` were both generated from the uncorrected `mb_muni_cover.rds` (confirmed by file timestamps, ~31 hours before `mb_muni_cover_corrected.rds` was created) and are stale -- Section 4.4's newly-added table supersedes the dept-cluster one; the quadratic one has not yet been regenerated and should not be cited until it is. (2) `colombia_deforestation_v3.R` itself still loads `mb_muni_cover.rds` directly (line 1025), which feeds its `validation_annual_trends.png` (national forest-cover trend by source) -- the same chart, rebuilt with the corrected file, now appears as Section 3.1's figure; `validation_annual_trends.png` itself is superseded and should not be regenerated from the script as currently written. `dept_battery.py`, `decile_plot.py`, `stock_breakdown_plot.py`, and `three_spec_battery.py` were run in Python (`pandas`, `pyreadr`, `linearmodels`, `matplotlib`) rather than R, since no R installation was available in this environment -- worth re-verifying in R if/when this analysis moves toward the paper draft itself, though the Python figures already cross-check against every existing R-computed figure they overlap with (Section 4.4's note).

---

## References

Boehmelt, Tobias, and Thomas Bernauer. 2025. "New evidence reveals curvilinear relationship between levels of democracy and deforestation." *Research and Politics*, January-March 2025: 1-8. https://doi.org/10.1177/20531680251320073

Cameron, A. Colin, and Douglas L. Miller. 2015. "A Practitioner's Guide to Cluster-Robust Inference." *Journal of Human Resources* 50(2): 317-372.

Clerici, N., et al. 2020. "Deforestation in Colombian protected areas increased during post-conflict periods." *Scientific Reports* 10: 4971. https://doi.org/10.1038/s41598-020-61861-y

Ertel, Jessica, Liz Goldman, Justine Spore, and John Brandt. 2023. "Data 101: Comparing tree cover data over time." Global Forest Watch Blog, June 6, 2023. https://www.globalforestwatch.org/blog/data-and-tools/tree-cover-data-comparison/

European Commission Joint Research Centre. Tropical Moist Forest v1_2025, Data Users' Guide. Accessed via Google Earth Engine (`projects/JRC/TMF/v1_2025/AnnualChanges`). See Vancutsem et al. (2021), below, for the primary methods citation this guide summarizes.

Fergusson, Leopoldo, Santiago Saavedra, and Juan F. Vargas. 2020. "The perils of misusing remote sensing data: The case of forest cover." HiCN Working Paper 331, Households in Conflict Network. https://hicn.org/wp-content/uploads/2020/06/HiCN-WP-331.pdf

Food and Agriculture Organization of the United Nations. FAOSTAT Land Use domain (bulk data), accessed July 2026. https://www.fao.org/faostat/en/#data/RL

Fundación Gaia Amazonas. 2025. *Documento de Bases Teóricas de Algoritmo (ATDB), MapBiomas Colombia Colección 3, Apéndice Colombia.* Source for Section 2.1.2's account of MapBiomas's Random Forest classifier, regional forest-class legends, canopy/height criteria, and post-classification temporal filter.

Fundación Gaia Amazonas. 2024. *Documento de Bases Teóricas de Algoritmo (ATDB), MapBiomas Colombia Colección 2, Apéndice Colombia.* Predecessor version; not cited directly since this analysis uses Collection 3.0, but consulted to confirm the temporal-filter description is not new to Collection 3.

Global Administrative Areas (GADM). Version 3.6, Colombia boundaries. https://gadm.org/

Global Land Analysis and Discovery (GLAD), University of Maryland. "Global Forest Change 2000-2024 (v1.12) Data Description." https://storage.googleapis.com/earthenginepartners-hansen/GFC-2024-v1.12/download.html. Source for Section 2.1.1's confirmation that the `gain` band remains a single, non-annual 2000-2012 cumulative layer in the current release.

Harding, Robin, Mounu Prem, Nelson A. Ruiz, and David L. Vargas. 2024a. "Buying a Blind Eye: Campaign Donations, Regulatory Enforcement, and Deforestation." *American Political Science Review* 118(2): 635-653. https://doi.org/10.1017/S0003055423000412

Harding, Robin, Mounu Prem, Nelson A. Ruiz, and David L. Vargas. 2024b. Replication Data for "Buying a Blind Eye: Campaign Donations, Regulatory Enforcement, and Deforestation." Harvard Dataverse. https://doi.org/10.7910/DVN/GYX7GC. Files reviewed in Section 2.1.1: `Hansen_V2.R`, `001_data_deforestation.do`, `Hansen_2000_2018.csv`.

Hansen, M., & Song, X.-P. (2018). *Vegetation Continuous Fields (VCF) Yearly Global 0.05 Deg* [Dataset]. NASA Land Processes Distributed Active Archive Center. https://doi.org/10.5067/MEASURES/VCF/VCF5KYR.001

Hansen, M. C., et al. 2013. "High-Resolution Global Maps of 21st-Century Forest Cover Change." *Science* 342(6160): 850-853. https://doi.org/10.1126/science.1244693

Henisz, Witold J. 2000. "The Institutional Environment for Economic Growth." *Economics & Politics* 12(1): 1-31. (Cited via the Quality of Government dataset compilation used in Boehmelt and Bernauer's replication package as the source of `h_polcon3`; not independently re-verified against Henisz's original publication.)

International Crisis Group. 2021. *A Broken Canopy: Preventing Deforestation and Conflict in Colombia.* Latin America Report N°91, 4 November 2021. https://www.crisisgroup.org/rpt/latin-america-caribbean/colombia/091-broken-canopy-deforestation-and-conflict-colombia

Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM). Sistema de Monitoreo de Bosques y Carbono (SMByC), "Dinámica de cambio de la cobertura de bosque" / "Bosque No Bosque." Accessed via `visualizador.ideam.gov.co/CatalogoObjetos/maps-gallery` and the SIAC map catalog (`siac.gov.co/catalogo-de-mapas`), July 2026. Base product's own processing protocol not yet located -- see Section 2.5's verification note.

Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM) / SMByC. *Protocolo de monitoreo de la restauración de bosques con sensores remotos.* https://www.ideam.gov.co/sites/default/files/archivos/protocolo_monitoreo_restauracion_bosque_con_sensores_remotos_smbyc.pdf. Confirms the forest definition, plantation/oil-palm exclusion, and 2013 annual-monitoring start date cited in Sections 2.4-2.5; a companion restoration-specific protocol, not the base product's own methodology document.

Katz-Asprilla, David, Marie-Gabrielle Piketty, Guido Briceño Castillo, Lilian Blanc, Jhon Camacho Peña, and Alain Karsenty. 2024. "Subnational assessment of legal and illegal deforestation in the Colombian Amazon: consequences for zero deforestation commitments." *Regional Environmental Change* 24(3). https://doi.org/10.1007/s10113-024-02264-x

MapBiomas Project. Collection 3.0 of the Annual Land Use and Land Cover Mapping of Colombia. Municipal-level statistics exported via Estadisticas > Cobertura > Nivel territorio: Municipios, saved as `output/mapbiomas_col_muni_stats.csv`.

MapBiomas Colombia. *FactSheet MapBiomas Colombia, Colección 3.* National class-area summary and brief methodology overview; cross-checked against the Fundación Gaia Amazonas ATDB for Section 2.1.2.

Potapov, P., et al. 2022. "The global 2000-2020 land cover and land use change dataset derived from the Landsat archive: first results." *Frontiers in Remote Sensing* 3: 856903. https://doi.org/10.3389/frsen.2022.856903

Prem, Mounu, Santiago Saavedra, and Juan F. Vargas. 2020. "End-of-conflict deforestation: Evidence from Colombia's peace agreement." *World Development* 129: 104852. https://doi.org/10.1016/j.worlddev.2019.104852

Sanford, Luke. 2023. "Democratization, Elections, and Public Goods: The Evidence from Deforestation." *American Journal of Political Science* 67(3): 748-763. https://doi.org/10.1111/ajps.12662

Sanford, Luke. 2021. Replication data for "Democratization, Elections, and Public Goods: The Evidence from Deforestation." Harvard Dataverse. https://doi.org/10.7910/DVN/EF7R0Z

Snyder, Richard. 2001. "Scaling Down: The Subnational Comparative Method." *Studies in Comparative International Development* 36(1): 93-110.

Song, X.-P., M. C. Hansen, S. V. Stehman, P. V. Potapov, A. Tyukavina, E. F. Vermote, and J. R. Townshend. 2018. "Global land change from 1982 to 2016." *Nature* 560: 639-643. https://doi.org/10.1038/s41586-018-0411-9

Vancutsem, C., F. Achard, J.-F. Pekel, G. Vieilledent, S. Carboni, D. Simonetti, J. Gallego, L.E.O.C. Aragão, and R. Nasi. 2021. "Long-term (1990-2019) monitoring of forest cover changes in the humid tropics." *Science Advances* 7(10): eabe1603. https://doi.org/10.1126/sciadv.abe1603

World Resources Institute. 2024. "Brazil and Colombia See Dramatic Reductions in Forest Loss, But New Fronts Keep Tropical Rates High." https://www.wri.org/news/release-brazil-and-colombia-see-dramatic-reductions-forest-loss-new-fronts-keep-tropical-rates

World Resources Institute, Global Forest Review. "Data and Methods." https://gfr.wri.org/data-methods. Source for Section 2.1.1's caution against subtracting `gain` from `loss` to compute net forest change.

Zvoleff, Alex. *gfcanalysis: Tools for Working with Hansen et al. Global Forest Change Dataset.* R package. https://github.com/azvoleff/gfcanalysis. The extraction package Harding et al. (2024b) use; its documentation states gain is "not available in the GFC product on an annualized basis" (Section 2.1.1).

---

## Appendix B: Full Regression Tables

Municipality- and year-fixed-effects panel regressions, clustered standard errors by municipality (parentheses). Democracy terms on their normalized 0-1 scale. TMF (primary) tables first, then GFC/MapBiomas (secondary).

**Note:** the department-level, period-split, and department-clustered models discussed in Appendix A are not yet exported as `etable()`/LaTeX tables -- those numbers still trace to standalone script console output, not a table here. See the Reproducibility housekeeping note (this reference previously pointed to "Open Question 6," a mismatch that predates this session's renumbering; corrected here).

### TMF -- linear (primary)

\input{fe_regression_table_tmf.tex}

### TMF -- quadratic (curvilinear, primary)

\input{fe_regression_table_tmf_quadratic.tex}

### GFC -- linear

\input{fe_regression_table_gfc.tex}

### GFC -- quadratic (curvilinear)

\input{fe_regression_table_gfc_quadratic.tex}

### MapBiomas -- linear

\input{fe_regression_table_mb.tex}

### MapBiomas -- quadratic (curvilinear)

\input{fe_regression_table_mb_quadratic.tex}

---

## Appendix C: Deforestation and Democracy by Municipality

Numeric complement to the choropleths in Section 4 (`map_democracy_vs_forestchange.png`, built from TMF). Tables below use municipality-level means across the full panel, GFC first, then MapBiomas.

### GFC

**Forest-cover-change tercile vs. mean snelect.** Municipalities grouped into terciles by their own mean `d_forest_pct`, then averaged on `snelect` within each group.

\input{appendix2_tercile_table.tex}

**Named leaderboard.** The 10 highest-loss and 10 lowest-loss/highest-gain municipalities.

\input{appendix2_leaderboard_top10.tex}

\input{appendix2_leaderboard_bottom10.tex}

### MapBiomas

Same two views, MapBiomas source.

\input{appendix2_tercile_table_mb.tex}

\input{appendix2_leaderboard_top10_mb.tex}

\input{appendix2_leaderboard_bottom10_mb.tex}
