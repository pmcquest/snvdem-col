# Handoff: Colombia Deforestation × Subnational Democracy

**Session date**: 2026-06-24 (v1); updated 2026-07-10 (v2)
**Script**: `snvdem-col/scripts/deforestation/colombia_deforestation.R`
**For**: drafting a paper section on the utility of subnational democracy data for empirical research questions

**2026-07-10 update**: the script was substantially revised to align with Sanford (2023)'s methodology, per two team meetings (July 2, July 9). The dependent variable, control, sample, and model changed -- see `deforestation_memo.md` (memo v2) for the full rationale and open questions. This handoff file is kept for the data/file-inventory reference below; **the memo is now the canonical methodology document.**

---

## What was built and confirmed (v2)

### Data

Municipal-level panel, Colombia, 2001–2023. ~1,100 municipalities × 23 years (municipalities with zero forest cover at the 2000 baseline are excluded, per Sanford's sample restriction).

**Democracy measures**: `snelect` (elections), `sncivlib` (civil liberties), `sndem` (full index, `= 0.5*(snelect+sncivlib)`) -- the **pre-benchmarked / unbenchmarked** SN-VDem values, run as three parallel model specifications rather than `snelect` alone. Loaded from `data/panel/06_benchmark/03_output/snvdem_col_benchmarked.rds` (supersedes v1's `data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds`, which no longer exists after the pipeline's benchmarking stage was added). Merge key: DIVIPOLA 5-digit code `MPIO_CDPMP`.

**Dependent variable**: percentage-point change in forest cover, `d_forest_pct(t) = forest_pct(t) - forest_pct(t-1)` -- not absolute loss ha or a loss rate. See memo v2 §2 for why (matches Sanford's DV exactly, and resolves the log-transform problem that motivated this revision).

**Control**: forest-cover level at t-1, `forest_pct(t-1)`. Reconstructed from Hansen's `treecover2000` band (≥30% canopy = "forest," 2000 baseline) minus cumulative masked `lossyear` loss. No other controls (decided 2026-07-09: agriculture/GDP already embedded in SN-VDem index construction). See memo v2 §3.

**Model**: two-way (municipality + year) fixed-effects panel regression, clustered SEs by municipality (`fixest::feols`), one control (above). Run separately for each of the three democracy measures, in both a linear and a curvilinear (squared democracy term) specification -- the latter tests the U-shaped democracy/deforestation relationship Boehmelt & Bernauer (2025) find at the country level. See memo v2 §6.

**Primary deforestation measure (Hansen GFC)**: Hansen/GFW Global Forest Change 2023 v1.11 -- both `lossyear` (annual loss) and `treecover2000` (2000 baseline canopy %, new in v2) rasters. Extracted via `exactextractr`, masked to the 2000 forest threshold. 30m resolution, 0.09 ha/pixel. Outputs: `scripts/deforestation/output/gfc_muni_loss.rds`, `gfc_muni_baseline2000.rds`, `gfc_muni_cover.rds`.

**Validation / robustness measure (MapBiomas)**: MapBiomas Colombia Collection 3.0, Formacion Boscosa class (native + flooded forest). Downloaded as pre-computed statistics CSV from the MapBiomas Colombia platform (no raster processing) -- already reports cover per year directly, so no baseline reconstruction needed, only the same DV/control/exclusion treatment as GFC. Output: `scripts/deforestation/output/mb_muni_cover.rds`.

### Validation results (v1, loss_ha-based -- see memo v2 for updated numbers)

At the municipality-year level (n ≈ 25,000):

- **Pearson r = 0.771** (p ≈ 0)
- **Spearman ρ = 0.625** (p ≈ 0)

The two sources diverge in 2002–2003: MapBiomas records ~350k ha/yr vs. GFC ~180k ha/yr. The most plausible explanation is that MapBiomas captures secondary vegetation clearing driven by Plan Colombia coca eradication and conflict-related displacement — transitions visible as land-cover class changes but not as primary canopy loss. The series converge from 2005 onward and both show the well-documented post-2016 Peace Accord deforestation surge.

These correlations were computed on v1's `loss_ha`-based measure. v2 recomputes the same cross-source validation on `d_forest_pct` (Section 3.3 of the script) -- expect similar but not identical numbers; the underlying loss detection is unchanged, only how it's expressed. Pending results, see memo v2 §8.5.

### Substantive finding (v1 -- see memo v2 for updated numbers)

The snelect × forest loss scatter shows a non-linear (U-shaped) relationship. Very low-snelect municipalities (remote, no institutional presence) have low deforestation — no economic frontier pressure. Mid-range snelect municipalities (frontier zones, some connectivity, weak institutions) show the highest loss rates. High-snelect municipalities show lower loss, consistent with institutional capacity constraining extraction.

This was based on v1's log-scaled, always-positive DV. Whether this same U-shape holds for the signed, percentage-point `d_forest_pct` DV (and for `sncivlib`/`sndem`, and in the FE regression net of the lagged-cover control) is an open question for v2 -- worth checking once the updated script finishes, not assumed to carry over.

---

## An open theoretical question worth addressing

### Does MapBiomas capture the institutionally-relevant deforestation more cleanly than Hansen GFC?

Hansen GFC detects *any* canopy loss: fires, selective logging, natural disturbance, primary clearing — many of which are not responsive to democratic institutions. MapBiomas tracks *land cover class transitions*, meaning it specifically detects when land is cleared for agriculture, cattle ranching, or colonization — exactly the activities that weak democratic institutions fail to constrain (unclear tenure, impunity for illegal clearing, absence of environmental enforcement).

**The prediction**: MapBiomas loss should correlate more strongly with `snelect` than GFC loss, because its signal is more specific to the mechanism (land-use decisions mediated by institutions) rather than all sources of canopy disturbance.

**This is empirically testable** by comparing the Pearson r values from:
- Section 2.5 of the script: `snelect`/`sncivlib`/`sndem` × GFC `d_forest_pct`
- Section 3.2 of the script: `snelect`/`sncivlib`/`sndem` × MapBiomas `d_forest_pct`

If MapBiomas r > GFC r, this is not just a robustness check — it is a theoretically meaningful finding that tells us something about the *mechanism*: democratic institutions matter primarily for land-use decisions at the secondary-vegetation frontier, not for all forms of canopy disturbance. The 2002–2003 divergence (when politically-driven secondary clearing spiked and MapBiomas caught it while GFC didn't) is preliminary evidence for this reading.

This distinction could sharpen the paper's theoretical contribution: rather than "democracy predicts deforestation," the claim becomes "democratic institutions constrain the land-use decisions that drive colonization-type deforestation, and this is measurable at the subnational level."

---

## Task for the new session

Draft a paper section (~800–1,200 words) arguing for the **utility of subnational democracy data** to address empirical questions, using Colombia deforestation as the primary illustration.

The co-author's framing: *"The combination of something relevant to Colombia and globally and something with the promise of good data."*

### Four arguments the section needs to make

1. **Why subnational variation matters**: national democracy scores mask enormous within-country heterogeneity. Colombia's range of `snelect` across municipalities spans as much variation as the full cross-national distribution. A national-level analysis of "Colombia" would find nothing.

2. **The Colombia–deforestation fit**: Colombia is a hard test — it has active conservation law, a functioning state at the center, and the FARC demobilization creating a natural experiment (former conflict zones opened to clearing after 2016). That `snelect` predicts deforestation *in this context* is significant.

3. **Global generalizability**: both the GFC dataset and the SN-VDem framework exist globally. The pipeline is directly portable to other high-deforestation democracies (Brazil, Indonesia, DRC, Peru, Myanmar). The Colombia analysis is a proof of concept.

4. **Data quality and the MapBiomas argument**: anchor on the r = 0.77 validation, but also raise the theoretical point above — that MapBiomas may be the *more appropriate* measure for testing the institutional mechanism, because it specifically captures land-use-driven clearing rather than all canopy disturbance. If Section 3.2 shows MapBiomas r > GFC r, this strengthens rather than complicates the narrative.

---

## Key files

| File | Description |
|---|---|
| `scripts/deforestation/colombia_deforestation.R` | Full analysis script. Sections 1–4 are the executed workflow; Appendices A–C are dead-end alternatives wrapped in `if (FALSE)`. |
| `scripts/deforestation/deforestation_memo.md` | v2 methodology memo -- canonical reference for the DV/control/model design |
| `scripts/deforestation/output/gfc_muni_loss.rds` | Hansen GFC: municipality × year × loss_ha, 2001–2023 (masked to 2000 forest baseline) |
| `scripts/deforestation/output/gfc_muni_baseline2000.rds` | Hansen GFC: municipality × forest_ha_2000 (new in v2) |
| `scripts/deforestation/output/gfc_muni_cover.rds` | Hansen GFC: municipality × year × forest_ha/forest_pct/forest_pct_lag1/d_forest_pct, 2000–2023 (new in v2) |
| `scripts/deforestation/output/mb_muni_cover.rds` | MapBiomas: same cover-series columns as above (replaces v1's `mb_muni_loss.rds`) |
| `scripts/deforestation/output/muni_areas.rds` | Municipality × area_ha (new in v2, shared across sections) |
| `scripts/deforestation/output/mapbiomas_col_muni_stats.csv` | Raw MapBiomas download (wide format, pre-computed statistics) |
| `scripts/deforestation/output/fe_regression_table_gfc.tex` / `_mb.tex` | FE regression tables, 3 democracy measures x 2 forest-data sources (new in v2) |
| `data/panel/06_benchmark/03_output/snvdem_col_benchmarked.rds` | snelect/sncivlib/sndem panel (v2; supersedes the retired `08_final_snvdem_data/MC/SN_Index_tentative.rds`) |
| `data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp` | Municipal boundaries (DANE 2018) |
| `data/geospatial/GFC-2023/` | Hansen GFC lossyear + treecover2000 tiles (8 tiles, ~2.3 GB total; treecover2000 added in v2) |

### Output figures

| File | What it shows |
|---|---|
| `scatter_snelect_forestchange.png` | Main result: snelect vs. GFC Δforest_pct (all years) -- v2 rename of `scatter_snelect_forestloss.png` |
| `scatter_snelect_forestchange_mb.png` | Robustness: snelect vs. MapBiomas Δforest_pct |
| `scatter_snelect_forestchange_period.png` | Pre/post 2016 Peace Accord, faceted |
| `timeseries_forestchange.png` | National trends: forest stock, mean Δforest_pct, mean snelect |
| `map_mean_forestchange.png` | Municipal choropleth, mean annual Δforest_pct (diverging scale, v2) |
| `validation_scatter_gfc_vs_mb.png` | Cross-source scatter, now on Δforest_pct |
| `validation_annual_trends.png` | Annual national forest stock, both sources |

---

## Technical notes for reference

- `clean_mpio(x)`: `str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")` — standardizes DIVIPOLA codes
- `FOREST_THRESHOLD <- 30` (v2, top of script): % canopy cover (Hansen `treecover2000`) required to call a pixel "forest," applied to both the 2000 baseline and each year's loss mask so cumulative loss can never exceed baseline stock. See memo v2 §8.1 for the open question on this threshold.
- MapBiomas CSV has no DIVIPOLA codes; matching uses normalized municipality + department names joined to shapefile `MPIO_CNMBR` / `DPTO_CCDGO`. Indigenous territories (resguardos) are unmatched and dropped (<5% of forest area).
- Parallel processing (`future.apply`) is blocked on this machine (ND campus IT blocks localhost socket connections). All extraction runs sequentially.
- MapBiomas FOREST_CLASS = 2 (Formacion Boscosa) confirmed by spatial frequency analysis in a prior session.
- The reconstructed GFC cover series has no forest-gain term (Hansen's `gain` band isn't used) and is therefore monotonically non-increasing by construction -- see memo v2 §3, §8.2.
