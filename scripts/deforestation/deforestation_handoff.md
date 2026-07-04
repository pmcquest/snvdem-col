# Handoff: Colombia Deforestation × Subnational Democracy

**Session date**: 2026-06-24  
**Script**: `snvdem-col/scripts/deforestation/colombia_deforestation.R`  
**For**: drafting a paper section on the utility of subnational democracy data for empirical research questions

---

## What was built and confirmed

### Data

Municipal-level panel, Colombia, 2001–2023. ~1,100 municipalities × 23 years.

**Democracy measure**: `snelect` = "Elections Free & Fair" component of the SN-VDem subnational index, normalized 0–1. Loaded from `data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds`. Merge key: DIVIPOLA 5-digit code `MPIO_CDPMP`.

**Primary deforestation measure (Hansen GFC)**: Hansen/GFW Global Forest Change 2023 v1.11 `lossyear` raster. Extracted via `exactextractr` across 23 binary masks → annual forest cover loss in ha per municipality. 30m resolution, 0.09 ha/pixel. Output: `scripts/output/gfc_muni_loss.rds`.

**Validation / robustness measure (MapBiomas)**: MapBiomas Colombia Collection 3.0, Formacion Boscosa class (native + flooded forest). Downloaded as pre-computed statistics CSV from the MapBiomas Colombia platform (no raster processing). Output: `scripts/output/mb_muni_loss.rds`.

### Validation results

At the municipality-year level (n ≈ 25,000):

- **Pearson r = 0.771** (p ≈ 0)
- **Spearman ρ = 0.625** (p ≈ 0)

The two sources diverge in 2002–2003: MapBiomas records ~350k ha/yr vs. GFC ~180k ha/yr. The most plausible explanation is that MapBiomas captures secondary vegetation clearing driven by Plan Colombia coca eradication and conflict-related displacement — transitions visible as land-cover class changes but not as primary canopy loss. The series converge from 2005 onward and both show the well-documented post-2016 Peace Accord deforestation surge.

### Substantive finding

The snelect × forest loss scatter shows a non-linear (U-shaped) relationship. Very low-snelect municipalities (remote, no institutional presence) have low deforestation — no economic frontier pressure. Mid-range snelect municipalities (frontier zones, some connectivity, weak institutions) show the highest loss rates. High-snelect municipalities show lower loss, consistent with institutional capacity constraining extraction.

---

## An open theoretical question worth addressing

### Does MapBiomas capture the institutionally-relevant deforestation more cleanly than Hansen GFC?

Hansen GFC detects *any* canopy loss: fires, selective logging, natural disturbance, primary clearing — many of which are not responsive to democratic institutions. MapBiomas tracks *land cover class transitions*, meaning it specifically detects when land is cleared for agriculture, cattle ranching, or colonization — exactly the activities that weak democratic institutions fail to constrain (unclear tenure, impunity for illegal clearing, absence of environmental enforcement).

**The prediction**: MapBiomas loss should correlate more strongly with `snelect` than GFC loss, because its signal is more specific to the mechanism (land-use decisions mediated by institutions) rather than all sources of canopy disturbance.

**This is empirically testable** by comparing the Pearson r values from:
- Section 2.5 of the script: `snelect` × GFC loss rate
- Section 3.2 of the script: `snelect` × MapBiomas loss rate

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
| `scripts/colombia_deforestation.R` | Full analysis script. Sections 1–3 are the executed workflow; Appendices A–C are dead-end alternatives wrapped in `if (FALSE)`. |
| `scripts/output/gfc_muni_loss.rds` | Hansen GFC: municipality × year × loss_ha, 2001–2023 |
| `scripts/output/mb_muni_loss.rds` | MapBiomas: municipality × year × mb_loss_ha, 2000–2023 |
| `scripts/output/mapbiomas_col_muni_stats.csv` | Raw MapBiomas download (wide format, pre-computed statistics) |
| `data/panel/08_final_snvdem_data/MC/SN_Index_tentative.rds` | snelect panel |
| `data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp` | Municipal boundaries (DANE 2018) |
| `data/geospatial/GFC-2023/` | Hansen GFC lossyear tiles (4 tiles, ~1.5 GB) |

### Output figures

| File | What it shows |
|---|---|
| `scatter_snelect_forestloss.png` | Main result: snelect vs. GFC loss rate (all years) |
| `scatter_snelect_forestloss_mb.png` | Robustness: snelect vs. MapBiomas loss rate |
| `scatter_snelect_forestloss_period.png` | Pre/post 2016 Peace Accord, faceted |
| `timeseries_snelect_forestloss.png` | National trends: forest loss and mean snelect |
| `map_mean_forestloss.png` | Municipal choropleth, mean annual loss |
| `validation_scatter_gfc_vs_mb.png` | Cross-source scatter (r = 0.77) |
| `validation_annual_trends.png` | Annual national totals, both sources |

---

## Technical notes for reference

- `clean_mpio(x)`: `str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")` — standardizes DIVIPOLA codes
- MapBiomas CSV has no DIVIPOLA codes; matching uses normalized municipality + department names joined to shapefile `MPIO_CNMBR` / `DPTO_CCDGO`. Indigenous territories (resguardos) are unmatched and dropped (<5% of forest area).
- Parallel processing (`future.apply`) is blocked on this machine (ND campus IT blocks localhost socket connections). All extraction runs sequentially.
- MapBiomas FOREST_CLASS = 2 (Formacion Boscosa) confirmed by spatial frequency analysis in a prior session.
