# Handoff: snvdem Divergence Analysis (Electoral vs. Civil Liberties)
**Project:** Measuring Democracy in Subnational Units Worldwide: A New Method  
**Authors:** McQuestion, Coppedge, Sisk, McMann  
**Session goal:** Develop a brief, self-contained divergence analysis for inclusion in the Colombia pilot section of the paper.

---

## Context

The paper produces a subnational democracy dataset (`snvdem`) for Colombia's ~1,125 municipalities across 2000–2023 (27,000 municipality-year observations). The composite index (`sndem`) is the equally weighted mean of two sub-indices:

- `snelect` — electoral freeness and fairness
- `sncivlib` — civil liberties protections

The paper's existing validation work found high multicollinearity (VIF ≈ 13.38) between these two components in regression models, suggesting they co-move substantially. The divergence analysis asks: **where and when do the two dimensions decouple, and what does that tell us about Colombian democracy?**

This is intended as a **brief section** (~400–600 words of prose + 1 figure + possibly 1 small table) within the existing Colombia pilot section. If the patterns are interesting, a follow-on piece is possible.

---

## Data

**Primary dataset:**
```r
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem24/data/panel/09_analysis_scripts/Exploratory/01_Expand/snvdem2.rds")
```

**Key variables:**
| Variable | Description |
|---|---|
| `MPIO_CDPMP` | Municipality code (5-digit, character) |
| `year` | Year (2000–2023) |
| `snelect` | Electoral fairness sub-index (raw) |
| `sncivlib` | Civil liberties sub-index (raw) |
| `sndem` | Composite index = mean(snelect, sncivlib) (raw) |
| `snelect_norm` | snelect normalized 0–1 (compute if not present) |
| `sncivlib_norm` | sncivlib normalized 0–1 (compute if not present) |
| `sndem_norm` | sndem normalized 0–1 (compute if not present) |

**Geospatial shapefile:**
```
G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp
```
Join key: `MPIO_CDPMP` (pad to 5 digits with leading zeros if needed).

**Normalization formula** (apply globally across all municipality-years before divergence calculations):
```r
snelect_norm  <- (snelect  - min(snelect,  na.rm=TRUE)) / (max(snelect,  na.rm=TRUE) - min(snelect,  na.rm=TRUE))
sncivlib_norm <- (sncivlib - min(sncivlib, na.rm=TRUE)) / (max(sncivlib, na.rm=TRUE) - min(sncivlib, na.rm=TRUE))
sndem_norm    <- (sndem    - min(sndem,    na.rm=TRUE)) / (max(sndem,    na.rm=TRUE) - min(sndem,    na.rm=TRUE))
```

---

## Analytical Tasks

### Step 0: Diagnostic (do this first)
Before any analysis, run these diagnostics and report the results — they determine how strongly to frame the divergence findings:

```r
# Correlation between the two normalized sub-indices
cor(snelect_norm, sncivlib_norm, use = "complete.obs")

# Divergence score
div_score <- snelect_norm - sncivlib_norm
# Positive = elections outperform civil liberties
# Negative = civil liberties outperform elections

mean(div_score, na.rm = TRUE)
sd(div_score, na.rm = TRUE)
range(div_score, na.rm = TRUE)
hist(div_score, breaks = 50, main = "Distribution of divergence scores")
```

**Interpretation guide:**
- If correlation > 0.90: divergences are small in absolute terms; frame the section around identifying notable exceptions
- If correlation < 0.80: divergences are substantively large; stronger framing is justified
- SD of div_score: gives a sense of how much variation there is to explain

---

### Step 1: Construct divergence score and typology

```r
master_clean <- snvdem %>%
  filter(!is.na(snelect) & !is.na(sncivlib)) %>%
  mutate(
    snelect_norm  = (snelect  - min(snelect))  / (max(snelect)  - min(snelect)),
    sncivlib_norm = (sncivlib - min(sncivlib)) / (max(sncivlib) - min(sncivlib)),
    sndem_norm    = (sndem    - min(sndem))    / (max(sndem)    - min(sndem)),
    div_score     = snelect_norm - sncivlib_norm
  )
```

**Compute per-municipality average divergence** (persistence measure):
```r
muni_div <- master_clean %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    mean_div     = mean(div_score, na.rm = TRUE),
    mean_elec    = mean(snelect_norm, na.rm = TRUE),
    mean_civlib  = mean(sncivlib_norm, na.rm = TRUE),
    mean_dem     = mean(sndem_norm, na.rm = TRUE),
    n_years      = n()
  )
```

**Typology** (apply to `muni_div`): Define thresholds based on the distribution of `mean_div` (e.g., top/bottom quartile or top/bottom decile — choose based on Step 0 diagnostics):
- **Elections-dominant**: `mean_div` > [upper threshold] (elections persistently outperform civil liberties)
- **Civil-liberties-dominant**: `mean_div` < [lower threshold] (civil liberties persistently outperform elections)  
- **Convergent**: everything in between

---

### Step 2: Map average divergence

Produce a choropleth map of `mean_div` by municipality. This is the primary deliverable figure.

**Suggested palette:** Diverging color scale centered at zero (e.g., `scale_fill_gradient2()` or `scale_fill_distiller(palette = "RdBu")`). Blue = elections lead; red = civil liberties lead; white = convergent.

```r
library(sf); library(tidyverse); library(viridis)

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp") %>%
  mutate(MPIO_CDPMP = str_pad(as.character(as.numeric(MPIO_CDPMP)), width = 5, side = "left", pad = "0"))

map_div <- muni_geo %>%
  left_join(muni_div, by = "MPIO_CDPMP")

ggplot(map_div) +
  geom_sf(aes(fill = mean_div), color = NA) +
  scale_fill_gradient2(
    low = "red3", mid = "white", high = "steelblue",
    midpoint = 0,
    name = "Divergence\n(Elec − CivLib)",
    na.value = "grey80"
  ) +
  labs(
    title = "Electoral vs. Civil Liberties Divergence",
    subtitle = "Average divergence score per municipality (2000–2023)",
    caption = "Positive (blue) = elections outperform civil liberties. Negative (red) = civil liberties outperform elections."
  ) +
  theme_void() +
  theme(legend.position = "right")
```

Export at 300 dpi to `08_final_snvdem_data/MC/imgs/`.

---

### Step 3: Identify illustrative municipalities

For the prose section, identify 3–5 municipalities that exemplify the extremes. These should be:
- **Named** (not just MPIO codes) — join a department/municipality name lookup if needed
- **Substantively interpretable** — ideally places already mentioned in the paper (e.g., Cocorná, Puerto López, Timbiquí from the "Zooming In" section) or well-known conflict-affected locations

```r
# Top elections-dominant municipalities
muni_div %>% arrange(desc(mean_div)) %>% head(10)

# Top civil-liberties-dominant municipalities
muni_div %>% arrange(mean_div) %>% head(10)
```

Check whether any of the Zooming In municipalities (Cocorná, Puerto López, Timbiquí) show notable divergence patterns — if so, note it for continuity with that section.

---

### Step 4: Optional — Temporal stability check

Ask whether divergences are stable over time or episodic:

```r
# For each municipality, what fraction of years fall in divergence extremes?
muni_stability <- master_clean %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    pct_elec_dominant  = mean(div_score > 0.10, na.rm = TRUE),  # adjust threshold
    pct_civlib_dominant = mean(div_score < -0.10, na.rm = TRUE),
    pct_convergent     = mean(abs(div_score) <= 0.10, na.rm = TRUE)
  )
```

A municipality with `pct_elec_dominant` > 0.80 has *structurally* divergent dimensions; one with 0.30 shows *episodic* divergence. This distinction is worth one sentence in the prose if the patterns look interesting.

---

### Step 5: Optional — Spatial clustering test (if time allows)

Test whether divergence clusters geographically using Moran's I. Only pursue this if Step 2 map suggests visually obvious clustering.

```r
library(spdep)
coords <- st_centroid(map_div) %>% st_coordinates()
nb <- knn2nb(knearneigh(coords, k = 5))
lw <- nb2listw(nb, style = "W")
moran.test(map_div$mean_div, lw, na.action = na.exclude)
```

If Moran's I is significant, note the statistic in the text as evidence of spatial clustering. Do not run LISA for this brief section — save for the follow-on piece.

---

## Output Files

Save all outputs to:
```
G:/Shared drives/snvdem/snvdem24/data/panel/08_final_snvdem_data/MC/imgs/
```

Suggested filenames:
- `divergence_map.png` — primary figure (Step 2)
- `divergence_diagnostics.txt` — printed output from Step 0 correlation/distribution checks
- `divergence_top_munis.csv` — table of most extreme municipalities (Step 3)

---

## Analytical Cautions

1. **High co-movement is expected.** The two sub-indices share geopredictors (e.g., violence/conflict affects both dimensions). Divergence is likely the exception, not the rule. Frame accordingly.

2. **Normalization is global.** Normalize across all municipality-years together, not within year or within municipality. This is consistent with how `sndem_norm` is computed elsewhere in the paper (see `ZoomIn.Rmd`).

3. **Divergence score is sensitive to the normalization range.** If the raw `snelect` and `sncivlib` scales have very different ranges, the normalized scores may artificially inflate apparent divergences. Report raw correlation alongside divergence score.

4. **Do not use divergence score as an outcome or predictor in a regression in this section.** The geopredictors used to construct `snelect` and `sncivlib` are the same, so any regression of divergence on those variables would be circular.

5. **The "Zooming In" section** already uses `sndem_norm` for the map and raw `sndem` for the table — ensure consistency. If you add divergence scores to any table, use normalized values (`snelect_norm`, `sncivlib_norm`) to make them directly comparable.

---

## Draft Prose Skeleton

For reference, here is a suggested structure for the ~500-word section:

> **[Opening sentence]** While the composite democracy index (`sndem`) captures overall municipal democratic performance, the two dimensions it combines — electoral fairness and civil liberties protection — do not always move together.

> **[Defining divergence]** We compute a divergence score as the difference between the normalized electoral and civil liberties sub-indices (`snelect_norm − sncivlib_norm`). Positive values indicate municipalities where elections are relatively freer and fairer than civil liberties are protected; negative values indicate the reverse.

> **[Diagnostic result]** Across the full dataset, the two components are highly correlated (*r* = [X]), consistent with our validation findings. Nevertheless, the distribution of divergence scores has a standard deviation of [X], and [X]% of municipalities show persistent divergence exceeding [threshold] across most of the study period.

> **[Geographic pattern + Figure reference]** Figure X maps average divergence by municipality across the full 2000–2023 period. [Describe the pattern — e.g., elections-dominant municipalities cluster in X; civil-liberties-dominant municipalities concentrate in Y.]

> **[Illustrative cases]** This pattern is consistent with Colombia's conflict geography: municipalities in [region] show persistently higher electoral scores relative to civil liberties, likely reflecting [mechanism]. Conversely, [region] municipalities where [mechanism] show the opposite pattern.

> **[Theoretical interpretation]** This decoupling resonates with broader arguments in the comparative politics literature distinguishing electoral from liberal democracy (citations). It also highlights a limitation of composite indices: aggregate scores can mask meaningful dimensional variation at the subnational level.

---

## R Environment

- **Primary packages:** `tidyverse`, `sf`, `spdep`, `ggrepel`, `patchwork`, `viridis`
- **Working directory:** `G:/Shared drives/snvdem/snvdem24/data/panel/`
- **Analysis script:** Add new code to `ZoomIn.Rmd` or create a new `divergence_analysis.Rmd` in the same directory

---

*Prepared for Claude Code handoff — April 2026*
