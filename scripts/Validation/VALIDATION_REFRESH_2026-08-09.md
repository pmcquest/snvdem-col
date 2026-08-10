# Validation tests refresh — 2026-08-09

Re-ran the three Colombia-specific validation tests in the manuscript against the
current pre-benchmark panel (`data/panel/05_weighting/03_output/snvdem_col_weighted.rds`
— 26,928 rows, 1,122 municipalities x 2000-2023), since the pipeline picked up
real fixes since these numbers were last computed (df01-df07 audit, population/area
corrections, July 2026 weighting-formula corrections). All four validation scripts
were also pointing at pre-reorg legacy paths that no longer exist on disk; those are
fixed in place.

**Out of scope:** Test 4 ("global trends," Appendix I) isn't reproducible here —
its script reads V-Dem archive files from Matthew Coppedge's own machine. A note
asking him to re-run it is at `note_to_coppedge.md` (sent separately).

---

## 1. Manuscript body — Table 4 (Rojas-Betancur / CdDS)

Source: `scripts/Validation/UMedellin/validation_rojas.Rmd` (fixed & re-run;
rendered output at `validation_rojas.html` in the same folder — PDF rendering
hit an unrelated Windows/LaTeX path issue with this environment, HTML has
identical content). Figures re-generated: `validation_rojas_files/figure-html/
fig8-1.png` (Fig. 7 in manuscript, Z-score scatter by year) and `fig9-1.png`
(Fig. 8, cross-correlation heatmap).

**Replace Table 4 with:**

| Year | Pearson r | Spearman r | N municipalities |
|---|---|---|---|
| 2016 | 0.429 | 0.445 | 1,054 |
| 2017 | 0.399 | 0.407 | 1,054 |
| 2018 | 0.447 | 0.464 | 1,054 |
| 2019 | 0.325 | 0.343 | 1,054 |
| 2020 | 0.331 | 0.349 | 1,054 |
| 2021 | 0.318 | 0.341 | 1,054 |
| 2022 | 0.361 | 0.379 | 1,054 |
| 2023 | 0.289 | 0.302 | 1,054 |

(All still significant at p < 0.001.)

**Text edits** (paragraph beginning "The results confirm this expectation..."):
- "ranging from 0.22 to 0.44 (Pearson) and 0.25 to 0.46 (Spearman)" →
  **"ranging from 0.29 to 0.45 (Pearson) and 0.30 to 0.46 (Spearman)"**
- The dip-in-2019-2021 / recovery pattern still holds, no change needed there.
- Earlier in the doc, "across roughly 975 municipalities per year" (describing
  the Rojas-Betancur overlap) → **"across roughly 1,054 municipalities per
  year"**. N increased because the current panel has fuller municipality
  coverage than before the population-data fixes (1,122 vs. the previous
  ~1,102), plus I fixed 3 real bugs in the raw `Datos_municipal.csv` along the
  way (wrong file encoding, an `Año`/`Year` column-name mismatch, one
  comma-corrupted score, and 3 exact-duplicate municipality rows/year that are
  now deduplicated).
- Figure 8's cross-correlation values shift slightly (e.g. A.Inst r≈0.45,
  Solid r≈0.49-0.50, CivLib r≈-0.27 to -0.28) but the pattern described in
  prose (near-zero on Law/Part/Comp, moderate positive on A.Inst/Solid,
  negative on CivLib) is unchanged — no text edit needed beyond re-inserting
  the regenerated figure.

---

## 2. Manuscript body — Table 5 (Arboleda student survey)

Source: `scripts/Validation/ArboledaV/Arboleda_validation_VIF_1.Rmd` (fixed &
re-run; docx export at `SNDEM_Validation_Results_VIF.docx` in the same folder).

**Replace Table 5 with:**

| | Overall (Rolling) | Overall (Annual) | Components (Annual) | Components (Raw) |
|---|---|---|---|---|
| Index (3-Yr Rolling) | 1.013*** (0.116) | | | |
| Index (Annual) | | 0.983*** (0.116) | | |
| Electoral (Annual) | | | 1.223*** (0.272) | |
| Civil Liberties (Annual) | | | -0.301 (0.331) | |
| Electoral (Raw) | | | | 2.271*** (0.505) |
| Civil Liberties (Raw) | | | | -0.630 (0.692) |
| Num. Obs. | 349 | 349 | 349 | 349 |
| R2 | 0.598 | 0.596 | 0.603 | 0.603 |
| R2 Within | 0.195 | 0.190 | 0.206 | 0.206 |
| FE: Arb_id | X | X | X | X |
| VIF (max) | — | — | 6.38 | 6.38 |

N is now uniform at 349 across all four models (previously split 245/300) —
the earlier split was caused by missingness in the old pre-fix panel that no
longer exists.

**Substantive change, needs a rewritten paragraph:** the component-level story
flips. Previously civil liberties was the (insignificant) larger coefficient
and electoral was near-zero; now **electoral fairness is the significant,
larger driver and civil liberties is small, negative, and not significant.**
VIF also dropped from 13.38 to 6.38 (moderate, not severe, multicollinearity).
Both Arboleda tests (this one and the expanded appendix version below) show
the same reversal, so this looks like a real effect of the underlying
weighting-formula corrections, not a fluke in one script.

**Suggested replacement for the paragraph beginning "The results predict a
statistically significant positive correlation...":**

> The results predict a statistically significant positive correlation
> between our index and the students' responses, with weak model fit because
> of the tendency to better recall extreme cases, as described above. The
> annual model suggests that a unit increase in the index is associated with
> a 98% increase in the probability that the student mentioned that
> municipality as a case of high democracy (p < 0.01). The rolling average
> scores are also positive and slightly stronger: moving from the lowest to
> the highest score on the index raises the probability by roughly 101% that
> a respondent identifies a municipality with stronger democratic features.
> Unlike in earlier estimates, the individual component measures now diverge:
> the electoral fairness component is itself a statistically significant
> positive predictor of respondent assessments (p < 0.01), while the civil
> liberties component is not. A variance inflation factor (VIF) of 6.38 in
> the component models indicates moderate — no longer severe — collinearity
> between the two components, suggesting respondents' assessments of
> subnational democracy track electoral fairness somewhat more distinctly
> from civil liberties than earlier estimates suggested.

(This replaces the old sentence about "severe multicollinearity... cannot
easily distinguish between elections or civil liberties" — that claim no
longer holds as stated.)

**Added 2026-08-09 — pseudo-replication caveat, found while building the
outlier table (Table H.1/H.2) above.** Only 66 distinct (respondent,
municipality) pairs underlie the full N=349 matched sample — 92.3% of the 349
rows (322 of them) come from a respondent-place pair that recurs across
multiple years, i.e. one person's single qualitative judgment about one place
gets counted as a separate observation for every year the department/year
matching procedure attaches it to. One respondent (Arb_id 5) alone accounts
for ~46% of all 349 rows via just 6 distinct places (e.g. Cali alone
contributes 48 rows — 2 low-quality question categories × 24 years — from
that one respondent). Respondent fixed effects absorb each person's *average*
tendency but not this within-respondent-place repetition specifically.

**Robustness check promoted into the permanent script, 2026-08-09.** Added
directly to `scripts/Validation/ArboledaV/Arboleda_validation_VIF_1.Rmd` as a
new "Section 2b" (collapses to one observation per (respondent, municipality)
pair, averaging the index score across the years attributed to each pair and
dropping the 1 internally-contradictory pair — N drops from 349 to 65), with
its own VIF check added to Section 3. **The two collapsed models are now two
extra columns directly in Table 5 itself** ("Overall (Annual, Collapsed)" /
"Components (Annual, Collapsed)") rather than a separate footnote-only
result — re-rendered and confirmed working end-to-end
(`SNDEM_Validation_Results_VIF.docx`, also fixed its internal title from a
stale "Table 3" to "Table 5" to match the manuscript). Also corrected the
"Table 3"→"Table 5" title and refreshed Section 5's interpretation notes to
match current numbers.

Add the paragraph below to the manuscript right after the Table 5 discussion
(same text as before, but corrected: the component-level non-significance at
N=65 is **not just a power problem** — VIF for the collapsed components model
is 19.8, so multicollinearity re-emerges as severe at that smaller N too,
confirmed after the table was actually rendered):

> As a robustness check on the concentration of respondent-place opinions
> noted above, we collapsed the 349 matched municipality-year observations to
> one observation per unique (respondent, municipality) pair — averaging the
> index score across all years attributed to that respondent's assessment of
> a given place, and dropping the single pair with internally contradictory
> labels. This reduces the effective sample from 349 to 65 independent
> judgments (Table 5, columns 5-6). The annual index remains a statistically
> significant predictor of respondent assessment in this collapsed
> specification (coefficient = 1.056, SE = 0.268, p < 0.001), essentially
> unchanged from — and if anything slightly stronger than — the full-sample
> estimate (0.983, SE = 0.116). This gives us confidence that the headline
> finding is not an artifact of a small number of respondents' opinions being
> repeatedly counted across years. The component-level distinction reported
> in Table 5, however, does not clearly survive this stricter test: at N =
> 65, neither the electoral nor the civil liberties component is individually
> significant, and the variance inflation factor rises to 19.8 (severe,
> versus 6.38 in the full sample) — so collinearity between the two
> components re-emerges at this smaller N, on top of reduced power from only
> 65 observations across two predictors and nine respondent fixed effects.

Headline result (Overall Annual/Rolling Index) survives the collapse and even
strengthens slightly. The component-level split (Table 5's newly-flipped
electoral-significant/civil-liberties-not finding) does **not** clearly
survive at N=65 — both underpowered and re-collinear, not necessarily wrong,
but not confirmed by this stricter test either.

---

## 3. Appendix H — Arboleda survey (expanded)

Source: `scripts/Validation/ArboledaV/Arboleda_validation_expanded.R` (fixed,
including 2 real bugs found along the way — `validation_set2` and
`outlier_report` were referenced but never defined in the original script,
breaking the outlier table and two figures below; both reconstructed and
verified). Outputs, all in `scripts/Validation/ArboledaV/expanded/`:
- `Regression_Results.docx` — Table H.2 (the expanded LPM table)
- `SNDEM_Outlier_Table_Final.docx` / `SNDEM_Outliers.csv` — Table H.1
- `Municipality_Mentions_Plot.png` — Figure H.1 equivalent (mentions over time)
- `responses_smoothed.png` — Figure H's "smoothed" panel: raw jittered points +
  a true LOESS smoothing line with confidence ribbon. Shows the real local
  structure in the data (near-zero probability of a "high quality" label below
  SNDEM≈0.6, then a sharp jump — the underlying relationship is more of a
  threshold than a gradient; verified by decile: 0-6% "high quality" share in
  the bottom 3 deciles of SNDEM score, 54-88% in the top 6).
- `responses_smoothed_GLM.png` — same panel, alternate version using a
  binomial GLM (logistic) smooth instead of LOESS: a smooth monotonic S-curve,
  more standard for a 0/1 outcome, but it papers over the threshold jump the
  LOESS version shows. **PM hasn't decided between these two yet** — both are
  kept on disk (and both are generated by the script) pending that call.
- `responses_grouped.png` — Figure H's "grouped" panel: index score binned
  into deciles, with % "high quality" nominations per bin (this is what an
  earlier pass of this file had mislabeled as "smoothed" — fixed 2026-08-09).
- `responses_violin_boxplot.png` — bonus, not one of Figure H's two panels;
  violin/boxplot of SNDEM score by respondent label, kept for reference.

**Table H.1 (outliers), final form, per PM's request:** dropped the
`is_high_quality` and `type` columns (Municipality/Year/SNDEM score only, both
over- and under-performers mixed in one list, undifferentiated by type since
that column is gone); deduplicated 2 exact-duplicate municipality-years caused
by multiple respondents nominating the same place (down to 53 unique rows from
55); rounded to 4 decimals; Year formatted as plain text so it doesn't pick up
a thousands separator (e.g. "2016" not "2,016"); laid out as two side-by-side
27-row column blocks (with a vertical divider) since the full list no longer
fits comfortably as one long column. `SNDEM_Outliers.csv` mirrors the same
deduplicated/rounded data in flat (single-column) form if that's more useful
for pasting into Excel/Word directly.

**Replace Table H.2 with:**

| | Rolling Index | Annual Index | Components |
|---|---|---|---|
| Democracy Index (Rolling) | 0.421 (0.317) | | |
| Democracy Index (Annual) | | 0.390 (0.317) | |
| Electoral Component | | | 0.551 (0.493) |
| Civil Liberties Component | | | -0.179 (0.296) |
| Num. Obs. | 13,772 | 13,772 | 13,772 |
| R2 | 0.320 | 0.315 | 0.322 |
| R2 Within | 0.070 | 0.064 | 0.072 |

Same non-significance pattern as before (nothing crosses p<0.1) — **no
narrative change needed here**, only the table numbers and N (13,772 now
uniform across all three models, vs. previously 9,680/11,738). Note the same
electoral/civil-liberties sign pattern shows up here too (electoral positive,
civil liberties negative), consistent with Table 5's finding above, though
neither is statistically significant in this expanded version.

**Table H.1 (outliers)** — Bogotá/Cali/Cartagena/Medellín still dominate the
"index high, respondent low" list, consistent with the original; see
`SNDEM_Outliers.csv` for the full refreshed list to paste in.

---

## 4. Manuscript body — "Validation with V-Dem outlier data"

**Missed in the first pass, added 2026-08-09 after PM caught the gap:** the
main manuscript body has its own prose describing these same numbers
(separate from the Appendix G tables/paragraph below) — it needs the
matching updates.

**Paragraph beginning "We collated and transformed this textual data...":**
- "(See Appendix E for more details.)" → **"(See Appendix G for more
  details.)"** (lettering fix, same as elsewhere)
- "This resulted in 10,422 municipality-year observations between 2000-2020
  (8,865 under 'less' free, and 1,557 under 'more free')" →
  **"This resulted in 9,149 municipality-year observations between 2000-2020
  (8,002 under 'less' free, and 1,147 under 'more free')"**
- "This represents slightly over a third (38.6%) of all municipality-year
  observations in our electoral fairness index." → **"This represents
  roughly a third (34.0%) of all municipality-year observations in our
  electoral fairness index."** (9,149 / 26,928)

**Paragraph beginning "First, we ran a simple Welch's two-sample t-test...":**
- "(t = -29.42)" → **"(t = -35.54)"**
- "mean score of 0.65 in our index, while those assessed as 'less' have a
  mean score of 0.54–a difference of 0.12 points" → **"mean score of 0.661
  in our index, while those assessed as 'less' have a mean score of
  0.500–a difference of 0.16 points"**

**Paragraph beginning "Second, we calculated the Cohen's d measure...":**
- "(Cohen's d ~ 0.80)" → **"(Cohen's d ~ 0.96)"**

**Paragraph beginning "Third, we calculated the correlation...":**
- "a consistent positive correlation of 0.276" → **"a consistent positive
  correlation of 0.302"**
- "Appendix E provides additional comparisons" → **"Appendix G provides
  additional comparisons"**

No narrative change needed — every number moves the same direction as the
Appendix G version (effect size and correlation both strengthen slightly,
N drops ~12%), so the "positive concurrent validity" conclusion holds as
written.

---

## 5. Appendix G — V-Dem expert-comment outliers

**Section-opening paragraph (before "Using these data, we conducted three
validation steps..."), found + fixed 2026-08-09 after PM asked directly
whether the intro needed anything:** this paragraph has its own separate
instance of "10,422 municipality-year observations between 2000-2020
(missing data on proper names for 2013, 2014, and 2020)" → **"9,149
municipality-year observations between 2000-2020 (missing data on proper
names for 2013, 2014, and 2020)"** — same fix, same missing years, just a
fourth place this number appears that the first pass didn't catch.

**Figure G.1 ("distribution of unique municipalities... identified over
time"), also never regenerated in this refresh until now:** added to
`valid_com_2026.R`, output at `scripts/Validation/VDem-com/imgs/
muni-peryear-2026.png`. Uses `expert_only_panel.rds` (all Less/More rows,
contradictions included — a coverage diagnostic, not the two-group
comparison) counting distinct municipalities mentioned per year. Also fixed
the reference line, which the original script hardcoded at 1,102 (the old,
pre-population-fix municipality count) — now 1,122, matching the current
correct total used everywhere else in this project. Visually confirms the
text: gaps at 2013, 2014, and 2020, all other years well below the 1,122
line.

Two-step re-run, since the old intermediate file (`outliers3.rds`) no longer
exists:
1. `scripts/Validation/VDem-com/review/vdemcoder_review.R` — fixed (stale
   paths + a missing `MunYrs` load + a date-parsing bug) and re-run to rebuild
   the expert-comment panel from the raw text source (`data/Vdem/comments/v2elsnless_v2elsnmore_colombia.csv`) fresh. Output: `scripts/Validation/VDem-com/review/expert_only_panel.rds`. This reproduces the manuscript's
   stated raw inputs exactly — 19 unique experts, 370 comments — so I'm
   confident it's the right source data and methodology.
2. **Promoted into a permanent script, 2026-08-09:**
   `scripts/Validation/VDem-com/valid_com_2026.R` (successor to the original
   `valid_com.R`, whose input no longer exists). Runs the t-test/Cohen's
   d/correlation analysis on the rebuilt panel + current
   `snvdem_col_weighted.rds` (excluding contradictory municipality-years),
   AND — added in this same pass, since neither existed before — exports
   Table G.1/G.2 as Word-openable RTF (`Table_G1_TTest_Results.rtf`,
   `Table_G2_CohensD_Results.rtf`, same folder) and regenerates **Figure
   G.2** (`imgs/boxplot-moreless-2026.png`), which had never actually been
   produced in this refresh before PM caught the gap — the memo previously
   only gave the underlying numbers, not the figure itself.

**Replace Table G.1 with:**

| Group | N | Mean | SD | t | df | p | Cohen's d |
|---|---|---|---|---|---|---|---|
| Less | 8,002 | 0.500 | 0.172 | -35.54 | 1,696.4 | < .001 | -0.96 |
| More | 1,147 | 0.661 | 0.139 | | | | |

**Replace Table G.2 with:** N Obs. Less=8,002 / More=1,147 (unchanged from
first pass); **Avg. sn index score row: 0.500 / 0.661** (was showing the
stale 0.535/0.651 — inconsistent with the already-updated Difference cell);
**Standard Deviation row: 0.172 / 0.139** (was showing the stale
0.144/0.143); Difference +0.161 (already correct); Cohen's d = 0.96
("Large", already correct).

**Text edits, corrected/expanded 2026-08-09 — the first pass only caught one
of three places these numbers appear in the appendix:**
- Paragraph beginning "Using these data, we conducted three validation
  steps...": "d ~ 0.8" → **"d ~ 0.96"**; "positive correlation of around
  0.267" → **"positive correlation of around 0.302"**; total
  municipality-year observations "10,422... (8,865 under 'less'... 1,557
  under 'more')" → **"9,149 (8,002 under 'less', 1,147 under 'more')"**
  (same 3 missing years, 2013/2014/2020 — N is ~9% lower, traced to the
  department-name-matching step in rebuilding the panel, not the underlying
  comment data, which matches exactly: 19 experts, 370 comments; headline
  strengthens rather than weakens, so not worth chasing further but flagged
  for transparency); "means: 0.65 in our index... 0.54" → **"0.661...
  0.500"**; "difference of 0.12 points" → **"difference of 0.16 points"**.
- **Separate paragraph beginning "Second, we calculated the Cohen's d
  value...":** "indicate a large effect size (d ~ 0.8)" → **"(d ~ 0.96)"**;
  "an 80% chance of choosing" → **"a 96% chance of choosing"**.
- **Separate paragraph beginning "Third, we calculated the
  correlation...":** "positive correlation of around 0.267" → **"positive
  correlation of around 0.302"**.

---

## Files touched (all fixes applied in place, in the repo)

- `scripts/Validation/UMedellin/validation_rojas.Rmd`
- `scripts/Validation/ArboledaV/Arboleda_validation_VIF_1.Rmd`
- `scripts/Validation/ArboledaV/Arboleda_validation_expanded.R`
- `scripts/Validation/VDem-com/review/vdemcoder_review.R`

No commits made — these are working-tree edits only, per usual practice of
asking before any git push.
