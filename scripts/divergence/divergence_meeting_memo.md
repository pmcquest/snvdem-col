# Divergence Analysis — Meeting Prep Memo
**For:** Co-author meeting, 2026-06-25  
**Prepared by:** Patrick McQuestion  
**The question for tomorrow:** Does the snelect/sncivlib divergence analysis belong in this paper, and what theoretical work does it do?

---

## 1. Building on the Co-Author's Observation

The draft already includes this:

> "As shown in Figure B.2 in the appendix, the expert weights for correlates of democratic elections and civil liberties are very similar, at least for Colombia. This is not a problem; it is what one should expect of relationships among democracy components, as is the case with many V-Dem variables. The fact that they are collinear by no means suggests dropping either elections or civil liberties from our study, as our goal is to measure democracy (the larger concept), not to apportion explanatory weights to these two components."

This observation settles one question: **is the collinearity a methodological problem for the composite?** No. The sub-indices share similar expert weights because they are measuring components of an integrated concept, and the composite captures that integrated syndrome correctly. This is not a concession to critics — it is what valid measurement looks like.

The divergence analysis addresses a different question, one the co-author's observation *opens* rather than closes: **given that the sub-indices are so structurally similar, what does it tell us when they diverge anyway?**

If shared expert weights and shared predictor structure largely account for the correlation between `snelect` and `sncivlib`, then variation in their relationship beyond that shared structure is especially meaningful when it appears — because it is not easily explained away as a measurement artifact. The Moran's I of 0.51 (p < 2.2e-16) tells us that whatever divergence does occur is spatially concentrated at the corridor scale, not randomly distributed across municipalities. That spatial structure is not noise from the measurement model; it maps onto Colombian conflict geography.

So the divergence analysis is not a critique of the composite — it accepts the co-author's point entirely. Its contribution is different: it asks whether examining the bounded variation in sub-index divergence, *given the composite is valid*, reveals something theoretically meaningful about Colombian subnational politics that the composite index would mask.

**The civil conflict literature says it should.** The subnational authoritarianism literature (Gibson 2013, *Boundary Control*) and the civil conflict literature (Romero 2003; Arjona 2016, *Rebelocracy*) converge on a key insight: armed actors do not treat all dimensions of democratic governance equivalently. The strategic logic differs by group type and territorial economy:

- *Paramilitary groups and successor organizations* had strong incentives to organize, rather than suppress, local electoral competition. Elections were instruments of territorial consolidation — access to municipal budgets, development contracts, administrative appointments, and national political cover. Romero (2003) documents this "paramilitary democracy" configuration directly: armed groups enable electoral outcomes while systematically repressing civil society, community organizations, and individual rights. The predicted divergence is **elections leading civil liberties**.

- *Guerrilla quasi-states* (FARC *frentes* operating in the Pacific, Putumayo) operated differently: they suppressed electoral competition and civil liberties simultaneously, functioning as parallel state substitutes with no interest in instrumentalizing elections. The predicted pattern is **convergence at the low end**, not elections-dominant divergence.

This is a falsifiable theoretical prediction. The divergence analysis is worth including in the paper because it *tests* that prediction, not merely because it describes a pattern in the data. And the prediction is confirmed: elections-dominant divergence concentrates in the Magdalena Medio and Santander paramilitary core; low-end convergence characterizes the Pacific Coast; the post-FARC period produces the highest elections-dominant reading in the series (2018), while the post-AUC period produced convergence. These are directional, theoretically-motivated results that a composite index would collapse to zero.

---

## 2. The Honest Against-Case

Three objections carry real weight:

**Shared predictors cap detectable divergence.** The same geocoded predictors construct both sub-indices. Divergence can only arise to the extent that the two models weight shared predictors *differently*. What we observe is a lower bound on true dimensional divergence. The divergence analysis cannot distinguish between (a) genuinely different electoral and civil liberties quality, (b) differential predictor weighting, or (c) expert scoring artifacts.

**Expert co-movement is a measurement ceiling.** V-Dem experts responding to the same local conditions in both electoral and civil liberties modules will channel the same underlying facts through two instruments. An expert who knows a municipality has heavy paramilitary presence will adjust both scores in similar directions. This creates a floor on how much dimensional independence can be detected, even if true divergence exists.

**Effect sizes are modest.** The mean divergence of +0.045 (normalized) is real but small. The 2018 peak (0.083) is the clearest reading, but it is still a modest absolute difference. A skeptical co-author or reviewer may reasonably argue the signal is too attenuated by shared-predictor noise to bear interpretive weight.

**The honest framing:** The divergence analysis is suggestive, not causal. It can show that the *pattern* of co-movement and divergence aligns with theoretical expectations — which is evidence for the face validity of the sub-indices. It cannot isolate true dimensional divergence from measurement artifacts. The paper should say this explicitly.

---

## 3. What the Numbers Show

| Statistic | Value |
|---|---|
| Correlation (snelect_norm, sncivlib_norm) | r = 0.958 |
| Mean normalized divergence | +0.045 (elections lead) |
| Mean raw divergence | −0.014 (civlib leads — direction flips with normalization) |
| Moran's I | 0.510, p < 2.2e-16 |

The Moran's I is the most important single number here: half the variance in divergence is explained by spatial autocorrelation. That spatial structure operates at exactly the scale of known conflict corridors — it is not random noise, and it is not explained by individual municipal characteristics alone.

**Two illustrative cases:**

*Civlib-dominant: Nariño highlands.* Municipalities near Ipiales and the Pasto plateau (Contadero, Guachucal, Pupiales, etc.) persistently score higher on civil liberties than electoral freedom. Strong indigenous *cabildo* governance predating the Colombian state provides civil authority even where narco-influence compromises electoral competition. This is the Arjona (2016) rebelocracy pattern: armed groups selectively tolerate existing institutions when those institutions serve non-threatening functions. The highland cabildos were not a threat to FARC logistics on the coast.

*Elections-dominant: Bolívar and Santander.* Municipalities in Sur de Bolívar and the Magdalena Medio (Cantagallo, El Guamo, Coromoro, Suratá) show elections-dominant divergence across the **full 2000–2023 period**, with no break at 2005. The AUC demobilized but the structural configuration — extractive economy, political machine, weak civil institutions — persisted under successor organizations. This is precisely what the "paramilitary democracy" literature predicts: the divergence is a long-run equilibrium, not a transitional effect.

**The post-FARC signal** is the clearest temporal finding. Mean divergence peaks in 2018 (0.083; 24% elections-dominant), the highest in the series. The AUC demobilization produced the opposite: convergence in both directions after 2005. The asymmetry — FARC peace produces elections-dominant divergence; AUC peace does not — is theoretically interpretable and would be invisible in a composite score.

---

## 4. Recommendation for the Paper

**Include it, briefly, as a construct validity diagnostic.** The theoretical argument in Section 1 gives it a purpose beyond description: the divergence analysis tests predictions from the civil conflict literature about how different armed actors interact with electoral versus civil liberties dimensions of democratic quality. That is a methodological contribution appropriate for *Perspectives on Politics*.

**Positioning:** A "probing the index" subsection of ~350 words. Not a separate finding; a diagnostic that supports confidence in the sub-indices independently.

**Draft language:**

> The high correlation between sub-indices (r = 0.958) reflects genuine structural co-movement: the same underlying conditions — state presence, conflict intensity, economic development — drive both electoral and civil liberties quality simultaneously. This is the expected behavior of a valid composite. The divergence analysis asks whether that co-movement breaks down in theoretically predictable ways. It does. Elections-dominant divergence concentrates in the paramilitary-legacy Magdalena Medio and Sur de Bolívar, consistent with the "paramilitary democracy" configuration Romero (2003) documents: armed groups instrumentalize elections while suppressing civil society. Low-end convergence characterizes the Pacific Coast, where guerrilla quasi-states suppressed both dimensions simultaneously. The post-2016 period shows the largest elections-dominant divergence in the series, consistent with electoral competition recovering faster than civil liberties in former FARC territories. Spatial autocorrelation (Moran's I = 0.51) confirms the clustering is non-random and operates at the corridor scale. These patterns are theoretically predicted; a composite index collapses them. We treat this as diagnostic evidence for the face validity of the sub-indices, while noting that shared predictors and expert co-movement create a floor on detectable divergence, and causal inference from these patterns requires further analysis.

---

## 5. What a Future Piece Would Do

The corridor-level analysis is a genuine contribution to the subnational democracy and Colombian conflict literatures — but that contribution needs more than a meeting memo.

**What exists:** ANOVA confirms a significant divergence gradient across independently-defined conflict corridors (F = 106.3, p < 2e-16). Three independent sources validate the corridor definitions without any access to democratic quality data: Salas-Salazar (2016) via spatial conflict density; CEV *Resistir no es aguantar*, Tomo 9 (2022) via 17 ethnic macro-territory corridors (Mapas 4–21, pp. 60–117); DANE sub-regional classifications.

**What the future piece would add:** LISA (local Moran's I) to formally identify spatial clusters rather than imposing corridor definitions. Corridor × time interactions across armed group transitions. LOO analysis identifying *which* indicators drive the Nariño vs. Bolívar signatures. Comparative corridor framing: what does the Magdalena Medio / Pacific Coast divergence gradient tell us about armed group type, territorial economy, and democratic quality?

The PoP paper earns one sentence: "The spatial structure of divergence (Moran's I = 0.51) suggests that systematic corridor-level analysis is a productive direction for future work."

---

*Supporting outputs in `scripts/divergence/output/`: `divergence_map.png`, `divergence_temporal.png`, `divergence_shock_periods.png`, `corridor_reference_map.png`, `divergence_map_corridors.png`, `divergence_corridors.png`*
