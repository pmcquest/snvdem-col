# ---- 1. Environment & Data Loading ----
library(dplyr)
library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)
library(sf)
library(ggrepel)
library(stringi)

# Load the expanded dataset
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/01_Expand/snvdem2.rds")

# ---- 2. Variable Definitions & Pre-EFA Cleaning ----
# We explicitly separate the 'axis' variables to prevent Ultra-Heywood cases
trimmed_vars <- c("urban", "econ_dev", "prox_cap", "nonviolent", 
                  "pop_density", "nonremote", "nonindig", "compete")

# Rename columns based on expert weights for each pillar
snvdem_emel <- snvdem %>%
  rename(urban=w_emel_0_1, econ_dev=w_emel_2_3, prox_cap=w_emel_4_5, axis_ns=w_emel_6_7, 
         axis_we=w_emel_8_9, nonviolent=w_emel_10_11, pop_density=w_emel_12, 
         nonremote=w_emel_13, nonindig=w_emel_14, compete=w_emel_15_16)

snvdem_cscw <- snvdem %>%
  rename(urban=w_cscw_0_1, econ_dev=w_cscw_2_3, prox_cap=w_cscw_4_5, axis_ns=w_cscw_6_7, 
         axis_we=w_cscw_8_9, nonviolent=w_cscw_10_11, pop_density=w_cscw_12, 
         nonremote=w_cscw_13, nonindig=w_cscw_14, compete=w_cscw_15_16)

# ---- 3. Execution: Refined EFA (8 Vars) ----
# Using oblimin rotation allows factors to correlate, which is theoretically sound for democracy
efa_emel_trim3 <- fa(snvdem_emel[, trimmed_vars], nfactors = 3, rotate = "oblimin")
efa_emel_trim2 <- fa(snvdem_emel[, trimmed_vars], nfactors = 2, rotate = "oblimin")
efa_cscw_trim3 <- fa(snvdem_cscw[, trimmed_vars], nfactors = 3, rotate = "oblimin")
efa_cscw_trim2 <- fa(snvdem_cscw[, trimmed_vars], nfactors = 2, rotate = "oblimin")

# 2 dims better for emel...
print(efa_emel_trim2$loadings, cutoff = 0.3, sort = TRUE)
# 3 dims better for cscw...
print(efa_cscw_trim3$loadings, cutoff = 0.3, sort = TRUE)

# ---- 4. Factor Score Generation & Index Synthesis ----
# Generate scores using 'tenBerge' to preserve factor correlations
emel_scores <- as.data.frame(predict(efa_emel_trim2, 
                                     data = snvdem_emel[, trimmed_vars], 
                                     return.scores = TRUE))
colnames(emel_scores) <- c("EMEL_Development", "EMEL_Inst_Centrality")

cscw_scores <- as.data.frame(predict(efa_cscw_trim3, 
                                     data = snvdem_cscw[, trimmed_vars], 
                                     return.scores = TRUE))
colnames(cscw_scores) <- c("CSCW_Urbanicity", "CSCW_Safety", "CSCW_Inst_Centrality")

# Merge and calculate Gaps
# Proportional weights based on variance explained
w_emel <- efa_emel_trim2$Vaccounted[2, 1:2] / sum(efa_emel_trim2$Vaccounted[2, 1:2])
w_cscw <- efa_cscw_trim3$Vaccounted[2, 1:3] / sum(efa_cscw_trim3$Vaccounted[2, 1:3])

snvdem_FA <- snvdem %>%
  bind_cols(emel_scores, cscw_scores) %>%
  mutate(
    MPIO_CDPMP = str_pad(as.numeric(MPIO_CDPMP), width = 5, side = "left", pad = "0"),
    EMEL_Composite = (EMEL_Development * w_emel[1]) + (EMEL_Inst_Centrality * w_emel[2]),
    CSCW_Composite = (CSCW_Urbanicity * w_cscw[1]) + (CSCW_Safety * w_cscw[2]) + (CSCW_Inst_Centrality * w_cscw[3]),
    Composite_Gap  = EMEL_Composite - CSCW_Composite,
    MR1_Gap        = EMEL_Development - CSCW_Safety
  )

# ---- 5. Visualization: The Quadrant Plot (Institutional Reach vs Safety) ----
snvdem_collapsed <- snvdem_FA %>%
  group_by(municipio) %>%
  summarise(across(c(EMEL_Development, CSCW_Safety, Composite_Gap), mean, na.rm = TRUE))

ggplot(snvdem_collapsed, aes(x = EMEL_Development, y = CSCW_Safety)) +
  geom_point(aes(color = Composite_Gap), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data = subset(snvdem_collapsed, Composite_Gap > 2.2), 
                  aes(label = municipio), size = 3) +
  scale_color_gradient2(low = "blue", mid = "grey90", high = "red") +
  labs(title = "The Democracy Gap", x = "Electoral Reach", y = "Civil Safety") +
  theme_minimal()
