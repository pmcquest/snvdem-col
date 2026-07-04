# Exploratory analysis of snvdem data

# Which criteria are most influential in the snvdem index?

library(tidyverse)
library(psych)
library(lavaan)
library(plm)
library(ggplot2)



## 1. Load and Prepare Data ----
snvdem <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/08_final_snvdem_data/master_snvdem_col.rds")
panel_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Outcomes/panel_imputed.rds")

# Add names and deptos
names_lookup <- panel_imputed %>%
  select(MPIO_CDPMP, municipio, depto) %>%
  distinct(MPIO_CDPMP, .keep_all = TRUE)

snvdem <- snvdem %>%
  left_join(names_lookup, by = "MPIO_CDPMP")

## 2. EMEL Analysis (Electoral) ----
emel_vars <- c("rural_urban", "econ_dev", "dist_cap", "north", "south", 
               "west", "east", "conflict", "pop_density", "remoteness", 
               "indig_pop", "ruling_party")

snvdem_emel <- snvdem %>%
  rename(
    rural_urban = emel0_1, econ_dev = emel2_3, dist_cap = emel4_5,
    north = emel6, south = emel7, west = emel8, east = emel9,
    conflict = emel10_11, pop_density = emel12, remoteness = emel13,
    indig_pop = emel14, ruling_party = emel15_16
  )

# Universal EFA for EMEL
efa_emel <- fa(snvdem_emel[, emel_vars], nfactors = 3, rotate = "oblimin")
print(efa_emel$loadings, cutoff = 0.3, sort = TRUE)

# CFA Model for EMEL
final_model <- '
  Geography =~ north + south + west + east + dist_cap + remoteness
  Development =~ econ_dev + pop_density + rural_urban
  Conflict_Social =~ conflict + indig_pop + ruling_party
  SN_Democracy =~ Geography + Development + Conflict_Social
'
fit_sn <- cfa(final_model, data = snvdem_emel, std.lv = TRUE)

## 3. CSCW Analysis (Civil Liberties) ----
cscw_vars <- emel_vars # using same variable names after renaming

snvdem_cscw <- snvdem %>%
  rename(
    rural_urban = cscw0_1, econ_dev = cscw2_3, dist_cap = cscw4_5,
    north = cscw6, south = cscw7, west = cscw8, east = cscw9,
    conflict = cscw10_11, pop_density = cscw12, remoteness = cscw13,
    indig_pop = cscw14, ruling_party = cscw15_16
  )

# Universal EFA for CSCW
efa_cscw <- fa(snvdem_cscw[, cscw_vars], nfactors = 3, rotate = "oblimin")
print(efa_cscw$loadings, cutoff = 0.3, sort = TRUE)

# Congruence check
# Max Congruence (0.87)	High Similarity. Your "Periphery" factor (Ind. Pop/Distance) is very consistent across both dimensions.
# Lowest Congruence (0.19)	Total Divergence. The variables that define "Electoral Fairness" in the capital do not translate to how "Civil Liberties" function in those same areas.
# Overall Verdict: Distinct Engines. Since no score hit >0.90, you should treat EMEL and CSCW as related but distinct pillars.
# Top 3 most influential variables based on their strongest factor loadings:
## Electoral Fairness (EMEL): Remoteness (0.765), Econ. Dev't (0.695), Pop. Density (0.642)
## Civil Liberties (CSCW) Econ. Dev't (0.837), Remoteness (0.723), West (Inverse) (-0.653)
congruence <- factor.congruence(efa_emel, efa_cscw)
print(congruence)

## 4. Extract Scores and Define Gap ----
# Where do the dimensions diverge? subtracting scores to identify municipalities where one pillar performs better than the other

# Extract scores using regression method
snvdem$emel_factor_score <- factor.scores(snvdem_emel[, emel_vars], efa_emel)$scores[,1]
snvdem$cscw_factor_score <- factor.scores(snvdem_cscw[, cscw_vars], efa_cscw)$scores[,1]

# Create is_urban flag and Democracy Gap
# 0. Global Definitions
top_cities_codes <- c("11001", "50001", "17001", "73001", "63001", 
                      "66001", "25754", "05001", "76834", "15001")
snvdem_full <- snvdem %>%
  mutate(
    is_urban = ifelse(MPIO_CDPMP %in% top_cities_codes, "Urban Center", "Non-Urban/Rural"),
    democracy_gap = emel_factor_score - cscw_factor_score
  )

# Top 10 for 2023
# Most are regional capitals: Bogota, Medellin, Villavicencio (Meta), Manizales (Caldas), Ibague (Tolima), Pereira (Risaralda), Armenia (Quindio), Tunja (Boyaca). Others are major urban centers: Tulua (Valle del Cauca), and Soacha (Cundinamarca)

discrepancy_top_10 <- snvdem_full %>%
  filter(year == 2023) %>%
  arrange(desc(abs(democracy_gap))) %>%
  select(MPIO_CDPMP, municipio, emel_factor_score, cscw_factor_score, democracy_gap) %>%
  head(10)
print(discrepancy_top_10)

## 5. Visualizations ----

# Plot 1: Urban Centers Divergence

df_plot_gap <- snvdem_full %>% filter(MPIO_CDPMP %in% top_cities_codes)

ggplot(df_plot_gap, aes(x = year, y = democracy_gap, group = municipio)) +
  annotate("rect", xmin = 2003, xmax = 2006, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(color = municipio), size = 1) +
  theme_minimal() +
  labs(title = "The Democracy Gap in Urban Centers", y = "Gap (Electoral > Liberties)")

# Plot 2: Urban vs Rural Landscape
df_summary <- snvdem_full %>%
  group_by(year, is_urban) %>%
  summarise(mean_gap = mean(democracy_gap, na.rm = TRUE),
            se_gap = sd(democracy_gap, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(df_summary, aes(x = year, y = mean_gap, color = is_urban, fill = is_urban)) +
  annotate("rect", xmin = 2003, xmax = 2006, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  geom_ribbon(aes(ymin = mean_gap - 2*se_gap, ymax = mean_gap + 2*se_gap), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Democracy Gap: Urban vs. Rural", y = "Average Gap")

## 6. Statistical Models ----

snvdem_reg <- snvdem_full %>% mutate(post_farc = ifelse(year >= 2017, 1, 0))

# Fixed Effects Model
fe_model <- plm(democracy_gap ~ post_farc, data = snvdem_reg, 
                index = c("MPIO_CDPMP", "year"), model = "within")
# interpretation: post_farc = 0.17: suggests that gap increased across municipalities after 2017 demobilization. Electoral fairness outpaced civil liberty protections. But in Urban cities, the gap may have increased fourfold (top 10 had +0.82 divergence)
# high statistical significance: democracy gap is not a random result
# R^2 = 0.037: 2016 Peace era may produce heterogeneous results; geography may be more important
# Gemini interpretation:
## Urban Centers	+0.82	Maximum Divergence. The state "perfects" elections while urban civil liberties hit a ceiling or decline due to social unrest.
## National Average	+0.17	General Trend. A slight but very consistent shift toward proceduralism over substantive rights across the whole territory.
## Rural Periphery	Low/Zero	Synchronization. In the deep rural areas, if security fails, both elections and liberties fail, so the "Gap" doesn't widen—the whole system just sinks.
summary(fe_model)

# Placebo Test (2012)
snvdem_placebo <- snvdem_reg %>% 
  filter(year < 2016) %>% 
  mutate(placebo_2012 = ifelse(year >= 2012, 1, 0))

placebo_model <- plm(democracy_gap ~ placebo_2012, data = snvdem_placebo, 
                     index = c("MPIO_CDPMP", "year"), model = "within")
# Coefficient = -0.488 suggests opposite trend: gap narrowing before 2017 process
summary(placebo_model)

# Interaction Model: Does the Peace Deal hit cities harder?
# 1. Ensure the variables exist in the dataframe being used for the model
snvdem_full <- snvdem_full %>%
  mutate(
    # Create the 0/1 dummy for Post-FARC
    post_farc = ifelse(year >= 2017, 1, 0),
    # Convert is_urban to a numeric dummy (1 for Urban, 0 for Rural) 
    # for the interaction calculation
    is_urban_dummy = ifelse(is_urban == "Urban Center", 1, 0)
  )

# 2. Run the Interaction Model
# Note: is_urban_dummy as a standalone will be dropped due to Fixed Effects (Within),
# but the INTERACTION term (post_farc:is_urban_dummy) will remain.
interaction_model <- plm(democracy_gap ~ post_farc * is_urban_dummy, 
                         data = snvdem_full, 
                         index = c("MPIO_CDPMP", "year"), 
                         model = "within")

# 3. View Results
summary(interaction_model)
# "Using a Fixed Effects interaction model ($N = 27,000$), we find that the 2017 Peace Deal acted as a decoupling shock to Colombian democracy. While a modest increase in the democracy gap was observed nationally ($\beta = 0.165, p < 0.001$), this effect was significantly amplified in major urban centers ($\beta_{interaction} = 0.655, p < 0.001$). This suggests that the post-conflict transition has prioritized electoral formalization in cities while leaving substantive civil protections stagnant, a phenomenon we term 'Urban Institutional Decoupling'."

# Option 1: Using broom (Recommended)
library(broom)

# Tidy the model and calculate 95% Confidence Intervals
model_results <- tidy(interaction_model, conf.int = TRUE) %>%
  filter(!is.na(estimate)) %>%
  mutate(term = recode(term, 
                       "post_farc" = "Baseline Peace Effect (Rural)",
                       "post_farc:is_urban_dummy" = "Additional Urban Effect (Interaction)"))

# Create the plot
ggplot(model_results, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  # Error bars representing the 95% CI
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1, color = "#2c3e50") +
  # Point estimates
  geom_point(size = 4, color = "#e74c3c") +
  theme_minimal() +
  labs(
    title = "Coefficient Plot: The Urban Democracy Gap (Post-2017)",
    subtitle = "Point estimates with 95% Confidence Intervals",
    x = "Estimate (Effect on Democracy Gap)",
    y = ""
  ) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )
