# Examining the divergence between basic dimensions

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
