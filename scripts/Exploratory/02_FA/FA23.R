# Factor analysis of snvdem data
# Which criteria are most influential in the snvdem index?

library(knitr)
library(kableExtra)

source("G:/Shared drives/snvdem/snvdem-col/scripts/Exploratory/02_FA/FA23_setup.R")
# Loads snvdem/snvdem_emel/snvdem_cscw, fits efa_emel_base/efa_cscw_base/
# efa_emel_trim/efa_cscw_trim, computes base_diag/trim_diag, and builds
# df1/df2 (the final_df table structure consumed by render_fa_table() below).
# See FA23_word_export.R for the Word/.docx renderer of the same df1/df2.

# Print raw loadings for each model (console only; df1/df2 already have the
# same numbers plus SS Loadings/Var Explained/Model Fit rows for the tables)
cat("\n--- EMEL (Electoral) Baseline Loadings ---\n")
print(efa_emel_base$loadings, cutoff = 0.3, sort = TRUE)
cat("\n--- CSCW (Civil Liberties) Baseline Loadings ---\n")
print(efa_cscw_base$loadings, cutoff = 0.3, sort = TRUE)

cat("\n--- EMEL (Electoral) Trimmed Loadings ---\n")
print(efa_emel_trim$loadings, cutoff = 0.3, sort = TRUE)
cat("\n--- CSCW (Civil Liberties) Trimmed Loadings ---\n")
print(efa_cscw_trim$loadings, cutoff = 0.3, sort = TRUE)

# Helper: render a final_df (from FA23_setup.R) as a plain-text print plus a
# styled kableExtra HTML table
render_fa_table <- function(final_df, title, num_vars) {
  cat("\n---", title, "(plain-text, copy/paste into email) ---\n\n")
  print(final_df, row.names = FALSE)
  cat("\n")

  tbl <- final_df %>%
    kable(caption = title, booktabs = TRUE, align = "lcccccc") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Electoral Pillar (EMEL)" = 3, "Civil Liberties Pillar (CSCW)" = 3)) %>%
    column_spec(2:4, background = "#f0f7ff") %>% column_spec(5:7, background = "#fffcf0") %>%
    column_spec(1, bold = TRUE) %>%
    row_spec((nrow(final_df)-3):nrow(final_df), bold = TRUE, background = "#eeeeee")

  for (j in 2:7) {
    column_values <- as.numeric(final_df[[j]][1:num_vars])
    tbl <- tbl %>% column_spec(j, color = spec_color(column_values, option = "D", end = 0.8))
  }
  return(tbl)
}

render_fa_table(df1, "Table 1: Baseline Structural Matrix", 12)
render_fa_table(df2, "Table 2: Refined Structural Matrix", 8)

# GDP (econ_dev) loading callout across all four models ----
gdp_loadings <- bind_rows(
  as.data.frame(unclass(efa_emel_base$loadings))["econ_dev", ] %>% mutate(Model = "EMEL Baseline (12 vars)"),
  as.data.frame(unclass(efa_cscw_base$loadings))["econ_dev", ] %>% mutate(Model = "CSCW Baseline (12 vars)"),
  as.data.frame(unclass(efa_emel_trim$loadings))["econ_dev", ] %>% mutate(Model = "EMEL Trimmed (8 vars)"),
  as.data.frame(unclass(efa_cscw_trim$loadings))["econ_dev", ] %>% mutate(Model = "CSCW Trimmed (8 vars)")
) %>%
  relocate(Model) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

cat("\n--- GDP (econ_dev) loading across models ---\n")
print(gdp_loadings)

# Suitability Footer (KMO / Bartlett) -- appended under Table 1 & Table 2 ----
# KMO and Bartlett's test are single numbers per model (not per-variable), so
# they don't belong as extra table rows like SS Loadings/Var Explained do;
# they're reported here as a compact footer instead of a separate file.
cat("\n--- Sampling Adequacy / Sphericity Footer (applies to Table 1 & Table 2 above) ---\n")
cat(sprintf("Table 1 (Baseline, 12 vars): KMO = %.3f | Bartlett p = %.3e\n",
            base_diag$kmo, base_diag$bart_p))
cat(sprintf("Table 2 (No-Geography, 8 vars): KMO = %.3f | Bartlett p = %.3e\n",
            trim_diag$kmo, trim_diag$bart_p))
cat("(KMO > 0.6 and Bartlett p < .05 indicate the data are suitable for factor analysis.)\n")

# 6. Comparative Suitability & Normality Summary ----
bind_rows(base_diag$diag, trim_diag$diag) %>%
  mutate(Status = ifelse(abs(skew) > 2 | abs(kurtosis) > 7, "⚠️ HIGH", "Normal")) %>%
  arrange(desc(abs(skew))) %>%
  kable(caption = "Variable Distribution Health Comparison", digits = 2, booktabs = TRUE) %>%
  kable_styling(bootstrap_options = "striped") %>%
  pack_rows("Baseline Variables", 1, 12) %>%
  pack_rows("Trimmed Variables", 13, 20)

cat("\n--- FINAL MODEL SUITABILITY COMPARISON ---\n")
cat(sprintf("Baseline: KMO = %.3f | Bartlett p = %.3e | CSCW TLI = %.2f\n", 
            base_diag$kmo, base_diag$bart_p, efa_cscw_base$TLI))
cat(sprintf("Trimmed:  KMO = %.3f | Bartlett p = %.3e | CSCW TLI = %.2f\n", 
            trim_diag$kmo, trim_diag$bart_p, efa_cscw_trim$TLI))
# Results suggest that the trimmed 8-variable model has less noise. It also explains more variation across the two dimensions. Roughly the dimensions can be interpreted as such:
## EMEL: MR1 = Development, MR2 = Centrality, MR3 = Urban.
## CSCW: MR1 = Urban, MR2 = Safety, MR3 = Centrality.

# [Gemini] "While the electoral dimension of democracy in Colombia is closely tied to economic development and geographic accessibility (EMEL MR1), the civil liberties dimension is almost exclusively defined by the absence of violence (CSCW MR2). The transition from a 12-variable baseline to an 8-variable refined model improved the TLI for Civil Liberties from an unacceptable 0.38 to a robust 0.91, proving that geographic location acts as statistical noise rather than a structural driver of democratic life."

# Factor scores for latent dimensions ----
# 1. Generate Scores for EMEL (Electoral)
# Method 'tenBerge' is preferred to preserve the factor correlations
emel_scores <- as.data.frame(predict(efa_emel_trim, 
                                     data = snvdem_emel[, trimmed_vars], 
                                     return.scores = TRUE))
# Rename based on Factor Loading interpretation
colnames(emel_scores) <- c("EMEL_Development", "EMEL_Inst_Centrality", "EMEL_Urbanicity")

# 2. Generate Scores for CSCW (Civil Liberties)
cscw_scores <- as.data.frame(predict(efa_cscw_trim, 
                                     data = snvdem_cscw[, trimmed_vars], 
                                     return.scores = TRUE))
# Rename based on Factor Loading interpretation
colnames(cscw_scores) <- c("CSCW_Urbanicity", "CSCW_Safety", "CSCW_Centrality")

# Merge into a single "Sub-Index" dataframe
snvdem_indices <- snvdem %>%
  bind_cols(emel_scores) %>%
  bind_cols(cscw_scores)

# "Democracy Gap" ----

# 1. Incorporate all 3 Dimensions using Weighted Sums
w_emel <- c((0.274/0.527), (0.148/0.527), (0.104/0.527)) # Proportional weights MR1, MR2, MR3 divided by cumulative variance explained
w_cscw <- c((0.183/0.464), (0.156/0.464), (0.125/0.464)) # Proportional weights MR1, MR2, MR3 divided by cumulative variance explained

# Create composite and MR1 (primary factor) scores for municipalities
snvdem_indices2 <- snvdem_indices %>%
  mutate(
    EMEL_Composite = (EMEL_Development * w_emel[1]) + (EMEL_Inst_Centrality * w_emel[2]) + (EMEL_Urbanicity * w_emel[3]),
    CSCW_Composite = (CSCW_Urbanicity * w_cscw[1]) + (CSCW_Safety * w_cscw[2]) + (CSCW_Centrality * w_cscw[3]),
    municipio = toupper(municipio),
    MPIO_CDPMP = str_pad(as.character(as.numeric(MPIO_CDPMP)), 5, pad = "0")
  ) %>%  
  # Calculate composite democracy gap: measures where electoral composite exceeds civil liberty composite scores
  mutate(Composite_Gap = EMEL_Composite - CSCW_Composite) %>%
  # Calculate Primary Factor gap: This measures where Electoral "Integration" (development level) exceeds Civil Liberty safety (nonviolence)
  mutate(MR1_Gap = EMEL_Development - CSCW_Safety) 

#---- Write the FA dataframe to rds ----
snvdem_FA <- snvdem_indices2 %>% select(1:36|75:84)
write_rds(snvdem_FA, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/09_analysis_scripts/Exploratory/02_FA/snvdem_FA.rds")


library(ggplot2)
library(tidyr)

# 1. Prepare data for the density plot
# We filter for a specific year or use the whole panel to see the overall distribution
density_data <- snvdem_FA %>%
  select(MR1_Gap, Composite_Gap) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Score")

# 2. Render the Density Plot
ggplot(density_data, aes(x = Score, fill = Metric)) +
  geom_density(alpha = 0.5, color = "white") +
  # Add a vertical line at 0 (The "Perfect Balance" point)
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  
  # Custom Colors
  scale_fill_manual(values = c("MR1_Gap" = "#d7191c", "Composite_Gap" = "#fdae61"),
                    labels = c("Composite Gap (All 3 Factors)", "Primary Gap (MR1 Only)")) +
  
  # Annotations for interpretation
  annotate("text", x = 1.5, y = 0.05, label = "Electoral Reach > Safety", 
           color = "#d7191c", fontface = "italic", size = 3) +
  annotate("text", x = -1.5, y = 0.05, label = "Safety > Electoral Reach", 
           color = "#2c7bb6", fontface = "italic", size = 3) +
  
  labs(title = "Distribution of the Colombian Democracy Gap",
       subtitle = "Comparing the Primary Dimension (MR1) vs. the Weighted Composite",
       x = "Gap Score (EMEL - CSCW)",
       y = "Density",
       fill = "Measurement Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

## Top 10 "Gapped" Municipalities ----
snvdem_FA %>%
  select(municipio, EMEL_Development, CSCW_Safety, Composite_Gap) %>%
  # Filter out any rows with missing names if necessary
  filter(!is.na(municipio)) %>%
  # We look for the largest absolute difference
  arrange(desc(Composite_Gap)) %>% 
  head(10) %>%
  mutate(
    Category = ifelse(Composite_Gap > 0, "Electoral > Civil", "Civil > Electoral")
  ) %>%
  kable(
    caption = "Table 3: Top 10 Municipalities with the Largest Positive Democracy Gap", 
    digits = 3, 
    booktabs = TRUE,
    col.names = c("Municipio", "Electoral (Dev/Reach)", "Civil (Safety)", "Gap Score", "Direction")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(4, bold = TRUE, color = "white", background = "#D7261E") # Highlight the Gap column


## Average scores over time ----
snvdem_collapsed <- snvdem_FA %>%
  mutate(municipio = toupper(municipio)) %>% # Standardizes 'Bogotá' vs 'BOGOTÁ'
  group_by(municipio) %>%
  summarise(
    Avg_EMEL_Dev = mean(EMEL_Development, na.rm = TRUE),
    Avg_CSCW_Safe = mean(CSCW_Safety, na.rm = TRUE),
    Avg_Gap = mean(Composite_Gap, na.rm = TRUE),
    .groups = 'drop'
  )

# Top 10 over time
snvdem_collapsed %>%
  arrange(desc(Avg_Gap)) %>%
  head(10) %>%
  kable(caption = "Table 4 (Corrected): Top 10 Unique Municipalities by Average Democracy Gap", 
        digits = 3, booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(4, bold = TRUE, background = "#D7261E", color = "white")


library(ggplot2)
library(ggrepel)

# Create the Quadrant Plot
ggplot(snvdem_collapsed, aes(x = Avg_EMEL_Dev, y = Avg_CSCW_Safe)) +
  # Reference lines for quadrants (using means)
  geom_vline(xintercept = mean(snvdem_collapsed$Avg_EMEL_Dev), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = mean(snvdem_collapsed$Avg_CSCW_Safe), linetype = "dashed", color = "gray50") +
  # The Data Points
  geom_point(aes(color = Avg_Gap), alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red") +
  # Label only the top "Gapped" municipalities
  geom_text_repel(data = subset(snvdem_collapsed, Avg_Gap > 2.4), 
                  aes(label = municipio), size = 3, fontface = "bold") +
  # Labels and Theme
  labs(title = "The Democracy Gap: Institutional Reach vs. Civil Safety",
       subtitle = "Red indicates municipalities where Electoral Fairness significantly outpaces Civil Liberties",
       x = "Electoral Development (EMEL Score)",
       y = "Civil Safety (CSCW Score)",
       color = "Gap Size") +
  theme_minimal()


# 1. Normalize and Collapse including the Region variable
library(stringi) # For removing accents easily

snvdem_reg_collapsed <- snvdem_FA %>%
  mutate(
    municipio = toupper(municipio),
    # Remove accents and whitespace first
    depto_clean = stri_trans_general(depto, "Latin-ASCII"),
    depto_clean = str_to_title(str_trim(depto_clean)),
    
    # Manual Mapping Dictionary for duplicates
    depto_final = case_when(
      str_detect(depto_clean, "Andres")     ~ "San Andrés",
      str_detect(depto_clean, "Bogota")     ~ "Bogotá D.C.",
      str_detect(depto_clean, "Guajira")    ~ "La Guajira",
      str_detect(depto_clean, "Norte")      ~ "Norte de Santander",
      str_detect(depto_clean, "Valle")      ~ "Valle del Cauca",
      depto_clean == "Choco"                ~ "Chocó",
      depto_clean == "Bolivar"              ~ "Bolívar",
      depto_clean == "Boyaca"               ~ "Boyacá",
      depto_clean == "Caqueta"              ~ "Caquetá",
      depto_clean == "Cordoba"              ~ "Córdoba",
      depto_clean == "Quindio"              ~ "Quindío",
      TRUE ~ depto_clean
    )
  ) %>%
  group_by(municipio, depto_final) %>% 
  summarise(
    Avg_EMEL_Dev = mean(EMEL_Development, na.rm = TRUE),
    Avg_CSCW_Safe = mean(CSCW_Safety, na.rm = TRUE),
    Avg_Gap      = mean(Composite_Gap, na.rm = TRUE),
    .groups = 'drop'
  )



# Mapping the democracy gap ----
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)

# 1. Simplify Data Prep: Collapse to Means and Categorize in one pipe ----
snvdem_map_ready <- snvdem_FA %>%
  mutate(
    # Explicitly name arguments to avoid side/pad confusion
    MPIO_CDPMP = str_pad(as.numeric(MPIO_CDPMP), width = 5, side = "left", pad = "0"),
    municipio = toupper(municipio)
  ) %>%
  group_by(MPIO_CDPMP, municipio) %>%
  summarise(
    across(c(EMEL_Development, CSCW_Safety, Composite_Gap), 
           \(x) mean(x, na.rm = TRUE), 
           .names = "Avg_{.col}"), 
    .groups = 'drop'
  )

# Calculate thresholds
e_mean <- mean(snvdem_map_ready$Avg_EMEL_Development)
c_mean <- mean(snvdem_map_ready$Avg_CSCW_Safety)

# Categorize and select labels (Distance-based outliers per quadrant)
map_labels <- snvdem_map_ready %>%
  mutate(Quadrant = case_when(
    Avg_EMEL_Development >= e_mean & Avg_CSCW_Safety >= c_mean ~ "Consolidated",
    Avg_EMEL_Development >= e_mean & Avg_CSCW_Safety < c_mean  ~ "Democracy Gap",
    Avg_EMEL_Development < e_mean  & Avg_CSCW_Safety < c_mean  ~ "Unsafe Periphery",
    Avg_EMEL_Development < e_mean  & Avg_CSCW_Safety >= c_mean ~ "Safe Periphery"
  ),
  dist = sqrt((Avg_EMEL_Development - e_mean)^2 + (Avg_CSCW_Safety - c_mean)^2)
  ) %>%
  group_by(Quadrant) %>%
  slice_max(order_by = dist, n = 5) %>% # Force top 5 per quadrant
  ungroup()

# 2. Join with Geography ----
# Load and Clean Shapefile with explicit padding arguments
muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp", quiet = TRUE) %>%
  mutate(MPIO_CDPMP = str_pad(as.numeric(MPIO_CDPMP), width = 5, side = "left", pad = "0"))

map_joined <- muni_geo %>% left_join(snvdem_map_ready, by = "MPIO_CDPMP")

# Prepare Centroids for Labels
label_points <- muni_geo %>%
  filter(MPIO_CDPMP %in% map_labels$MPIO_CDPMP) %>%
  st_centroid() %>%
  left_join(map_labels, by = "MPIO_CDPMP")

# 3. Optimized Plotting ----
ggplot(data = map_joined) +
  geom_sf(aes(fill = Avg_Composite_Gap), color = "white", size = 0.01) +
  scale_fill_gradient2(low = "#0571b0", mid = "#f7f7f7", high = "#ca0020", midpoint = 0, name = "Index Gap") +
  
  # The labeling layer
  geom_label_repel(data = label_points, 
                   aes(label = municipio, geometry = geometry, color = Quadrant),
                   stat = "sf_coordinates", size = 2.5, fontface = "bold",
                   box.padding = 0.8, point.padding = 0.3, force = 40,
                   max.overlaps = 20, # Higher tolerance for labels
                   segment.size = 0.2, show.legend = TRUE) +
  
  scale_color_manual(values = c("Consolidated" = "#2c7bb6", 
                                "Democracy Gap" = "#d7191c", 
                                "Unsafe Periphery" = "#fdae61", 
                                "Safe Periphery" = "#abd9e9")) +
  labs(title = "Colombian Democracy: Institutional Outliers",
       subtitle = "Distance-based outliers relative to national means of Reach (EMEL) and Safety (CSCW)",
       caption = "Source: SNVDEM 2024") +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(face="bold"))


