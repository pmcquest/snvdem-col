# Visualize geo-coded data before and after imputation

library(tidyverse)

# raw data
df_raw <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/02_cleaned_data/df_col_clean.rds")
# imputed data
df_imputed <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/imputed_master_panel.rds")

# 1. Prepare Raw Data (from your plot_data1 source)
plot_data1 <- df_raw %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
df_raw1 <- plot_data1 %>% 
  mutate(status = "Raw")

# 2. Prepare Imputed Data (from your plot_data2 source)
plot_data2 <- df_imputed %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
df_imputed1 <- plot_data2 %>% 
  mutate(status = "Imputed")

# 3. Combine them
exclude_vars <- c("axis_ns", "axis_we", "disp_ns", "disp_we", 
                  "nAllRds_13", "lAllRds_13", "nAR_13pkm", 
                  "lAR_13pkm", "lMjRds_13pkm", "RulParD_15t16", "ViolInd_1011", "year", "PropInd_14", "PropEtn_14")
combined_data1 <- bind_rows(df_raw1, df_imputed1) %>%
  # Filter to keep only variables that exist in the imputed set
  filter(variable %in% unique(plot_data2$variable)) %>%
  filter(!(variable %in% exclude_vars))

# Visualization ----
comparison_plot <- ggplot(combined_data1, aes(x = value, fill = status)) +
  # Using position = "identity" to overlay the plots
  geom_density(alpha = 0.4, color = NA) + 
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = c("Raw" = "#e41a1c", "Imputed" = "#377eb8")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Before-and-After: Distribution of Raw vs. Imputed Variables",
    subtitle = "Red areas indicate original data; Blue indicates the distribution after imputation",
    x = "Value",
    y = "Density",
    fill = "Data Status:"
  )

print(comparison_plot)

# Save the comparison plot
ggsave(
  filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/diagnostics/Raw_vs_Imputed.png",
  plot = comparison_plot,
  width = 16,       # Wide enough for 3 columns
  height = 14,      # Height should be adjusted based on the number of variables
  units = "in",
  dpi = 300,        # High resolution for publication/reports
  bg = "white"      # Ensures a clean background even with transparency
)

library(tidyverse)

# 1. Identify common variables to ensure a clean match
# We only want to visualize what was actually imputed
common_vars <- intersect(unique(plot_data1$variable), unique(plot_data2$variable))

# 2. Prepare the combined dataset with explicit filtering
combined_comparison <- bind_rows(
  plot_data1 %>% filter(variable %in% common_vars) %>% mutate(status = "Raw"),
  plot_data2 %>% filter(variable %in% common_vars) %>% mutate(status = "Imputed")
) %>%
  # Ensure 'Raw' is plotted behind 'Imputed' for visibility
  mutate(status = factor(status, levels = c("Raw", "Imputed")))

# 3. Create the Overlaid Density Plot
final_plot <- ggplot(combined_comparison, aes(x = value, fill = status)) +
  geom_density(alpha = 0.5, color = NA, position = "identity") + 
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = c("Raw" = "#ef8a62", "Imputed" = "#67a9cf")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 7, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Comparison of Distributions: Raw vs. Imputed",
    subtitle = "Overlaying only variables present in the final imputed master panel",
    x = "Variable Value",
    y = "Density",
    fill = "Data Source"
  )

print(final_plot)
