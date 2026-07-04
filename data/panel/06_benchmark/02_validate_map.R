#---- Step 8b: Validate the benchmarked sndem_final by mapping it across all years ----

# Purpose: visually confirm the qnorm()-based EL/CL combination (01_benchmark.R, per MC's
# email 2026-07-02) produces a sensible spatial + temporal pattern -- no inverted, flattened,
# or exaggerated trend. Generates ONE faceted plot across all 24 years so the color scale is
# fixed globally (ggplot2 assigns a single continuous scale across facets automatically when
# the whole facet_wrap() is built from one plot object -- per MC: "I think that happens
# automatically if you generate the big facet plot all at once").

library(tidyverse)
library(sf)
library(scales)

master_df <- readRDS("G:/Shared drives/snvdem/snvdem-col/data/panel/07_final_snvdem_data/snvdem_col_final.rds")

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp",
                     quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

master_df <- master_df %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_df <- muni_geo %>% left_join(master_df, by = "MPIO_CDPMP")

#---- One faceted map, all 24 years, single fixed color scale ----
# Sequential viridis "magma" (default direction: low = dark, high = bright), one shared scale
# fixed to the full panel's own min/max (not per-facet auto-scaled, so the scale doesn't reset
# year to year).
color_limits <- range(map_df$sndem_final, na.rm = TRUE)
facet_all_years <- ggplot(map_df) +
  geom_sf(aes(fill = sndem_final), color = NA) +
  facet_wrap(~year, ncol = 6) +
  scale_fill_viridis_c(option = "magma", limits = color_limits,
                        name = "sndem_final\n(global Z-score)", na.value = "grey90") +
  labs(title = "Subnational democracy in Colombia, 2000-2023",
       subtitle = "0 = full V-Dem 2000-2023 country-year average (elections + civil liberties, globally standardized, averaged)",
       caption = "Color scale fixed across all panels (single ggplot object).") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 8), legend.position = "right")

ggsave(facet_all_years,
       filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/sndem_final_facet_allyears.png",
       width = 14, height = 10, dpi = 300)

#---- Sanity check: fixed-scale legend actually spans the full-panel range, not a per-facet one ----
cat("sndem_final range used for the shared color scale:\n")
print(round(range(map_df$sndem_final, na.rm = TRUE), 3))

#---- Three-dimension comparison at milestone years (EL / CL / combined), current columns ----
map_long <- map_df %>%
  filter(year %in% c(2000, 2008, 2016, 2023)) %>%
  pivot_longer(cols = c(EL_col_gz, CL_col_gz, sndem_final), names_to = "Index_Type", values_to = "Score") %>%
  mutate(Index_Type = case_when(
    Index_Type == "EL_col_gz"   ~ "Elections (global std.)",
    Index_Type == "CL_col_gz"   ~ "Civil liberties (global std.)",
    Index_Type == "sndem_final" ~ "Combined (sndem_final)"
  ))

dims_limits <- range(map_long$Score, na.rm = TRUE)  # EL alone reaches further than sndem_final
facet_dims <- ggplot(map_long) +
  geom_sf(aes(fill = Score), color = NA) +
  facet_grid(year ~ Index_Type) +
  # Same palette as the full facet map above, but limits span all three columns (EL/CL/
  # combined) so none of them get clipped -- EL_col_gz alone reaches further than sndem_final.
  scale_fill_viridis_c(option = "magma", limits = dims_limits,
                        name = "Global\nZ-score", na.value = "grey90") +
  labs(title = "Electoral vs. civil liberties vs. combined index",
       subtitle = "Milestone years, 2000-2023 -- fixed color scale") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9), legend.position = "bottom",
        legend.key.width = unit(2, "cm"))

ggsave(facet_dims,
       filename = "G:/Shared drives/snvdem/snvdem-col/data/panel/06_benchmark/sndem_dims_milestone_years.png",
       width = 10, height = 12, dpi = 300)

cat("\nSaved:\n  sndem_final_facet_allyears.png\n  sndem_dims_milestone_years.png\n")
