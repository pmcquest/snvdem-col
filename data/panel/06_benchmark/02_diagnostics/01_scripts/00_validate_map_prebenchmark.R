#---- Step 5b: Validate the pre-benchmarked snvdem_col_weighted data by mapping it ----

# Purpose: same idea as 01_validate_map.R (which maps the later, benchmarked sndem_final), but
# for this step's own output -- lets the two be compared side by side to see what benchmarking
# (Step 6) actually changes, and to sanity-check the Step 5 formula update on its own before
# benchmarking is even applied. Generates ONE faceted plot across all years so the color scale
# is fixed globally, same reasoning as 01_validate_map.R.

library(tidyverse)
library(sf)
library(scales)

panel_dir <- "G:/Shared drives/snvdem/snvdem-col/data/panel"
img_dir   <- file.path(panel_dir, "06_benchmark", "02_diagnostics", "02_outputs")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

master_df <- readRDS(file.path(panel_dir, "05_weighting", "03_output", "snvdem_col_weighted.rds"))

clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")

muni_geo <- st_read("G:/Shared drives/snvdem/snvdem-col/data/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp",
                     quiet = TRUE) %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

master_df <- master_df %>% mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP))

map_df <- muni_geo %>% left_join(master_df, by = "MPIO_CDPMP")

#---- One faceted map, all years, single fixed color scale ----
color_limits <- range(map_df$sndem, na.rm = TRUE)
facet_all_years <- ggplot(map_df) +
  geom_sf(aes(fill = sndem), color = NA) +
  facet_wrap(~year, ncol = 6) +
  scale_fill_viridis_c(limits = color_limits,
                        name = "sndem\n(pre-benchmark)", na.value = "grey90") +
  labs(title = "Subnational democracy in Colombia, pre-benchmarking (2000-2023)",
       subtitle = "Unweighted 50/50 average of snelect + sncivlib (v. 2026-07-26)",
       caption = "Color scale fixed across all panels (single ggplot object).") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 8), legend.position = "right")

ggsave(facet_all_years,
       filename = file.path(img_dir, "sndem_prebenchmark_facet_allyears_2026-07-26.png"),
       width = 14, height = 10, dpi = 300)

cat("sndem (pre-benchmark) range used for the shared color scale:\n")
print(round(color_limits, 3))

#---- Three-dimension comparison at milestone years (EL / CL / combined) ----
map_long <- map_df %>%
  filter(year %in% c(2000, 2008, 2016, 2023)) %>%
  pivot_longer(cols = c(snelect, sncivlib, sndem), names_to = "Index_Type", values_to = "Score") %>%
  mutate(Index_Type = case_when(
    Index_Type == "snelect"  ~ "Elections (snelect)",
    Index_Type == "sncivlib" ~ "Civil liberties (sncivlib)",
    Index_Type == "sndem"    ~ "Combined (sndem)"
  ))

dims_limits <- range(map_long$Score, na.rm = TRUE)
facet_dims <- ggplot(map_long) +
  geom_sf(aes(fill = Score), color = NA) +
  facet_grid(year ~ Index_Type) +
  scale_fill_viridis_c(limits = dims_limits,
                        name = "Score\n(pre-benchmark)", na.value = "grey90") +
  labs(title = "Electoral vs. civil liberties vs. combined index, pre-benchmarking",
       subtitle = "Milestone years, 2000-2023 -- fixed color scale, July formula") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9), legend.position = "bottom",
        legend.key.width = unit(2, "cm"))

ggsave(facet_dims,
       filename = file.path(img_dir, "sndem_prebenchmark_dims_milestone_years_2026-07-26.png"),
       width = 10, height = 12, dpi = 300)

cat("\nSaved:\n  sndem_prebenchmark_facet_allyears_2026-07-26.png\n  sndem_prebenchmark_dims_milestone_years_2026-07-26.png\n")
