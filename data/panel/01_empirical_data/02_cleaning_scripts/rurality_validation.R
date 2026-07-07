
#---- Rurality Index Validation (CEDE vs. ColOpenData) ----

##---- Purpose ----
# df01-01234513-clean.R uses CEDE's IndRur_0t1 (rurality index, criterion 0-1)
# for 1993-2020, patched with a ColOpenData-derived rural population ratio for
# 2021-2023 (CEDE's own panel ends in 2020). This script independently checks
# that patch: over the years CEDE and ColOpenData both cover (1993-2020), we
# recompute the same ratio (rural population / total population) from
# ColOpenData alone and correlate it against CEDE's own IndRur_0t1. A strong,
# stable correlation supports trusting ColOpenData for the 2021-2023 years
# CEDE doesn't cover.

##---- History ----
# This check previously lived inline in df01-01234513-clean.R, comparing CEDE
# against TerriData's 2018 census export (R18l) for 2018-2020 only. That
# export was missing 20 municipalities (19 Areas No Municipalizadas in
# Amazonas/Guainia/Vaupes + San Andres -- corregimientos departamentales with
# no municipal government, plus one genuine gap in DANE's export, verified
# 2026-07-05). ColOpenData's population_projections.xlsx (Los Andes
# Epiverse-TRACE) has complete 1122-municipality coverage for every year
# 1985-2030, including the same urban ("cabecera_municipal") vs. rural
# ("centros_poblados_y_rural_disperso") breakdown, so both the 2021-2023 patch
# in df01 and this validation moved to ColOpenData, and the validation window
# widened from 3 years to the full 1993-2020 CEDE overlap.

#---- Script ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

base <- "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/"

MunYrs <- read_rds(paste0(base, "01_source_files/MunYrs.rds"))

# CEDE's own rurality index, independent of df01_clean.rds so this script can
# run standalone (matches every other df0X-clean.R script's pattern of reading
# its own raw sources rather than depending on another script's output).
CEDE_rur <- read_excel(
  paste0(base, "01_source_files/source_files/a2-CEDE_PM/2022/General/PANEL_CARACTERISTICAS_GENERALES(2021).xlsx"),
  col_types = "text"
) %>%
  # Select by name, not position: df01 reorders columns before renaming by
  # position, but this script reads the raw file directly, where "indrural"
  # is column 21, not 10 (mixing these up silently pulls the wrong column).
  select(MPIO_CDPMP = codmpio, year = ano, IndRur_cede = indrural) %>%
  mutate(
    MPIO_CDPMP = ifelse(nchar(MPIO_CDPMP) == 4, paste0("0", MPIO_CDPMP), MPIO_CDPMP),
    year = as.numeric(year),
    IndRur_cede = as.numeric(IndRur_cede)
  ) %>%
  filter(MPIO_CDPMP %in% MunYrs$MPIO_CDPMP)

# Same rural-ratio formula CEDE uses (pobl_rur / pobl_tot), recomputed from
# ColOpenData population instead. area == "cabecera_municipal" is urban,
# "centros_poblados_y_rural_disperso" is rural. codigo_municipio/municipio are
# swapped for 2005-2019 in the source file (codigo_municipio holds the name,
# municipio holds the 5-digit code) -- un-swap before use so no data is lost.
ColOp_rur <- read_excel(paste0(base, "01_source_files/source_files/ColOpenData/population_projections.xlsx")) %>%
  filter(area %in% c("cabecera_municipal", "centros_poblados_y_rural_disperso")) %>%
  mutate(MPIO_CDPMP = ifelse(grepl("^[0-9]{5}$", codigo_municipio), codigo_municipio, municipio)) %>%
  select(MPIO_CDPMP, year = ano, area, total) %>%
  filter(MPIO_CDPMP %in% MunYrs$MPIO_CDPMP) %>%
  pivot_wider(names_from = area, values_from = total) %>%
  mutate(IndRur_colop = centros_poblados_y_rural_disperso /
           (cabecera_municipal + centros_poblados_y_rural_disperso)) %>%
  select(MPIO_CDPMP, year, IndRur_colop)

#---- Compare over the CEDE/ColOpenData overlap (1993-2020) ----
comparison <- inner_join(CEDE_rur, ColOp_rur, by = c("MPIO_CDPMP", "year")) %>%
  filter(year >= 1993, year <= 2020)

cor_by_year <- comparison %>%
  group_by(year) %>%
  summarize(
    correlation = cor(IndRur_cede, IndRur_colop, use = "complete.obs"),
    n_obs = sum(!is.na(IndRur_cede) & !is.na(IndRur_colop))
  )

print(cor_by_year, n = Inf)

cat("\nPooled correlation, 1993-2020:",
    cor(comparison$IndRur_cede, comparison$IndRur_colop, use = "complete.obs"), "\n")

#---- Diagnostic plots ----
trend_plot <- ggplot(cor_by_year, aes(x = year, y = correlation)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue") +
  ylim(0, 1) +
  labs(
    title = "CEDE vs. ColOpenData Rurality Index Correlation by Year",
    subtitle = "Validates the ColOpenData patch used for 2021-2023 in df01-01234513-clean.R",
    x = "Year", y = "Correlation"
  ) +
  theme_minimal()

ggsave(paste0(base, "05_diagnostics/rurality_validation_correlation.png"), trend_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

scatter_plot <- ggplot(comparison, aes(x = IndRur_cede, y = IndRur_colop)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "CEDE vs. ColOpenData Rurality Index, 1993-2020",
    x = "CEDE IndRur_0t1", y = "ColOpenData rural ratio"
  ) +
  theme_minimal()

ggsave(paste0(base, "05_diagnostics/rurality_validation_scatter.png"), scatter_plot,
       width = 8, height = 8, dpi = 300, bg = "white")

write_csv(cor_by_year, paste0(base, "05_diagnostics/rurality_validation_correlation.csv"))
