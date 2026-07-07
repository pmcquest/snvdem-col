
#---- Fiscal performance ----

##---- Criteria measured ----
# 2: Areas that are less economically developed. (0=No, 1=Yes) [v2*_2] 
# 3: Areas that are more economically developed. (0=No, 1=Yes) [v2*_3]


##---- Data Sources ----
# Data 1 (not here): CEDE (U de Los Andes) Panel Municipal General (2022) (https://datoscede.uniandes.edu.co/catalogo-de-datos/). We draw "poverty" data from General panel (see a01_45_13-clean.R).
# Data 2: CEDE (U de Los Andes) Panel Municipal Gobierno (2022) (https://datoscede.uniandes.edu.co/catalogo-de-datos/). 
# Data 3: DNP economic accounting data for municipalities.
# Note: DNP also has a municipal GDP (Valor Agregado Municipal) measure, but it is limited to 2011 onwards. (https://www.dane.gov.co/index.php/estadisticas-por-tema/cuentas-nacionales/cuentas-nacionales-departamentales; See methodology here: https://www.dane.gov.co/files/operaciones/PIB/departamental/DSO-VAM-MET-001-V2.pdf)

#---- Script for cleaning ----
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

##---- 2-3 Economic development (?) ----
# There are different measures similar to GDP--VAM (municipal), PBI (departmental)--as well as other proxies for development (e.g., nightlights).
# IPUMS-Col. data includes variables such as: home ownership, access to electricity, water supply, floor material, wall material, and number of families. This could be used as well. 

###----Municipal fiscal performance (2000-2023)----
# In Spanish: Indice de Desempeño Fiscal (Municipal) (https://www.dnp.gov.co/LaEntidad_/subdireccion-general-descentralizacion-desarrollo-territorial/direccion-descentralizacion-fortalecimiento-fiscal/Paginas/informacion-fiscal-y-financiera.aspx). This measure by DNP combines various fiscal data (municipal income, spending, borrowing, etc.). According to CEDE, the variables included in the index are:
## Buen balance en su desempeño fiscal
## Suficientes recursos para sostener su funcionamiento
## Cumplimiento a los límites de gasto de funcionamiento según la Ley 617/00
## Importante nivel de recursos propios (solvencia tributaria) como contrapartida a los recursos de SGP
## Altos niveles de inversión
## Adecuada capacidad de respaldo del servicio de su deuda
## Generación de ahorro corriente, necesario para garantizar su solvencia financiera

# Scores range from 0-100. 
# Note: The advantage of this measure is that it revolves around fiscal data, which presumably can be accessed in other countries. The disadvantage is that the formula for calculating the index is slightly adjusted from time to time (e.g., both 2021 and 2022 introduced changes) and may not make sense in other countries.

# We will combine two sources: CEDE (2000-2020) and DNP-DANE (2021-2023):

#### A) CEDE data (2000-2020) ----
CEDE02 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/a2-CEDE_PM/2022/Gobierno/PANEL_BUEN_GOBIERNO(2021).xlsx")
colnames(CEDE02)[1] = "MPIO_CDPMP"
colnames(CEDE02)[2] = "year"

CEDE02 <- CEDE02[c("MPIO_CDPMP", "year", "DF_desemp_fisc")] 

CEDE02$MPIO_CDPMP <- as.character(CEDE02$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE02$MPIO_CDPMP <- ifelse(nchar(CEDE02$MPIO_CDPMP) == 4, paste0("0", CEDE02$MPIO_CDPMP), CEDE02$MPIO_CDPMP)

# rename
CEDE02 <- CEDE02 %>%
  rename(IDF_2t3 = 3) %>%
  filter(year >= 2000)

#### B) DNP data (2021-2023) ----
# site: https://www.dnp.gov.co/LaEntidad_/subdireccion-general-descentralizacion-desarrollo-territorial/direccion-descentralizacion-fortalecimiento-fiscal/Paginas/informacion-fiscal-y-financiera.aspx

## Metodologia nueva
IDF23 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/2-3_EconDevt/IDF/ResultadosIDF_Nueva_MetodologIa_2023_Act.xlsx", sheet = "Municipios 2023", range = "A7:AF1109")
IDF23 <- IDF23 %>%
  rename(MPIO_CDPMP = 1) %>%
  mutate(year = 2023) %>%
  select(1|17|33)

IDF22 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/2-3_EconDevt/IDF/Resultados IDF-Nueva Metodología 2022.xlsx", sheet = "Municipios 2022", range = "A6:AF1108")
IDF22 <- IDF22 %>%
  rename(MPIO_CDPMP = 1) %>%
  mutate(year = 2022) %>%
  select(1|17|33)

IDF21 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/2-3_EconDevt/IDF/IDF_2021_Nueva_Metodologia.xlsx", 
                    sheet = "Municipios 2021", range = "A7:AH1109")
IDF21 <- IDF21 %>%
  rename(MPIO_CDPMP = 1) %>%
  mutate(year = 2021) %>%
  select(1|18|35)

## Metodologia anterior 
IDF22a <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/2-3_EconDevt/IDF/Resultados IDF -Anterior Metodología 2022.xlsx", sheet = "Municipios 2022 - antigua metod", range = "A6:N1108")
IDF22a <- IDF22a %>%
  rename(MPIO_CDPMP = 1, Resultados = 13) %>%
  mutate(year = 2022) %>%
  select(1|13|15)

IDF21a <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/2-3_EconDevt/IDF/IDF_2021_Metodologia_Anterior.xlsx", sheet = "Municipios 2021", range = "A8:Q1109")
IDF21a <- IDF21a %>%
  rename(MPIO_CDPMP = 1, Resultados = 10) %>%
  mutate(year = 2021) %>%
  select(1|10|18)


# Compare new and old versions
idf_comparison <- bind_rows(
  IDF21  %>% mutate(Version = "Nueva", Source = "IDF21"),
  IDF21a %>% mutate(Version = "Anterior", Source = "IDF21a"),
  IDF22  %>% mutate(Version = "Nueva", Source = "IDF22"),
  IDF22a %>% mutate(Version = "Anterior", Source = "IDF22a"),
  IDF23  %>% mutate(Version = "Nueva", Source = "IDF23")
)

# Calculate the averages per year and version
idf_summary <- idf_comparison %>%
  group_by(year, Version) %>%
  summarise(mean_score = mean(Resultados, na.rm = TRUE), .groups = "drop")

# Visualization: significant decrease with Nueva Metodologia
ggplot(idf_summary, aes(x = factor(year), y = mean_score, fill = Version)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(mean_score, 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Nueva" = "#3498db", "Antigua" = "#e74c3c")) +
  labs(
    title = "Comparison of Average Municipal IDF Scores (2021-2023)",
    subtitle = "Highlighting the difference between Nueva Metodologia and Anterior",
    x = "Year",
    y = "Average Score (Resultados)",
    fill = "Dataset Version"
  ) +
  theme_minimal() +
  ylim(0, 100)


#### Compare with CEDE02 data (2000-2020) ----

historical_avg <- CEDE02 %>%
  group_by(year) %>%
  summarise(avg_idf = mean(IDF_2t3, na.rm = TRUE)) %>%
  mutate(Group = "Historical (CEDE)")

# Path 1: Nueva metodologia
nuevo_branch <- bind_rows(
  IDF21 %>% rename(avg_idf = Resultados),
  IDF22 %>% rename(avg_idf = Resultados),
  IDF23 %>% rename(avg_idf = Resultados)
) %>%
  group_by(year) %>%
  summarise(avg_idf = mean(avg_idf, na.rm = TRUE)) %>%
  mutate(Group = "Nueva Version")

# Path 2: Anterior metodologia
anterior_branch <- bind_rows(
  IDF21a %>% rename(avg_idf = Resultados),
  IDF22a %>% rename(avg_idf = Resultados),
  # Note: Using IDF23 here as the continuation of the A-series for comparison
  IDF23  %>% rename(avg_idf = Resultados) 
) %>%
  group_by(year) %>%
  summarise(avg_idf = mean(avg_idf, na.rm = TRUE)) %>%
  mutate(Group = "Anterior version")

# 3. Combine and Plot
plot_df <- bind_rows(historical_avg, nuevo_branch, anterior_branch)

ggplot(plot_df, aes(x = year, y = avg_idf, color = Group, linetype = Group, group = Group)) +
  geom_line(linewidth = 1.2) + # Use linewidth instead of size for newer ggplot2
  geom_point(size = 2) +
  scale_color_manual(values = c("Historical (CEDE)" = "black", 
                                "Standard Version" = "blue", 
                                "Version A" = "red")) +
  # Ensuring the linetypes match your group names exactly
  scale_linetype_manual(values = c("Historical (CEDE)" = "solid", 
                                   "Standard Version" = "dashed", 
                                   "Version A" = "twodash")) +
  labs(
    title = "IDF Trend Continuity Check",
    subtitle = "Comparing Historical CEDE (2000-2020) with 2021-2023 Candidates",
    x = "Year",
    y = "Average IDF Score",
    color = "Data Source",
    linetype = "Data Source"
  ) +
  theme_minimal()

# Combine IDF datasets into long format and rename "Resultados" to "IDF_0t1"
# NOTE 2026-07-06: IDF23 (loaded above, DNP's actual 2023 fiscal-performance results) is
# deliberately NOT included here -- only IDF21a/IDF22a ("Anterior"/old methodology) are, for
# consistency with the historical CEDE-based series (2000-2020) and the 2021-2022 "Anterior"
# files. There is no "Anterior"-methodology reconciliation of 2023 to match that scale (only the
# "Nueva" file exists for 2023 -- see IDF23 above and the Nueva-vs-Anterior comparison plots in
# this script, which exist for exactly this reason: the two methodologies produce different
# levels). Net effect: IDF_2t3 is 100% missing for 2023 in df02_clean.rds -- not because DNP
# hasn't published 2023 data (they have), but because it's on the wrong methodology vintage to
# splice in without a level correction. Currently imputed downstream (02_imputation/
# 01_imputation_scripts/imp23_FiscalCART_v3.R) rather than reconciled at the source. If a
# principled Nueva->Anterior adjustment is ever derived (e.g. from the 2021/2022 overlap, where
# both versions exist), IDF23 could be added to IDF_combined below instead of relying on
# imputation for this year. See imputation_methodology_memo_2026-07-05.md, Section 2b.
IDF_combined <- bind_rows(IDF21a, IDF22a) %>%
  rename(IDF_2t3 = Resultados)

summary(IDF_combined)

#---- Merge df----
df02 <- full_join(IDF_combined, CEDE02, by = c("MPIO_CDPMP", "year", "IDF_2t3"))



##----- Completeness ----
# Calculate completeness across all variables per year
completeness_summary <- df02 %>%
  mutate(
    # Count non-missing values for each row excluding 'MPIO_CDPMP' and 'year'
    non_missing_values = rowSums(!is.na(select(., -MPIO_CDPMP, -year))),  
    
    # Calculate total possible values (excluding 'MPIO_CDPMP' and 'year')
    total_values = ncol(.) - 2,  # Total number of variables excluding 'MPIO_CDPMP' and 'year'
    
    # Calculate completeness percentage for each observation
    completeness_percentage = (non_missing_values / total_values) * 100
  )
print(completeness_summary)
summary(completeness_summary$completeness_percentage)

# Average completeness by year
completeness_by_year <- completeness_summary %>%
  group_by(year) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_year)

# average completeness by municipality
completeness_by_municipality <- completeness_summary %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_municipality)

library(ggplot2)
ggplot(completeness_by_year, aes(x = year, y = avg_completeness)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Data Completeness by Year", x = "Year", y = "Completeness (%)") +
  theme_minimal()

# Check for NA's 
na_counts <- colSums(is.na(df02))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
# Print the number of NAs for each variable
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))

#---- Save cleaned dataset ----
write_rds(df02, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/03_clean_outputs/df02_clean.rds")
