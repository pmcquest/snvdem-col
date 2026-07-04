
# snvdem analysis of HRDAG data v2 ----

# We can use HRDAG data on homicides, disappearances (forced or not), sequesters, and recruitment (1985-2018), which includes documented, imputed, and estimated numbers of victims of Colombia's armed conflict. These data could be considered "thick" indicators for (#11) illicit activity, and perhaps extend into our conceptualizations of (#10) civil unrest.
# More information on the 'verdata' package here: https://github.com/HRDAG/verdata/blob/main/inst/docs/README-en.md

## Load data ----
# Setup and load 'verdata' library
#setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG")
pacman::p_load(ggplot2, dplyr, rmarkdown, verdata, LCMCR, here,
               arrow, dplyr, rlang, purrr, glue, tidyr, stringr, 
               gridExtra)
options(warn = -1)

library(verdata)

### Authenticate data ----
# "The function confirm_files authenticates the files that you have downloaded. Seeing as each violation type has 100 replicate files, this function authenticates the data without needing to read every file into R. This is to save computational resources or avoid unnecessary computation should you not want to use all 100 replicates in your analysis."

# v2 
confirmar_homi <- verdata::confirm_files(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/homicidio-v2.parquet"),
                                         "homicidio", c(1:10), version= "v2")
confirmar_desa <- verdata::confirm_files(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/desaparicion-v2.parquet"),
                                         "desaparicion", c(1:10), version= "v2")
confirmar_recl <- verdata::confirm_files(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/reclutamiento-v2.parquet"),
                                         "reclutamiento", c(1:10), version= "v2")
confirmar_secu <- verdata::confirm_files(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/secuestro-v2.parquet"),
                                         "secuestro", c(1:10), version= "v2")

### Read replicates ----
# "The function read_replicates allows the user to do two things: read the replicate files into R in a single data frame (whether using the .csv or .parquet versions) and verify that their contents are exactly the same as the published data."

replicas_homi <- verdata::read_replicates(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/homicidio-v2.parquet"),
                                          "homicidio", c(1:10), version= "v2")
replicas_desa <- verdata::read_replicates(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/desaparicion-v2.parquet"),
                                          "desaparicion", c(1:10), version= "v2")
replicas_secu <- verdata::read_replicates(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/secuestro-v2.parquet"),
                                          "secuestro", c(1:10), version= "v2")

replicas_recl <- verdata::read_replicates(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/v2/verdata-parquet/reclutamiento-v2.parquet"),
                                          "reclutamiento", c(1:10), version= "v2")


### Test data ----

# If we were to count the duplicate values of individual cases, we would find 10 duplicates each because of the imputation function uses 10 replicate files.
# Count duplicates in match_group_id
duplicate_count <- replicas_homi %>%
  group_by(match_group_id) %>%
  filter(n() > 1) %>%
  summarize(duplicate_count = n())
# View the result
print(duplicate_count)

# We can also count unique municipalities per year
unique_muni_per_year <- replicas_homi %>%
  group_by(yy_hecho) %>%
  summarize(unique_muni = n_distinct(muni_code_hecho))
# View the result
print(unique_muni_per_year)


## Extract Documented Data (1985-2022) ----
# In order to use this data for snvdem, we will need to aggregate 'documented' victims at the municipal level, by year. (We can later explore 'estimated' victims.)

# a) visualize the variables, many of which are "in_*" which signify whether victims were identified with this source or not.
names(replicas_homi)

# b) Aggregate the data by type, year, and municipality

### Homicidios (documented) ----
td_homi_ym <- verdata::summary_observed("homicidio",
                                        replicas_homi, 
                                        strata_vars = c("yy_hecho", "muni_code_hecho"),
                                        conflict_filter = FALSE, # we are interested in all victims, not just those before 2016 when the "conflict" ended
                                        forced_dis_filter = FALSE,
                                        edad_minors_filter = FALSE,
                                        include_props = TRUE)
# As in other data conversions, we need to correct the "municipal code" values with only 4 digits:
td_homi_ym$muni_code_hecho <- ifelse(nchar(td_homi_ym$muni_code_hecho) == 4, 
                                     paste0("0", td_homi_ym$muni_code_hecho), 
                                     td_homi_ym$muni_code_hecho)
td_homi_ym <- td_homi_ym %>%
  mutate(yy_hecho = as.numeric(yy_hecho)) %>% 
  rename(year = 1, MPIO_CDPMP = 2, obs_homi = 3, homi_prop_na = 4, homi_prop = 5) %>%
  arrange(desc(obs_homi))
#colSums(is.na(td_homi_ym))

### Desapariciones (documented) ----
# "the data about displacement is concentrated in the RUV. Since MSE is based on overlaps (and non-overlaps) between lists, the estimates are affected by the high importance of the RUV. So, we recommend working with the replicate files created through statistical imputation to study this violence. That is, it was not possible to calculate the underreporting or use MSE." (JEP 2025, 52)
td_desa_ym <- verdata::summary_observed("desaparicion",
                                        replicas_desa, 
                                        strata_vars = c("yy_hecho", "muni_code_hecho"),
                                        conflict_filter = FALSE,
                                        forced_dis_filter = FALSE,
                                        edad_minors_filter = FALSE,
                                        include_props = TRUE)
td_desa_ym$muni_code_hecho <- ifelse(nchar(td_desa_ym$muni_code_hecho) == 4, 
                                     paste0("0", td_desa_ym$muni_code_hecho), 
                                     td_desa_ym$muni_code_hecho)
td_desa_ym <- td_desa_ym %>%
  mutate(yy_hecho = as.numeric(yy_hecho)) %>% 
  rename(year = 1, MPIO_CDPMP = 2, obs_desa = 3, desa_prop_na = 4, desa_prop = 5) %>%
  arrange(desc(obs_desa))
colSums(is.na(td_homi_ym))

### Secuestros (documented) ----
td_secu_ym <- verdata::summary_observed("secuestro",
                                        replicas_secu, 
                                        strata_vars = c("yy_hecho", "muni_code_hecho"),
                                        conflict_filter = FALSE, 
                                        forced_dis_filter = FALSE,
                                        edad_minors_filter = FALSE,
                                        include_props = TRUE)
td_secu_ym$muni_code_hecho <- ifelse(nchar(td_secu_ym$muni_code_hecho) == 4, 
                                     paste0("0", td_secu_ym$muni_code_hecho), 
                                     td_secu_ym$muni_code_hecho)
td_secu_ym <- td_secu_ym %>%
  mutate(yy_hecho = as.numeric(yy_hecho)) %>% 
  rename(year = 1, MPIO_CDPMP = 2, obs_secu = 3, secu_prop_na = 4, secu_prop = 5) %>%
  arrange(desc(obs_secu))


### Reclutamiento (documented) ----
td_recl_ym <- verdata::summary_observed("reclutamiento",
                                        replicas_recl, 
                                        strata_vars = c("yy_hecho", "muni_code_hecho"),
                                        conflict_filter = FALSE, 
                                        forced_dis_filter = FALSE,
                                        edad_minors_filter = FALSE,
                                        include_props = TRUE)
td_recl_ym$muni_code_hecho <- ifelse(nchar(td_recl_ym$muni_code_hecho) == 4, 
                                     paste0("0", td_recl_ym$muni_code_hecho), 
                                     td_recl_ym$muni_code_hecho)
td_recl_ym <- td_recl_ym %>%
  mutate(yy_hecho = as.numeric(yy_hecho)) %>% 
  rename(year = 1, MPIO_CDPMP = 2, obs_recl = 3, recl_prop_na = 4, recl_prop = 5) %>%
  arrange(desc(obs_recl))



## Save as csv ----
write.csv(td_homi_ym, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/4vars/td_homi_ym.csv", row.names = FALSE)
write.csv(td_desa_ym, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/4vars/td_desa_ym.csv", row.names = FALSE)
write.csv(td_secu_ym, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/4vars/td_secu_ym.csv", row.names = FALSE)
write.csv(td_recl_ym, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/4vars/td_recl_ym.csv", row.names = FALSE)

## Combine statistics (by mun. and year) ----
td_HRDAG_ym <- td_homi_ym %>%
  full_join(td_desa_ym, by = c("MPIO_CDPMP", "year")) %>%
  full_join(td_secu_ym, by = c("MPIO_CDPMP", "year")) %>%
  full_join(td_recl_ym, by = c("MPIO_CDPMP", "year"))
# summary(td_HRDAG_ym)



### Graph summary statistics ----
# Summing the obs_* variables by year across all municipalities
td_summed <- td_HRDAG_ym %>%
  group_by(year) %>%
  summarise(
    obs_homi = sum(obs_homi, na.rm = TRUE),
    obs_desa = sum(obs_desa, na.rm = TRUE),
    obs_secu = sum(obs_secu, na.rm = TRUE),
    obs_recl = sum(obs_recl, na.rm = TRUE)
  )

# Transform the data to long format for ggplot2
td_long <- td_summed %>%
  pivot_longer(cols = c(obs_homi, obs_desa, obs_secu, obs_recl), 
               names_to = "variable", 
               values_to = "value")

# Plot the data with lines
ggplot(td_long, aes(x = year, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Summed Observed Variables Over Time",
       x = "Year",
       y = "Total Value",
       color = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Calculate CDF data ----
eCDF_10t11a <- ecdf(td_HRDAG_ym$obs_homi)
td_HRDAG_ym$HHomi_10t11c <- eCDF_10t11a(td_HRDAG_ym$obs_homi)
eCDF_10t11b <- ecdf(td_HRDAG_ym$obs_desa)
td_HRDAG_ym$HDesa_10t11c <- eCDF_10t11b(td_HRDAG_ym$obs_desa)
eCDF_10t11c <- ecdf(td_HRDAG_ym$obs_secu)
td_HRDAG_ym$HSecu_10t11c <- eCDF_10t11c(td_HRDAG_ym$obs_secu)
eCDF_10t11d <- ecdf(td_HRDAG_ym$obs_recl)
td_HRDAG_ym$HRecl_10t11c <- eCDF_10t11d(td_HRDAG_ym$obs_recl)

## Save as csv ----
write.csv(td_HRDAG_ym, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/td_HRDAG_ym.csv", row.names = FALSE)



#----Extract Estimated Data (1985-2022)----

# needs work...
listas <- readRDS(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/verdata-examples/Resultados-CEV/Estimacion/output-estimacion/yy_hecho-is_conflict-perpetrador-homicidio.rds"))

td_secu_ym2 <- verdata::combine_replicates("secuestro",
                                           td_secu_ym,
                                           replicas_secu, 
                                           strata_vars = c("yy_hecho", "muni_code_hecho"),
                                           conflict_filter = FALSE,
                                           forced_dis_filter = FALSE,
                                           edad_minors_filter = FALSE,
                                           include_props = TRUE)
# The idea is to try to include imputed data for sequesters so as to reduce the NAs. But it's not working for me.
tabla_documentada <- arrow::read_parquet(here::here("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/10-11_HRDAG/verdata-examples/Resultados-CEV/Documentados/output-documentados/secuestro-is_conflict-documentado.parquet"))

tabla_combinada <- verdata::combine_replicates("secuestro", 
                                               tabla_documentada,
                                               replicas_secu, 
                                               strata_vars = "is_conflict", 
                                               conflict_filter = FALSE,
                                               forced_dis_filter = FALSE, 
                                               edad_minors_filter = FALSE,
                                               include_props = FALSE)
paged_table(tabla_combinada, options = list(rows.print = 10, cols.print = 5))

# testing...
replicas_secu <- replicas_secu %>%
  mutate(yy_hecho = as.numeric(yy_hecho)) %>% 
  rename(year = 52, MPIO_CDPMP = 46)
paged_table(replicas_secu, options = list(rows.print = 10, cols.print = 5))

replicas_secu$MPIO_CDPMP <- ifelse(nchar(replicas_secu$MPIO_CDPMP) == 4, 
                                   paste0("0", replicas_secu$MPIO_CDPMP), 
                                   replicas_secu$MPIO_CDPMP)

