# ====================================================================== #
# Rebuild the MapBiomas municipal crosswalk to recover Entidad Territorial
# Indigena (ETI) rows that the name-based join was silently dropping.
# ~20.1M ha (29% of national "Formacion Boscosa") was being lost this way,
# concentrated in Amazonas/Guainia/Vaupes/Putumayo.
#
# Two fixes, in order:
#   1. Prefix-strip ETI labels ("Entidad Territorial Indigena de/del ...",
#      "Territorio Indigena ...") and match the remainder against municipio
#      names in the SAME department -- several ETI labels directly embed a
#      real municipio name (e.g. "...de La Chorrera" -> La Chorrera,
#      Amazonas; "...Miriti Parana" -> Mirit-Parana, Amazonas).
#   2. A manual alias table for ordinary municipios whose MapBiomas label
#      uses a shortened/colloquial name that doesn't match DIVIPOLA's
#      official long form (e.g. "Leguizamo" -> "Puerto Leguizamo").
#
# Genuinely supra-municipal ETI territories with no single corresponding
# municipio (the majority of the 20.1M ha, e.g. "Entidad Territorial
# Indigena Consejo Regional Indigena del Medio Amazonas", "...PANI",
# "...CIMTAR") are NOT apportioned here -- doing that correctly requires
# real ETI polygon geometry intersected against municipio boundaries,
# which we don't have on hand. They remain excluded, but the amount and
# location of what's excluded is now fully quantified below rather than
# silently dropped.
# ====================================================================== #

suppressMessages({ library(readr); library(dplyr); library(sf); library(stringr); library(tidyr) })

out_dir  <- "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/output"
shp_path <- "G:/Shared drives/snvdem/snvdem-col/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp"
clean_mpio <- function(x) str_pad(as.character(as.numeric(x)), width = 5, side = "left", pad = "0")
norm_str <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x); x <- gsub("[^a-z0-9 ]", " ", x); gsub("\\s+", " ", trimws(x))
}

dept_codes <- tibble::tribble(
  ~dpto_norm,                               ~DPTO_CCDGO,
  "amazonas","91","antioquia","05","arauca","81","atlantico","08","bolivar","13",
  "boyaca","15","caldas","17","caqueta","18","casanare","85","cauca","19","cesar","20",
  "choco","27","cordoba","23","cundinamarca","25","guainia","94","guaviare","95",
  "huila","41","la guajira","44","magdalena","47","meta","50","narino","52",
  "norte de santander","54","putumayo","86","quindio","63","risaralda","66",
  "san andres providencia y santa catalina","88",
  "archipielago de san andres providencia y santa catalina","88",
  "santander","68","sucre","70","tolima","73","valle del cauca","76",
  "vaupes","97","vichada","99","bogota d c","11","bogota","11"
)

muni_lookup <- st_read(shp_path, quiet = TRUE) %>% st_drop_geometry() %>%
  mutate(MPIO_CDPMP = clean_mpio(MPIO_CDPMP), DPTO_CCDGO = as.character(DPTO_CCDGO),
         mpio_norm = norm_str(MPIO_CNMBR)) %>%
  select(MPIO_CDPMP, DPTO_CCDGO, mpio_norm, MPIO_CNMBR)

# ---- Manual alias table for ordinary municipios with non-standard MapBiomas labels ----
aliases <- tibble::tribble(
  ~mb_norm,                       ~DPTO_CCDGO, ~alias_target_norm,
  "leguizamo",                    "86",        "puerto leguizamo",
  "lopez",                        "19",        "lopez de micay",
  "el carmen",                    "27",        "el carmen de atrato",
  "el carmen",                    "68",        "el carmen de chucuri",
  "rioviejo",                     "13",        "rio viejo",
  "montanita",                    "18",        "la montanita",
  "atrato yuto",                  "27",        "atrato",
  "becerrill",                    "20",        "becerril",
  "carmen de viboral",            "05",        "el carmen de viboral",
  "buga",                         "76",        "guadalajara de buga",
  "cucuta",                       "54",        "san jose de cucuta",
  "sotara",                       "19",        "sotara paispamba",
  "el tablon",                    "52",        "el tablon de gomez",
  "mariquita",                    "73",        "san sebastian de mariquita",
  "tolu",                         "70",        "santiago de tolu",
  "toluviejo",                    "70",        "san jose de toluviejo",
  "san vicente",                  "05",        "san vicente ferrer",
  "villa de leiva",               "15",        "villa de leyva",
  "ubate",                        "25",        "villa de san diego de ubate",
  "cuaspud",                      "52",        "cuaspud carlosama",
  "tumaco",                       "52",        "san andres de tumaco",
  "el retiro",                    "05",        "retiro",
  "santuario",                    "05",        "el santuario",
  "purisima",                     "23",        "purisima de la concordia"
)
# Note: "Sincé" (Sucre) and "Santa Cruz" (Narino) are deliberately NOT
# aliased -- an earlier substring-matching attempt wrongly matched "Since"
# to "Sincelejo" (a different municipio), so anything we can't verify
# against an exact official-name match is left unmatched rather than guessed.

raw <- read_csv(file.path(out_dir, "mapbiomas_col_muni_stats.csv"), show_col_types = FALSE,
                 locale = locale(encoding = "UTF-8"))
names(raw) <- make.names(names(raw))

# ---- Full panel, all years, Formacion Boscosa ----
mb_long <- raw %>%
  filter(grepl("Formacion Boscosa|Bosque", class_level_1, ignore.case = TRUE)) %>%
  pivot_longer(matches("^X[0-9]{4}$"), names_to = "year", values_to = "area_ha") %>%
  mutate(year = as.integer(gsub("X", "", year)), area_ha = as.numeric(area_ha)) %>%
  filter(year >= 2000, year <= 2023) %>%
  mutate(mpio_norm = norm_str(municipio), dpto_norm = norm_str(departamento)) %>%
  left_join(dept_codes, by = "dpto_norm")

# Direct match (original method)
direct <- mb_long %>% left_join(muni_lookup, by = c("mpio_norm", "DPTO_CCDGO"))
still_unmatched_direct <- direct %>% filter(is.na(MPIO_CDPMP))

cat("Direct match: ", format(sum(direct$area_ha[!is.na(direct$MPIO_CDPMP)], na.rm=TRUE)/24, big.mark=","),
    "ha/yr avg matched |", format(sum(still_unmatched_direct$area_ha, na.rm=TRUE)/24, big.mark=","), "ha/yr avg unmatched\n")

# ---- Fix 1: alias table ----
alias_fixed <- still_unmatched_direct %>%
  select(-MPIO_CDPMP, -MPIO_CNMBR) %>%
  left_join(aliases, by = c("mpio_norm" = "mb_norm", "DPTO_CCDGO" = "DPTO_CCDGO")) %>%
  mutate(match_norm = coalesce(alias_target_norm, mpio_norm)) %>%
  left_join(muni_lookup, by = c("match_norm" = "mpio_norm", "DPTO_CCDGO" = "DPTO_CCDGO"))

n_alias_recovered <- alias_fixed %>% filter(!is.na(MPIO_CDPMP)) %>% distinct(municipio) %>% nrow()
cat("Alias table recovered", n_alias_recovered, "distinct municipio labels\n")

still_unmatched_2 <- alias_fixed %>% filter(is.na(MPIO_CDPMP))

# ---- Fix 2: ETI prefix-strip + substring match against municipios in same department ----
strip_eti <- function(x) {
  x <- gsub("^entidad territorial ind[ií]gena( de| del| de la| de los)?\\s*", "", x, ignore.case = TRUE)
  x <- gsub("^territorio ind[ií]gena( de| del| de la| de los)?\\s*", "", x, ignore.case = TRUE)
  trimws(x)
}

eti_candidates <- still_unmatched_2 %>%
  mutate(eti_stripped = norm_str(strip_eti(municipio))) %>%
  distinct(municipio, DPTO_CCDGO, eti_stripped)

# STRICT exact-match only after stripping ETI boilerplate -- substring
# containment was tried first and produced a false positive ("Since" matched
# "Sincelejo", two different Sucre municipios, because one string happens to
# prefix the other). Exact match after normalization is much safer: it only
# fires when the ETI label, minus its boilerplate prefix, IS the municipio
# name, not merely similar to it.
eti_match <- eti_candidates %>%
  left_join(muni_lookup %>% select(DPTO_CCDGO, mpio_norm, MPIO_CDPMP, MPIO_CNMBR), by = "DPTO_CCDGO",
            relationship = "many-to-many") %>%
  filter(!is.na(mpio_norm), eti_stripped == mpio_norm) %>%
  distinct(municipio, DPTO_CCDGO, .keep_all = TRUE) %>%
  select(municipio, DPTO_CCDGO, matched_MPIO_CDPMP = MPIO_CDPMP, matched_MPIO_CNMBR = MPIO_CNMBR)

cat("\nETI name-embedding matches found:\n")
print(eti_match)

still_unmatched_3 <- still_unmatched_2 %>%
  left_join(eti_match, by = c("municipio", "DPTO_CCDGO")) %>%
  mutate(MPIO_CDPMP = coalesce(MPIO_CDPMP, matched_MPIO_CDPMP))

recovered_eti_ha <- still_unmatched_3 %>% filter(!is.na(MPIO_CDPMP)) %>%
  summarise(ha = sum(area_ha, na.rm=TRUE)/24) %>% pull(ha)
cat("\nETI substring-match recovered ~", format(recovered_eti_ha, big.mark=","), "ha/yr avg\n")

final_unmatched <- still_unmatched_3 %>% filter(is.na(MPIO_CDPMP))
cat("\n=== FINAL remaining unmatched (genuinely supra-municipal, not apportioned) ===\n")
final_unmatched %>% filter(year == 2000) %>% group_by(municipio, departamento) %>%
  summarise(ha = sum(area_ha, na.rm=TRUE), .groups="drop") %>% arrange(desc(ha)) %>%
  print(n = 40)
cat("\nTotal remaining unmatched, year 2000:", format(sum(final_unmatched$area_ha[final_unmatched$year==2000], na.rm=TRUE), big.mark=","), "ha\n")

saveRDS(list(direct = direct, alias_fixed = alias_fixed, eti_match = eti_match,
             final_unmatched = final_unmatched),
        "G:/Shared drives/snvdem/snvdem-col/scripts/deforestation/Analysis-v2/mb_crosswalk_diagnostics.rds")
