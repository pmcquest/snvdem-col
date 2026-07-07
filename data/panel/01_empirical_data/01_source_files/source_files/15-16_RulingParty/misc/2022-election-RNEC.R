# 2022 Election results in Colombia

# Aggregating and cleaning the data from RNEC
# i have a df (rp22a) with units of analysis at the neighborhood level (Puesto). i want to aggregate the values for the variable "Votos" from the Puesto level to the municipal level (Municipio), but maintaining the categories for the variable "Código Candidato". the new df will have municipal-level summaries of the votes per candidate (Código Candidato).

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel/15-16_RulingParty/Presidencia")

library(dplyr)
library(readxl)
library(readr)

# Step 1: import 2022 data
rp22a <- read_csv("RNEC_Resultados2022.csv")
rp22a <- rp22a %>%
  rename(MPIO_CDPMP2 = `Código Municipio`) %>%
  rename(candidato = `Código Candidato`)

# Step 3: because mun. codes are off, import 2018 data for assigning correct codes
rp18 <- read_csv("2018_presidencia_segunda_vuelta_dta_c27d4515ed.csv")
# Ensure both municipality columns have the same name for joining
rp18 <- rp18 %>%
  rename(Municipio = municipio) %>%
  rename(Departamento = departamento) %>%
  rename(MPIO_CDPMP = codmpio)

# Step 4: standardize mun. names
library(stringi)
rp22a <- rp22a %>%
  mutate(Departamento = tolower(trimws(Departamento)),
         Municipio = tolower(trimws(Municipio)),
         Municipio = stri_trans_general(Municipio, "Latin-ASCII"))
rp18 <- rp18 %>%
  mutate(Departamento = tolower(trimws(Departamento)),
         Municipio = tolower(trimws(Municipio)),
         Municipio = stri_trans_general(Municipio, "Latin-ASCII"))

# Step 5: Aggregate rp22a to the municipal level
rp22a_municipal <- rp22a %>%
  group_by(Municipio, candidato) %>%
  summarise(total_votos = sum(Votos, na.rm = TRUE), .groups = "drop")

# Step 5: identify unmatched mun.
mismatches <- rp22a_municipal %>%
  left_join(rp18 %>% select(Municipio, MPIO_CDPMP), by = "Municipio") %>%
  select(Municipio) %>%
  distinct()

# Step 6: Function to find the best match based on string similarity
find_closest_match <- function(mun, mun_list) {
  distances <- stringdist::stringdist(mun, mun_list, method = "jw")  # Jaro-Winkler distance
  best_match <- mun_list[which.min(distances)]
  return(best_match)
}

# Step 7: Apply fuzzy matching for unmatched municipalities
rp22a_municipal <- rp22a_municipal %>%
  mutate(closest_match = sapply(Municipio, function(m) find_closest_match(m, rp18$Municipio)))

# Step 8: Join mun. codes -- check here...
rp22b_municipal <- rp22a_municipal %>%
  select(candidato, total_votos, closest_match) %>%
  rename(Municipio = closest_match) 
rp22b_municipal <- rp22b_municipal %>%
  left_join(rp18 %>% select(Municipio, MPIO_CDPMP), by = "Municipio")


## check matches
library(stringdist)
# Function to find the best match based on Departamento and Municipio
find_closest_match <- function(dep, mun, rp18_df) {
  # Filter rp18 by the same Departamento
  rp18_subset <- rp18_df %>% filter(Departamento == dep)
  
  if (nrow(rp18_subset) == 0) return(NA)  # If no match in the same Departamento, return NA
  
  # Compute string distances between Municipio values
  distances <- stringdist(mun, rp18_subset$Municipio, method = "jw")
  
  # Select the best match (minimum distance)
  best_match <- rp18_subset$Municipio[which.min(distances)]
  
  return(best_match)
}

# Apply fuzzy matching within each Departamento
rp22a <- rp22a %>%
  mutate(closest_match = mapply(find_closest_match, Departamento, Municipio, MoreArgs = list(rp18_df = rp18)))

# View potential mismatches
head(rp22a %>% select(Departamento, Municipio, closest_match))

# Calculate string distance between original and matched Municipio
rp22a <- rp22a %>%
  mutate(match_quality = stringdist(Municipio, closest_match, method = "jw"))

# Identify weak matches (higher distance = worse match)
suspicious_matches <- rp22a %>%
  filter(match_quality > 0.2 | is.na(closest_match)) %>%  # Adjust threshold as needed
  select(Departamento, Municipio, closest_match, match_quality)

# View mismatches
print(suspicious_matches)

## matches with scores above .255 are incorrect. We must manually correct them
# Filter out observations where match_quality is too high (>= 0.255)
suspicious_obs <- rp22a %>%
  filter(match_quality >= 0.255) %>%
  select(Departamento, Municipio, closest_match, match_quality)
# Unique original Municipio values (incorrect matches)
unique_municipios <- suspicious_obs %>%
  select(Departamento, Municipio) %>%
  distinct()
# Unique incorrect closest matches
unique_closest_matches <- suspicious_obs %>%
  select(Departamento, Municipio, closest_match) %>%
  distinct()
# Print results
print(unique_municipios)
print(unique_closest_matches)

# 
correction_table <- data.frame(
  Municipio_original = c("itagui", "belmira", "norosi", "coloso", "la union", "lerida", "pacoa", "taraira", "austria"),
  Corrected_Municipio = c("santa fe de antioquia", "ciudad bolivar", "santa cruz de mompox", "santiago de tolu", "san jose de toluviejo", "san sebastian de mariquita", "morichal", "pacoa", "hungria")
)
  
# Apply corrections
rp22a <- rp22a %>%
  left_join(correction_table, by = c("Municipio" = "Municipio_original")) %>%
  mutate(closest_match = ifelse(!is.na(Corrected_Municipio), Corrected_Municipio, closest_match)) %>%
  select(-Corrected_Municipio)

remaining_mismatches <- rp22a %>%
  filter(match_quality >= 0.255 | is.na(closest_match)) %>%
  select(Departamento, Municipio, closest_match, match_quality)
print(remaining_mismatches)  # Should be much smaller or empty!





# Step 9: check if any values missing
unmatched <- rp22b_municipal %>%
  filter(is.na(MPIO_CDPMP)) %>%
  select(Municipio) %>%
  distinct()

# Step 10: eliminate duplicates
rp22_municipal <- rp22b_municipal %>%
  select(MPIO_CDPMP, candidato, total_votos) %>%
  distinct()  # Remove duplicate rows
# number of obs is more than 1100 municipalities because this data includes non-domestic mun. codes (voting overseas)


write.csv(rp22_municipal, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/15-16_RulingParty/Presidencia/rp22.csv", row.names = FALSE)

