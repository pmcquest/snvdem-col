# Data wrangle for elections (#15-16 Support for ruling party)

#---- Presidential votes (1958-2022) ----
# Within the framework of this municipal-level V-Dem analysis, the criteria "Strong / Weak support for ruling party" can be operationalized using electoral data, specifically by using the 'margin of victory' (MoV) variable for winner and runner-up candidates in the run-off election (second round). Voting data can be found online on Registraduria (RNEC) website. Data for presidential elections goes back to 1958, but municipal boundaries have shifted since then.

# NOTE on methodology, verified/discussed 2026-07-05: `vtotal` (votototal) is
# the sum of ALL votes cast in a municipality -- every candidate plus
# no-mark/null/blank -- not just the top-2 candidates' votes. This is
# deliberate, not an oversight: the variable is meant to capture "support for
# the [eventual national] ruling party in the municipality" as a share of the
# whole electorate, not the winner's margin relative to the runner-up alone.
# A winner who takes 52% against a fragmented first-round field genuinely has
# less municipal support than one who takes 67% in a clean two-candidate
# race, even if their lead over the specific runner-up looks similar -- so
# diluting by minor-candidate votes is the intended behavior.
# For the five run-off years (1998, 2010, 2014, 2018, 2022) this makes
# little practical difference since a run-off ballot only has the top-2
# candidates plus invalid-vote categories (confirmed for 2010: codigo_lista
# 1/2 are ~97% of vtotal, the remainder is 999/998/997). But 2002 and 2006
# were decided outright in the FIRST round (no run-off existed that year), so
# this script substitutes first-round data for those two years, where more
# candidates on the ballot means a larger, more fragmented electorate in the
# denominator -- e.g. RulPar_15t16 comes out 0.6044 for 2002 and 0.6987 for
# 2006 using vtotal, vs. 0.6252/0.739 if computed as Uribe/(Uribe+Serpa)
# instead. Kept as vtotal-based deliberately (per the above), so these two
# years are measuring "how dominant was the winner across the full
# electorate" on the same basis as every other year, not "how big was the
# winner's lead over the runner-up specifically."

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)


##---- 2022: Petro (ColHumana) > Hernandez (LIGA) ----
rp22 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/MOE_resultados2022.csv") # this long dataset contains only 2 candidates because it was a second-round election. other datasets will include more rows depending on the number of candidates running. Because RNEC municipal codes are off for this year (see Antioquia, among others--https://observatorio.registraduria.gov.co/historico-resultados.html), I retrieved data from MOE: https://www.datoselectorales.org/datos/resultados-electorales.


# Codigo municipio should have 5 digits
rp22$MPIO_CDPMP <- as.character(rp22$codmpio)
rp22$MPIO_CDPMP <- ifelse(nchar(rp22$MPIO_CDPMP) == 4, sprintf("0%s", rp22$MPIO_CDPMP), rp22$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp22 <- rp22 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
p22d0 <- rp22 %>%
  group_by(MPIO_CDPMP, codparti) %>%
  pivot_wider(names_from = codparti, values_from = votos) %>% # Pivot to wide format
  rename(Petro = `1235`, # the "codigo" will change in other election years
         Hernandez = `1076`,
         nomark = `998`,
         null = `997`,
         blank = `996`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p22d2 <- p22d0 %>%
  select(MPIO_CDPMP, Petro) %>%
  filter(!is.na(Petro))
p22d2 <- p22d2[!duplicated(p22d2$MPIO_CDPMP), ]
p22d3 <- p22d0 %>%
  select(MPIO_CDPMP, Hernandez) %>%
  filter(!is.na(Hernandez))
p22d3 <- p22d3[!duplicated(p22d3$MPIO_CDPMP), ]
p22d4 <- p22d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p22d4 <- p22d4[!duplicated(p22d4$MPIO_CDPMP), ]
p22d5 <- p22d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p22d5 <- p22d5[!duplicated(p22d5$MPIO_CDPMP), ]
p22d6 <- p22d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p22d6 <- p22d6[!duplicated(p22d6$MPIO_CDPMP), ]
p22d7 <- p22d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p22d7 <- p22d7[!duplicated(p22d7$MPIO_CDPMP), ]

RP22s <- list(p22d2, p22d3, p22d4, p22d5, p22d6, p22d7)
merged_RP22 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP22s)

df2rm00 = c("rp22", "p22d0", "p22d2", "p22d3", "p22d4", "p22d5", "p22d6", "p22d7", "df2rm00")
rm(list = df2rm00)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP22 <- merged_RP22 %>%
  mutate(MOV_top2 = Petro-Hernandez) %>% #Raw vote-count margin of victory
  mutate(Petro_pct = Petro/vtotal) %>%
  mutate(Hernandez_pct = Hernandez/vtotal) %>%
  mutate(MOV_pct = Petro_pct-Hernandez_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP22$Petro)/sum(RP22$vtotal))*100, 2) # 49.77% for Petro
round((sum(RP22$Hernandez)/sum(RP22$vtotal))*100, 2) # 46.74% for Hernandez
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2022): while the percentages are slightly different, they are close to 1%. However, vote totals (sums) are almost the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated.

RP22_15t16 <- RP22 %>%
  select(1|12) %>%
  mutate(year = 2022)

df2rm01 = c("merged_RP22", "RP22", "RP22s", "df2rm01")
rm(list = df2rm01)
n_distinct(RP22_15t16$MPIO_CDPMP)

##---- 2018: Duque (CD) > Petro (ColHumana) ----
rp18 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/2018_presidencia_segunda_vuelta_dta_c27d4515ed.csv") # this long dataset contains only 2 candidates because it was a second-round election. other datasets will include more rows depending on the number of candidates running.

# Codigo municipio should have 5 digits
rp18$MPIO_CDPMP <- as.character(rp18$codmpio)
rp18$MPIO_CDPMP <- ifelse(nchar(rp18$MPIO_CDPMP) == 4, sprintf("0%s", rp18$MPIO_CDPMP), rp18$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp18 <- rp18 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
p18d0 <- rp18 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Petro = `1`, # the "codigo lista" will change in other election years
         Duque = `2`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p18d2 <- p18d0 %>%
  select(MPIO_CDPMP, Petro) %>%
  filter(!is.na(Petro))
p18d2 <- p18d2[!duplicated(p18d2$MPIO_CDPMP), ]
p18d3 <- p18d0 %>%
  select(MPIO_CDPMP, Duque) %>%
  filter(!is.na(Duque))
p18d3 <- p18d3[!duplicated(p18d3$MPIO_CDPMP), ]
p18d4 <- p18d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p18d4 <- p18d4[!duplicated(p18d4$MPIO_CDPMP), ]
p18d5 <- p18d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p18d5 <- p18d5[!duplicated(p18d5$MPIO_CDPMP), ]
p18d6 <- p18d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p18d6 <- p18d6[!duplicated(p18d6$MPIO_CDPMP), ]
p18d7 <- p18d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p18d7 <- p18d7[!duplicated(p18d7$MPIO_CDPMP), ]

RP18s <- list(p18d2, p18d3, p18d4, p18d5, p18d6, p18d7)
merged_RP18 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP18s)

df2rm2 = c("rp18", "p18d0", "p18d2", "p18d3", "p18d4", "p18d5", "p18d6", "p18d7", "df2rm2")
rm(list = df2rm2)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP18 <- merged_RP18 %>%
  mutate(MOV_top2 = Duque-Petro) %>% #Raw vote-count margin of victory
  mutate(Duque_pct = Duque/vtotal) %>%
  mutate(Petro_pct = Petro/vtotal) %>%
  mutate(MOV_pct = Duque_pct-Petro_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP18$Duque)/sum(RP18$vtotal))*100, 2) # 53.23% for Duque
round((sum(RP18$Petro)/sum(RP18$vtotal))*100, 2) # 41.16% for Petro
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2018): while the percentages are slightly different, they are <1%. However, vote totals (sums) are exactly the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated.

round(mean(RP18$MOV_pct)*100, 2) #Duque municipal-level margin is 25.39% on average (quite high).
round(sd(RP18$MOV_pct), 3) #SD for the margin is larger than the mean (39.8%) suggesting abnormal distribution
# hist(RP18$MOV_pct, main = "Histogram of MOV", xlab = "MOV", col = "skyblue", border = "black") #left-tail long (margin for Petro)
RP18_15t16 <- RP18 %>%
  select(1|12) %>%
  mutate(year = 2018)

df2rm3 = c("merged_RP18", "RP18", "RP18s", "df2rm3")
rm(list = df2rm3)


##---- 2014: Santos (PdeU) > Zuluaga (CD) ----
rp14 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/2014_presidencia_segunda_vuelta_dta_6e5f969601.csv")

# Codigo municipio should have 5 digits
rp14$MPIO_CDPMP <- as.character(rp14$codmpio)
rp14$MPIO_CDPMP <- ifelse(nchar(rp14$MPIO_CDPMP) == 4, sprintf("0%s", rp14$MPIO_CDPMP), rp14$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp14 <- rp14 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
#Note: check to see who is labeled what number before pivoting from codigo_lista
p14d0 <- rp14 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Santos = `1`, 
         Zuluaga = `2`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p14d2 <- p14d0 %>%
  select(MPIO_CDPMP, Santos) %>%
  filter(!is.na(Santos))
p14d2 <- p14d2[!duplicated(p14d2$MPIO_CDPMP), ]
p14d3 <- p14d0 %>%
  select(MPIO_CDPMP, Zuluaga) %>%
  filter(!is.na(Zuluaga))
p14d3 <- p14d3[!duplicated(p14d3$MPIO_CDPMP), ]
p14d4 <- p14d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p14d4 <- p14d4[!duplicated(p14d4$MPIO_CDPMP), ]
p14d5 <- p14d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p14d5 <- p14d5[!duplicated(p14d5$MPIO_CDPMP), ]
p14d6 <- p14d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p14d6 <- p14d6[!duplicated(p14d6$MPIO_CDPMP), ]
p14d7 <- p14d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p14d7 <- p14d7[!duplicated(p14d7$MPIO_CDPMP), ]

RP14s <- list(p14d2, p14d3, p14d4, p14d5, p14d6, p14d7)
merged_RP14 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP14s)

df2rm4 = c("rp14", "p14d0", "p14d2", "p14d3", "p14d4", "p14d5", "p14d6", "p14d7", "df2rm4")
rm(list = df2rm4)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP14 <- merged_RP14 %>%
  mutate(MOV_top2 = Santos-Zuluaga) %>% #Raw vote-count margin of victory
  mutate(Santos_pct = Santos/vtotal) %>%
  mutate(Zuluaga_pct = Zuluaga/vtotal) %>%
  mutate(MOV_pct = Santos_pct-Zuluaga_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP14$Santos)/sum(RP14$vtotal))*100, 2) # 49.56% for Santos (50.99% on Wiki)
round((sum(RP14$Zuluaga)/sum(RP14$vtotal))*100, 2) # 43.73% for Zuluaga (45.00% on Wiki)
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2014): while the percentages are slightly different, they are close to 1% different. However, vote totals (sums) are exactly the same.

RP14_15t16 <- RP14 %>%
  select(1|12) %>%
  mutate(year = 2014)

df2rm5 = c("merged_RP14", "RP14", "RP14s", "df2rm5")
rm(list = df2rm5)

##---- 2010: Santos (PSUN) > Mockus (PV) ----
rp10 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/2010_presidencia_segunda_vuelta_dta_256a2f2747.csv")

# Codigo municipio should have 5 digits
rp10$MPIO_CDPMP <- as.character(rp10$codmpio)
rp10$MPIO_CDPMP <- ifelse(nchar(rp10$MPIO_CDPMP) == 4, sprintf("0%s", rp10$MPIO_CDPMP), rp10$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp10 <- rp10 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
#Note: check to see who is labeled what number before pivoting from codigo_lista
p10d0 <- rp10 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Mockus = `1`, 
         Santos = `2`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p10d2 <- p10d0 %>%
  select(MPIO_CDPMP, Santos) %>%
  filter(!is.na(Santos))
p10d2 <- p10d2[!duplicated(p10d2$MPIO_CDPMP), ]
p10d3 <- p10d0 %>%
  select(MPIO_CDPMP, Mockus) %>%
  filter(!is.na(Mockus))
p10d3 <- p10d3[!duplicated(p10d3$MPIO_CDPMP), ]
p10d4 <- p10d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p10d4 <- p10d4[!duplicated(p10d4$MPIO_CDPMP), ]
p10d5 <- p10d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p10d5 <- p10d5[!duplicated(p10d5$MPIO_CDPMP), ]
p10d6 <- p10d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p10d6 <- p10d6[!duplicated(p10d6$MPIO_CDPMP), ]
p10d7 <- p10d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p10d7 <- p10d7[!duplicated(p10d7$MPIO_CDPMP), ]

RP10s <- list(p10d2, p10d3, p10d4, p10d5, p10d6, p10d7)
merged_RP10 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP10s)

df2rm6 = c("rp10", "p10d0", "p10d2", "p10d3", "p10d4", "p10d5", "p10d6", "p10d7", "df2rm6")
rm(list = df2rm6)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP10 <- merged_RP10 %>%
  mutate(MOV_top2 = Santos-Mockus) %>% #Raw vote-count margin of victory
  mutate(Santos_pct = Santos/vtotal) %>%
  mutate(Mockus_pct = Mockus/vtotal) %>%
  mutate(MOV_pct = Santos_pct-Mockus_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP10$Santos, na.rm = TRUE)/sum(RP10$vtotal, na.rm = TRUE))*100, 2)
# 67.9% for Santos
round((sum(RP10$Mockus, na.rm = TRUE)/sum(RP10$vtotal, na.rm = TRUE))*100, 2) # 26.98% for Mockus
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2010): while the percentages are slightly different, they are around 1%. However, vote totals (sums) are exactly the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated by Wikipedia.


RP10_15t16 <- RP10 %>%
  select(1|12) %>%
  mutate(year = 2010)
df2rm7 = c("merged_RP10", "RP10", "RP10s", "df2rm7")
rm(list = df2rm7)

##---- 2006: Uribe (Primero Col.) > Gaviria (Polo Democratico) ----
# Decided outright in the first round (no run-off held) -- see the
# denominator-consistency note at the top of this script for what that means
# for MOV_pct/RulPar_15t16 here relative to the run-off years.
rp06 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/2006_presidencia_dta_9eb2e9319c.csv")

# Codigo municipio should have 5 digits
rp06$MPIO_CDPMP <- as.character(rp06$codmpio)
rp06$MPIO_CDPMP <- ifelse(nchar(rp06$MPIO_CDPMP) == 4, sprintf("0%s", rp06$MPIO_CDPMP), rp06$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp06 <- rp06 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
#Note: check to see who is labeled what number before pivoting from codigo_lista
p06d0 <- rp06 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Uribe = `3`, 
         Gaviria = `4`,
         Serpa = `5`,
         Mockus = `7`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p06d2 <- p06d0 %>%
  select(MPIO_CDPMP, Uribe) %>%
  filter(!is.na(Uribe))
p06d2 <- p06d2[!duplicated(p06d2$MPIO_CDPMP), ]
p06d3 <- p06d0 %>%
  select(MPIO_CDPMP, Gaviria) %>%
  filter(!is.na(Gaviria))
p06d3 <- p06d3[!duplicated(p06d3$MPIO_CDPMP), ]
p06d4 <- p06d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p06d4 <- p06d4[!duplicated(p06d4$MPIO_CDPMP), ]
p06d5 <- p06d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p06d5 <- p06d5[!duplicated(p06d5$MPIO_CDPMP), ]
p06d6 <- p06d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p06d6 <- p06d6[!duplicated(p06d6$MPIO_CDPMP), ]
p06d7 <- p06d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p06d7 <- p06d7[!duplicated(p06d7$MPIO_CDPMP), ]

RP06s <- list(p06d2, p06d3, p06d4, p06d5, p06d6, p06d7)
merged_RP06 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP06s)

df2rm8 = c("rp06", "p06d0", "p06d2", "p06d3", "p06d4", "p06d5", "p06d6", "p06d7", "df2rm8")
rm(list = df2rm8)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP06 <- merged_RP06 %>%
  mutate(MOV_top2 = Uribe-Gaviria) %>% #Raw vote-count margin of victory
  mutate(Uribe_pct = Uribe/vtotal) %>%
  mutate(Gaviria_pct = Gaviria/vtotal) %>%
  mutate(MOV_pct = Uribe_pct-Gaviria_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP06$Uribe, na.rm = TRUE)/sum(RP06$vtotal, na.rm = TRUE))*100, 2)
# 61.43% for Uribe
round((sum(RP06$Gaviria, na.rm = TRUE)/sum(RP06$vtotal, na.rm = TRUE))*100, 2) # 21.7% for Gaviria
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2006): while the percentages are slightly different, they are around 1%. However, vote totals (sums) are exactly the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated by Wikipedia.

RP06_15t16 <- RP06 %>%
  select(1|12) %>%
  mutate(year = 2006)
df2rm9 = c("merged_RP06", "RP06", "RP06s", "df2rm9")
rm(list = df2rm9)

##---- 2002: Uribe (Primero Col.) > Serpa (Liberal) ----
# Also decided outright in the first round -- see the denominator-consistency
# note at the top of this script.
#
# 2002 is also the year with the most municipality-level missingness of any
# run-off/first-round year in this dataset (verified 2026-07-05): comparing
# against MunYrs's 1122 municipalities, 7 are entirely absent from the raw
# 2002 file (05475, 13490, 19300, 23682, 23815, 27025, 27425), and another 14
# municipalities that ARE present (15550 Pisba, 19701 Santa Rosa, 25580 Puli,
# 50245 El Calvario, 50686 San Juanito, 52427 Magui, 85136 La Salina, 85279
# Recetor, 85315 Sacama, 94883 San Felipe, 94885 La Guadalupe, 95015 Calamar,
# 95200 Miraflores, 97161 Caruru) have votos == 0 for every single candidate,
# including Uribe and Serpa -- i.e. no election result was recorded there at
# all that year, plausibly reflecting the armed conflict disrupting voting or
# reporting in these remote/frontier municipalities (several are in
# Guainia/Guaviare/Vaupes and rural Casanare/Boyaca). Since vtotal is also 0
# in those 14 cases, Uribe_pct/Serpa_pct below become 0/0 = NaN, and
# RulPar_15t16 comes out NA (R's is.na(NaN) is TRUE) -- correctly, if
# incidentally, since there's no real election result to measure. By
# comparison only 1 municipality shows this pattern in 2006 and 2010 each, and
# 0 from 2014 onward -- consistent with 2002 being a peak-conflict year before
# later security-policy changes. This is genuine historical missingness, not
# a bug in this script.
rp02 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/2002_presidencia_dta_c5a0392d8f.csv")

# Codigo municipio should have 5 digits
rp02$MPIO_CDPMP <- as.character(rp02$codmpio)
rp02$MPIO_CDPMP <- ifelse(nchar(rp02$MPIO_CDPMP) == 4, sprintf("0%s", rp02$MPIO_CDPMP), rp02$MPIO_CDPMP)
# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp02 <- rp02 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
#Note: check to see who is labeled what number before pivoting from codigo_lista
p02d0 <- rp02 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Uribe = `4`, 
         Serpa = `8`,
         Garzon = `1`,
         nomark = `997`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p02d2 <- p02d0 %>%
  select(MPIO_CDPMP, Uribe) %>%
  filter(!is.na(Uribe))
p02d2 <- p02d2[!duplicated(p02d2$MPIO_CDPMP), ]
p02d3 <- p02d0 %>%
  select(MPIO_CDPMP, Serpa) %>%
  filter(!is.na(Serpa))
p02d3 <- p02d3[!duplicated(p02d3$MPIO_CDPMP), ]
p02d4 <- p02d0 %>%
  select(MPIO_CDPMP, nomark) %>%
  filter(!is.na(nomark))
p02d4 <- p02d4[!duplicated(p02d4$MPIO_CDPMP), ]
p02d5 <- p02d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p02d5 <- p02d5[!duplicated(p02d5$MPIO_CDPMP), ]
p02d6 <- p02d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p02d6 <- p02d6[!duplicated(p02d6$MPIO_CDPMP), ]
p02d7 <- p02d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p02d7 <- p02d7[!duplicated(p02d7$MPIO_CDPMP), ]

RP02s <- list(p02d2, p02d3, p02d4, p02d5, p02d6, p02d7)
merged_RP02 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP02s)

df2rm9 = c("rp02", "p02d0", "p02d2", "p02d3", "p02d4", "p02d5", "p02d6", "p02d7", "df2rm9")
rm(list = df2rm9)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP02 <- merged_RP02 %>%
  mutate(MOV_top2 = Uribe-Serpa) %>% #Raw vote-count margin of victory
  mutate(Uribe_pct = Uribe/vtotal) %>%
  mutate(Serpa_pct = Serpa/vtotal) %>%
  mutate(MOV_pct = Uribe_pct-Serpa_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP02$Uribe, na.rm = TRUE)/sum(RP02$vtotal, na.rm = TRUE))*100, 2)
# 52.11% for Uribe
round((sum(RP02$Serpa, na.rm = TRUE)/sum(RP02$vtotal, na.rm = TRUE))*100, 2) # 31.24% for Serpa
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_2002): while the percentages are slightly different, they are around 1%. However, vote totals (sums) are exactly the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated by Wikipedia.

RP02_15t16 <- RP02 %>%
  select(1|12) %>%
  mutate(year = 2002)
df2rm0 = c("merged_RP02", "RP02", "RP02s", "df2rm0")
rm(list = df2rm0)


##---- 1998: Pastrana (Conservador) > Serpa (Liberal) ----
rp98 <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/1998_presidencia_segunda_vuelta_dta_ff6ed0c5cc.csv")

# Codigo municipio should have 5 digits
rp98$MPIO_CDPMP <- as.character(rp98$codmpio)
n_distinct(rp98$MPIO_CDPMP) #1156 codes: includes consulates (depto = 9)
rp98 <- rp98 %>% 
  filter(coddpto != 9)
rp98$MPIO_CDPMP <- ifelse(nchar(rp98$MPIO_CDPMP) == 4, sprintf("0%s", rp98$MPIO_CDPMP), rp98$MPIO_CDPMP)
n_distinct(rp98$MPIO_CDPMP)

# Step 1: Calculate total votes cast at municipal level (allows for percentages later on).
rp98 <- rp98 %>%
  group_by(MPIO_CDPMP) %>%
  mutate(votototal = sum(votos)) %>%
  mutate(vtpc = votos/votototal) %>% #optional: useful for quick look at percentages, SD, etc. 
  mutate(vtpc0 = round(vtpc*100, 2)) 
# For mapping at the municipal level, values may take the form of: percent difference in votes between top winner and runner-up candidate(s). This requires a few steps:
#Step 2: Pivot municipal voting behavior into wide format.
#Note: check to see who is labeled what number before pivoting from codigo_lista
p98d0 <- rp98 %>%
  group_by(MPIO_CDPMP, codigo_lista) %>%
  pivot_wider(names_from = codigo_lista, values_from = votos) %>% # Pivot to wide format
  rename(Pastrana = `1`, 
         Serpa = `2`,
         null = `998`,
         blank = `999`)
## Step 3: Create new dfs for voting behavior at municipal level -- is there a more efficient code?
p98d2 <- p98d0 %>%
  select(MPIO_CDPMP, Pastrana) %>%
  filter(!is.na(Pastrana))
p98d2 <- p98d2[!duplicated(p98d2$MPIO_CDPMP), ]
p98d3 <- p98d0 %>%
  select(MPIO_CDPMP, Serpa) %>%
  filter(!is.na(Serpa))
p98d3 <- p98d3[!duplicated(p98d3$MPIO_CDPMP), ]

p98d5 <- p98d0 %>%
  select(MPIO_CDPMP, null) %>%
  filter(!is.na(null))
p98d5 <- p98d5[!duplicated(p98d5$MPIO_CDPMP), ]
p98d6 <- p98d0 %>%
  select(MPIO_CDPMP, blank) %>%
  filter(!is.na(blank))
p98d6 <- p98d6[!duplicated(p98d6$MPIO_CDPMP), ]
p98d7 <- p98d0 %>%
  group_by(MPIO_CDPMP) %>%
  summarise(vtotal = first(votototal))
p98d7 <- p98d7[!duplicated(p98d7$MPIO_CDPMP), ]

RP98s <- list(p98d2, p98d3, p98d5, p98d6, p98d7)
merged_RP98 <- Reduce(function(x, y) merge(x, y, by = "MPIO_CDPMP", all = TRUE), RP98s)

df2rm9 = c("rp98", "p98d0", "p98d2", "p98d3", "p98d5", "p98d6", "p98d7", "df2rm9")
rm(list = df2rm9)

## Step 3: Create margin of support variable for winner and runner-up candidates. Positive value indicates support for winner; negative indicates opposition. Variable order may vary depending on the election year data from RNEC
RP98 <- merged_RP98 %>%
  mutate(MOV_top2 = Pastrana-Serpa) %>% #Raw vote-count margin of victory
  mutate(Pastrana_pct = Pastrana/vtotal) %>%
  mutate(Serpa_pct = Serpa/vtotal) %>%
  mutate(MOV_pct = Pastrana_pct-Serpa_pct) %>% #Standardized percent margin of victory
  mutate(RulPar_15t16 = (MOV_pct + 1) / 2)

round((sum(RP98$Pastrana, na.rm = TRUE)/sum(RP98$vtotal, na.rm = TRUE))*100, 2)
# 49.89% for Pastrana
round((sum(RP98$Serpa, na.rm = TRUE)/sum(RP98$vtotal, na.rm = TRUE))*100, 2) # 46.17% for Serpa
# Validation with Wikipedia totals (https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Colombia_de_1998): while the percentages are slightly different, they are around 1%. However, vote totals (sums) are exactly the same. The divergence likely has to do with how no mark, null, or blank votes are tabulated by Wikipedia.

RP98_15t16 <- RP98 %>%
  select(1|11) %>%
  mutate(year = 1998)
df2rm0 = c("merged_RP98", "RP98", "RP98s", "df2rm0")
rm(list = df2rm0)

# ---- Compilation ----
Pres98_22_15t16 <- bind_rows(RP98_15t16, RP02_15t16, RP06_15t16, RP10_15t16, RP14_15t16, RP18_15t16, RP22_15t16)
Pres98_22_15t16 <- Pres98_22_15t16 %>%
  filter(MPIO_CDPMP != "000NA", !str_starts(MPIO_CDPMP, "09")) %>%
  group_by(year) %>% # Group by year to perform the operation within each year
  mutate(RulParD_15t16 = ifelse(RulPar_15t16 > 0.66, 1, 0)) 
n_distinct(Pres98_22_15t16$MPIO_CDPMP)



write_rds(Pres98_22_15t16, "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/p9822_15t16.rds")
write.csv(Pres98_22_15t16, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/01_empirical_data/01_source_files/source_files/15-16_RulingParty/Presidencia/p9822_15t16.csv", row.names = FALSE)
