
#---- CEDE Violencia and HRDAG ----

##---- Criteria measured ----
# 10: Areas of civil unrest (including areas where insurgent groups are active). (0=No, 1=Yes) [v2*_10] 
# 11: Areas where illicit activity is widespread. (0=No, 1=Yes) [v2*_11] 


##---- Data Sources ----
# Data 1: CEDE (U de Los Andes) Panel Municipal Violencia (2022) (https://datoscede.uniandes.edu.co/catalogo-de-datos/). 
# Data 2: Human rights Data Analysis Group (HRDAG) collaborated with Colombia's Truth Commission (CEV) to calculate homicides, disappearances (forced or not), sequesters, and recruitment (1985-2018). These data include documented, imputed, and estimated numbers of victims of Colombia's armed conflict. More information on HRDAG's 'verdata' package here: https://github.com/HRDAG/verdata/blob/main/inst/docs/README-en.md.

# Note: Data on civil unrest and illicit activity are tricky. Official data include reports of displacement or confinements to MinDefense, but there may be reporting issues. (CEDE also offers historic dummy variables of presence of La Violencia and Land Conflicts, which are very thin.) HRDAG-CEV in Colombia produced estimates based on reports from hundreds of NGOs.

#---- Script for cleaning ----
setwd("G:/Shared drives/snvdem/snvdem-col")
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

##---- 10 Civil unrest ----

###---- CEDE data (1993-2021) ----
# Displacements (1993-2020), Coca Eradication (1998-2020)

# note: transform Crime by 100k (see penultimate section)
CEDE03 <- read_excel("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/a2-CEDE_PM/2022/Violencia/PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")
CEDE03 <- CEDE03 %>%
  rename(MPIO_CDPMP = 1, year = 2)

# Because the import creates a numeric field for DANE code, we must convert this numeric variable to character and then assure each observation has the corresponding 5 digits
CEDE03$MPIO_CDPMP <- as.character(CEDE03$MPIO_CDPMP)
# Add a 0 before values with 4 digits only
CEDE03$MPIO_CDPMP <- ifelse(nchar(CEDE03$MPIO_CDPMP) == 4, paste0("0", CEDE03$MPIO_CDPMP), CEDE03$MPIO_CDPMP)

# We selected the variables with the most data/least missingness
# Calculated standard deviations and missing value counts
variations <- CEDE03 %>%
  summarise_all(~sd(., na.rm = TRUE)) %>%
  gather(key = "variable", value = "sd")
missing_counts <- data.frame(
  variable = names(CEDE03),
  missing_count = colSums(is.na(CEDE03))
)
# Combine standard deviations and missing value counts to create a table
combined_data <- variations %>%
  left_join(missing_counts, by = "variable")
result <- combined_data %>%
  arrange(missing_count) %>% # Arrange by ascending missing counts
  head(20) %>% # Select the top 20 least missing
  arrange(desc(sd)) # Arrange by descending standard deviation
print(result)
# Among the variables that are most conceptually linked to civil unrest, the least missing (non-static, non-historical) variable with relatively higher SD is displacements (including receiving and expulsing locations). We will use the general variable of displacement as a form of physical "unrest"--being removed from one's home.
# Manual eradication is less conceptually linked to civil unrest, but there are reports of manual eradication being a conflictual process between campesinos and military personnel (e.g., https://www.connectas.org/especiales/claroscuros-de-la-erradicacion-forzada-en-colombia/), which may drive civil unrest. While this variable has high missingness, this is to be expected given the unique socio-political and environmental features of coca cultivation. 
## Threats and Hostilities from ELN or FARC have high missingness (nearly 1/3 of total municipality-years without observations), and conflict is static. 
## We can complement displacement data with Osorio's "violent presence of armed actors (ViPAA)" data set.

###---- ViPAA variables (1977-2019) ----
ViPAA <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_Osorio/ViPAA-Col/Database/VIPAA_Dens1011.rds")
summary(ViPAA)
ViPAA <- ViPAA %>%
  rename(ADens_10 = 3)
# Check time-trends and list of groups. TO what extent are groups switching focus
colSums(is.na(ViPAA))
# Actor_Count | 


# There are other variables in the dataset that are arguably more conceptually linked to "11. Illicit activity": coca growth, robberies, and homicide. 
CEDE03 <- CEDE03[c("MPIO_CDPMP", "year", "d_desplaza", "errad_manual", "H_coca", "hurto", "homicidios")]

CEDE03 <- CEDE03 %>%
  rename(Desp_10 = 3, Errad_10 = 4, HCoca_11 = 5, Hurto_11 = 6, Homic_11 = 7) %>%
  mutate(year = as.numeric(year))
colSums(is.na(CEDE03))
# Errad_10 and HCoca_11 contain high levels of missingness (88% and 99.5% respectively). We may need to remove these. 

##---- 11 Illicit activity ----
###---- CEDE data (1998-2020)
# Coca hectares (1999-2020), Robbery (2003-2020), Homicides (2003-2020)

# Check skewness
library(e1071)
skewness(CEDE03$HCoca_11, na.rm = TRUE)
CEDE03$HCoca_11_log <- log(CEDE03$HCoca_11 + 1)

# Nerge and create new clean df
df04 <- full_join(CEDE03, ViPAA, by = c("MPIO_CDPMP", "year"))


###---- HRDAG variables (1985-2022) ----
# HRDAG data import: homicides, disappearances, sequesters, and recruitment
HRDAG <- read_csv("G:/Shared drives/snvdem/snvdem-col/data/panel/data_raw/10-11_HRDAG/td_HRDAG_ym.csv")
colSums(is.na(HRDAG))
HRDAG <- HRDAG %>%
  select(1:3|6|9|12) %>% 
  rename(HHomi_11 = 3, HDesa_11 = 4, HSecu_11 = 5, HRecl_11 = 6)
colSums(is.na(HRDAG))
# Homicides[2] | 25.6% missingness
# Disappearances | 54% missingness
# Kidnappings | 73% missingness -- may need to be replaced
# Recruitments | 84% missingness -- may need to be replaced


#---- Merge df----
df04 <- full_join(df04, HRDAG, by = c("MPIO_CDPMP", "year"))
colSums(is.na(df04))
# Actor_Count | 



##----- Completeness ----
# Overall = 35%
completeness_summary <- df04 %>%
  mutate(
    non_missing_values = rowSums(!is.na(select(., -MPIO_CDPMP, -year))),  
    total_values = ncol(.) - 2,
    completeness_percentage = (non_missing_values / total_values) * 100
  )
summary(completeness_summary$completeness_percentage)

# Average completeness by year = 34.5%
completeness_by_year <- completeness_summary %>%
  group_by(year) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
summary(completeness_by_year$avg_completeness)

# average completeness by municipality = 35.4%
completeness_by_municipality <- completeness_summary %>%
  group_by(MPIO_CDPMP) %>%
  summarise(
    avg_completeness = mean(completeness_percentage, na.rm = TRUE)
  )
print(completeness_by_municipality)
summary(completeness_by_municipality$avg_completeness)
# Average completeness is around 35%. This may increase once year-span is reduced to 2000-2022, and may be sufficient for imputation techniques.
library(ggplot2)
ggplot(completeness_by_year, aes(x = year, y = avg_completeness)) +
  geom_line(color = "blue", size = 1.2) +
  labs(title = "Data Completeness by Year", x = "Year", y = "Completeness (%)") +
  theme_minimal()

# Check for NA's 
na_counts <- colSums(is.na(df04))
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
cat(paste(names(na_counts_sorted), na_counts_sorted, sep = ": ", collapse = "\n"))


###----Visualization over time----
selected_vars <- df04 %>%
  select(year, matches("(_10|_11)$"))
mean_per_year <- selected_vars %>%
  group_by(year) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
mean_long <- mean_per_year %>%
  pivot_longer(-year, names_to = "variable", values_to = "mean_value")

ggplot(mean_long, aes(x = year, y = mean_value, color = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Means of Civil Unrest and Illicit Activity variables over time",
       x = "Year",
       y = "Mean Value",
       color = "Variable") +
  theme(legend.position = "bottom")



#---- Save cleaned dataset ----
write_rds(df04, "data/panel/data_cleaned/df04_clean.rds")
