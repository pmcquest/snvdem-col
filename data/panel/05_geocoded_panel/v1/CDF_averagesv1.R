#---- Step 2: CDF data averages ----

# Step 1: Wrangle raw data, clean it, then impute missing values (Folders 01-04)
# Step 2 (this script): Calculate averages of Empirical CDF data  (Folder 05)
# Step 3: Subset V-Dem data, calculate criteria averages, apply national range (Folder 06)
# Step 4: Weight Averaged CDF data by V-Dem data (Folder 07-08)
# Step 5: Map geolocated levels of democracy (Folder 09)


## ----Setup ----
# load needed packages
library(tidyverse)
library(corrplot)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


#---- Calculate average scores for criteria ----
# We import the CDF dataset of all variables and select which variables to average for the individual criteria to be weighted
colvars_cdf <- read_rds("G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/imputed_cdf_panel.rds")
colnames(colvars_cdf)
summary(colvars_cdf)

#---- Correlations ----
## A. Compute correlation matrix to make sure there are no perfectly correlated variables
cor_matrix <- cor(colvars_cdf[, c("IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                  "DisBog_4t5", #4-5
                                  "north6", "south7", "west8", "east9", #6-9* -- maybe simplify?
                                  "axis_ns", "axis_we",
                                  "Desp_1011", "VDays_1011", "HHomix_1011", "ViolInd_1011", #10-11
                                  "DenPob_12", #12
                                  "DisMer_13", "nAR_13pkm", "lAR_13pkm", "lMjRds_13", #13 (include per km area)
                                  "PropInd_14", #14: Indigenous selected (highly correlated to Ethnic Population (0.78))
                                  "RulPar_15t16", "RulParD_15t16") #15-16
                              ], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

## B. Compute second correlation with different subset
cor_matrix2 <- cor(colvars_cdf[, c("IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                  "DisBog_4t5", #4-5
                                  #6-9* -- maybe simplify?
                                  "axis_ns", "axis_we",
                                  "ViolInd_1011", #10-11
                                  "DenPob_12", #12
                                  "DisMer_13", "lMjRds_13", #13: least correlated (0.50 with Violence)
                                  "PropInd_14", #14: Indigenous selected (highly correlated to Ethnic Population (0.78))
                                  "RulPar_15t16") #15-16
], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix2, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

## Subset data ----
colvars_cdfw <- colvars_cdf[, c("MPIO_CDPMP", "year", "DPTO_CCDGO", # year and categorical data
                                "IndRur_0t1", "PIB_2t3", "IDF_2t3", #0-1, 2-3
                                "DisBog_4t5", #4-5
                                "north6", "south7", "west8", "east9", #6-9*: originals used to avoid confusion--but high correlations noted...
                                "ViolInd_1011", #10-11: select only the factor score variable
                                "DenPob_12", #12
                                "DisMer_13", "lAR_13pkm", #13: keep roads per Km^2, but note it's correlated to 12 Pop. Sparseness (0.8) and to other indicator of 13 Remoteness (Distance to market, -0.6)
                                "PropInd_14", #14
                                "RulPar_15t16")] #15-16: if poor result, try Dichotomous measure (2/3 voted for ruling party)


#----Ensure correct score directions----
# MC (04/02/25)// To ensure that the index is calculated correctly, we have to make sure that [the municipality-year score (score_it)] for each dimension is calculated so that the high values are associated with greater democracy and the low values are associated with less democracy. In many cases, that is straightforward, because we expect high values of per capita GDP, for example, are associated with more democracy. That is true for urban % of population, too. ... make sure high values go with expected higher democracy and low values go with expected lower democracy.
colnames(colvars_cdfw)

colvars_cdfw <- colvars_cdfw %>%
  # Assumptions for each dimension:
  mutate(Urban_0t1 = 1 - IndRur_0t1) %>% #0 = rural is less democratic; 1 = urban is more democratic)
  # PIB_2t3: # 0 = lower GDP is less democratic; 1 = higher GDP is more democratic
  # IDF_2t3: # 0 = lower IDF score is less democratic, 1 = higher IDF score is more democratic
  mutate(BogProx_4t5 = 1 - DisBog_4t5) %>% # 0 = more distance is less democratic; 1 = less distance is more democratic
  mutate(nonnorth_6 = 1 - north6) %>% # 0 = more north is less democratic; 1 = less north is more democratic
  mutate(nonsouth_7 = 1 - south7) %>% # 0 = more south is less democratic; 1 = less south is more democratic
  mutate(nonwest_8 = 1 - west8) %>% # 0 = more west is less democratic; 1 = less west is more democratic
  mutate(noneast_9 = 1 - east9) %>% # 0 = more east is less democratic; 1 = less east is more democratic
  mutate(nonviolent_10t11 = 1 - ViolInd_1011) %>% # 0 = more violence is less democratic; 1 = less violence is more democratic 
  # DenPob_12: # 0 = lower density is less democratic; 1 = higher density is more democratic
  mutate(MerProx_13 = 1 - DisMer_13) %>% # 0 = more distance to market is less democratic; 1 = less distance to market is more democratic
  # lAR_13pkm: 0 = fewer roads per km is less democratic; 1 = more roads is more democratic
  mutate(NonIndig_14 = 1 - PropInd_14) %>% # 0 = more indigenous population is less democratic; 1 = less indigenous population is more democratic (*Need to review with team)
  mutate(Compete_15t16 = 1 - RulPar_15t16) # 0 = more support margin is less democratic; 1 = less support margin is more democratic

# Remove old variables
colvars_cdfw <- colvars_cdfw %>%
  select(-IndRur_0t1, -DisBog_4t5, -north6, -south7, 
       -west8, -east9, -ViolInd_1011, -DisMer_13, -PropInd_14, -RulPar_15t16) %>%
  rename(lARpkm_13 = "lAR_13pkm")

##----Visualize trends ----
# econ variables are showing steady improvements, but nothing like a spike in 2017
df1 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize( 
    across(
      c("Urban_0t1", "PIB_2t3", "IDF_2t3", "BogProx_4t5"),
      ~mean(.x, na.rm = TRUE)
    )
  )

# crime factor scores as expected going down
df2 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize(
    across("nonviolent_10t11", 
           ~mean(., na.rm = TRUE))) # Criteria 10-11

# ruling party shows a dip in 2017
df3 <- colvars_cdfw %>%
  group_by(year) %>%
  dplyr::summarize(
    across(c("DenPob_12", "lAR_13pkm", "MerProx_13", "NonIndig_14", "Compete_15t16"), 
           ~mean(., na.rm = TRUE))) # Criteria 12-16


df1 <- df1 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df2 <- df2 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")
df3 <- df3 %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")

# Adjust data source as needed:
ggplot(df1, aes(x = year, y = value, color = variable)) + geom_line()
ggplot(df2, aes(x = year, y = value, color = variable)) + geom_line()
ggplot(df3, aes(x = year, y = value, color = variable)) + geom_line()



#---- Calculate average of the CDF'd individual variables---- 
# Select the variables you want to average (columns 4 to 17)
selected_vars <- colvars_cdfw[, 4:17]
ending_digits <- str_extract(names(selected_vars), "\\_.*")
averages_list <- list()

for (digit in unique(ending_digits)) {
  vars_to_avg <- selected_vars[, ending_digits == digit, drop = FALSE] # drop=FALSE to keep as dataframe even if there is only one column.
  avg_name <- paste0("avg", str_remove(digit, "_")) # Create a name for the average variable
  averages_list[[avg_name]] <- rowMeans(vars_to_avg, na.rm = TRUE)
}

averages_df <- bind_cols(averages_list)
averages_df <- cbind(colvars_cdfw[, 1:2], averages_df)

summary(averages_df)

##---- visualize means per variable ----
dfavg <- averages_df %>%
  group_by(year) %>%
  dplyr::summarize(
    across(2:13, ~mean(., na.rm = TRUE)))

dfavg2 <- dfavg %>%
  pivot_longer(-year, names_to = "variable", values_to = "value")

label_data <- dfavg2 %>%
  group_by(variable) %>%
  filter(year == max(year)) %>%
  ungroup()

ggplot(dfavg2, aes(x = year, y = value, group = variable, color = variable)) + 
  geom_line(linewidth = 1) + 
  geom_text(
    data = label_data, 
    aes(label = variable), 
    hjust = -0.1, 
    size = 3.5,
    fontface = "bold"
  ) +
  xlim(min(dfavg2$year), max(dfavg2$year) + 1.5) +
  labs(
    title = "Average Trends Over Time (Criteria 0-16)",
    subtitle = "0 = Associated with Less Democracy; 1 = Associated with More Democracy",
    x = "Year",
    y = "Mean Value (Average across Municipalities)",
    color = "Variable" 
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#---- Write to rds----
write_rds(averages_df, file = "G:/Shared drives/snvdem/snvdem-col/data/panel/05_geocoded_panel/v1/CDF_averages_v1.rds")

