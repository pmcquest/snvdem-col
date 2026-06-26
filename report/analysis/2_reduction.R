#---- Step 2: Factor analysis of within-country variables ----

# Step 1: create country data df and ecdf country data df (standardized)
# Step 2 (this script): Data reduction (calculate factor scores)
# Step 3: merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem24/report/analysis")

library(tidyverse)
library(knitr)
library(psych)
#install.packages("psychTools")
library(psychTools)
#install.packages("GPArotation")
library(GPArotation)
library(kableExtra)

# Load the data
colvars_raw <- read.csv("1_col18_v2.csv", encoding = "UTF-8")
colvars_cdf <- read.csv("1_col18e_v2.csv", encoding = "UTF-8")
#Codigo municipio should have 5 digits. We do it for the CDF data
colvars_cdf$MPIO_CDPMP <- as.character(colvars_cdf$MPIO_CDPMP)
colvars_cdf$MPIO_CDPMP <- ifelse(nchar(colvars_cdf$MPIO_CDPMP) == 4, sprintf("0%s", colvars_cdf$MPIO_CDPMP), colvars_cdf$MPIO_CDPMP)

#----- Raw variables -----
names(colvars_raw)

# omitting PBID variable for now, and using per capita measures if applicable
fa_raw <- fa(colvars_raw[, c(5:6, 8:13, 37:41, 19, 26:27, 33, 35:36)], nfactors = 4, rotate = "oblimin", 
             scores = "regression", SMC=FALSE, fm="minres")
## Warning: ultra-Heywood case... Try different numbers of factors? Raw number issue?

# One kind of summary of the estimates
print(fa_raw)
# A different kind of summary
fa_raw[["Structure"]]


# Save the predicted values as a df:
preds_raw <- as.data.frame(predict(fa_raw, colvars_raw[, c(5:6, 8:9, 20:32)]))

#----- Standardized variables -----
names(colvars_cdf)
# omitting the cardinal directions variable for now
fa_cdf <- fa(colvars_cdf[, c(5:6, 8:9, 31:35, 27, 19:20, 22:26, 28:30)], 
             nfactors = 5, rotate = "oblimin", 
             scores = "regression", SMC=FALSE, fm="minres")
# Try different numbers of factors....

#print(fa_cdf)
fa_cdf[["Structure"]]


# nice table
# Extract factor loadings
loadings_matrix <- as.data.frame(fa_cdf$Structure[])
new_row_names <- c("Rurality (0-1)", "VAM (2-3)", "IPM (2-3)", "Distance Bog. (4-5)", 
                   "Displaced (10)", "Confinement (10)", "Robberies (11)", "Homicides (11)", 
                   "Eradication (11)", "Pop. Density (12)", "Total Pop. (12)", "Distance Market (13)", 
                   "Indigenous (14)", "Ethnic (14)", "Indig. per cap. (14)", "Ethnic per cap. (14)", 
                   "Rural Party (15-16)")
rownames(loadings_matrix) <- new_row_names
latex_table <- kable(loadings_matrix, caption = "Factor Analysis Loadings", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center")
print(latex_table)
# Convert the table to HTML
html_table <- knit_print(latex_table)
# Create an HTML file and write the HTML content
html_file <- "factor_analysis.html"
writeLines(html_table, html_file)



##---- Create municipal-level dataframes of FA results ----

preds_cdf <- as.data.frame(predict(fa_cdf, colvars_cdf[, c(5:6, 8:9, 33:37, 25, 29, 26:28, 30:32)]))
fa_IDs <- as.data.frame(cbind(colvars_cdf[,c(1:2)], preds_cdf)) %>%
  group_by(depto) %>%
  mutate(MR1mean = mean(MR1, na.rm = TRUE), #col1 in the analysis
         MR2mean = mean(MR2, na.rm = TRUE), #col2 in the analysis
         MR3mean = mean(MR3, na.rm = TRUE), #col4 in the analysis
         MR4mean = mean(MR4, na.rm = TRUE)) #col3 in the analysis
write.csv(fa_IDs, file = "G:/Shared drives/snvdem/snvdem24/report/analysis/2_FAcol18.csv", row.names = FALSE)

## ----- Descriptive plots -----
library(ggridges)
ggplot(fa_IDs, aes(x=MR1, y=reorder(depto, MR1))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR1mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "Factor 1: Indigenous and ethnic, with less support for ruling party",
       title = "Means (dots) and distributions")
ggplot(fa_IDs, aes(x=MR2, y=reorder(depto, MR2))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR2mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "Factor 2: Wealthy, densely populated urban areas",
       title = "Means (dots) and distributions")
ggplot(fa_IDs, aes(x=MR4, y=reorder(depto, MR4))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR4mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "Factor 3: Poor, sparsely populated areas distant from Bogota",
       title = "Means (dots) and distributions")
ggplot(fa_IDs, aes(x=MR3, y=reorder(depto, MR3))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR3mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "Factor 4: Areas with victims of violence and unrest",
       title = "Means (dots) and distributions")





# ----- Merge FA w/ geometries to make maps -----
## Maps
library(sf)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)

col <- st_read("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)

colmap_data <- merge(x = fa_IDs, y = col, by = "MPIO_CDPMP", all.x = TRUE)
colmap_data <- st_as_sf(colmap_data)


F1 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR1)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "Factor score", 
       caption = "F1: Indigenous, less RP support, distant from Bog.")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem24/report/analysis/2FAfigs/FA1of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F2 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR2)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +  
  labs(fill = "Factor score", 
       caption = "F2: Wealthy, densely populated urban areas")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem24/report/analysis/2FAfigs/FA2of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F3 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR4)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "Factor score", 
       caption = "F3: Poor sparsely populated areas distant from Bogota")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem24/report/analysis/2FAfigs/FA3of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F4 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR3)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "Factor score", 
       caption = "F4: Victims of violence and unrest")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem24/report/analysis/2FAfigs/FA4of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

FA_4 <- grid.arrange(F1 + theme(plot.title = element_text(size = 16)), 
                     F2 + theme(plot.title = element_text(size = 16)), 
                     F3 + theme(plot.title = element_text(size = 16)), 
                     F4 + theme(plot.title = element_text(size = 16)), 
                       top = textGrob("Factor analysis: Four latent factors in the data (Colombia 2018)", 
                                      gp = gpar(fontsize = 14)), ncol = 2)
#suppressMessages(FA_4)
ggsave(filename = "G:/Shared drives/snvdem/snvdem24/report/analysis/2FAfigs/FA-4_map.png", FA_4,
       height = 10, width = 10, device = "png", units = "in")

#comparison with fewer indicators... 
fa_cdf_small <- fa(colvars_cdf[, c(5:6, 9, 23, 25:27, 32)], nfactors = 4, rotate = "oblimin", 
             scores = "regression", SMC=FALSE, fm="minres")
# Try different numbers of factors....

#print(fa_cdf)
fa_cdf_small[["Structure"]]
