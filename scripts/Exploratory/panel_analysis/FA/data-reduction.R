#---- Step 2: Factor analysis of within-country variables ----

# Step 1: Wrangle raw data, clean it, then impute missing values
# Step 2 (this script): Data reduction (calculate factor scores)
# Step 3: merge-in V-Dem data (weighted by coder-level analysis)
# Step 4: Map geolocated levels of democracy

setwd("G:/Shared drives/snvdem/snvdem-col/data/panel")

library(tidyverse)
library(knitr)
library(psych)
library(psychTools)
library(GPArotation)
library(kableExtra)
library(dplyr)
library(tidyr)
library(corrplot)
library(reshape2)

# Load the data
colvars_cdf <- read_rds("final_data/colvars_cdf.rds")

# Compute the correlation matrix to make sure there are no perfectly correlated variables
cor_matrix <- cor(colvars_cdf[, c(16:19, 30, 7, 14:15, 20:28, 31, 35, 40, 32:34)], use = "complete.obs")
# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")



# ---- Factor Analysis (FA) ----
## ---- Pooled FA ----
# Run Factor Analysis on the entire dataset (2000-2020), selecting relevant variables
fa_pooled_vm <- fa(colvars_cdf[, c(16, 18:19, 30, 7, 14:15, 20:28, 31, 35, 40, 32:34)], nfactors = 5, rotate = "varimax", scores = "regression", fm = "ml")

# other FA estimation score method for time-series data?
# fa_pooled_ob <- fa(colvars_cdf[, c(16:19, 30, 7, 14:15, 20:28, 31, 40, 32:34)], nfactors = 4, rotate = "oblimin", scores = "bartlett", fm = "pa")


# View the factor loadings
pooled_loadings <- fa_pooled_vm$loadings
# Adjust plot margins
par(mar = c(5, 8, 4, 2) + 0.1) # Increase left margin (mar[2])
fa.diagram(fa_pooled_vm)
# Save the plot to a file with larger dimensions
png("G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/FA/factor_diagram.png", width = 1000, height = 800) # Adjust width and height
fa.diagram(fa_pooled_vm)
dev.off()

# visualize
library(corrplot)
corrplot(pooled_loadings, method = "circle", type = "full", tl.col = "black", tl.srt = 45)


### ----- Export factor loadings -----

fa_pooled_vm[["Structure"]]


# nice table
# Extract factor loadings
corrplot(pooled_loadings, method = "circle", type = "full", tl.col = "black", tl.srt = 45)
loadings_matrix <- as.data.frame(fa_pooled_vm$Structure[])
new_row_names <- c("Rurality (0-1)", "Urban pop. (1)",
                   "GDP (2-3)", "Fiscal Perf. (2-3)", "Distance Bog. (4-5)",
                   "North-South (6-7)", "East-West (9-8)",
                   "Displaced (10)", "Eradication (10)", "Log. Coca Has. (11)", 
                   "Robberies (11)", "Homicides (11)",
                   "Homicide-CEV (11)", "Disappear-CEV (11)", "Kidnap-CEV (11)", "ForcedRec-CEV (11)",
                   "Pop. Density (12)", "Distance Market (13)", "Road density (13)",
                   "Indigenous pop. (14)", "Ethnic pop. (14)", "Ruling Party (15-16)")
rownames(loadings_matrix) <- new_row_names
latex_table <- kable(loadings_matrix, caption = "Factor Analysis Loadings", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center")
print(latex_table)
# Convert the table to HTML
html_table <- knit_print(latex_table)
# Create an HTML file and write the HTML content
html_file <- "G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/FA/factor_analysis-5.html"
writeLines(html_table, html_file)



## ---- Create municipal-level dataframes of FA results ----

preds_cdf <- as.data.frame(predict(fa_pooled_vm, colvars_cdf[, c(16, 18:19, 30, 7, 14:15, 20:28, 31, 35, 40, 32:34)]))
fa_IDs <- as.data.frame(cbind(colvars_cdf[,c(1:2)], preds_cdf)) %>%
  group_by(MPIO_CDPMP) %>%
  mutate(ML2mean = mean(ML2, na.rm = TRUE), #col1 in the analysis
         ML5mean = mean(ML5, na.rm = TRUE), #col2 in the analysis
         ML3mean = mean(ML3, na.rm = TRUE), #col3 in the analysis
         ML1mean = mean(ML1, na.rm = TRUE), #col4 in the analysis
         ML4mean = mean(ML4, na.rm = TRUE)) #col5 in the analysis
write_rds(fa_IDs, "G:/Shared drives/snvdem/snvdem-col/data/panel/final_data/FA/FAcol0020.rds")


### ----- Descriptive plots -----
library(ggridges)
ggplot(fa_IDs, aes(x=ML1, y=reorder(MPIO_CDPMP, ML1))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = ML1mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "Factor 1: Indigenous and ethnic, rural populations",
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



## ---- Year-by-year FA ----
# Run FA for each year and store the results

fa_yearly <- lapply(unique(colvars_cdf$year), function(year) {
  fa_data <- subset(colvars_cdf, year == year)
  fa(fa_data[, c(16:19, 30, 7, 14:15, 20:28, 31, 40, 32:34)], nfactors = 4, rotate = "varimax", fm = "minres")
})

# Extract the factor loadings for each year
yearly_loadings <- lapply(fa_yearly, function(fa_res) fa_res$loadings)

# Create a list of reshaped factor loadings for each year
yearly_loadings_df <- lapply(1:length(fa_yearly), function(i) {
  
  # Extract the factor loadings matrix for each year
  loadings <- fa_yearly[[i]]$loadings
  
  # Remove the last rows containing summary statistics (we'll keep just the loadings)
  loadings <- loadings[1:(nrow(loadings) - 3), ]  # Exclude last 3 rows (summary statistics)
  
  # Convert matrix to data frame with appropriate row and column names
  loadings_df <- as.data.frame(loadings)
  loadings_df$variable <- rownames(loadings)  # Add variable names as a new column
  loadings_df$year <- unique(colvars_cdf$year)[i]  # Add year as a new column
  
  # Reshape the data to long format using pivot_longer
  loadings_long <- loadings_df %>%
    pivot_longer(cols = starts_with("MR"), names_to = "factor", values_to = "value")
  
  return(loadings_long)  # Return the reshaped data frame
})

# Combine all yearly data into one data frame
yearly_loadings_combined <- do.call(rbind, yearly_loadings_df)

# Create the heatmap for factor loadings per year
ggplot(yearly_loadings_combined, aes(x = factor, y = variable, fill = value)) +
  geom_tile() +
  facet_wrap(~ year, scales = "free_y") +  # Facet by year to have a separate heatmap for each year
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Factor Loadings per Year", x = "Factors", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(expand = c(0, 0))  # Optional: remove extra space on the y-axis


##----FA for illicity activity (#11)----


# ----- Merge FA w/ geometries to make maps -----
## Maps
library(sf)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)

col <- st_read("G:/Shared drives/snvdem/snvdem-col/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
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
#ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/2FAfigs/FA1of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F2 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR2)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +  
  labs(fill = "Factor score", 
       caption = "F2: Wealthy, densely populated urban areas")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/2FAfigs/FA2of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F3 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR4)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "Factor score", 
       caption = "F3: Poor sparsely populated areas distant from Bogota")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/2FAfigs/FA3of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

F4 <- ggplot() +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR3)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        plot.caption = element_text(size = 12)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  labs(fill = "Factor score", 
       caption = "F4: Victims of violence and unrest")
#ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/2FAfigs/FA4of4_map.png", 
#       height = 10, width = 10, device = "png", units = "in")

FA_4 <- grid.arrange(F1 + theme(plot.title = element_text(size = 16)), 
                     F2 + theme(plot.title = element_text(size = 16)), 
                     F3 + theme(plot.title = element_text(size = 16)), 
                     F4 + theme(plot.title = element_text(size = 16)), 
                       top = textGrob("Factor analysis: Four latent factors in the data (Colombia 2018)", 
                                      gp = gpar(fontsize = 14)), ncol = 2)
#suppressMessages(FA_4)
ggsave(filename = "G:/Shared drives/snvdem/snvdem-col/report/analysis/2FAfigs/FA-4_map.png", FA_4,
       height = 10, width = 10, device = "png", units = "in")

#comparison with fewer indicators... 
fa_cdf_small <- fa(colvars_cdf[, c(5:6, 9, 23, 25:27, 32)], nfactors = 4, rotate = "oblimin", 
             scores = "regression", SMC=FALSE, fm="minres")
# Try different numbers of factors....

#print(fa_cdf)
fa_cdf_small[["Structure"]]
