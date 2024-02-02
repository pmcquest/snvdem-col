# Notes for geo-spatial analysis

# Source data:
# Micromaps is a package that combines columns and maps for visualization
## https://www.jstatsoft.org/article/view/v063i02
# Some authors have already used this for mapping in Colombia:
## http://nebulae-co.github.io/colmaps/micromapa

# The same authors created a package for R called "colmaps" that contains 
# Colombian geographic data: 
## https://github.com/nebulae-co/colmaps?tab=readme-ov-file

devtools::install_github("nebulae-co/colmaps")
library(colmaps)
head(municipios@data)
# this data can be mapped using ggplot2
library(ggplot2)
colmap(municipios) +
  ggtitle("Municipios en Colombia")
## error message about deprecated feature, says "Please migrate to sf"
# with these packages, we can merge Colombian panel data...
library(readr)
CMPD <- read_csv("CMPD.csv") #file located in data/panel containing municipal observations
head(CMPD)
library(dplyr)
CMPD <- CMPD %>% 
  rename(id = CODIGODANE)
CMPD <- CMPD %>% 
  rename(id_depto = DeptCode)
CMPD$id <- ifelse(nchar(CMPD$id) == 4, paste0("0", CMPD$id), CMPD$id)
CMPD$id_depto <- ifelse(nchar(CMPD$id_depto) == 1, paste0("0", CMPD$id_depto), CMPD$id_depto)

colmap(municipios, subset(CMPD, Year == 2020), var = "Mortalidad")
colmap(municipios, subset(CMPD, Year == 2020), var = "PDET")
colmap(municipios, subset(CMPD, Year == 2020), var = "altura")
colmap(municipios, subset(CMPD, Year == 2020), var = "discapital")
colmap(municipios, subset(CMPD, Year == 2020), var = "ValueAdded")

# and by departamento...
CMPD %>%
  filter(Year == 2020) %>%
  group_by(id_depto) %>%
  summarise(tasa_homicidio = 10^5 * sum(homicidios, na.rm = TRUE) /
              sum(pobl_tot, na.rm = TRUE)) %>%
  colmap(departamentos, data = ., data_id = "id_depto")
# next steps: 
# 1. collect panel data on relevant variables for this analysis (e.g., rurality, GDP, violence, etc.)
# 2. create a ggplot for time-series mapping for relevant variables



## For municipios, average the responses. But if not, graphically we can create an alpha-value to overlay. After we have the geometries we need, we can find intersection/overlaps where the geometries overlaps.
## How to combine these variables? Simple average, factor analysis, machine learning? W/ Factor scores (some variables would be correlated w/ other variables) -- but there may be other dimensions. 
# related authors also created interactive maps which provide an interesting
# model for the subnational democracy visualization we may do:

# same authors also created interactive maps which provide an interesting
# model for the subnational democracy visualization:
## http://finiterank.github.io/homicidios/
  
# Then there are other sources, as listed here: https://freegisdata.rtwilson.com/
##  GADM: https://gadm.org/download_country.html
## 'ggmap' package imports data from Google and other websites: https://github.com/dkahle/ggmap
## additional R packages available:
install.packages("geodata")
library(geodata)

## Questions:
# 1. loading shapefiles is not so easy, because R has a 5mb limit. Is it even advantageous to download and read shp files, or is it more efficient to connect to websites (using 'ggmap' for example)? If the former option is better, how do I load shp files in R larger than 5mb?
library(sf)
st_read()
## 2. There are already R packages out there for evaluating countries using mapping. Is there an equivalent of the 'geobr' package for Colombia?


