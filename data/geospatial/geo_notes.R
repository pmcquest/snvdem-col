# Notes for geo-spatial analysis

# Source data:
## Inputs from websites: 
### 'ggmap' package imports data from Google and other websites: https://github.com/dkahle/ggmap

# Many basic R packages available:
install.packages("geodata")
library(geodata)

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
# 
library(ggplot2)
colmap(municipios) +
  ggtitle("Municipios en Colombia")
# error message about deprecated feature, says "Please migrate to sf"
# with these package, we can merge Colombian panel data...
library(readr)
CMPD <- read_csv("CMPD.csv")
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



# related authors also created interactive maps which provide an interesting
# model for the subnational democracy visualization we may do:
## http://finiterank.github.io/homicidios/
  
# Then there are other sources, as listed here: https://freegisdata.rtwilson.com/
##  GADM: https://gadm.org/download_country.html

###

# loading shapefiles...
library(sf)
st_read()

# Questions:
## Is there an equivalent of the 'geobr' package for Colombia?


