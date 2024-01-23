# Notes for geo-spatial analysis

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


# related authors also created interactive maps which provide an interesting
# model for the subnational democracy visualization we may do:
## http://finiterank.github.io/homicidios/
  
# Then there are other sources, as listed here: https://freegisdata.rtwilson.com/
##  GADM: https://gadm.org/download_country.html


# Questions:
## Is there an equivalent of the 'geobr' package for Colombia?
