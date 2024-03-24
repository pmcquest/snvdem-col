
# libraries we need
libs <- c(
  "tidyverse", "geodata",
  "sf", "ggmap", "maps",
  "ggrepel"
)

# install missing libraries
installed_libs <- libs %in% rownames(
  installed.packages()
)
if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

# load libraries
invisible(lapply(
  libs, library,
  character.only = T
))

col <- st_read("G:/My Drive/git/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
# st_layers("G:/My Drive/git/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
plot(st_geometry(col))
