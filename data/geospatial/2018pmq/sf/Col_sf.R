
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

col <- st_read("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
# st_layers("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
plot(st_geometry(col))


# Import the shapefile
col2 <- st_read("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/sf/shp/Merge18_col.shp")
# Identify variables that start with "STP"
STP_vars <- grep("^STP", names(col2), value = TRUE)
# Remove those variables from the dataframe
col2 <- col2[, !names(col2) %in% STP_vars]
