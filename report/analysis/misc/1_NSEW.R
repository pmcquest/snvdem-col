##---- 6-9 NSWE ----

setwd("G:/Shared drives/snvdem/snvdem24")

#data from Matt Sisk
reg <- read_csv("G:/Shared drives/snvdem/snvdem24/data/geospatial/6-9_NSWE/COL_NSEW.csv")

reg <- reg %>%
  mutate(ns_6t9 = north - south) %>%
  mutate(ew_6t9 = east - west) %>%
  mutate(nsv_6t9 = abs(ns_6t9 - mean(ns_6t9))) %>%
  mutate(ewv_6t9 = abs(ew_6t9 - mean(ew_6t9)))

