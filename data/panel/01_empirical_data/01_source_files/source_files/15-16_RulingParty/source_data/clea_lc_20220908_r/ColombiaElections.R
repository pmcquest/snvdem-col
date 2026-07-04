
library(tidyverse)
library(haven)

load("G:/My Drive/git/snvdem-col/data/geospatial/2018pmq/15-16_RulingParty/source_data/clea_lc_20220908_r/clea_lc_20220908.RData")
Col98 <- filter(clea_lc_20220908, ctr_n=="Colombia")
saveRDS(Col98, file = "clea_Col98.rds")

rm(list = ls())

clea_Col98 <- readRDS("G:/My Drive/git/snvdem-col/data/geospatial/2018pmq/15-16_RulingParty/source_data/clea_lc_20220908_r/clea_Col98.rds")

head(clea_Col98)
