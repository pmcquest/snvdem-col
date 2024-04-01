
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

#--------------------
#Notes
#--------------------
# Rurality 0-1
#For example, a high rural index multiplied by a high proportion of V-Dem's response will produce correspondingly high scores. This may occur with the values for less free and fair elections for rural (.875) vs. urban (.125) areas.
# ([indrur0t1]*Rural)+((1-[indrur0t1])*Urban)
# .80*.875 + (1-.8)*.125
IR18 <- IR18 %>% 
  mutate(el_0t1x = (indrural*v13_col_sn_2018$v2elsnlfc_0)+((1-indrural)*v13_col_sn_2018$v2elsnlfc_1)) %>%
  mutate(em_0t1x = (indrural*v13_col_sn_2018$v2elsnmrfc_0)+((1-indrural)*v13_col_sn_2018$v2elsnmrfc_1)) %>%
  mutate(cs_0t1x = (indrural*v13_col_sn_2018$v2clrgstch_0)+((1-indrural)*v13_col_sn_2018$v2clrgstch_1)) %>%
  mutate(cw_0t1x = (indrural*v13_col_sn_2018$v2clrgwkch_0)+((1-indrural)*v13_col_sn_2018$v2clrgwkch_1))

# Min-max normalization function
nz <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Create new variables with normalized values. Normalization may be important when comparing across variable scales.
df_nz <- data.frame(el_0t1xs = nz(IR18$el_0t1x),
                    em_0t1xs = nz(IR18$em_0t1x),
                    cs_0t1xs = nz(IR18$cs_0t1x),
                    cw_0t1xs = nz(IR18$cw_0t1x))
# Combine the normalized variables with the original dataframe
IR18 <- cbind(IR18, df_nz)


#------------
# Economic development 2-3
# This was the original code, looking at median thresholds...
median(ED18$VAM18_2t3) #municipal threshold as median: 137.6316
median(ED18$PBID18_2t3, na.rm = TRUE) #departmental threshold as median: 26884.21 (with NA values excluded)
## We may want to use quintile power, but we don't want to assign too much weight to these categories. 

#Q2: in "areas that are less economically developed" there are ...
## at the municipal level...
ED18 <- ED18 %>% 
  #"less free and fair subnational elections" 
  mutate(elsnl_2m  = case_when( #Reduced the variable names to less than 10 characters in order to avoid merging issues in ArcMap
    VAM18_2t3< 138 ~ v13_col_sn_2018$v2elsnlfc_2, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_2m  = case_when(
    VAM18_2t3< 138 ~ v13_col_sn_2018$v2elsnmrfc_2, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_2m  = case_when(
    VAM18_2t3< 138 ~ v13_col_sn_2018$v2clrgstch_2, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_2m  = case_when(
    VAM18_2t3< 138 ~ v13_col_sn_2018$v2clrgwkch_2, TRUE ~ NA_real_)) %>% 
  ## at the departmental level
  #"less free and fair subnational elections" 
  mutate(elsnl_2d = case_when(
    PBID18_2t3< 26884 ~ v13_col_sn_2018$v2elsnlfc_2, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_2d = case_when(
    PBID18_2t3< 26884 ~ v13_col_sn_2018$v2elsnmrfc_2, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_2d = case_when(
    PBID18_2t3< 26884 ~ v13_col_sn_2018$v2clrgstch_2, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_2d = case_when(
    PBID18_2t3< 26884 ~ v13_col_sn_2018$v2clrgwkch_2, TRUE ~ NA_real_))

#Q3: in "areas that are more economically developed" there are ...
## at the municipal level...
ED18 <- ED18 %>% 
  #"less free and fair subnational elections" 
  mutate(elsnl_3m  = case_when(
    VAM18_2t3> 138 ~ v13_col_sn_2018$v2elsnlfc_3, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_3m  = case_when(
    VAM18_2t3> 138 ~ v13_col_sn_2018$v2elsnmrfc_3, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_3m  = case_when(
    VAM18_2t3> 138 ~ v13_col_sn_2018$v2clrgstch_3, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_3m  = case_when(
    VAM18_2t3> 138 ~ v13_col_sn_2018$v2clrgwkch_3, TRUE ~ NA_real_)) %>% 
  ## at the departmental level
  #"less free and fair subnational elections" 
  mutate(elsnl_3d = case_when(
    PBID18_2t3> 26884 ~ v13_col_sn_2018$v2elsnlfc_3, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_3d = case_when(
    PBID18_2t3> 26884 ~ v13_col_sn_2018$v2elsnmrfc_3, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_3d = case_when(
    PBID18_2t3> 26884 ~ v13_col_sn_2018$v2clrgstch_3, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_3d = case_when(
    PBID18_2t3> 26884 ~ v13_col_sn_2018$v2clrgwkch_3, TRUE ~ NA_real_))


