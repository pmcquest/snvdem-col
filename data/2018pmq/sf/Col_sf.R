
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

#----
# Cardinal regions 
# aggregate the values for regional-level maps
col_aggregated1 <- col %>%
  group_by(cdir6t9) %>%
  summarize(
    el_t6t9 = unique(el_t6t9),
    cw_t6t9 = unique(cw_t6t9)
  )
custom_colors1 <- c("pink", "orchid4")
custom_labels1 <- c("Less Free Elections", "Weak Civil Liberties")

col_aggregated2 <- col %>%
  group_by(cdir6t9) %>%
  summarize(
    em_t6t9 = unique(em_t6t9),
    cs_t6t9 = unique(cs_t6t9)
  )
custom_colors2 <- c("lightblue", "darkgreen")
custom_labels2 <- c("More Free Elections", "Strong Civil Liberties")


elcw6t9 <- ggplot() +
  geom_sf(data = col_aggregated1, aes(fill = el_t6t9, alpha = 0.5), color = "transparent") + 
  geom_sf(data = col_aggregated1, aes(fill = cw_t6t9, alpha = 0.5), color = "transparent") +
  labs(title = "Less free elections, and weaker civil liberties",
       caption = "6-9. North-South-West-East") +
  scale_fill_gradient(name = "V-Dem (mean)",
                      low = custom_colors1[1],
                      high = custom_colors1[2],
                      guide = guide_legend()) +
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) + 
  theme_void()


emcs6t9 <- ggplot() +
  geom_sf(data = col_aggregated2, aes(fill = em_t6t9, alpha = 0.5), color = "transparent") + 
  geom_sf(data = col_aggregated2, aes(fill = cs_t6t9, alpha = 0.5), color = "transparent") +
  labs(title = "More free elections, and stronger civil liberties",
       caption = "6-9. North-South-West-East") +
  scale_fill_gradient(name = "V-Dem (mean)",
                      low = custom_colors2[1],
                      high = custom_colors2[2],
                      guide = guide_legend()) +
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) + 
  theme_void()



#----
# 15-16. Support for ruling party


#Q15: in "areas where support [10% MOV] for the national ruling party is strong" there are ...
## instead of arbitrary cut-point
RP18 <- RP18 %>% 
  #"less free and fair subnational elections" 
  mutate(elsnl_15 = case_when(
    MOV_pct > 0.10 ~ v13_col_sn_2018$v2elsnlfc_15, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_15 = case_when(
    MOV_pct > 0.10 ~ v13_col_sn_2018$v2elsnmrfc_15, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_15 = case_when(
    MOV_pct > 0.10 ~ v13_col_sn_2018$v2clrgstch_15, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_15 = case_when(
    MOV_pct > 0.10 ~ v13_col_sn_2018$v2clrgwkch_15, TRUE ~ NA_real_))

#Q16: in "areas where support [10% MOV] for the national ruling party is weak" there are ...
RP18 <- RP18 %>% 
  #"less free and fair subnational elections" 
  mutate(elsnl_16 = case_when(
    MOV_pct < -0.10 ~ v13_col_sn_2018$v2elsnlfc_16, TRUE ~ NA_real_)) %>%
  #"more free and fair subnational elections"
  mutate(elsnmr_16 = case_when(
    MOV_pct < -0.10 ~ v13_col_sn_2018$v2elsnmrfc_16, TRUE ~ NA_real_)) %>%
  #"stronger civil liberties" 
  mutate(clrgst_16 = case_when(
    MOV_pct < -0.10 ~ v13_col_sn_2018$v2clrgstch_16, TRUE ~ NA_real_)) %>%
  #"weaker civil liberties" 
  mutate(clrgwk_16 = case_when(
    MOV_pct < -0.10 ~ v13_col_sn_2018$v2clrgwkch_16, TRUE ~ NA_real_))

RP18 <- RP18[complete.cases(RP18$FID), ] # remove NA case
