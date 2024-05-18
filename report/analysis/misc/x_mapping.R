#----- Mapping subnational data -----

setwd("G:/Shared drives/snvdem/snvdem24")

##----- Step 1: Load shapefile -----
# example: Colombia 

library(sf)
col <- st_read("G:/Shared drives/snvdem/snvdem24/data/geospatial/2018pmq/BaseLayer/MGN_ANM_MPIOS.shp")
col <- col %>%
  select(1:8)




#---- Misc: Summary measures ----#

library(ggplot2)
library(gridExtra)
library(stringr)
col <- merge(col, VAM18, by = "MPIO_CDPMP", all.x = TRUE)
col$logVAM18_2t3 <- log(col$VAM18_2t3)

# Merge CDF data to CDF df
eCDF_2t3m <- ecdf(col$VAM18_2t3)
col$VAM18_2t3c <- eCDF_2t3m(col$VAM18_2t3)



c3d <- ggplot() +
  geom_sf(data = col, aes(fill = logVAM18_2t3), color = "transparent", alpha = 0.5) + # check results
  labs(title = "VAM 2018: Log", caption = "Log VAM (in billions $COP; exchange USD $252,000)") + 
  scale_fill_continuous(guide = guide_colorbar(
    title = "Log VAM (billions $COP)",
    title.position = "top",  # Change title position
    label.position = "right"),
    low = "lightblue",
    high = "darkblue") + # Change label position
  theme(legend.title = element_text(size = 6)) +
  theme_void()
c3c <- ggplot() +
  geom_sf(data = col, aes(fill = VAM18_2t3c), color = "transparent", alpha = 0.5) + # check results
  labs(title = "VAM 2018: ECDF", caption = "ECDF of VAM") +
  scale_fill_continuous(guide = guide_colorbar(
    title = "ECDF VAM (billions $COP)",
    title.position = "top",  # Change title position
    label.position = "right"),
    low = "lightblue",
    high = "darkblue") + # Change label position
  theme(legend.title = element_text(size = 6)) +
  theme_void()
ap_2t3 <- grid.arrange(c3d + theme(plot.title = element_text(size = 14)), 
                       c3c + theme(plot.title = element_text(size = 14)), 
                       #top = textGrob("Comparing Log and ECDF values for VAM", gp = gpar(fontsize = 14)),         
                       ncol = 2)


#for preliminary mapping of combined scores in cardinal directions...
col <- col %>%
  mutate(m1_t6t9 = (el_t6t9 + cw_t6t9) / 2) %>% # weak sn democracy
  mutate(m2_t6t9 = (em_t6t9 + cs_t6t9) / 2) # strong sn democracy




# calculate the arithmetic mean for the continuous variables
col$el_i = (col$el_c0+col$el_c1+col$el_c2m+col$el_c3m+col$el_c4t5+col$el_t6t9+col$el_c15+col$el_c16) / 8
col$em_i = (col$em_c0+col$em_c1+col$em_c2m+col$em_c3m+col$em_c4t5+col$em_t6t9+col$em_c15+col$em_c16) / 8
col$cs_i = (col$cs_c0+col$cs_c1+col$cs_c2m+col$cs_c3m+col$cs_c4t5+col$cs_t6t9+col$cs_c15+col$cs_c16) / 8
col$cw_i = (col$cw_c0+col$cw_c1+col$cw_c2m+col$cw_c3m+col$cw_c4t5+col$cw_t6t9+col$cw_c15+col$cw_c16) / 8

