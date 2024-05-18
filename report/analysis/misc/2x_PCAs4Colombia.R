## Principal Components analysis of Colombia variables
library(tidyverse)

setwd("C:/Users/mcoppedg/Dropbox/External (1)")
# Load the data
colvars <- read.csv("V18.csv", encoding = "UTF-8")
names(colvars)

# Select variables for PCA
selvars <- select(colvars, el_c0, cw_c0, 
                  el_c1, em_c1, cs_c1, 
                  el_c2m, em_c2m, cw_c2m, 
                  el_c3m, em_c3m, cs_c3m, 
                  el_c4t5, cw_c4t5, 
                  el_t6t9, cs_t6t9, cw_t6t9, 
                  em_c15, cs_c15, cw_c16)

# Make the "less" and "weaker" variables negative
selvars_neg <- selvars %>%
  mutate(el_c0 = -el_c0,
         cw_c0 = -cw_c0,
         el_c1 = el_c1,
         el_c2m = -el_c2m, 
         cw_c2m = -cw_c2m, 
         el_c3m = -el_c3m, 
         el_c4t5 = -el_c4t5, 
         cw_c4t5 = -cw_c4t5, 
         el_t6t9 = -el_t6t9, 
         cw_t6t9 = -cw_t6t9,
         cw_c16 = -cw_c16)

pc <- prcomp(selvars_neg,
             center = TRUE,
             scale. = TRUE,
             rank. = 4)
#attributes(pc)
summary(pc)
print(pc)

# Apparently some variables are perfectly correlated.

# correlation matrix
cor(selvars_neg, use = "pairwise")
names(selvars)

# Combine the variables that are perfectly correlated
selvars2 <- selvars_neg %>%
  mutate(c01 = (el_c0+cw_c0+el_c1+em_c1+cs_c1)/5,
         c23m = (el_c2m+em_c2m+cw_c2m+el_c3m+em_c3m+cs_c3m)/6,
         elcw_c4t5 = (el_c4t5+cw_c4t5)/2,
         emcscw_c1516 = (em_c15+cs_c15+cw_c16)/3) %>%
  select(c01, c23m, elcw_c4t5, el_t6t9, cs_t6t9, cw_t6t9, emcscw_c1516)

pc <- prcomp(selvars2,
             center = TRUE,
             scale. = TRUE,
             rank. = 4)
#attributes(pc)
summary(pc)
print(pc)
plot(prcomp(selvars2))
preds <- as.data.frame(predict(pc, selvars2))

ggplot(preds, aes(x = PC1, y= PC2)) + geom_point()
ggplot(preds, aes(x = PC3, y= PC4)) + geom_point()
ggplot(preds, aes(x = PC1, y= PC3)) + geom_point()
ggplot(preds, aes(x = PC2, y= PC4)) + geom_point()
ggplot(preds, aes(x = PC2, y= PC3)) + geom_point()

cor(preds, use = "pairwise") # All forced to be orthogonal.

## Merging with IDs to compare distributions

pc_IDs <- as.data.frame(cbind(colvars[,c(4,10)], preds)) %>%
          group_by(depto) %>%
          mutate(PC1mean = mean(PC1, na.rm = TRUE),
                 PC2mean = mean(PC2, na.rm = TRUE))

ggplot(pc_IDs, 
       aes(x=PC1, color = as.factor(depto))) +
  geom_line(stat = "density") +
  guides(color = "none")

library(ggridges)
ggplot(pc_IDs, aes(x=-PC1, y=reorder(depto, -PC1))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = -PC1mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "first principal component*(-1)",
       title = "Means (dots) and distributions")
ggsave(filename = "PCA1_ridgeline.png", height = 10, width = 6, device = "png", units = "in")

ggplot(pc_IDs, aes(x=PC2, y=reorder(depto, PC2))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = PC2mean), color = "firebrick") +
   theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "second principal component",
       title = "Means (dots) and distributions")
ggsave(filename = "PCA2_ridgeline.png", height = 10, width = 6, device = "png", units = "in")

ggplot(pc_IDs, aes(x = -PC1, y = PC2, color = depto)) +
  geom_point()


## Oblique rotation: not possible. PCA forced the different dimensions to be uncorrelated. 
#Scatterplot rotated until uncorrelated. Urban-rural dimension is somewhat correlated, but they're not the same. 
# FA can be exploratory. Looking for patterns in the data. We interpret what those dimensions are. 
## Gives clues about what that measures. The maps show that. 


## Factor analysis
factanal(selvars2, factors = 3, rotation = "promax") # factors set after some prelim analysis. 
# the eigen-value: keep as many factors as eigen-value of 1 or more. 1 means the factor contains the value of one variable. 
## measure of data reduction efficiency. Larger is better. 

cor(selvars2, use = "pairwise")
# The singular matrix is probably due to perfect correlation
# among c23m, em_c3m, and cs_c3m. Just drop the last two.
selvars3 <- select(selvars2, c01, c23m, elcw_c4t5, el_t6t9, cs_t6t9, cw_t6t9)
# Now try again:
factanal(selvars3, factors = 1, rotation = "promax")
# Not solvable, I think because at this point there are 
# too few variables for the number of factors.

# an alternative
library(psych)
install.packages("GPArotation")
library(GPArotation)
fa_col <- fa(selvars2, nfactors = 3, rotate = "oblimin", 
   scores = "regression", SMC=FALSE, fm="minres")

#summary(fa_col)
print(fa_col)
fa_col[["Structure"]]
preds_fa <- as.data.frame(predict(fa_col, selvars2))

fa_IDs <- as.data.frame(cbind(colvars[,c(4,5,10)], preds_fa)) %>%
  group_by(depto) %>%
  mutate(MR1mean = mean(MR1, na.rm = TRUE),
         MR2mean = mean(MR2, na.rm = TRUE),
         MR3mean = mean(MR3, na.rm = TRUE))

# Descriptive plots
ggplot(fa_IDs, aes(x=MR1, y=reorder(depto, MR1))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR1mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "first factor",
       title = "Means (dots) and distributions")
ggsave(filename = "FA1_ridgeline.png", height = 10, width = 6, device = "png", units = "in")

ggplot(fa_IDs, aes(x=-MR2, y=reorder(depto, -MR2))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = -MR2mean), color = "firebrick") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "second factor*(-1)",
       title = "Means (dots) and distributions")
ggsave(filename = "FA2_ridgeline.png", height = 10, width = 6, device = "png", units = "in")

ggplot(fa_IDs, aes(x=MR3, y=reorder(depto, MR3))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR3mean), color = "firebrick") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "third factor",
       title = "Means (dots) and distributions")
ggsave(filename = "FA3_ridgeline.png", height = 10, width = 6, device = "png", units = "in")

## Comparison of first and second dimensions
pcfa_IDs <- as.data.frame(cbind(pc_IDs, preds_fa))

# Making depto labels
library(ggrepel)

pcfa_IDs <- pcfa_IDs %>%
  mutate(depto_label = ifelse(MPIO_CCDGO == 1, depto, NA))

ggplot(pcfa_IDs, aes(x = -PC1, y = PC2, color = depto)) +
  geom_point() +
  geom_text_repel(aes(label = depto_label), size = 3, max.overlaps = Inf) +
  geom_smooth(method = lm, color = "black", se = FALSE) +
  guides(color = "none") + theme_light() +
  labs(title = "Comparison of first and second principal components",
       caption = "Only the first municipio of each department is labeled,\npresumably the capital or largest city",
       x = "First principal component*(-1)",
       y = "Second principal component")
ggsave(filename = "PC1_PC22_scatter.png", height = 8, width = 8, device = "png", units = "in")

ggplot(pcfa_IDs, aes(x = -MR2, y = MR1, color = depto)) +
  geom_point() +
  geom_text_repel(aes(label = depto_label), size = 3, max.overlaps = Inf) +
  geom_smooth(method = lm, color = "black", se = FALSE) +
  guides(color = "none") + theme_light() +
  labs(title = "Comparison of first and second factor score",
       caption = "Only the first municipio of each department is labeled,\npresumably the capital or largest city",
       x = "Second factor score*(-1)",
       y = "First factor score")
ggsave(filename = "FA1_FA2_scatter.png", height = 8, width = 8, device = "png", units = "in")

# Why three levels of FA1?
ggplot(colvars, aes(x = el_t6t9)) +
  geom_line(stat = "density")

ggplot(colvars, aes(x = cs_t6t9)) +
  geom_line(stat = "density")

ggplot(colvars, aes(x = cw_t6t9)) +
  geom_line(stat = "density")

## Comparison of PCA and FA estimates
ggplot(pcfa_IDs, aes(x = -PC1, y = -MR2, color = depto)) +
  geom_point() +
  geom_text_repel(aes(label = depto_label), size = 3, max.overlaps = Inf) +
  guides(color = "none") + theme_light() +
  labs(title = "Comparison of principal components and factor scores",
       caption = "Only the first municipio of each department is labeled,\npresumably the capital or largest city",
       x = "First principal component*(-1)",
       y = "Second factor score*(-1)")
ggsave(filename = "PC1_FA2_scatter.png", height = 8, width = 8, device = "png", units = "in")

ggplot(pcfa_IDs, aes(x = PC2, y = -MR1, color = depto)) +
  geom_point() +
  geom_text_repel(aes(label = depto_label), size = 3, max.overlaps = Inf) +
  guides(color = "none") + theme_light() +
  labs(title = "Comparison of principal components and factor scores",
       caption = "Only the first municipio of each department is labeled,\npresumably the capital or largest city",
       x = "Second principal component*(-1)",
       y = "First factor score*(-1)")
ggsave(filename = "PC2_FA1_scatter.png", height = 8, width = 8, device = "png", units = "in")

## Forced onto one dimension
fa_col_1 <- fa(selvars2, nfactors = 1, rotate = "oblimin", 
             scores = "regression", SMC=FALSE, fm="minres")
#summary(fa_col_1)
print(fa_col_1)
fa_col_1[["Structure"]]
preds_fa_1 <- as.data.frame(predict(fa_col_1, selvars2))

fa1_IDs <- as.data.frame(cbind(colvars[,c(4,5,10)], preds_fa_1)) %>%
  group_by(depto) %>%
  mutate(MR1mean = mean(MR1, na.rm = TRUE),
         mpio_cnmbr = tolower(MPIO_CNMBR))

ggplot(fa1_IDs, aes(x=MR1, y=reorder(depto, MR1))) +
  geom_density_ridges(fill = "cornsilk", scale=1.5) +
  geom_point(aes(x = MR1mean), color = "navy") +
  theme_ridges(font_size=12) + 
  theme_light() +
  theme(legend.position = "none") +
  labs(y = "", x = "single factor",
       title = "Means (dots) and distributions")
## This single factor is mostly a reflection of NSEW regions.
## We will need to work on combining it with other criteria.

ggsave(filename = "FA1_1_ridgeline.png", height = 10, width = 6, device = "png", units = "in")


## Maps
library(sf)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)

download.file("https://github.com/pmcquest/snvdem24/tree/main/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.shp", 
              "MGN_ANM_MPIOS.shp", "wininet", quiet = FALSE)
download.file("https://github.com/pmcquest/snvdem24/tree/main/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.dbf", 
              "MGN_ANM_MPIOS.dbf", "wininet", quiet = FALSE)
download.file("https://github.com/pmcquest/snvdem24/tree/main/data/geospatial/MGN_ANM_MPIOS/MGN_ANM_MPIOS.prj", 
              "MGN_ANM_MPIOS.prj", "wininet", quiet = FALSE)


setwd('C:/Users/mcoppedg/Dropbox/External (1)/shapefiles')
colmap = read_sf("MGN_ANM_MPIOS.shp")
colmap <- colmap %>%
  mutate(name_2 = tolower(NAME_2))

ggplot() +
  geom_sf(data = colmap, color = "skyblue", fill = "cornsilk") +
  theme_void()

## creating a translation file
sf_IDs <- sort(unique(colmap$name_2))
factor_IDs <- sort(unique(fa1_IDs$mpio_cnmbr))
write.csv(sf_IDs, file = "sf_IDs.csv", fileEncoding = "UTF-8")
write.csv(factor_IDs, file = "factor_IDs.csv", fileEncoding = "UTF-8")

# Derived from comparing the csv files
colmap$name_2 <- case_match(colmap$name_2, .default = colmap$name_2, 
                    "abrego" ~ "ábrego",
                    "agua de díos" ~ "agua de dios",
                    "ancuyá" ~ "ancuya",
                    "aranzazú" ~ "aranzazu",
                    "barranco minas" ~ "barrancominas",
                    "becerríl" ~ "becerril",
                    "belén de los andaquies" ~ "belén de los andaquíes",
                    "beteitiva" ~ "betéitiva",
                    "bogotá d.c." ~ "bogotá, d.c.",
                    "busbanza" ~ "busbanzá",
                    "caldonó" ~ "caldono",
                    "carolina del principe" ~ "carolina",
                    "condotó" ~ "condoto",
                    "cuaspud" ~ "cuaspud carlosama",
                    "don matías" ~ "donmatías",
                    "el carmen de chucurí" ~ "el carmen de chucuri",
                    "el carmen del darién" ~ "carmen del darién",
                    "el peñon" ~ "el peñón",
                    "el sopetrán" ~ "sopetrán",
                    "el tablón de gomez" ~ "el tablón de gómez",
                    "el zulía" ~ "el zulia",
                    "falán" ~ "falan",
                    "guapí" ~ "guapi",
                    "guarandá" ~ "guaranda",
                    "guarné" ~ "guarne",
                    "güicán" ~ "güicán de la sierra",
                    "iquira" ~ "íquira",
                    "izá" ~ "iza",
                    "la playa de belén" ~ "la playa",
                    "la uribe" ~ "uribe",
                    "lebríja" ~ "lebrija",
                    "mirití-paraná" ~ "mirití - paraná",
                    "morichal nuevo" ~ "morichal",
                    "palo cabildo" ~ "palocabildo",
                    "pequé" ~ "peque",
                    "piendamó" ~ "piendamó - tunía",
                    "pueblo rico" ~ "pueblorrico",
                    "pueblo viejo" ~ "puebloviejo",
                    "puerto alegria" ~ "puerto alegría",
                    "puerto inírida" ~ "inírida",
                    "purísima" ~ "purísima de la concepción",
                    "ragonvalía" ~ "ragonvalia",
                    "río iro" ~ "río iró",
                    "sabanas de san angel" ~ "sabanas de san ángel",
                    "salazar de las palmas" ~ "salazar",
                    "san andrés de cuerquia" ~ "san andrés de cuerquía",
                    "san antonio de palmito" ~ "palmito",
                    "san bernardino de sahagún" ~ "sahagún",
                    "san cristobal" ~ "san cristóbal",
                    "san estanislao de kostka" ~ "san estanislao",
                    "san juan de pasto" ~ "pasto",
                    "san juan de río seco" ~ "san juan de rioseco",
                    "san luis de cubarral" ~ "cubarral",
                    "san luís" ~ "san luis",
                    "san miguel de mocoa" ~ "mocoa",
                    "san sebastian de mariquita" ~ "san sebastián de mariquita",
                    "san vicente" ~ "san vicente ferrer",
                    "santa cruz de lorica" ~ "lorica",
                    "santa cruz" ~ "santacruz",
                    "santafé de antioquia" ~ "santa fé de antioquia",
                    "santiago de cali" ~ "cali",
                    "santo domingo de silos" ~ "silos",
                    "sincé" ~ "san luis de sincé",
                    "sotará" ~ "sotará - paispamba",
                    "suazá" ~ "suaza",
                    "tarquí" ~ "tarqui",
                    "tocaíma" ~ "tocaima",
                    "toguí" ~ "togüí",
                    "tolú" ~ "santiago de tolú",
                    "toluviejo" ~ "san josé de toluviejo",
                    "topagá" ~ "tópaga",
                    "tumaco" ~ "san andrés de tumaco",
                    "tunungua" ~ "tununguá",
                    "ubaqué" ~ "ubaque",
                    "umbita" ~ "úmbita",
                    "uribía" ~ "uribia",
                    "valle del guamuéz" ~ "valle del guamuez",
                    "vista hermosa" ~ "vistahermosa",
                    "yacuanquér" ~ "yacuanquer",
                    "zetaquirá" ~ "zetaquira"
)


# For single factor
colmap_data <- merge(x = fa1_IDs, y = colmap, by.x = c("depto", "mpio_cnmbr"), 
                     by.y = c("NAME_1", "name_2"), all.y = TRUE)

colmap_data <- st_as_sf(colmap_data)

library(viridis)
ggplot() +
  coord_sf(ylim = c(-5, 13)) +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR1)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        legend.position = c(.8,.65)) +
  scale_fill_viridis_c() + labs(fill = "single\nfactor", 
                                caption = "Some municipios missing due to faulty merge.")
ggsave(filename = "FA1_map.png", height = 10, width = 10, device = "png", units = "in")

# for three-factor solution
fa_IDs$mpio_cnmbr <- tolower(fa_IDs$MPIO_CNMBR)

colmap_data <- merge(x = fa_IDs, y = colmap, by.x = c("depto", "mpio_cnmbr"), 
                     by.y = c("NAME_1", "name_2"), all.y = TRUE)

colmap_data <- st_as_sf(colmap_data)

ggplot() +
  coord_sf(ylim = c(-5, 13)) +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR1)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        legend.position = c(.8,.65)) +
  scale_fill_viridis_c() + labs(fill = "first\nof three\nfactors", 
                                caption = "Some municipios missing due to faulty merge.")
ggsave(filename = "FA1of3_map.png", height = 10, width = 10, device = "png", units = "in")

ggplot() +
  coord_sf(ylim = c(-5, 13)) +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR2)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        legend.position = c(.8,.65)) +
  scale_fill_viridis_c() + labs(fill = "second\nof three\nfactors", 
                                caption = "Some municipios missing due to faulty merge.")
ggsave(filename = "FA2of3_map.png", height = 10, width = 10, device = "png", units = "in")

ggplot() +
  coord_sf(ylim = c(-5, 13)) +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = MR3)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        legend.position = c(.8,.65)) +
  scale_fill_viridis_c() + labs(fill = "third\nof three\nfactors", 
                                caption = "Some municipios missing due to faulty merge.")
ggsave(filename = "FA3of3_map.png", height = 10, width = 10, device = "png", units = "in")

## a weighted average of all three dimensions
# .6MR2 + .3MR1 + .1MR3

colmap_data <- colmap_data %>%
  mutate(wt.3.6.1 = .3*MR1 - .6*MR2 + .1*MR3)

ggplot() +
  coord_sf(ylim = c(-5, 13)) +
  geom_sf(data = colmap_data, color="transparent", linewidth = 0.01, aes(fill = wt.3.6.1)) +
  theme_void() + 
  theme(panel.background = element_rect(color = "transparent", fill = "white"),
        legend.position = c(.8,.65)) +
  scale_fill_viridis_c() + labs(fill = "weighted\nmean of 3\nfactors", 
                                caption = "Some municipios missing due to faulty merge.")
ggsave(filename = "wt.3_.6.1_map.png", height = 10, width = 10, device = "png", units = "in")
