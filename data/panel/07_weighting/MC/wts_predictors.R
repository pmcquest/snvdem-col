# Combining weights and geocoded predictor variables

library(tidyverse)
library(haven)


weights <- read_dta("ELCLweights_wide.dta")
weights_col <- filter(weights, country_text_id == "COL" & year>1999)
# Average the weights for civil unrest and illicit activities
weights_col <- weights_col %>%
  mutate(wt_el_1011 = (el_Civil_unrest + el_Illicit_activity)/2,
         wt_cl_1011 = (cl_Civil_unrest + cl_Illicit_activity)/2)

# Which paired variables have high values that disfavor democracy?



predictors <- readRDS("C:/Users/mcoppedg/Dropbox/External (1)/CDF_averages_v1.rds")
names(predictors)
[1] "MPIO_CDPMP" "year"       "avg2t3"     "avg12"      "avg13"      "avg0t1"    
[7] "avg4t5"     "avg6"       "avg7"       "avg8"       "avg9"       "avg10t11"  
[13] "avg14"      "avg15t16"  

predictors %>%
  summarize(across(avg2t3:avg15t16, \(x) median(x, na.rm = TRUE)))
avg2t3     avg12     avg13    avg0t1    avg4t5      avg6      avg7      avg8      avg9
1 0.5053241 0.5000185 0.4933519 0.4999815 0.4999815 0.4999815 0.6110926 0.4999815 0.5857593
avg10t11     avg14 avg15t16
1 0.5001667 0.4999815 0.499963
# The median is about 0.5 for all predictors except avg7 (South) and avg9 (East).
# Create hi and lo versions of each paired variable.
predict_hilo <- predictors %>%
  mutate(avg0t1hi = ifelse(avg0t1>.5, avg0t1, 0),
         avg0t1lo = ifelse(avg0t1<=.5, avg0t1, 0),
         avg2t3hi = ifelse(avg2t3>.5, avg2t3, 0),
         avg2t3lo = ifelse(avg2t3<=.5, avg2t3, 0),
         avg4t5hi = ifelse(avg4t5>.5, avg4t5, 0),
         avg4t5lo = ifelse(avg4t5<=.5, avg4t5, 0),
         avg10t11hi = ifelse(avg10t11>.5, avg10t11, 0),
         avg10t11lo = ifelse(avg10t11<=.5, avg10t11, 0),
         avg15t16hi = ifelse(avg15t16>.5, avg15t16, 0),
         avg15t16lo = ifelse(avg15t16<=.5, avg15t16, 0))

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg2t3hi + avg2t3lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg4t5hi +avg4t5lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1hi + avg0t1lo, y = avg10t11hi +avg10t11lo)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg0t1, y = avg15t16)) +
  geom_point(alpha = .1) + geom_smooth()

ggplot(predict_hilo, aes(x = avg10t11, y = avg2t3)) +
  geom_point(alpha = .1) + geom_smooth()
# Looks very random.

# Check this version:
colvars_cdf <- read_rds("imputed_cdf_panel.rds")
colnames(colvars_cdf)
summary(colvars_cdf)

ggplot(colvars_cdf, aes(x = IndRur_0t1, y = DisBog_4t5)) +
  geom_point(alpha = .1) + geom_smooth()
# It looks pretty much the same.


# Proceed cautiously
# Merge predictors and weights
Indices <- merge(weights_col, predict_hilo, by = c("year"), all.y = TRUE)

write_dta(Indices, path = "Indices.dta")

# Constructing each index: FF elections
Indices_both <- Indices %>%
  mutate(snelect = (avg0t1hi*el_Urban+avg0t1lo*el_Rural+avg10t11*wt_el_1011+avg12*el_Sparse_population+avg13*el_Remote+avg14*(1-el_Indigenous)+avg15t16hi*(1-el_Ruling_party_strong)+avg15t16lo*(1-el_Ruling_party_weak)+avg2t3hi*el_More_development+avg2t3lo*el_Less_development+avg4t5hi*(1-el_Inside_capital)+avg4t5lo*(1-el_Outside_capital)+avg6*el_North+avg7*el_South+avg8*el_West+avg9*el_East)/(el_Urban+el_Rural+wt_el_1011+el_Sparse_population+el_Remote+(1-el_Indigenous)+(1-el_Ruling_party_strong)+(1-el_Ruling_party_weak)+el_More_development+el_Less_development+(1-el_Inside_capital)+(1-el_Outside_capital)+el_North+el_South+el_West+el_East), 
         sncivlib = (avg0t1hi*cl_Urban+avg0t1lo*cl_Rural+avg10t11*wt_cl_1011+avg12*cl_Sparse_population+avg13*cl_Remote+avg14*(1-cl_Indigenous)+avg15t16hi*(1-cl_Ruling_party_strong)+avg15t16lo*(1-cl_Ruling_party_weak)+avg2t3hi*cl_More_development+avg2t3lo*cl_Less_development+avg4t5hi*(1-cl_Inside_capital)+avg4t5lo*(1-cl_Outside_capital)+avg6*cl_North+avg7*cl_South+avg8*cl_West+avg9*cl_East)/(el_Urban+el_Rural+wt_el_1011+el_Sparse_population+el_Remote+(1-el_Indigenous)+(1-el_Ruling_party_strong)+(1-el_Ruling_party_weak)+el_More_development+el_Less_development+(1-el_Inside_capital)+(1-el_Outside_capital)+el_North+el_South+el_West+el_East))

write_dta(Indices, path = "Indices_both.dta")

# Visual exploration
ggplot(Indices_both, aes(x = snelect, y = sncivlib)) +
  geom_point(alpha = .1) +
  theme_light()

# Getting coordinates
viol <- readRDS("C:/Users/mcoppedg/Dropbox/External (1)/imputed_master_panel.rds")
coords <- select(viol, MPIO_CDPMP, year, ns_6t9, we_6t9)

Indices_both_coords <- merge(Indices_both, coords, by = c("MPIO_CDPMP", "year"), all.x = TRUE)

plot <- ggplot(filter(Indices_both_coords, year==2020), 
               aes(y=ns_6t9, x=-we_6t9, color=pnorm(snelect))) +
  geom_point(shape=16, size = 1, alpha = .7) +
    theme_classic() +
  scale_color_gradient(low = "red", high = "blue", limits = c(0.05,.85)) +
  labs(x = "East-West", y =" North-South", color = "SN election\nscores", 
       title="Subnational election index, 2020")
plot
ggsave(filename = "snelect2020.png", device = "png", height=6.5, width=6.5, units = "in")

plot <- ggplot(filter(Indices_both_coords, year==2020), 
               aes(y=ns_6t9, x=-we_6t9, color=sncivlib)) +
  geom_point(shape=16, size = 1, alpha = .7) +
  theme_classic() +
  scale_color_gradient(low = "red", high = "blue", limits = c(0.05,.85)) +
  labs(x = "East-West", y =" North-South", color = "SN civlib\nscores", 
       title="Subnational civil liberties index, 2020")
plot
ggsave(filename = "sncivlib2020.png", device = "png", height=6.5, width=6.5, units = "in")

