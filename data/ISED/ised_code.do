use "ised_data.dta", clear

*Set panel data
xtset statecode year
*
*Table 2
bys country_name: xtsum  year
*
*
*FIGURE 2, 6, 7 and 8, CREATED IN R 
*
*
*Create and country country-year mean estimates
bys country_id: egen country_mean_ised=mean(ised_i)
bys country_id: egen country_mean_vdemlevel=mean(subnat_elecfreefair_vdem)
*
label var country_mean_ised "Country Average of ISED (1 Value per country)"
label var country_mean_vdemlevel "Country Average of V-Dem Subnat Dem (1 Value per country)"
*
*
bys country_id year: egen country_year_mean_ised=mean(ised_i)
bys country_id year: egen country_year_mean_vdemlevel=mean(subnat_elecfreefair_vdem)
*
label var country_year_mean_ised "Country-Year Average of ISED (1 Value per year per country)"
label var country_year_mean_vdemlevel "Country-year Average of V-Dem Subnat Dem (1 Value per year per country)"
*
*
sort year
*
*
*IMPORTANT NOTE: I DO NOT HAVE THE AUTHORIZATION TO 
*SHARE V-DEM ITEMS WHICH IS WHY FIG 3 AND 4 CANNOT BE READILY REPRODUCED. 
*FOR TRANSPARENCEY HOWEVER, I INCLUDE THE CODE BELOW.
****FIGURE 3: ISED & V-DEM ITEMS — LEVELS
twoway(lowess country_year_mean_ised year if country_name=="Mexico", yaxis(1) ytitle("Index of Subnational Electoral Democracy", axis(1) size(small)))|| lowess  country_year_mean_vdemlevel year if country_name=="Mexico", yaxis(2) ytitle("V-Dem Level of Subnat. Dem.", axis(2) size (small)) scheme(lean1) leg( label(1 "ISED Country-Year Mean") label(2 "V-Dem Subnat. Dem Country-Year Mean ") pos(6) col(2) size(small)) subtitle("Mexico",ring(0) pos(10))
graph export fig3_quad1_mex.pdf
*
twoway(lowess country_year_mean_ised year if country_name=="Colombia", yaxis(1) ytitle("Index of Subnational Electoral Democracy", axis(1) size(small)))|| lowess  country_year_mean_vdemlevel year if country_name=="Colombia", yaxis(2) ytitle("V-Dem Level of Subnat. Dem.", axis(2) size (small)) scheme(lean1) leg( label(1 "ISED Country-Year Mean") label(2 "V-Dem Subnat. Dem Country-Year Mean ") pos(6) col(2) size(small)) subtitle("Colombia",ring(0) pos(10))
graph export fig3_quad2_col.pdf
*
twoway(lowess country_year_mean_ised year if country_name=="Venezuela", yaxis(1) ytitle("Index of Subnational Electoral Democracy", axis(1) size(small)))|| lowess  country_year_mean_vdemlevel year if country_name=="Venezuela", yaxis(2) ytitle("V-Dem Level of Subnat. Dem.", axis(2) size (small)) scheme(lean1) leg( label(1 "ISED Country-Year Mean") label(2 "V-Dem Subnat. Dem Country-Year Mean ") pos(6) col(2) size(small)) subtitle("Venezuela",ring(0) pos(10))
graph export fig3_quad3_Ven.pdf
*

twoway(lowess country_year_mean_ised year if country_name=="United States of America", yaxis(1) ytitle("Index of Subnational Electoral Democracy", axis(1) size(small)))|| lowess  country_year_mean_vdemlevel year if country_name=="United States of America", yaxis(2) ytitle("V-Dem Level of Subnat. Dem.", axis(2) size (small)) scheme(lean1) leg( label(1 "ISED Country-Year Mean") label(2 "V-Dem Subnat. Dem Country-Year Mean ") pos(6) col(2) size(small)) subtitle("United States of America",ring(0) pos(10))
graph export fig3_quad4_usa.pdf
*
*
*CREATE MEASURE OF HETEROGENEITY
bys country_id year: egen country_year_het_ised=sd(ised_i)
*
*
*TRANSFORM V-DEM ITEM FROM HOMOGENEITY TO HETEROGENEITY
gen vdem_het=1/subnat_demvar_vdem
*
label var country_year_het_ised "ISED Std Dev by country -year"
label var vdem_het "V-dem Unevenness in terms of Heterogeneity"
*
****
**** FIGURE 4: ISED & V-DEM ITEMS — UNNEVENNESS
twoway(lowess country_year_het_ised year if country_name=="India", yaxis(1) ytitle("ISED Country-Year Std. Dev.", axis(1) size(small)))||lowess vdem_het year if country_name=="India", yaxis(2) ytitle("V-Dem Subnat. Elect. Unevenness", axis(2) size (small)) leg(label(1 "ISED Country-Year Std. Dev.") label(2 "V-Dem Subnat. Electoral Unevenness ")pos(6) col(2) size(small)) subtitle("India",ring(0) pos(10.5)) scheme(lean1)
graph export fig4_quad1_india.pdf
*
twoway(lowess country_year_het_ised year if country_name=="Brazil", yaxis(1) ytitle("ISED Country-Year Std. Dev.", axis(1) size(small)))||lowess vdem_het year if country_name=="Brazil", yaxis(2) ytitle("V-Dem Subnat. Elect. Unevenness", axis(2) size (small)) leg(label(1 "ISED Country-Year Std. Dev.") label(2 "V-Dem Subnat. Electoral Unevenness ")pos(6) col(2) size(small)) subtitle("Brazil",ring(0) pos(10)) scheme(lean1)
graph export fig4_quad2_bra.pdf
*
twoway(lowess country_year_het_ised year if country_name=="Uruguay", yaxis(1) ytitle("ISED Country-Year Std. Dev.", axis(1) size(small)))||lowess vdem_het year if country_name=="Uruguay", yaxis(2) ytitle("V-Dem Subnat. Elect. Unevenness", axis(2) size (small)) leg(label(1 "ISED Country-Year Std. Dev.") label(2 "V-Dem Subnat. Electoral Unevenness ")pos(6) col(2) size(small)) subtitle("Uruguay",ring(0) pos(10)) scheme(lean1)
graph export fig4_quad3_uruguay.pdf
*
twoway(lowess country_year_het_ised year if country_name=="United States of America", yaxis(1) ytitle("ISED Country-Year Std. Dev.", axis(1) size(small)))||lowess vdem_het year if country_name=="United States of America", yaxis(2) ytitle("V-Dem Subnat. Elect. Unevenness", axis(2) size (small)) leg(label(1 "ISED Country-Year Std. Dev.") label(2 "V-Dem Subnat. Electoral Unevenness ")pos(6) col(2) size(small)) subtitle("United States of America",ring(0) pos(10)) scheme(lean1)
graph export fig4_quad4_usa.pdf
*
*
*
*FIGURE 5 — SUBNATIONAL DEMOCRATIC TRAJECTORIES
*
*ARGENTINA
tsline ised_i if statecode==110,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==111,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==112,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==113,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==114,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==115,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==116,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==117,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==118,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==119,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==120,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==121,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==122,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==123,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==124,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==125,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==126,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==127,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==128,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==129,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==132,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==133,  lp(solid) lcolor(gs14)|| ///
tsline ised_i if statecode==130,  lp(solid) lcolor(black)|| tsline ised_i if statecode==131,  lp(solid) lcolor(black)|| ///
tsline country_year_mean_ised if country_name=="Argentina", lp(solid) xtitle("Year") scheme(lean1) lcolor(blue) leg(off) ytitle("Index of Subnational Electoral Democracy", size(small)) subtitle("Argentina",ring(0) pos(10)) || ///
scatter country_year_mean_ised year if year==2018 & country_name=="Argentina", ms(none)  mlabc(black) mlabsize(vsmall) mlabel(country_name) mlabp(6) leg(off)|| ///
scatter ised_i year if year==1999 & statecode==130, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(12) leg(off) || ///
scatter ised_i year if year==1995 & statecode==131, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(6) leg(off)
graph export fig5_quad1_arg.pdf
*
*
*INDIA
 tsline ised_i if statecode==910,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==922,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==912,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==913,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==914,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==915,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==916,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==917,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==918,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==919,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==920,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==921,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==924,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==925,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==926,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==927,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==928,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==929,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==930,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==931,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==932,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==933,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==934,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==935,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==937,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==938,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==939,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==940,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==911,  lp(solid) lcolor(black) || tsline ised_i if statecode==923,  lp(solid) lcolor(black)|| ///
 tsline country_year_mean_ised if country_name=="India", lp(solid) xtitle("Year") scheme(lean1) lcolor(dkorange) leg(off) ytitle("Index of Subnational Electoral Democracy", size(small)) subtitle("India",ring(0) pos(10)) || ///
scatter country_year_mean_ised year if year==2018 & country_name=="India", ms(none)  mlabc(black) mlabsize(vsmall) mlabel(country_name) mlabp(6) leg(off)|| ///
scatter ised_i year if year==1999 & statecode==911, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(6) leg(off) || ///
scatter ised_i year if year==1999 & statecode==923, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(6) leg(off)
graph export fig5_quad2_india.pdf
*
*
*COLOMBIA
 tsline ised_i if statecode==510,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==511,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==512,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==513,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==514,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==516,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==517,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==518,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==519,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==520,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==521,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==542,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==523,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==524,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==525,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==526,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==527,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==528,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==529,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==530,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==531,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==532,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==533,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==534,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==535,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==536,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==541,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==538,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==539,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==540,  lp(solid) lcolor(gs14)|| ///
 tsline ised_i if statecode==537,  lp(solid) lcolor(black)|| tsline ised_i if statecode==522,  lp(solid) lcolor(black)|| ///
 tsline country_year_mean_ised if country_name=="Colombia", lp(solid) xtitle("Year") scheme(lean1) lcolor(cranberry) leg(off) ytitle("Index of Subnational Electoral Democracy", size(small)) subtitle("Colombia",ring(0) pos(10)) || ///
 scatter country_year_mean_ised year if year==2018 & country_name=="Colombia", ms(none)  mlabc(black) mlabsize(vsmall) mlabel(country_name) mlabp(6) leg(off)|| ///
scatter ised_i year if year==2015 & statecode==537, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(12) leg(off)|| ///
scatter ised_i year if year==2015 & statecode==522, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(6) leg(off) 
graph export fig5_quad3_colombia.pdf, replace
*
*
*VENEZUELA
tsline ised_i if statecode==701,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==702,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==703,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==706,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==707,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==708,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==709,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==710,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==711,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==712,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==713,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==714,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==715,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==716,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==717,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==718,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==719,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==720,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==723,  lp(solid) lcolor(gs14)|| tsline ised_i if statecode==722,  lp(solid) lcolor(gs14)||  ///
tsline ised_i if statecode==705,  lp(dash) lcolor(black)|| tsline ised_i if statecode==704, lp(solid) lcolor(black)|| ///
tsline country_year_mean_ised if country_name=="Venezuela", lp(solid) xtitle("Year") scheme(lean1) lcolor(red) leg(off) ytitle("Index of Subnational Electoral Democracy", size(small)) subtitle("Venezuela",ring(0) pos(10)) || ///
scatter country_year_mean_ised year if year==2020 & country_name=="Venezuela", ms(none)  mlabc(black) mlabsize(vsmall) mlabel(country_name) mlabp(9) leg(off)|| ///
scatter ised_i year if year==2008 & statecode==705, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(12) leg(off)|| ///
scatter ised_i year if year==2000 & statecode==704, ms(none) mlabc(black) mlabsize(vsmall) mlabel(state_name) mlabp(6) leg(off) 
*
graph export fig5_quad4_ven.pdf, replace
*
*
*
*FIGURE 7: REGIME CLASSIFICATION
sum competition_i participation_i if country_name=="Brazil"
*
gen reg_class_bra=.
recode reg_class_bra (.=1) if competition_i>=58.70 & participation_i>=35.15 & country_name=="Brazil"
recode reg_class_bra (.=2) if competition_i>=58.70 & participation_i<35.15 & country_name=="Brazil"
recode reg_class_bra (.=3) if competition_i>=38.17 & competition_i<58.70 & participation_i>=35.15 & country_name=="Brazil"
recode reg_class_bra (.=4) if competition_i>=38.17 & competition_i<58.70 & participation_i<35.15 & country_name=="Brazil"
recode reg_class_bra (.=5) if competition_i<38.17 & participation_i>=35.15 & country_name=="Brazil"
recode reg_class_bra (.=6) if competition_i<38.17 & participation_i<35.15 & country_name=="Brazil"
*
label def regtype 1"Democracy" 2"Competitive Oligarchy" 3"Electoral Authoritarianism" 4"Electoral Oligarchy" 5"Plebiscitary Authoritarianism" 6"Closed Authoritarianism"
*
label val reg_class_bra regtype
label var reg_class_bra "Regim Classification Brazil Val. Only"
*
save "ised_data_2.dta", replace
*
*FOR THE REST OF THE FIGURES
*THE CODE BELOW RUNS IN R
*
*
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(dplyr)
library(cowplot)
#
iod_data<-read_dta("ised_data_2.dta")
#
#FIGURE 2: VALIDITY 
#
#IMPORTANT NOTE: I DO NOT HAVE THE AUTHORIZATION TO 
#SHARE ALTERNATIVE MEASURES (BEER,GRUMBACH, ETC..)
#WHICH IS WHY THESE VALIDITY GRAPHS CANNOT BE READILY REPRODUCED. 
#FOR TRANSPARENCEY HOWEVER, I INCLUDE THE CODE BELOW.
#
#LATAM
ised_data %>% 
  filter(country_id %in% c(1,2,3)) %>% 
  ggplot(aes(x=mean_altindx_latam,y=mean_ised))+
  geom_point(aes(shape=country_name),color="grey30", size=1)+
  geom_smooth(method="lm", color="darkred")+
  stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, label.x=0.70, label.y=9)+
  geom_text_repel(aes(label=ifelse(year==1993 & state_name %in% c("Santa Fe",
                                                                  "Buenos Aires (Ciudad)","Hidalgo","Guerrero",
                                                                  "Campeche","Baja California","Yucatan") , paste(state_label),'')), force = 1)+
  geom_text_repel(aes(label=ifelse(year==2014 & state_name %in% c("Buenos Aires (Ciudad)") , paste(state_label),'')), force = 1)+
  theme_classic()+
  labs(
    subtitle = "Argentina, Brazil and Mexico",
    y= "Subnational Democracy Index (Mean)",
    x= "Alternative Index (Mean)",
    shape="Country"
  )+
  theme(legend.position = c(0.1, 0.8))
#USA
ised_data %>% 
  filter(mean_ised<25) %>% 
  ggplot(aes(x=mean_altindx_usa,y=mean_ised))+
  geom_point(shape=1,color="grey30", size=1)+
  geom_smooth(method="lm", color="navyblue")+
  stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, label.x=0.40, label.y=8)+
  geom_text_repel(aes(label=ifelse(year==1982 & state_name %in% c("South Carolina",
                                                                  "Missouri","Tenessee","Georgia",
                                                                  "Colorado","Maine","Washington") , paste(state_label),'')), force = 1)+
  theme_classic()+
  labs(
    subtitle = "United States of America",
    y= "Subnational Democracy Index (Mean)",
    x= "Alternative Index (Mean)"
  )  
#INDIA
ised_data %>% 
  filter(country_name=="India") %>% 
  ggplot(aes(x=mean_altindx_fidalgo ,y=mean_ised))+
  geom_point(shape=1, color="grey30", size=1)+
  geom_smooth(method="lm", color="darkorange2")+
  stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, label.x=0.5, label.y=15)+
  geom_text_repel(aes(label=ifelse(year==1980 & mean_altindx_fidalgo<0 & mean_ised<20 , paste(state_label),'')), force = 1)+
  geom_text_repel(aes(label=ifelse(year==1980 & mean_altindx_fidalgo>0 & mean_ised>=29 , paste(state_label),'')), force = 1)+
  theme_classic() +
  labs(
    subtitle = "India",
    y= "Subnational Democracy Index (Mean)",
    x= "Alternative Index (Mean)"
  )   
#POOLED
ised_data %>% 
  filter(country_name %in% c("Argentina","Brazil","Colombia","India",
                             "Mexico","United States of America")) %>% 
  ggplot(aes(x=overall_alt_index_z,y=ised_z))+
  geom_point(aes(shape=country_name), color="grey30", size=1)+
  geom_smooth(method="lm", color="darkblue")+
  stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, label.x=0.79, label.y=0.02)+
  theme_classic()+
  labs(
    subtitle = "Standardized and Pooled",
    y= "Subnational Democracy Index (Mean)",
    x= "Alternative Indices (Mean)",
    shape="Country") +
  theme(legend.position="bottom")
#
#FIGURE 6: MEXICAN STATES
#
ised_data %>% 
  filter(country_id==3)%>% 
  ggplot(aes(x=year))+
  geom_smooth(aes(y=competition_i,color="darkred"))+
  geom_smooth(aes(y=participation_i, color="purple3"),
              linetype="twodash")+
  facet_wrap(~state_name)+
  scale_y_continuous(name="Competition",
                     sec.axis = sec_axis( trans=~.*1, name="Participation"))+ 
  labs(x="Year",
       color="Democratic Components")+
  scale_color_identity(guide="legend",
                       breaks=c("darkred","purple3"),
                       labels=c("Competition","Participation"))+
  theme_classic()+
  guides(color=guide_legend(ncol=2,override.aes=list(fill=NA)))+
  theme(legend.position = c(0.55, 0.06))ised_data %>% 
  filter(country_id==3)%>% 
  ggplot(aes(x=year))+
  geom_smooth(aes(y=competition_i,color="darkred"))+
  geom_smooth(aes(y=participation_i, color="purple3"),
              linetype="twodash")+
  facet_wrap(~state_name)+
  scale_y_continuous(name="Competition",
                     sec.axis = sec_axis( trans=~.*1, name="Participation"))+ 
  labs(x="Year",
       color="Democratic Components")+
  scale_color_identity(guide="legend",
                       breaks=c("darkred","purple3"),
                       labels=c("Competition","Participation"))+
  theme_classic()+
  guides(color=guide_legend(ncol=2,override.aes=list(fill=NA)))+
  theme(legend.position = c(0.55, 0.06))
#
#
#FIGURE 8: US INDIA GDP
#
india_ised_data %>% 
  ggplot(aes(x=log_sgdp,y=ised_i))+
  geom_point(shape=1,color="grey30", size=1)+
  stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, label.x=7.5, label.y=8)+
  geom_smooth(method = "lm", color="darkorange2")+
  labs(x="GDP per capita PPP (log)",
       y="Index of Subnational Electoral Democracy",
       subtitle = "India")+
  theme_classic()

us_ised_data %>% 
  ggplot(aes(x=log_gdppcppp,y=ised_i, colour=ds_text))+
  geom_point(shape=1,color="grey30", size=1)+
  geom_smooth(aes(colour=ds_text), method = "lm")+
  scale_colour_manual(values=c("darkmagenta","turquoise4"))+
  stat_cor(aes(colour=ds_text),method="pearson", p.accuracy = 0.001, r.accuracy = 0.01,label.x=15.6, show.legend = FALSE)+
  labs(x="GDP per capita PPP (log)",
       y="Index of Subnational Electoral Democracy",
       subtitle = "United States",
       colour="Deep South Dummy")+
  theme_classic()+
  guides(color=guide_legend(ncol=2,override.aes=list(fill=NA)))+
  theme(legend.position = c(0.75, 0.09))
