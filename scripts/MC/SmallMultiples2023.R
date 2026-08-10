###                 Small Multiples             ###

# 1. Wrapped facets
# 2. Facets in a grid
# 3. Scatterplot matrix
# 4. Combining different kinds of graphs

rm(list=ls())
library(tidyverse)
library(haven)
setwd('C:/Users/mcoppedg/Dropbox/VisualizingPolitics/Data')

#Questions about political parties, US and abroad
#How have GOP and Dems evolved on L-R scale historically?
#vparty <- read_dta("V-Party.dta")
vparty <- read_dta("V-Party_v2.dta")
vpUS <- filter(vparty, country_name=="United States of America" & v2pashname %in% c("Dem", "Rep") & year>=1970)
#vpUS <- zap_labels(vpUS)

#MM vs. ordinal version of the left-right variable
ggplot(vparty, aes(x=v2pariglef, y=v2pariglef_ord)) +
  geom_point()

#Introduction to position variables one at a time
ggplot(vpUS, aes(y=v2pariglef, x=year, color=v2pashname)) +
         geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2pariglef_codehigh, ymin=v2pariglef_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Economic Left (low)-Right (high) Scale", y="score",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))

ggsave(filename = "riglef.png", device = "png", height = 6, width = 8, 
       units = "in")
#How have they evolved on other dimensions?
#Anti-elitism
ggplot(vpUS, aes(y=v2paanteli, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paanteli_codehigh, ymin=v2paanteli_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Anti-elitism", y="score", 
       caption = "How important is anti-elite rhetoric for this party?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "anteli.png", device = "png", height = 6, width = 8, 
       units = "in")

#People-centrism
ggplot(vpUS, aes(y=v2papeople, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2papeople_codehigh, ymin=v2papeople_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="People-centrism", y="score",
       caption = "Do leaders of this party glorify the ordinary people and identify themselves as part of them?", 
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "people.png", device = "png", height = 6, width = 8, 
       units = "in")

#Non-demonization of opponents
ggplot(vpUS, aes(y=v2paopresp, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paopresp_codehigh, ymin=v2paopresp_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Non-demonization of opponents", y="score",
       caption = "Prior to this election, have leaders of this party used severe personal attacks or tactics of
demonization against their opponents?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "opresp.png", device = "png", height = 6, width = 8, 
       units = "in")

#Political Pluralism
ggplot(vpUS, aes(y=v2paplur, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paplur_codehigh, ymin=v2paplur_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Political pluralism", y="score",
       caption = "Prior to this election, to what extent was the leadership of this political party clearly committed to\nfree and fair elections with multiple parties, freedom of speech, media, assembly and association?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "plur.png", device = "png", height = 6, width = 8, 
       units = "in")

#Minority rights
ggplot(vpUS, aes(y=v2paminor, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paminor_codehigh, ymin=v2paminor_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Minority rights", y="score",
       caption = "According to the leadership of this party, how often should the will of the majority\nbe implemented even if doing so would violate the rights of minorities?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "minor.png", device = "png", height = 6, width = 8, 
       units = "in")

#Rejection of political violence
ggplot(vpUS, aes(y=v2paviol, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paviol_codehigh, ymin=v2paviol_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Rejection of political violence", y="score",
       caption = "To what extent does the leadership of this party\nexplicitly discourage the use of violence against domestic political opponents?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "viol.png", device = "png", height = 6, width = 8, 
       units = "in")

#Support for immigration
ggplot(vpUS, aes(y=v2paimmig, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paimmig_codehigh, ymin=v2paimmig_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Support for immigration", y="score",
       caption = "What is the party's position regarding immigration into the country?\n[high=strongly supports]",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "immig.png", device = "png", height = 6, width = 8, 
       units = "in")

#LGBT social equality
ggplot(vpUS, aes(y=v2palgbt, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2palgbt_codehigh, ymin=v2palgbt_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Support for LGBT social equality", y="score",
       caption = "What is this party's position toward social equality for\nthe lesbian, gay, bisexual, and transgender (LGBT) community?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "lgbt.png", device = "png", height = 6, width = 8, 
       units = "in")

#Cultural superiority
ggplot(vpUS, aes(y=v2paculsup, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2paculsup_codehigh, ymin=v2paculsup_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Cultural superiority", y="score",
       caption = "To what extent does the party leadership promote the cultural superiority\nof a specific social group or the nation as a whole?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "culsup.png", device = "png", height = 6, width = 8, 
       units = "in")

#Religious principles
ggplot(vpUS, aes(y=v2parelig, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2parelig_codehigh, ymin=v2parelig_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Religious principles", y="score",
       caption = "To what extent does this party invoke God, religion, or\nsacred/religious texts to justify its positions?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "relig.png", device = "png", height = 6, width = 8, 
       units = "in")

#Gender equality
ggplot(vpUS, aes(y=v2pagender, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2pagender_codehigh, ymin=v2pagender_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Gender equality", y="score",
       caption = "What is the share of women in national-level leadership positions of this political party?\n[high=balanced]",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4))
ggsave(filename = "gender.png", device = "png", height = 6, width = 8, 
       units = "in")


#Anti-pluralism
ggplot(vpUS, aes(y=v2xpa_antiplural, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2xpa_antiplural_codehigh, ymin=v2xpa_antiplural_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Antipluralism", y="score",
       caption = "To what extent does the party show a lacking commitment to\ndemocratic norms prior to elections?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "antiplur.png", device = "png", height = 6, width = 8, 
       units = "in")

#Populism
ggplot(vpUS, aes(y=v2xpa_popul, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  geom_ribbon(aes(ymax=v2xpa_popul_codehigh, ymin=v2xpa_popul_codelow , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", title="Populism", y="score",
       caption = "To what extent do representatives of the party use populist rhetoric (narrowly defined)?",
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(0,1))
ggsave(filename = "popul.png", device = "png", height = 6, width = 8, 
       units = "in")

#Making a data frame to pivot for faceting
vpUS_med <- select(vpUS, "v2paenname", "v2pashname", "year", "v2pariglef", "v2paanteli", "v2papeople", "v2paopresp", "v2paplur", "v2paminor", "v2paviol", "v2paimmig", "v2palgbt", "v2paculsup", "v2parelig", "v2pagender")
vpUS_lo <- select(vpUS, "v2paenname", "v2pashname", "year", "v2pariglef_codelow", "v2paanteli_codelow", "v2papeople_codelow", "v2paopresp_codelow", "v2paplur_codelow", "v2paminor_codelow", "v2paviol_codelow", "v2paimmig_codelow", "v2palgbt_codelow", "v2paculsup_codelow", "v2parelig_codelow", "v2pagender_codelow")
vpUS_hi <- select(vpUS, "v2paenname", "v2pashname", "year", "v2pariglef_codehigh", "v2paanteli_codehigh", "v2papeople_codehigh", "v2paopresp_codehigh", "v2paplur_codehigh", "v2paminor_codehigh", "v2paviol_codehigh", "v2paimmig_codehigh", "v2palgbt_codehigh", "v2paculsup_codehigh", "v2parelig_codehigh", "v2pagender_codehigh")

vpUS_med_long <- vpUS_med %>%
  pivot_longer(-c(v2paenname, v2pashname, year),
               names_to = "position",
               values_to = "med")

vpUS_lo_long <- vpUS_lo %>%
  pivot_longer(-c(v2paenname, v2pashname, year),
               names_to = "position",
               values_to = "lo")

vpUS_hi_long <- vpUS_hi %>%
  pivot_longer(-c(v2paenname, v2pashname, year),
               names_to = "position",
               values_to = "hi")

vpUS_positions <- cbind(vpUS_med_long, vpUS_lo_long$lo, vpUS_hi_long$hi)
vpUS_positions <- rename(vpUS_positions, lo = "vpUS_lo_long$lo", hi = "vpUS_hi_long$hi")
vpUS_positions <- arrange(vpUS_positions, position, year)
table(vpUS_positions$position)

vpUS_positions$position <- factor(vpUS_positions$position, 
               levels = c("v2paanteli", "v2paculsup", "v2pagender", "v2paimmig", "v2palgbt", "v2paminor", "v2paopresp", "v2papeople", "v2paplur", "v2parelig", "v2pariglef", "v2paviol"), 
               labels = c("anti-elitism", "cultural superiority", "gender equality", "support for immigration", "support for LGBT rights", "minority rights", "non-demonization\nof opponents", "people-centrism", "political pluralism", "religious principles", "economic left-right", "rejection of\npolitical violence"))

#Making a facet plot
ggplot(vpUS_positions, aes(y=med, x=year, color=v2pashname)) +
  geom_line() + theme_light() +
  facet_wrap(~ reorder(position, med), ncol = 4) +
  geom_ribbon(aes(ymax=hi, ymin=lo , 
                  fill=v2pashname), color = "white", alpha=.3) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x="", y="score", 
       color=NULL, fill=NULL) +
  scale_y_continuous(limits = c(-4,4)) +
  theme(strip.text = element_text(size=rel(1), color="black"), 
        strip.background=element_rect(fill="cornsilk")) 
ggsave(filename = "positions_facet.png", device = "png", height = 8, width = 11, 
       units = "in")


###          Regional Comparisons

#Latin America & the US
#Wrangling first
vparty <- V_Dem_CPD_Party_V1
vpartyAms <- filter(vparty, e_regionpol==2 | country_text_id=="USA")
vparty2Amsrecent <- filter(vpartyAms, year>=2015)
write.csv(vparty2Amsrecent, file="vparty2Amsrecent.csv")
#Before this step I exported to csv to delete unwanted observations 
#and rename the US "United States".
vpartyAmsrecent <- read.csv("vparty2Amsrecent.csv")
vpartyAmsrecent <- filter(vpartyAmsrecent, !is.na(v2pariglef_ord)) %>%
  mutate(cntyr = paste(country_name, year))
vpartyAmsrecent$v2pariglef_ord <- factor(vpartyAmsrecent$v2pariglef_ord,
                                         labels = c("L", "CL", "C", "CR", "R"))

#without facets
ggplot(vpartyAmsrecent,  
       aes(v2pariglef, v2xpa_popul)) +
  geom_text(aes(label = v2pashname, color=v2pariglef_ord), size = 3.8) + 
  labs(x="left-right position", y="degree of populism") +
  theme_light() +  guides(color = "none") +
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_manual(values = c("firebrick", "red", "purple", "blue", "black")) 
ggsave(filename = "party2_no_facet.png", device = "png", height = 8, width = 11, 
       units = "in") 

#no facets, color by country
library(viridis)

ggplot(vpartyAmsrecent,  
       aes(v2pariglef, v2xpa_popul)) +
  geom_text(aes(label = v2pashname, color=country_name), size = 3.8) + 
  labs(x="left-right position", y="degree of populism", color = NULL) +
  theme_light() +  
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_viridis_d()
ggsave(filename = "party2_no_facet.png", device = "png", height = 8, width = 11, 
       units = "in") 

#With facets by country-years
ggplot(vpartyAmsrecent,  
       aes(v2pariglef, v2xpa_popul)) +
  geom_text(aes(label = v2pashname, color=v2pariglef_ord), size = 3.8) + 
  labs(x="left-right position", y="degree of populism") +
  facet_wrap(~ cntyr, nrow=4) +
  theme_light() +  guides(color = "none") +
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_manual(values = c("firebrick", "red", "purple", "blue", "black")) +
  theme(strip.text = element_text(size=rel(1), color="blue2"), 
        strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet.png", device = "png", height = 8, width = 11, 
       units = "in")


#Europe & the US
#Wrangling first
vpartyEur <- filter(vparty, e_regionpol==5 | country_text_id=="USA")
vpartyEurrecent <- filter(vpartyEur, year>=2015)
write.csv(vpartyEurrecent, file="vparty2Eurrecent.csv")
#Before this step I exported to csv to delete unwanted observations 
#and rename the US "United States".
vpartyEurrecent <- read.csv("vparty2Eurrecent.csv")
vpartyEurrecent <- filter(vpartyEurrecent, !is.na(v2pariglef_ord)) %>%
  mutate(cntyr = paste(country_name, year))
vpartyEurrecent$v2pariglef_ord <- factor(vpartyEurrecent$v2pariglef_ord,
                                         labels = c("FL", "L", "CL", "C", "CR", "R", "FR"))
ggplot(vpartyEurrecent,  
       aes(v2pariglef, v2xpa_popul)) +
  geom_text(aes(label = v2pashname, color=v2pariglef_ord), size = 3.8) + 
  labs(x="left-right position", y="degree of populism") +
  facet_wrap(~ cntyr, nrow=4) +
  theme_light() +  guides(color = "none") +
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_manual(values = c("sienna", "firebrick", "red", "purple", "blue", "blue3", "black")) +
  theme(strip.text = element_text(size=rel(1), color="blue2"), 
        strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet.png", device = "png", height = 8, width = 11, 
       units = "in")

#Substituting anti-pluralism for populism
ggplot(vpartyEurrecent,  
       aes(v2pariglef, v2xpa_antiplural)) +
  geom_text(aes(label = v2pashname, color=v2pariglef_ord), size = 3.8) + 
  labs(x="left-right position", y="degree of anti-pluralism") +
  facet_wrap(~ cntyr, nrow=4) +
  theme_light() +  guides(color = "none") +
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_manual(values = c("sienna", "firebrick", "red", "purple", "blue", "blue3", "black")) +
  theme(strip.text = element_text(size=rel(1), color="blue2"), 
        strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet2.png", device = "png", height = 8, width = 11, 
       units = "in")


#Populism vs. anti-pluralism, colors by L-R position
ggplot(vpartyEurrecent,  
       aes(v2xpa_popul, v2xpa_antiplural)) +
  geom_text(aes(label = v2pashname, color=v2pariglef_ord), size = 3.8) + 
  labs(x="degree of populism", y="degree of anti-pluralism") +
  facet_wrap(~ cntyr, nrow=4) +
  theme_light() +  guides(color = "none") +
  scale_x_continuous(expand = expansion(mult = c(.09, .09))) +
  scale_color_manual(values = c("sienna", "firebrick", "red", "purple", "blue", "blue3", "black")) +
  theme(strip.text = element_text(size=rel(1), color="blue2"), 
        strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet3.png", device = "png", height = 8, width = 11, 
       units = "in")



# 2. Facets arranged on a grid defined by two factor variables

#Limit the dataframe to three regions
regions2 <- filter(vparty, e_regionpol==2 | e_regionpol==5)
regions2$e_regionpol <- factor(regions2$e_regionpol,
                               labels = c("Latin America & Caribbean", "W. Europe & N. America"))

#Create a factor variable for three historical periods
regions2 <- filter(regions2, year>1947) %>%
  mutate(period = ifelse(year>=1948 & year<1988, "Cold War", "Post-Cold War"))

#Basic
#Syntax: facet_grid(rowvariable ~ columnvariable)
# Here are some histograms in a grid.

#First tabulate L-R positions by region and period:
LRtab <- as.data.frame(with(regions2, prop.table(table(e_regionpol, period, v2pariglef_ord), 
                                                 margin = c(1,2))))
#Label the categories
LRtab$v2pariglef_ord <- factor(LRtab$v2pariglef_ord,
                     labels = c("Far left", "Left", "Center-Left", 
                                "Center", "Center-Right", "Right", "Far Right"))

#Use facet_grid() to make the plot  
ggplot(LRtab, aes(x=v2pariglef_ord, y=Freq*100)) + 
  geom_bar(stat = "identity", fill="firebrick") +
  facet_grid(e_regionpol ~ period) +   theme_light() +
  labs(y="percent", x="", caption="All parties weighted equally") + 
  ggtitle("Left-Right Distribution of Parties by Region and Historical Period") + 
theme(strip.text = element_text(size=rel(1), color="black"), 
      strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet_hist.png", device = "png", height = 6, width = 10, 
       units = "in")

#You can put ANY kind of geom in the facets!
ggplot(regions2, aes(v2xpa_antiplural, v2xpa_popul)) + 
  geom_point() +
  facet_grid(e_regionpol ~ period) +
  ggtitle("Antipluralism and Populism by Region and Historical Period")

#Prettier, adding colors for party positions
ggplot(regions2, aes(v2xpa_antiplural, v2xpa_popul, color=as.factor(v2pariglef_ord))) + 
  geom_point() +
  facet_grid(e_regionpol ~ period) +
  ggtitle("Antipluralism and Populism by Region and Historical Period") +
labs(x="degree of antipluralism", y="degree of populism") +
  theme_light() +  guides(color = "none") +
  scale_color_manual(values = c("sienna", "firebrick", "red", "purple", "blue", "blue3", "black")) +
theme(strip.text = element_text(size=rel(1), color="blue2"), 
      strip.background=element_rect(fill="cornsilk")) 

ggsave(filename = "party_facet_grid.png", device = "png", height = 8, width = 11, 
       units = "in")


#                  3. Scatterplot Matrix

# Chang says to use pairs() in base R, but 
# ggplot2 can now use a function from a new package.
install.packages("GGally")
library(GGally)

#  How are votes and seats related? Does it depend on ideology?
ggpairs(vparty, columns = c("v2pavote", "v2paseatshare",
                          "v2pariglef"))
# Note: In ggpairs(), everything goes inside 
# the main parentheses. No +s.

#  Inserting variable labels & placing them at left and bottom
ggpairs(vparty, columns = c("v2pavote", "v2paseatshare",
                            "v2pariglef"),
        columnLabels = c("Vote share", 
                         "Seat share", 
                         "Left-right position"),
        switch = "both")

#  Adding a title
ggpairs(vparty, columns = c("v2pavote", "v2paseatshare",
                            "v2pariglef"),
        columnLabels = c("Vote share", 
                         "Seat share", 
                         "Left-right position"),
        switch = "both",
        title="Party positions and sizes")

#  With color, without correlations
ggpairs(vparty, columns = c("v2pavote", "v2paseatshare",
                            "v2pariglef"),
        columnLabels = c("Vote share", 
                         "Seat share", 
                         "Left-right position"),
        switch = "both",
        title="Party positions and sizes",
        ggplot2::aes_(color=~as.factor(v2pariglef_ord), alpha=~.1))

ggsave(filename = "matrix.png", device = "png", height = 8, width = 11, 
       units = "in")

#Or with more variables
ggpairs(regions2, columns = c("v2pariglef", "v2paimmig",
                            "v2palgbt", "v2pagender", "v2pawelf" ),
        columnLabels = c("Left-right position",  
                         "Support for immigration", "Support for LGBT", 
                         "Support for women", "Support for welfare"),
        switch = "both",
        title="Party positions",
        ggplot2::aes_(alpha=~.05))


ggsave(filename = "matrix6.png", device = "png", height = 11, width = 12, 
       units = "in")
#                              4. Combining graphs

# For a great guide to combining simple (non-ggplot2) 
# plots of different types into one graph, see
#  https://www.statmethods.net/advgraphs/layout.html
# For more complex instructions using ggplot graphs 
# (as below), see
#  https://rpubs.com/MarkusLoew/13295. 
# The "top=" option allows for a common title for the 
# combined graphs.
# For advanced instructions using ggarrange in ggpubr, 
# see
#  https://www.r-bloggers.com/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

# Simple 2x2 example
install.packages("gridExtra")
library(gridExtra) #Conflicts with dplyr. 
#You may have to reload ggplot2.
library(ggplot2)

# Basic
lineR <- ggplot(filter(vpUS, v2pashname=="Rep"),
               aes(year, v2pavote)) +
  geom_line(color="red2") +
  scale_y_continuous(limits=c(40,57)) +
  geom_smooth(color="red", se=FALSE) +
  theme_light() + labs(x="", y="Vote share", title="Republican")
lineR

lineD <- ggplot(filter(vpUS, v2pashname=="Dem"),
                aes(year, v2pavote)) +
  geom_line(color="blue2") +
  scale_y_continuous(limits=c(40,57)) +
  geom_smooth(color="blue", se=FALSE) +
  theme_light() + labs(x="", y="Vote share", title="Democratic")
lineD

seatR <- ggplot(filter(vpUS, v2pashname=="Rep"),
                aes(year, v2paseatshare)) +
  geom_line(color="red2") +
  scale_y_continuous(limits=c(30,70)) +
  geom_smooth(color="red", se=FALSE) +
  theme_light() + labs(x="", y="Seat share")
seatR

seatD <- ggplot(filter(vpUS, v2pashname=="Dem"),
                aes(year, v2paseatshare)) +
  geom_line(color="blue2") +
  scale_y_continuous(limits=c(30,70)) +
  geom_smooth(color="blue", se=FALSE) +
  theme_light() + labs(x="", y="Seat share")
seatD

ratioR <- ggplot(filter(vpUS, v2pashname=="Rep"),
                aes(year, v2paseatshare/v2pavote)) +
  geom_point(color="red2") +
  geom_abline(intercept=1, slope=0) +
  scale_y_continuous(limits=c(.75,1.25)) +
  geom_smooth(color="red2", fill="red", alpha = .5) +
  theme_light() + labs(x="", y="Ratio of seats to votes")
ratioR

ratioD <- ggplot(filter(vpUS, v2pashname=="Dem"),
                aes(year, v2paseatshare/v2pavote)) +
  geom_point(color="blue2") +
  geom_abline(intercept=1, slope=0) +
  scale_y_continuous(limits=c(.75,1.25)) +
  geom_smooth(color = "blue2", fill="blue", alpha=.5) +
  theme_light() + labs(x="", y="Ratio of seats to votes")
ratioD

Combined <- grid.arrange(lineR, lineD, seatR, seatD, ratioR, ratioD, 
                      nrow=3, top="Electoral System Biases")

ggsave(Combined, filename = "combined.png", device = "png", 
       height = 11, width = 12, units = "in")