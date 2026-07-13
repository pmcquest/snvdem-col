# Takes full_forest.Rdata, full_upsampled.Rdata, merges with data on economic, demographic, and political units to
# produce analysis datasets at 0.05dd and 0.5dd scales
# Requires:
#     - full_forest.Rdata
#     - full_updsampled.Rdata
#     - polityIV data (2015 release, https://www.systemicpeace.org/inscrdata.html)
#     - Database of Political Institutions (Downloaded 2018, https://datacatalog.worldbank.org/dataset/wps2283-database-political-institutions)
#     - VDem dataset (Downloaded 2018, https://www.v-dem.net/en/)
#     - World Development Indicators (Downloaded 2018, https://databank.worldbank.org/reports.aspx?source=world-development-indicators)
#     Indicators downloaded: "NV.AGR.TOTL.ZS","SL.AGR.EMPL.ZS","SP.POP.GROW","SP.POP.TOTL","NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD"
#     - International Political Economy Data Resource (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28003)
# Produces:
#     - full.Rdata
#     - full_upsampled_10.Rdata


# if not already installed
# install.packages("tidyverse","readxl","countrycode","foreign","data.table")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(readxl)
library(countrycode)
library(foreign)
library(data.table)

# Load forest data and country names
load("../data/output/full_forest.Rdata")

# rename a couple of variables
defo_dat<-master %>% rename(forest=tot) %>%dplyr::select(x,y,forest,area,year,nn_forest,Countryeng,GID_1,GID_2,GID_0,FID,GEZ_TERM,Unsdcode)
defo_dat<-rename(defo_dat,un=Unsdcode) %>% mutate(un = as.integer(un))

### fix countries ###
#fix code
defo_dat <- defo_dat %>%
    mutate(un=if_else(Countryeng=="Sudan",as.integer(729),un)) %>%
    mutate(un=if_else(Countryeng=="Kazakhstan",as.integer(398),un))


# Fill in missing years with average of surrounding years, means forest.diff for 1994=1995 and 2000=2001. This works against finding an effect.
y1993<-defo_dat %>% filter(year==1993)
y1995<-defo_dat %>% filter(year==1995)
y1994<-y1993 %>% mutate(forest = (y1995$forest+.$forest)/2) %>% mutate(nn_forest=(y1995$nn_forest+.$nn_forest)/2) %>% mutate(year=1994)

y1999<-defo_dat %>% filter(year==1999)
y2001<-defo_dat %>% filter(year==2001)
y2000<-y1999 %>% mutate(forest = (y2001$forest+.$forest)/2) %>% mutate(nn_forest=(y2001$nn_forest+.$nn_forest)/2) %>% mutate(year=2000)

defo<-rbind(defo_dat,y2000,y1994)

rm(y1993,y1994,y1995,y1999,y2000,y2001,defo_dat,country_names,master)

# get first differenced measure and lagged forest and neighbor forest
defo %<>% group_by(FID) %>%
    arrange(year) %>%
    mutate(forest.diff=forest-dplyr::lag(forest)) %>%
    mutate(forest.l = dplyr::lag(forest)) %>%
    mutate(nn_forest.l=dplyr::lag(nn_forest))%>%
    mutate(defo=1)

#### Polity ####
# read in Polity IV data
p4 <- read_excel("../data/Elections/p4v2015.xls",sheet = 1) %>% 
    dplyr::select(ccode,scode,year,polity2) %>% 
    filter(year>=1982) %>%
    mutate(un=countrycode(ccode,"cown","un")) %>%
    mutate(polity=1)

#### DPI ####
# read in DPI data
dpi<-read.dta("../data/Elections/DPI2015_stata12.dta") %>%
    filter(year>=1982) %>%
    filter(year<=2016) %>% 
    # Select relevant variables
    dplyr::select(countryname, ifs, year, liec, eiec, mdmh, mdms, pluralty, pr, percent1, 
                  percentl, dateleg, dateexec, legelec, exelec, totalseats, numgov, numvote, oppvote, maj) %>%
    replace(.,.==-999,NA) %>%
    replace(.,.==888,NA) %>%
    # get DPI measure of margin
    mutate(margin=numvote-oppvote) %>%
    # set election equal to 1 if there was a legislative or executive election
    mutate(election_DPI=as.integer(legelec==1 | exelec==1)) %>%
    mutate(margin=replace(margin,election_DPI==1,NA)) %>%
    mutate(maj=replace(maj,election_DPI==1,NA)) %>%
    mutate(margin=replace(margin,numvote==0 & oppvote==0,NA)) %>%
    mutate(etype = if_else(legelec==1 & exelec==0, "leg",
                                      if_else(legelec==0 & exelec==1,"exec",
                                              if_else(legelec==1 & exelec==1,"both","none"))))

# problem is DPI records the vote shares for the year after the election
# so, delete margin for election years, lag that, then delete for non-election years
dpi<-dpi %>% arrange(ifs,year) %>%
    group_by(ifs) %>%
    mutate(margin = dplyr::lead(margin)) %>%
    mutate(maj = dplyr::lead(maj)) %>%
    mutate(margin=replace(margin,election_DPI==0,NA)) %>%
    mutate(maj=replace(maj,election_DPI==0,NA)) 

# get cumulative sum and total elections by country
dpi<-dpi %>% group_by(ifs) %>%
    mutate(sum_elec = cumsum(election_DPI)) %>%
    mutate(tot_elec = max(sum_elec)) %>% ungroup()

# get country codes to match
dpi$ifs<-as.character(dpi$ifs)
dpi$un<-countrycode(dpi$ifs,"iso3c","un")
dpi$dpi<-1

## Fix codes for particular countries
dpi <- dpi %>%
    mutate(un=ifelse(countryname=="Soviet Union",643,un))%>%
    mutate(un=ifelse(countryname=="Czech Rep.",203,un)) %>%
    mutate(un=ifelse(countryname=="Romania",642,un)) %>%
    #mutate(un=ifelse(countryname=="Congo (DRC)",180,un)) %>%
    mutate(un=ifelse(countryname=="Taiwan",1013,un)) %>%
    mutate(un=ifelse(countryname=="Timor-Leste",180,un))

#### VDEM ####
# load VDEM data
Vdem<-readRDS("../data/Elections/V-Dem-CY-Full+Others-v10.rds") %>% 
    filter(year>1981 & year<2017) %>%
    dplyr::select(COWcode, year, v2elprescons, v2elvotlrg, v2elvotsml, v2ellocons, v2elloeldm, v2elloelsy, v2ellostsl, v2ellostss, v2eltvrig) %>%
    mutate(votediff = (v2ellostsl-v2ellostss)) %>% 
    mutate(incumbentMOV = if_else(v2eltvrig==0,votediff,1-votediff)) %>% #multiply vot difference by 1-whether incumbent won
    mutate(absvotediff = abs(votediff)) %>%
    mutate(un=countrycode(COWcode,"cown","un")) %>%
    mutate(votediff.norm = 100-absvotediff) %>%
    mutate(topparty.norm = abs(50-v2ellostsl)) %>%
    mutate(WL = incumbentMOV>=0)

#### WDI ####
# load world bank development indicators
wbi<-read_csv("../data/WB/WBI.csv")
# reshape to country/year indicators
wbi_long<-wbi %>% pivot_longer(cols = 5:42)
colnames(wbi_long)<-c("Country.name", "Country.code","Series.name", "Series.code","year","value")
wbi_long$year<-as.character(wbi_long$year)
wbi_long$year<-as.numeric(substring(wbi_long$year,1,4))
wbi_wide<-wbi_long %>% dplyr::select(-Series.name) %>% 
    drop_na(Series.code) %>% 
    pivot_wider(names_from = Series.code, values_from = value)
wbi<-wbi_wide[wbi_wide$year>=1982 & wbi_wide$year<=2016,]
rm(wbi_long,wbi_wide)

# fix country codes
wbi$un<-countrycode(wbi$Country.code,"iso3c","un")
wbi$wbi<-1
wbi<-wbi%>%
    mutate(un=ifelse(Country.code=="TWN",1013,un))

# rename variables
setnames(wbi, c("NV.AGR.TOTL.ZS", 
                "SL.AGR.EMPL.ZS", 
                "SP.POP.GROW",
                "SP.POP.TOTL",
                "NY.GDP.PCAP.KD.ZG",
                "NY.GDP.PCAP.KD"),
         c("Ag.pct","Ag.emp","Pop.growth","Pop.total","PCGDP.change", "PCGDP"))

# lag important variables
wbi %<>% arrange(un,year) %>%
    group_by(un) %>%
    mutate(Ag.pct.diff=Ag.pct-dplyr::lag(Ag.pct)) %>%
    mutate(Ag.pct.l=dplyr::lag(Ag.pct)) %>%
    mutate(PCGDP.l=dplyr::lag(PCGDP)) %>%
    mutate(PCGDP.change.l=dplyr::lag(PCGDP.change)) %>%
    mutate(Pop.total.l=dplyr::lag(Pop.total)) %>%
    mutate(Pop.growth.l=dplyr::lag(Pop.growth)) %>%
    mutate(Ag.emp.l=dplyr::lag(Ag.emp))

#### IPE Data ####
# load IPE dataset
ipe<-read_tsv("../data/IPE_V3.tab")

# choose years, fix countries
ipe<-ipe %>% mutate(un=countrycode(ccode,"cown","un")) %>%
    filter(year>=1982) %>%
    filter(year<=2016) %>%
    mutate(ipe=1) %>%
    mutate(un=ifelse(country=="Taiwan",1013,un))%>%
    mutate(un=ifelse(country=="Germany (Prussia)",NA,un)) %>%
    mutate(un=ifelse(country=="German Federal Republic",276,un))

# select variables from each dataset 
ipe<-ipe %>% dplyr::select(un,year,democracy_BX)
dpi<-dpi %>% dplyr::select(un,year,election_DPI,margin,legelec,exelec,percentl,percent1,dateleg,dateexec,sum_elec,tot_elec,etype,pr,pluralty, maj)
p4<-p4 %>% dplyr::select(un, year, polity2)
wbi<-wbi %>%dplyr::select(un,year,PCGDP.l,PCGDP.change.l,Pop.growth.l,Ag.pct.l,Ag.emp.l)

# join
full <- defo %>% left_join(p4,by=c("un","year")) %>%
    left_join(dpi,by=c("un","year")) %>% 
    left_join(wbi,by=c("un","year")) %>%
    left_join(ipe,by=c("un","year")) %>%
    left_join(Vdem,by=c("un","year"))

rm(defo)

# create Polity classes
full<-mutate(full,Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                                      ifelse(polity2 %in% -4:4,"Anoc",
                                             ifelse(polity2 %in% 5:10,"Democ",NA)))) %>%
    # make GDP variables in thousands of dollars
    mutate(PCGDP.l = PCGDP.l/1000) %>% 
    mutate(PCGDP.change.l=PCGDP.change.l/1000)

full$margin.norm<-100-abs(full$votediff)
full$close80<-as.numeric(full$margin.norm>80)
full$close80[is.na(full$margin.norm)==1]<-0
full$close80[full$margin.norm<=80]<-NA
full$close80[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
full$close90<-as.numeric(full$margin.norm>90)
full$close90[is.na(full$margin.norm)==1]<-0
full$close90[full$margin.norm<=90]<-NA
full$close90[is.na(full$election_DPI)]<-NA

full <- full %>% group_by(FID) %>% mutate(tot_forest = cumsum(forest)) %>% filter(tot_forest>0) %>% ungroup()

# select variables used in later analysis
full <- full %>% select(x,y,area,year,Countryeng,GID_1,GID_2,GID_0,FID,GEZ_TERM,un,
                        polity2,Polity_class,
                        votediff, margin, maj,
                        forest,forest.l,forest.diff,nn_forest,nn_forest.l,
                        PCGDP.l,PCGDP.change.l,Pop.growth.l,democracy_BX,
                        election_DPI,close80,close90,margin.norm,
                        pr, Ag.pct.l,Ag.emp.l)

save(full, file = "../data/output/full.Rdata")
rm(full)

############################## Upsampled ###################################
# do the same for upsampled data

load("../data/output/forest_upsampled_10.Rdata")

# rename a couple of variables
defo_dat<-master %>% rename(forest=tot) %>%dplyr::select(x,y,forest,area,year,nn_forest,Countryeng,GID_1,GID_2,GID_0,FID,GEZ_TERM,Unsdcode)
defo_dat<-rename(defo_dat,un=Unsdcode)%>% mutate(un = as.integer(un))

### fix countries ###
#fix code
defo_dat <- defo_dat %>%
    mutate(un=if_else(Countryeng=="Sudan",as.integer(729),un)) %>%
    mutate(un=if_else(Countryeng=="Kazakhstan",as.integer(398),un))

# Fill in missing years with average of surrounding years, means forest.diff for 1994=1995 and 2000=2001
y1993<-defo_dat %>% filter(year==1993)
y1995<-defo_dat %>% filter(year==1995)
y1994<-y1993 %>% mutate(forest = (y1995$forest+.$forest)/2) %>% mutate(nn_forest=(y1995$nn_forest+.$nn_forest)/2) %>% mutate(year=1994)

y1999<-defo_dat %>% filter(year==1999)
y2001<-defo_dat %>% filter(year==2001)
y2000<-y1999 %>% mutate(forest = (y2001$forest+.$forest)/2) %>% mutate(nn_forest=(y2001$nn_forest+.$nn_forest)/2) %>% mutate(year=2000)

defo<-rbind(defo_dat,y2000,y1994)

rm(y1993,y1994,y1995,y1999,y2000,y2001,defo_dat,country_names,master)


defo %<>% group_by(FID) %>%
    arrange(year) %>%
    mutate(forest.diff=forest-dplyr::lag(forest)) %>%
    mutate(forest.l = dplyr::lag(forest)) %>%
    mutate(nn_forest.l=dplyr::lag(nn_forest))%>%
    mutate(defo=1)


full <- defo %>% left_join(p4,by=c("un","year")) %>%
    left_join(dpi,by=c("un","year")) %>% 
    left_join(wbi,by=c("un","year")) %>%
    left_join(ipe,by=c("un","year")) %>%
    left_join(Vdem,by=c("un","year"))

rm(defo,dpi,ipe,p4,wbi)

full<-mutate(full,Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                                      ifelse(polity2 %in% -4:4,"Anoc",
                                             ifelse(polity2 %in% 5:10,"Democ",NA)))) %>%
    # make GDP variables in thousands of dollars
    mutate(PCGDP.l = PCGDP.l/1000) %>% 
    mutate(PCGDP.change.l=PCGDP.change.l/1000)

full$margin.norm<-100-abs(full$votediff)
full$close80<-as.numeric(full$margin.norm>80)
full$close80[is.na(full$margin.norm)==1]<-0
full$close80[full$margin.norm<=80]<-NA
full$close80[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
full$close90<-as.numeric(full$margin.norm>90)
full$close90[is.na(full$margin.norm)==1]<-0
full$close90[full$margin.norm<=90]<-NA
full$close90[is.na(full$election_DPI)]<-NA

full <- full %>% group_by(FID) %>% mutate(tot_forest = cumsum(forest)) %>% filter(tot_forest>0) %>% ungroup()

full <- full %>% dplyr::select(x,y,area,year,Countryeng,GID_1,GID_2,GID_0,FID,GEZ_TERM,un,
                        polity2,Polity_class,
                        votediff, margin, maj,
                        forest,forest.l,forest.diff,nn_forest,nn_forest.l,
                        PCGDP.l,PCGDP.change.l,Pop.growth.l,democracy_BX,
                        election_DPI,close80,close90,margin.norm,
                        pr, Ag.pct.l,Ag.emp.l)

save(full, file="../data/output/full_upsampled_10.Rdata")
length(full$forest)