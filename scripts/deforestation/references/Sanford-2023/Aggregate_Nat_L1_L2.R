#### Create L2, L1, National datasets for full, tropical/subtropical, temperate/boreal ####
# Takes the high resolution dataset and creates datasets aggregated to administrative levels 0, 1, and 2 for tropical and nontropical forest types
# Requires:
#     - full.Rdata
# Produces:
#     - GID_1_aggregated.Rdata
#     - GID_2_aggregated.Rdata
#     - country_aggregated.Rdata
#     - GID_1_aggregated_tropical.Rdata
#     - GID_2_aggregated_tropical.Rdata
#     - country_aggregated_tropical.Rdata
#     - GID_1_aggregated_nontropical.Rdata
#     - GID_2_aggregated_nontropical.Rdata
#     - country_aggregated_nontropical.Rdata

# If not already installed:
# install.packages("tidyverse")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
load("../data/output/full.Rdata")
obs_per_GID_1<-full %>% filter(year==1982) %>% group_by(GID_1) %>% summarise(n_obs = n()) %>% select(GID_1, n_obs)
GID_1_full<-full %>% group_by(GID_1,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_1,by="GID_1") %>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                                    ifelse(polity2 %in% -4:4,"Anoc",
                                           ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_1_full,file = "../data/output/GID_1_aggregated.Rdata")
print("done with complete")

print("Tropical and Subtropical")
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","Tropical mountain system","Subtropical humid forest","Tropical shrubland")
full_tropical <- full %>% filter(GEZ_TERM %in% tropical_sub)
obs_per_GID_1<-full_tropical %>% filter(year==1982) %>% group_by(GID_1) %>% summarise(n_obs = n()) %>% select(GID_1, n_obs)
GID_1_full_tropical<-full_tropical %>% group_by(GID_1,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_1,by="GID_1")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_1_full_tropical,file = "../data/output/GID_1_aggregated_tropical.Rdata")
print("done with tropical")

'%!in%' <- function(x,y)!('%in%'(x,y))
print("Temperate and boreal")
full_nontropical <- full %>% filter(GEZ_TERM %!in% tropical_sub)
obs_per_GID_1<-full_nontropical %>% filter(year==1982) %>% group_by(GID_1) %>% summarise(n_obs = n()) %>% select(GID_1, n_obs)
GID_1_full_nontropical<-full_nontropical %>% group_by(GID_1,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_1,by="GID_1")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_1_full_nontropical,file = "../data/output/GID_1_aggregated_nontropical.Rdata")
print("done with nontropical")



obs_per_GID_2<-full %>% filter(year==1982) %>% group_by(GID_2) %>% summarise(n_obs = n()) %>% select(GID_2, n_obs)
GID_2_full<-full %>% group_by(GID_2,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_2,by="GID_2")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_2_full,file = "../data/output/GID_2_aggregated.Rdata")
print("done with complete")

print("Tropical and Subtropical")
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","Tropical mountain system","Subtropical humid forest","Tropical shrubland")
full_tropical <- full %>% filter(GEZ_TERM %in% tropical_sub)
obs_per_GID_2<-full_tropical %>% filter(year==1982) %>% group_by(GID_2) %>% summarise(n_obs = n()) %>% select(GID_2, n_obs)
GID_2_full_tropical<-full_tropical %>% group_by(GID_2,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_2,by="GID_2")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_2_full_tropical,file = "../data/output/GID_2_aggregated_tropical.Rdata")
print("done with tropical")

print("Temperate and boreal")
full_nontropical <- full %>% filter(GEZ_TERM %!in% tropical_sub)
obs_per_GID_2<-full_nontropical %>% filter(year==1982) %>% group_by(GID_2) %>% summarise(n_obs = n()) %>% select(GID_2, n_obs)
GID_2_full_nontropical<-full_nontropical %>% group_by(GID_2,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_GID_2,by="GID_2")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(GID_2_full_nontropical,file = "../data/output/GID_2_aggregated_nontropical.Rdata")
print("done with nontropical")



obs_per_country<-full %>% filter(year==1982) %>% group_by(un) %>% summarise(n_obs = n()) %>% select(un, n_obs)
country_full<-full %>% group_by(un,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_country,by="un")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(country_full,file = "../data/output/country_aggregated.Rdata")
print("done with complete")

print("Tropical and Subtropical")
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","Tropical mountain system","Subtropical humid forest","Tropical shrubland")
full_tropical <- full %>% filter(GEZ_TERM %in% tropical_sub)
obs_per_country<-full_tropical %>% filter(year==1982) %>% group_by(un) %>% summarise(n_obs = n()) %>% select(un, n_obs)
country_full_tropical<-full_tropical %>% group_by(un,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_country,by="un")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(country_full_tropical,file = "../data/output/country_aggregated_tropical.Rdata")
print("done with tropical")

'%!in%' <- function(x,y)!('%in%'(x,y))
print("Temperate and boreal")
full_nontropical <- full %>% filter(GEZ_TERM %!in% tropical_sub)
obs_per_country<-full_nontropical %>% filter(year==1982) %>% group_by(un) %>% summarise(n_obs = n()) %>% select(un, n_obs)
country_full_nontropical<-full_nontropical %>% group_by(un,year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% left_join(obs_per_country,by="un")%>%
    mutate(Polity_class=ifelse(polity2 %in% -10:-5,"Autoc",
                               ifelse(polity2 %in% -4:4,"Anoc",
                                      ifelse(polity2 %in% 5:10,"Democ",NA))))
save(country_full_nontropical,file = "../data/output/country_aggregated_nontropical.Rdata")
print("done with nontropical")