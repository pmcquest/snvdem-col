# takes the Hansen 2018 data, converts to panel forest change with country labels
# Requires: 
#     - Hansen dataset: VCF5KYR_1982.tif through VCF5KYR_2016.tif (https://www.nature.com/articles/s41586-018-0411-9)
#     - Global administrative boundaries data level 2 (https://gadm.org/)
#     - Global ecological zones data http://www.fao.org/geonetwork/srv/en/metadata.show?id=1255
# Produces:
#     - full_forest.Rdata


# if not already installed
# install.packages("sf","raster","tidyverse","countrycode")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(sf)
library(raster)
library(tidyverse)
library(countrycode)


# Load national boundaries, forest raster, ecological zones

# Load global administrative level 2 boundaries from https://gadm.org/
gadm_l2<-st_read("../data/gadm36_levels.gpkg",layer = "level2")
gadm_l2<-gadm_l2 %>% 
    mutate(Unsdcode = countrycode(GID_0,"iso3c","un"),
           Countryeng = NAME_0) %>% 
    mutate(Unsdcode=ifelse(GID_0=="TWN",1013,Unsdcode)) %>% 
    dplyr::select(GID_1,GID_2,GID_0,Unsdcode,Countryeng)

# Load 1982 forest raster
# From https://www.nature.com/articles/s41586-018-0411-9
forest1982<-raster("../data/VCF/VCF5KYR_1982.tif")


# load global ecological zones from http://www.fao.org/geonetwork/srv/en/metadata.show?id=1255
GEZ<-read_sf("../data/GEZ/eco_zone.shp",crs = 4326)

# function to get area of raster cell, forest of neighboring cells (rook's case), convert to points
get_neighbors<-function(raster){
    # get area of raster
    raster$area<-area(raster)
    # convert to data frame
    df<-as.data.frame(raster,xy=T)
    names(df)<-c("x","y","tot","area")
    # add integer coordinates
    df$x_num<-floor(df$x/0.05 + 3600.5)
    df$y_num<-floor(df$y/0.05 + 1801)
    # get cells above and below given cell
    df<-df %>% group_by(x_num) %>% arrange(y_num) %>% mutate(above=dplyr::lag(tot)) %>% mutate(below=dplyr::lead(tot)) %>% ungroup()
    # get cells to left and right of given cell
    df<-df %>% group_by(y_num) %>% arrange(x_num) %>% mutate(left=dplyr::lag(tot)) %>% mutate(right=dplyr::lead(tot)) %>% ungroup()
    # sum them
    df$nn_forest<-df$left + df$right + df$above + df$below
    # return dataset
    return(df%>% dplyr::select(x,y,tot,area,nn_forest))
}
# run on 1982 raster
f1982<-get_neighbors(forest1982)

# add spatial information
P4S.latlon <- CRS("+proj=longlat +no_defs +datum=WGS84")
system.time(tot_pts0<-st_as_sf(f1982,coords=c("x","y"),crs = 4326))
tot_pts0$year<-1982

# add country characteristics
system.time(a.data <- st_join(tot_pts0, gadm_l2))
a.data$x<-st_coordinates(a.data)[,1]
a.data$y<-st_coordinates(a.data)[,2]

# Add unique identifier "FID code" to each location
b.data <- a.data %>% arrange(x,y)# %>% mutate(FID=1:length(tot))
b.data$FID <- 1:length(b.data$tot)


# drop observations outside of countries
system.time(c.data<-b.data %>% filter(!is.na(Unsdcode)))

# add GEZ characteristics, save dictionary (GEZ is Global Ecological Zone)
system.time(d.data <- st_join(c.data, GEZ))
GEZ_dictionary <- d.data %>% dplyr::select(GID_1,GID_2,GID_0,Unsdcode,Countryeng,x,y,FID,GEZ_CLASS,GEZ_TERM) %>% st_set_geometry(NULL)
# save(GEZ_dictionary,file="../data/output/gez_dictionary.Rdata")

#### Add data for other years ####
# build up raster dataset using dictionary created above

# 1982 data
i<-1982
raster<-raster(paste("../data/VCF/VCF5KYR_",i,".tif",sep=""))
raster$area<-area(raster)
df<-as.data.frame(raster,xy=T)
names(df)<-c("x","y","tot","area")
df$x_num<-floor(df$x/0.05 + 3600.5)
df$y_num<-floor(df$y/0.05 + 1801)
df<-df %>% group_by(x_num) %>% arrange(y_num) %>% mutate(above=dplyr::lag(tot)) %>% mutate(below=dplyr::lead(tot)) %>% ungroup()
df<-df %>% group_by(y_num) %>% arrange(x_num) %>% mutate(left=dplyr::lag(tot)) %>% mutate(right=dplyr::lead(tot)) %>% ungroup()
df$nn_forest<-(df$left + df$right + df$above + df$below)/4
df <- df %>% right_join(GEZ_dictionary,by=c("x","y"))
df<-df%>% dplyr::select(x,y,tot,area,nn_forest,GID_1,GID_2,GID_0,Countryeng,Unsdcode,FID,GEZ_CLASS,GEZ_TERM)
df$year<-i
master<-df
# rbind(df,master)
# end guts

# run other years
system.time(for(i in c(1983:1993,1995:1999,2001:2016)){
    print(i)
    raster<-raster(paste("../data/VCF/VCF5KYR_",i,".tif",sep=""))
    raster$area<-area(raster)
    df<-as.data.frame(raster,xy=T)
    names(df)<-c("x","y","tot","area")
    df$x_num<-floor(df$x/0.05 + 3600.5)
    df$y_num<-floor(df$y/0.05 + 1801)
    df<-df %>% group_by(x_num) %>% arrange(y_num) %>% mutate(above=dplyr::lag(tot)) %>% mutate(below=dplyr::lead(tot)) %>% ungroup()
    df<-df %>% group_by(y_num) %>% arrange(x_num) %>% mutate(left=dplyr::lag(tot)) %>% mutate(right=dplyr::lead(tot)) %>% ungroup()
    df$nn_forest<-(df$left + df$right + df$above + df$below)/4
    df <- df %>% right_join(GEZ_dictionary,by=c("x","y"))
    df<-df%>% dplyr::select(x,y,tot,area,nn_forest,GID_1,GID_2,GID_0,Countryeng,Unsdcode,FID,GEZ_CLASS,GEZ_TERM)
    df$year<-i
    master<-rbind(master,df)
})

# save output
save(master,file="../data/output/full_forest.Rdata")
