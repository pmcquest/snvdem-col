#  install.packages("water")
# #devtools::install_github('azvoleff/gfcanalysis')
# install.packages("gfcanalysis")
#  install.packages("rworldmap")
#  install.packages("rgdal")
library(rgdal)
library(water)
library(gfcanalysis)
library(rworldmap)
library(raster)
library(sp)
library(sf) 



# ---- Define folders and relevant values ---- #

## Detect system and define main folder accordingly:
root_folder <- "C:/Users/David/Dropbox/Research/Tesis/Donor_deforestation/replication/APSR_replication/1_data/_data_construction/Raster_processing"
main_folder <- "C:/Users/David/Dropbox/Research/Tesis/Donor_deforestation/replication/APSR_replication/1_data/_data_construction/Raster_processing/Deforestation"

# -- Output folder -- #
output_folder    <- paste(main_folder,"/gfc", sep = "")
shapefile_folder <- paste(root_folder,"/Shapefiles/Municipios_shape", sep = "")
output_data <- paste(main_folder,"/output_data", sep = "")
setwd(output_folder)

# -- Relevant values -- #
forest_porcentage <- 50


##----------- Files preparation ---------###

# -- Colombia Municipal raster -- #
Col <- readOGR(shapefile_folder, 'MunicipiosNew')
Col <- spTransform(Col, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

Col$muni_code = as.numeric(as.character(Col$MPIO_CCDGO))

# check proyection
# plot(Col, col="#E0FFFF")
# plot(plantations_mun, add = T)

# -- Hansen -- #
tiles_all <- calc_gfc_tiles(aoi = Col)

# Number of tiles needed
print(length(tiles_all))

# Dowload all tiles
# download_tiles(tiles_all, output_folder, dataset = "GFC-2019-v1.7")


######################################################################
## --------------- Contruction of municipal Data ----------------- ##
######################################################################

# Notes:
# If the loop below fails at any point, you can restart for the point of failure by subsetting muni_list
# If this takes too long lets only check for the  munis w/ running 
# muni_list <- c(5002 , 5004 , 5031 , 5044 , 5055 , 5086 , 5091 , 5120 , 5125 , 5134 , 5142 , 5145 , 5150 , 5172 , 5209 , 5212 , 5237 , 5250 , 5264 , 5310 , 5313 , 5318 , 5353 , 5361 , 5368 , 5480 , 5585 , 5607 , 5647 , 5649 , 5658 , 5660 , 5665 , 5667 , 5674 , 5686 , 5736 , 5756 , 5761 , 5790 , 5809 , 5842 , 5847 , 5854 , 5856 , 5887 , 8137 , 8141 , 8296 , 8372 , 8421 , 8520 , 8558 , 8832 , 8849 , 13030 , 13160 , 13188 , 13222 , 13248 , 13300 , 13442 , 13549 , 13580 , 13600 , 13647 , 13650 , 13655 , 13667 , 13838 , 13894 , 15001 , 15087 , 15109 , 15114 , 15135 , 15162 , 15172 , 15204 , 15215 , 15236 , 15293 , 15299 , 15367 , 15407 , 15425 , 15480 , 15491 , 15500 , 15531 , 15580 , 15646 , 15681 , 15686 , 15693 , 15723 , 15759 , 15776 , 15798 , 15806 , 15837 , 17174 , 17380 , 17442 , 17486 , 17541 , 17614 , 17662 , 17867 , 17873 , 17877 , 18001 , 18094 , 18256 , 18410 , 18460 , 18785 , 19050 , 19075 , 19100 , 19130 , 19290 , 19318 , 19364 , 19397 , 19418 , 19473 , 19513 , 19548 , 19585 , 19698 , 19701 , 19743 , 19785 , 19807 , 20011 , 20013 , 20238 , 20443 , 20517 , 20770 , 20787 , 23068 , 23162 , 23168 , 23300 , 23350 , 23419 , 23500 , 23570 , 23580 , 23660 , 23678 , 25019 , 25040 , 25099 , 25120 , 25126 , 25178 , 25183 , 25214 , 25260 , 25269 , 25279 , 25290 , 25295 , 25324 , 25328 , 25335 , 25368 , 25402 , 25407 , 25426 , 25430 , 25436 , 25438 , 25486 , 25491 , 25518 , 25596 , 25645 , 25649 , 25658 , 25662 , 25736 , 25743 , 25772 , 25781 , 25785 , 25797 , 25799 , 25815 , 25839 , 25851 , 25867 , 25885 , 27001 , 27025 , 27050 , 27077 , 27099 , 27135 , 27205 , 27245 , 27250 , 27361 , 27413 , 27580 , 27600 , 27660 , 27745 , 27787 , 41006 , 41016 , 41078 , 41206 , 41298 , 41359 , 41396 , 41483 , 41524 , 41615 , 41660 , 41668 , 41676 , 41872 , 44035 , 44110 , 44279 , 44874 , 47030 , 47058 , 47170 , 47189 , 47245 , 47318 , 47555 , 47660 , 47675 , 47692 , 47707 , 47720 , 50110 , 50124 , 50270 , 50287 , 50318 , 50330 , 50350 , 50370 , 50400 , 50450 , 50568 , 50577 , 50590 , 50680 , 50686 , 50711 , 52079 , 52083 , 52203 , 52207 , 52224 , 52227 , 52233 , 52240 , 52254 , 52258 , 52320 , 52323 , 52354 , 52381 , 52385 , 52405 , 52418 , 52480 , 52520 , 52540 , 52560 , 52585 , 52612 , 52621 , 52685 , 52720 , 52786 , 52788 , 52838 , 54051 , 54125 , 54172 , 54174 , 54206 , 54250 , 54261 , 54377 , 54398 , 54518 , 54670 , 54673 , 54680 , 54820 , 63130 , 63190 , 63212 , 63470 , 63548 , 63594 , 66045 , 66088 , 66318 , 66440 , 66456 , 66572 , 66682 , 68077 , 68121 , 68176 , 68179 , 68190 , 68211 , 68229 , 68245 , 68255 , 68320 , 68322 , 68324 , 68327 , 68370 , 68377 , 68385 , 68397 , 68418 , 68444 , 68500 , 68502 , 68524 , 68549 , 68573 , 68575 , 68855 , 68895 , 70110 , 70235 , 70473 , 70508 , 70708 , 70742 , 70820 , 73026 , 73030 , 73055 , 73124 , 73148 , 73226 , 73236 , 73275 , 73319 , 73443 , 73449 , 73555 , 73585 , 73616 , 73675 , 73686 , 73770 , 73861 , 73870 , 76020 , 76054 , 76100 , 76113 , 76122 , 76126 , 76233 , 76243 , 76248 , 76318 , 76400 , 76828 , 76845 , 76863 , 76890 , 81300 , 81591 , 85125 , 85136 , 85162 , 85230 , 85250 , 85263 , 85279 , 85300 , 85315 , 85325 , 85430 , 86001 , 86219 , 86320 , 86569 , 86573 , 86749 , 86755 , 86757 , 86760 , 88564 , 95001 , 95200 , 97666 )


muni_list <- c(unique(Col$muni_code))


counter = 0

for(mun in muni_list){
  
    counter = counter + 1

    print(paste("Start: mun", mun, "#", counter, "of 1122"))  
    
    # -- Define Area of Interest AoI -- #
    # AoI <- Col
    AoI = subset(Col, muni_code==mun)
    AoI <- spTransform(AoI, "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
                    
    # ------------ Hansen Data ------------- #
    
    tiles <- calc_gfc_tiles(aoi = AoI)
    
    # Number of tiles needed
    print(length(tiles))
    
    
    # -- Get and prepare data -- #
    
    # Dowload tiles
    download_tiles(tiles, output_folder, dataset = "GFC-2019-v1.7")
    
    
    # extract data
    gfc <- extract_gfc(aoi = AoI, data_folder = output_folder, to_UTM = FALSE , stack = "change", dataset = "GFC-2019-v1.7")
    
    # process data given the threshold
    gfc_threshold <- threshold_gfc(gfc, forest_threshold = forest_porcentage)
    
    # yearly data
    gfc_stats <- gfc_stats(AoI, gfc_threshold, dataset = "GFC-2019")
    
    
    # export data (I prefer to export to avoid increasing use of ram and to have backups)
    outfile <- paste0(output_data,'/Hansen',mun,'.csv')
    
    write.csv2(gfc_stats$loss_table, file=outfile, row.names=T)
    
    print(paste("DONE! : mun", mun, "#", counter, "of 1122")) 

}

# join datasets

data <- data.frame(X = integer(),
                 year=integer(),
                 aoi=character(),
                 cover=double(),
                 loss=double(),
                 muni_code = integer(),
                 stringsAsFactors=FALSE)

counter = 0

for(mun in muni_list){
  
  counter = counter + 1

  infile <- paste0(output_data,'/Hansen',mun,'.csv')
  file <- read.csv2(infile, header = T, sep = ";")
  file$muni_code = mun
  
  data <- rbind(data, file)
  
  print(paste("DONE! : mun", mun, "#", counter, "of 1122"))
  
}

lastfile <- paste0(output_data,'/Hansen_2000_2018','.csv')

write.csv2(data,lastfile, sep = ",")  
