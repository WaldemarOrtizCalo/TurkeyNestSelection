#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-11-13 

# Purpose: Spatial Mapping of locations 

###############################################################################
#   Library / Functions / Data                                              ####

##  Library                                                                 ####
library(mapview)
library(sf)
library(sp)
library(dplyr)
library(ggmap)
##  Functions                                                               ####

##  Data                                                                    ####
Data <- read.csv("1.Data/Final_MissingCases.csv")
Spatial_Data <- subset(Data, Data$Nest_Type == "Nest") %>% na.omit()

###############################################################################
#   Data Prep                                                               ####
#   Creating Spatial Object                                                 ####

#   Designation of Coordinate Information  
xy <- Spatial_Data[,c("Location_X","Location_Y")] 

#   Creating Spatial Object
TurkeySpatial <- SpatialPointsDataFrame(coords = xy, data = Spatial_Data,
                               proj4string = CRS("+init=epsg:32615"))
#   Recoding the Nest Fate to be a categorical characters variable 
TurkeySpatial$Nest_Fate <- ifelse(TurkeySpatial$Nest_Fate == 1, "Successful","Failed")

#   Making Map                                                              ####

Map <- TurkeySpatial %>%
  mapview(zcol = "Nest_Fate", burst = TRUE, fill = c("green","red") )

Map
register_google(key = "AIzaSyBCYv3TaFhykGEgUof3X1RGgPwncUrPg5s")
# Fixed Map 
basemap <- get_map(location=c(lon = -92, lat = 40), zoom=11, maptype = 'satellite', source = 'google')
register_google(key = "AIzaSyBCYv3TaFhykGEgUof3X1RGgPwncUrPg5s")


ggmap(basemap) + geom_point(aes(TurkeySpatial$Location_X,TurkeySpatial$Location_Y))
