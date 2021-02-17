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

mapview(TurkeySpatial,zcol = "Nest_Fate", burst = TRUE, fill = c("green","red"))+ mapview(WeatherStationsSpatial)



xyweather <- WeatherStations[,c("Lon","Lat")] 

WeatherStationsSpatial <- SpatialPointsDataFrame(coords = xyweather, data = WeatherStations,
                                        proj4string = CRS("+init=epsg:4326"))

map2<-WeatherStationsSpatial %>%
  mapview(burst = TRUE, fill = c("green","red") )
