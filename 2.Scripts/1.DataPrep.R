#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-01-20 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(tidyverse)
library(survival)
library(survminer)
library(sf)
library(mapview)
library(nngeo)
library(foreach)

#      Functions                                                            ####


#      Data                                                                 ####
#        [Turkey Data]                                                      ####

# Data Upload
Data <- read.csv("1.Data/Final_MissingCases.csv") %>% 
  subset(Data, Data$Nest_Type == "Nest") %>% na.omit()


# Creating Spatial Object                                         

TurkeySpatial <- st_as_sf(x = Spatial_Data,                         
                          coords = c("Location_X", "Location_Y"),
                          crs = "+init=epsg:32615")

# Recoding the Nest Fate to be a categorical characters variable 
TurkeySpatial$Nest_Fate <- ifelse(TurkeySpatial$Nest_Fate == 1, "Successful","Failed")

# Making Map  

TurkeySpatial %>%
  mapview(zcol = "Nest_Fate", burst = TRUE, fill = c("green","red") )

#        [Weather Data]                                                     ####

# Importing weather data
WeatherStations <- read_csv("1.Data/WeatherStations.csv")

# Making into a spatial object

WeatherStationsSpatial <- st_as_sf(x = WeatherStations,                         
                                   coords = c("Lon", "Lat"),
                                   crs = "+init=epsg:4326") %>% 
  st_transform(crs = "+init=epsg:32615")

# Checking the Data visually 
mapview(WeatherStationsSpatial)

###############################################################################
#   Calculating Distance Between Nests and Stations                         ####

# Calculating Distance Matrix. Row = Nests and Columns = Weather Stations.
# The output is the index for the weather station. 
DistanceMatrix <- st_distance(TurkeySpatial,WeatherStationsSpatial) %>% as.data.frame()

# Finding weather station Indices 
Indices <- st_nn(TurkeySpatial,WeatherStationsSpatial)

Indices <- do.call(rbind,Indices)

# Turkey Data with the Closest Weather Station name
WeatherStationTurkey <- foreach(i = 1:length(Indices), .combine = rbind) %do% {
  Nest <- TurkeySpatial[i,]
  WeatherStation <- WeatherStationsSpatial[Indices[i],]
  df <- cbind(Nest,WeatherStation)
}

###############################################################################
#   Adding Weather Data                                                     ####



