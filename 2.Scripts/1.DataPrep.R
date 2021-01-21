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
library(sp)

#      Functions                                                            ####


#      Data                                                                 ####
#        [Turkey Data]                                                      ####

Data <- read.csv("1.Data/Final.csv")
Data_MC <- read.csv("1.Data/Final_MissingCases.csv")

#        [Weather Data]                                                     ####

# Importing weather data
WeatherStations <- read_csv("1.Data/WeatherStations.csv")

# Making into a spatial object

#   Designation of Coordinate Information  
xy_weather <- WeatherStations[,c("Lat","Lon")] 

#   Creating Spatial Object
WeatherStationsSpatial <- SpatialPointsDataFrame(coords = xy_weather, data = WeatherStations,
                                        proj4string = CRS("+init=epsg:4326"))

WeatherStationsSpatial <- spTransform(WeatherStationsSpatial,
                                      CRS = CRS("+init=epsg:32615"))

Map <- WeatherStationsSpatial %>%
  mapview(burst = TRUE)

###############################################################################