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
library(lubridate)

#      Functions                                                            ####


#      Data                                                                 ####
#        [Turkey Data]                                                      ####

# Data Upload
Data <- read.csv("1.Data/Final_MissingCases.csv") %>% 
  subset(.$Nest_Type == "Nest") %>% na.omit()


# Creating Spatial Object                                         

TurkeySpatial <- st_as_sf(x = Data,                         
                          coords = c("Location_X", "Location_Y"),
                          crs = "+init=epsg:32615")

# Recoding the Nest Fate to be a categorical characters variable 

TurkeySpatial$Nest_Fate <- ifelse(TurkeySpatial$Nest_Fate == 1, "Successful","Failed")

# Making Map  

TurkeySpatial %>%
  mapview(zcol = "Nest_Fate", burst = TRUE, fill = c("green","red") )

# Making Date data consistent

TurkeySpatial$Incubation_Start <- mdy(TurkeySpatial$Incubation_Start)
TurkeySpatial$Incubation_End <- mdy(TurkeySpatial$Incubation_End)

#        [Station Location Data]                                            ####

# Importing weather data
WeatherStations <- read_csv("1.Data/WeatherStations.csv")

# Making into a spatial object

WeatherStationsSpatial <- st_as_sf(x = WeatherStations,                         
                                   coords = c("Lon", "Lat"),
                                   crs = "+init=epsg:4326") %>% 
  st_transform(crs = "+init=epsg:32615")

# Checking the Data visually 
mapview(WeatherStationsSpatial)

#        [Weather Station Data]                                             ####

# Creating one data-frame with all of the data (2015 - 2020)
StationData <- list.files("1.Data",
                          pattern = "Weather_",
                          full.names = T) %>% 
  lapply(read.csv)

StationData <- do.call(rbind,StationData)

# Standardizing Date Format

StationData$Date <- mdy(StationData$Date)

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
  cbind(TurkeySpatial[i,],WeatherStationsSpatial[Indices[i],])
}

###############################################################################
#   Adding Weather Data                                                     ####

# Starting to Write up function for weather variables
start <- WeatherStationTurkey$Incubation_Start[1]
end <- WeatherStationTurkey$Incubation_End[1]

df <- subset(StationData,
             StationData$Station == WeatherStationTurkey$StationName[1]) %>% 
  filter(between(Date,as.Date(start), as.Date(end)))