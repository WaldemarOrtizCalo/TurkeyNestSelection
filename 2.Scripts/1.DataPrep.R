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

#      Functions                                                            ####


#      Data                                                                 ####


#        [TurkeyData]                                                       ####

Data <- read.csv("1.Data/Final.csv")
Data_MC <- read.csv("1.Data/Final_MissingCases.csv")

#        [Weather Data]                                                     ####


WeatherStations <- read.csv("1.Data/WeatherStations.csv")

###############################################################################