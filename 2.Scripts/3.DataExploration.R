#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-01-25 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

library(tidyverse)
library(survival)
library(survminer)
library(stargazer)
library(PerformanceAnalytics)
library(effects)
library(GGally)

#      Functions                                                            ####

#      Data                                                                 ####

#        [Original]                                                         ####

Data <- read.csv("1.Data/Final.csv")

Data_MC <- read.csv("1.Data/Final_MissingCases.csv")

#        [Weather Data]                                                     ####

Data_Weather <- read.csv("1.Data/FinalData.csv")

Data_Weather$Nest_Fate <- if_else(Data_Weather$Nest_Fate == "Successful", 1, 0)

###############################################################################
#   Site Selection Analysis                                                 ####
#      Data Prep                                                            ####

# Eliminating Hens with repeat nest
Data_NestSelection <- subset(Data_MC, Data_MC$Nest_Number == 1) %>% .[,c(1:(ncol(Data_MC)-3))]

# Changing the Nest type to a binary variable
Data_NestSelection$Nest_Type <- ifelse(Data_NestSelection$Nest_Type == "Nest",1,0) # 1 = Used ; 0 = Available

#      Variable Prep                                                        ####

# Dependent Variable
response <- Data_NestSelection[,'Nest_Type']

# Strata
strata <- Data_NestSelection[,'Hen_ID']

HabitatType              <- factor(Data_NestSelection$Habitat_Type, levels = c("Opening","Edge","Forest"))
Forest_100m              <- Data_NestSelection$Amount_Forest_100m
Herb_100m                <- Data_NestSelection$Amount_Herb_100m
Crop_100m                <- Data_NestSelection$Amount_Crop_100m
Other_100m               <- Data_NestSelection$Amount_Other_100m
ForestEdge_100m          <- Data_NestSelection$Amount_ForestEdge_100m
Forest_200m              <- Data_NestSelection$Amount_Forest_200m
Herb_200m                <- Data_NestSelection$Amount_Herb_200m
Crop_200m                <- Data_NestSelection$Amount_Crop_200m
Other_200m               <- Data_NestSelection$Amount_Other_200m
ForestEdge_200m          <- Data_NestSelection$Amount_ForestEdge_200m
Forest_400m              <- Data_NestSelection$Amount_Forest_400m
Herb_400m                <- Data_NestSelection$Amount_Herb_400m
Crop_400m                <- Data_NestSelection$Amount_Crop_400m
Other_400m               <- Data_NestSelection$Amount_Other_400m
ForestEdge_400m          <- Data_NestSelection$Amount_ForestEdge_400m
BasalArea                <- Data_NestSelection$Basal_Area 
ForestEdgeDist           <- Data_NestSelection$ForestEdgeDistance
Height_GC                <- Data_NestSelection$Height_GC
Percent_CanopyClosure    <- Data_NestSelection$Percent_CanopyClosure
Percent_BareGroundLitter <- Data_NestSelection$Percent_BareGroundtoLiter
Percent_Grasses          <- Data_NestSelection$Percent_Grasses
Percent_Forbs            <- Data_NestSelection$Percent_Forbs
Percent_WoodyVeg         <- Data_NestSelection$Percent_WoodyVeg
Percent_SC1              <- Data_NestSelection$Percent_ScreeningCover_0_0.5
Percent_SC2              <- Data_NestSelection$Percent_ScreeningCover_0.5_1.0
Percent_SC3              <- Data_NestSelection$Percent_ScreeningCover_1.0_1.8




#      Plotting                                                             ####

base <- ggplot(Data_NestSelection)

base + geom_boxplot(aes(as.factor(Nest_Type),Data_NestSelection[,6]))+
  ylab(colnames(Data_NestSelection[6]))+
  xlab("Nest Type")+ scale_x_discrete(labels = c('Available', "Used"))

CovPlot_Selection <-function(index){
  
  base <- ggplot(Data_NestSelection)
  
  p <- base + geom_boxplot(aes(as.factor(Nest_Type),Data_NestSelection[,index]))+
    ylab(colnames(Data_NestSelection[index]))+
    xlab("Nest Type")+
    ggtitle(colnames(Data_NestSelection[index]))+theme_bw()
  
  return(p)
}

CovList_Selection <- c(6:20,24:34)

lapply(CovList_Selection, CovPlot_Selection)

###############################################################################
#   Site Success Analysis                                                   ####
#      Data Prep                                                            ####

# Removing Random nest from the data 
Data_CoxHaz <- subset(Data_Weather,Data_Weather$Nest_Type == "Nest")

# Making Sure Number of days is a numeric variable 
Data_CoxHaz$NumberOfDays <- as.numeric(Data_CoxHaz$NumberOfDays)
Data_CoxHaz$Nest_Fate <- as.numeric(Data_CoxHaz$Nest_Fate)

#      Variable Prep                                                        ####


# Dependent Variable as a Survival Object 

response <- Surv(Data_CoxHaz$NumberOfDays,Data_CoxHaz$Nest_Fate)

# Independent Variables [Habitat]

HabitatType              <- factor(Data_CoxHaz$Habitat_Type, levels = c("Opening","Edge","Forest"))
Forest_100m              <- Data_CoxHaz$Amount_Forest_100m
Herb_100m                <- Data_CoxHaz$Amount_Herb_100m
Crop_100m                <- Data_CoxHaz$Amount_Crop_100m
Other_100m               <- Data_CoxHaz$Amount_Other_100m
ForestEdge_100m          <- Data_CoxHaz$Amount_ForestEdge_100m
Forest_200m              <- Data_CoxHaz$Amount_Forest_200m
Herb_200m                <- Data_CoxHaz$Amount_Herb_200m
Crop_200m                <- Data_CoxHaz$Amount_Crop_200m
Other_200m               <- Data_CoxHaz$Amount_Other_200m
ForestEdge_200m          <- Data_CoxHaz$Amount_ForestEdge_200m
Forest_400m              <- Data_CoxHaz$Amount_Forest_400m
Herb_400m                <- Data_CoxHaz$Amount_Herb_400m
Crop_400m                <- Data_CoxHaz$Amount_Crop_400m
Other_400m               <- Data_CoxHaz$Amount_Other_400m
ForestEdge_400m          <- Data_CoxHaz$Amount_ForestEdge_400m
BasalArea                <- Data_CoxHaz$Basal_Area 
ForestEdgeDist           <- Data_CoxHaz$ForestEdgeDistance
Height_GC                <- Data_CoxHaz$Height_GC
Percent_CanopyClosure    <- Data_CoxHaz$Percent_CanopyClosure
Percent_BareGroundLitter <- Data_CoxHaz$Percent_BareGroundtoLiter
Percent_Grasses          <- Data_CoxHaz$Percent_Grasses
Percent_Forbs            <- Data_CoxHaz$Percent_Forbs
Percent_WoodyVeg         <- Data_CoxHaz$Percent_WoodyVeg
Percent_SC1              <- Data_CoxHaz$Percent_ScreeningCover_0_0.5
Percent_SC2              <- Data_CoxHaz$Percent_ScreeningCover_0.5_1.0
Percent_SC3              <- Data_CoxHaz$Percent_ScreeningCover_1.0_1.8

# Independent Variables [Weather]
PrecipAmount             <- Data_CoxHaz$Total_Precip
PrecipDays               <- Data_CoxHaz$RainDays
MeanTemp                 <- Data_CoxHaz$MeanTemp
MaxTemp                  <- Data_CoxHaz$MaxTemp
MinTemp                  <- Data_CoxHaz$MinTemp


#      Plotting                                                             ####

base <- ggplot(Data_CoxHaz)

CovPlot_Success <-function(index){
  
  base <- ggplot(Data_CoxHaz)
  
  p <- base + geom_boxplot(aes(as.factor(Nest_Fate),Data_CoxHaz[,index]))+
    ylab(colnames(Data_CoxHaz[index]))+
    xlab("Nest Success")+
    ggtitle(colnames(Data_CoxHaz[index]))+theme_bw()
  
  return(p)
}

CovList_Success <- c(6:20,22:32,38:42)

lapply(CovList_Success, CovPlot_Success)
###############################################################################