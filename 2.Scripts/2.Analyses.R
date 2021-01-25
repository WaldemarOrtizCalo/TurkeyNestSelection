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

#      Functions                                                            ####

#      Data                                                                 ####

#        [Original]                                                         ####

Data <- read.csv("1.Data/Final.csv")

Data_MC <- read.csv("1.Data/Final_MissingCases.csv")

#        [Weather Data]                                                     ####

Data_Weather <- read_csv("1.Data/FinalData.csv")

Data_Weather$Nest_Fate <- if_else(Data_Weather$Nest_Fate == "Successful", 1, 0)

###############################################################################
#   Site Selection Analysis (Not Done)                                      ####
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

# Independent Variables
Micro01 <- Data_NestSelection[,'Amount_Forest_100m']
Micro02 <- df[,'Basal Area']
Micro03 <- df[,'Forest Edge Distance']
Micro04 <- df[,'% Canopy Closure']
Micro05 <- df[,'GC Height']
Micro06 <- df[,'% BG/Litter']
Micro07 <- df[,'% Grasses']
Micro08 <- df[,'% Forbs']
Micro09 <- df[,'% Woody Veg.']
Micro10 <- df[,'% SC (0.0-0.5 m)']
Micro11 <- df[,'% SC (0.5-1.0 m)']
Micro12 <- df[,'% SC (1.0-1.8 m)']



#      Model Sets                                                           ####

# Model 

m1 <- clogit(response ~ Micro01 + strata(strata),data = Data_NestSelection)

summary(m1)

###############################################################################
#   Site Selection Analysis                                                 ####
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

#      Model Sets                                                           ####
#        [Global Model]                                                     ####

Global_HazCox <- coxph(response ~ HabitatType + 
                         Forest_100m + Forest_200m + Forest_400m +
                         Herb_100m + Herb_200m + Herb_400m +
                         Crop_100m + Crop_200m + Crop_400m +
                         Other_100m + Other_200m + Other_400m +
                         ForestEdge_100m + ForestEdge_200m + ForestEdge_400m +
                         BasalArea + ForestEdgeDist + Height_GC +
                         Percent_CanopyClosure + Percent_BareGroundLitter +
                         Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                         Percent_SC1 + Percent_SC2 + Percent_SC3)

summary(Global_HazCox)

#        [Habitat Model]  (Significant)                                     ####
Habitat_HazCox <- coxph(response ~ HabitatType)

summary(Habitat_HazCox)

#        [Micro Model]                                                      ####
Micro_HazCox <- coxph(response ~ BasalArea + ForestEdgeDist + Height_GC +
                        Percent_CanopyClosure + Percent_BareGroundLitter +
                        Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                        Percent_SC1 + Percent_SC2 + Percent_SC3)

summary(Micro_HazCox)

#        [Macro Model] (Somewhat significant)                               ####
Macro_HazCox <- coxph(response ~ Forest_100m + Forest_200m + Forest_400m +
                        Herb_100m + Herb_200m + Herb_400m +
                        Crop_100m + Crop_200m + Crop_400m +
                        Other_100m + Other_200m + Other_400m +
                        ForestEdge_100m + ForestEdge_200m + ForestEdge_400m)

summary(Macro_HazCox)

#        [Global 100m Model]                                                ####
Macro100m_HazCox <- coxph(response ~ Forest_100m + Herb_100m + Crop_100m + 
                            Other_100m + ForestEdge_100m)

summary(Macro100m_HazCox)

#        [Global 200m Model]                                                ####
Macro200m_HazCox <- coxph(response ~ Forest_200m + Herb_200m + Crop_200m + 
                            Other_200m + ForestEdge_200m)

summary(Macro200m_HazCox)



#        [Global 400m Model]                                                ####
Macro400m_HazCox <- coxph(response ~ Forest_400m + Herb_400m + Crop_400m + 
                            Other_400m + ForestEdge_400m)

summary(Macro400m_HazCox)



#        [Forest 100m Model]                                                ####
Forest100m_HazCox <- coxph(response ~ Forest_100m)

summary(Forest100m_HazCox)


#        [Forest 200m Model]                                                ####
Forest200m_HazCox <- coxph(response ~ Forest_200m)

summary(Forest200m_HazCox)



#        [Forest 400m Model]                                                ####
Forest400m_HazCox <- coxph(response ~ Forest_400m)

summary(Forest400m_HazCox)



#        [Herb 100m Model]                                                  ####
Herb100m_HazCox <- coxph(response ~ Herb_100m)

summary(Herb100m_HazCox)

#        [Herb 200m Model]                                                  ####
Herb200m_HazCox <- coxph(response ~ Herb_200m)

summary(Herb200m_HazCox)


#        [Herb 400m Model]                                                  ####
Herb400m_HazCox <- coxph(response ~ Herb_400m)

summary(Herb400m_HazCox)


#        [Crop 100m Model]                                                  ####
Crop100m_HazCox <- coxph(response ~ Crop_100m)

summary(Crop100m_HazCox)


#        [Crop 200m Model] (Somewhat Significant)                           ####
Crop200m_HazCox <- coxph(response ~ Crop_200m)

summary(Crop200m_HazCox)



#        [Crop 400m Model] (Somewhat Significant)                           ####
Crop400m_HazCox <- coxph(response ~ Crop_400m)

summary(Crop400m_HazCox)



#        [Other 100m Model]                                                 ####
Other100m_HazCox <- coxph(response ~ Other_100m)

summary(Other100m_HazCox)


#        [Other 200m Model]                                                 ####
Other200m_HazCox <- coxph(response ~ Other_200m)

summary(Other200m_HazCox)



#        [Other 400m Model]                                                 ####
Other400m_HazCox <- coxph(response ~ Other_400m)

summary(Other400m_HazCox)



#        [Forest Edge 100m Model]                                           ####
ForestEdge100m_HazCox <- coxph(response ~ ForestEdge_100m)

summary(ForestEdge100m_HazCox)


#        [Forest Edge 200m Model]                                           ####
ForestEdge200m_HazCox <- coxph(response ~ ForestEdge_200m)

summary(ForestEdge200m_HazCox)



#        [Forest Edge 400m Model]                                           ####
ForestEdge400m_HazCox <- coxph(response ~ ForestEdge_400m)

summary(ForestEdge400m_HazCox)



#        [Basal Area Model]                                                 ####

BasalArea_HazCox <- coxph(response ~ BasalArea)

summary(BasalArea_HazCox)

#        [Forest Edge Dist Model]                                           ####

ForestEdgeDist_HazCox <- coxph(response ~ ForestEdgeDist)

summary(ForestEdgeDist_HazCox)

#        [Height GC Model]                                                  ####

HeightGC_HazCox <- coxph(response ~ Height_GC)

summary(HeightGC_HazCox)

#        [Percent Canopy Closure Model]                                     ####

PercentCanopyClosure_HazCox <- coxph(response ~ Percent_CanopyClosure)

summary(PercentCanopyClosure_HazCox)

#        [Percent Bare Ground Litter Model]                                 ####

PercentBareGroundLitter_HazCox <- coxph(response ~ Percent_BareGroundLitter)

summary(PercentBareGroundLitter_HazCox)

#        [Percent Grasses Model]                                            ####

PercentGrasses_HazCox <- coxph(response ~ Percent_Grasses)

summary(PercentGrasses_HazCox)

#        [Percent Forbs Model]                                              ####

PercentForbs_HazCox <- coxph(response ~ Percent_Forbs)

summary(PercentForbs_HazCox)

#        [Percent WoodyVeg Model]                                           ####

PercentWoodyVeg_HazCox <- coxph(response ~ Percent_WoodyVeg)

summary(PercentWoodyVeg_HazCox)

#        [Percent SC1 Model]                                                ####

PercentSC1_HazCox <- coxph(response ~ Percent_SC1)

summary(PercentSC1_HazCox)

#        [Percent SC2 Model]                                                ####

PercentSC2_HazCox <- coxph(response ~ Percent_SC2)

summary(PercentSC2_HazCox)

#        [Percent SC3 Model]                                                ####

PercentSC3_HazCox <- coxph(response ~ Percent_SC3)

summary(PercentSC3_HazCox)








#        [Precipitation Amount Model]                                       ####

MeanPrecip_HazCox <- coxph(response ~ PrecipAmount)

summary(MeanPrecip_HazCox)

#        [Precipitation Days Model]                                         ####

PrecipDays_HazCox <- coxph(response ~ PrecipDays)

summary(PrecipDays_HazCox)

#        [Mean Temp Model]                                                  ####

MeanTemp_HazCox <- coxph(response ~ MeanTemp)

summary(MeanTemp_HazCox)

#        [MaxTemp Model]                                                    ####

MaxTemp_HazCox <- coxph(response ~ MaxTemp)

summary(MaxTemp_HazCox)

#        [MinTemp Model]                                                    ####

MinTemp_HazCox <- coxph(response ~ MinTemp)

summary(MinTemp_HazCox)


#   Model Diagnostics                                                       ####

# Testing for the proportional-hazards assumption

# Notes: Based on the ouput of this model, you need to make sure that the test
# is not statistically for any of the covariates. If they are not statistically
# significant, you can assume proportional hazards. 


test.ph <- cox.zph(HazCox)

ggcoxzph(test.ph)



###############################################################################
