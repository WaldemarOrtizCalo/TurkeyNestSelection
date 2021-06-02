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

#      Model Sets                                                           ####

m1 <- clogit(response ~ Percent_Forbs + strata(strata),data = Data_NestSelection)
#        [Global Model]                                 No Convergance      ####

Global_NestSelect <- clogit(response ~ HabitatType + 
                         Forest_100m + Forest_200m + Forest_400m +
                         Herb_100m + Herb_200m + Herb_400m +
                         Crop_100m + Crop_200m + Crop_400m +
                         Other_100m + Other_200m + Other_400m +
                         ForestEdge_100m + ForestEdge_200m + ForestEdge_400m +
                         BasalArea + ForestEdgeDist + Height_GC +
                         Percent_CanopyClosure + Percent_BareGroundLitter +
                         Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                         Percent_SC1 + Percent_SC2 + Percent_SC3 +
                         strata(strata),data = Data_NestSelection)

summary(Global_NestSelect)

#        [Habitat Model]                                Close               ####

Habitat_NestSelect <- clogit(response ~ HabitatType + 
                         strata(strata),data = Data_NestSelection)

summary(Habitat_NestSelect)

#        [Micro Model]                                  Close               ####

Micro_NestSelect <- glm(response ~ BasalArea + ForestEdgeDist + Height_GC +
                        Percent_CanopyClosure + Percent_BareGroundLitter +
                        Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                        Percent_SC1 + Percent_SC2 + Percent_SC3, 
                        data = Data_NestSelection, family = "binomial")

summary(Micro_NestSelect)


#        [Macro Model]                                  Very Significant    ####
Macro_NestSelect <- glm(response ~ Forest_100m + Forest_200m + Forest_400m +
                        Herb_100m + Herb_200m + Herb_400m +
                        Crop_100m + Crop_200m + Crop_400m +
                        Other_100m + Other_200m + Other_400m +
                        ForestEdge_100m + ForestEdge_200m + ForestEdge_400m, 
                        data = Data_NestSelection, family = "binomial")

summary(Macro_NestSelect)

#        [Global 100m Model]                            Very Significant    ####
Macro100m_NestSelect <- clogit(response ~ Forest_100m + Herb_100m + Crop_100m + 
                             Other_100m + ForestEdge_100m + 
                             strata(strata),data = Data_NestSelection)

summary(Macro100m_NestSelect)

#        [Global 200m Model]                            Very Significant    ####
Macro200m_NestSelect <- clogit(response ~ Forest_200m + Herb_200m + Crop_200m + 
                            Other_200m + ForestEdge_200m+ 
                             strata(strata),data = Data_NestSelection)

summary(Macro200m_NestSelect)

#        [Global 400m Model]                            Very Significant    ####
Macro400m_NestSelect <- clogit(response ~ Forest_400m + Herb_400m + Crop_400m + 
                            Other_400m + ForestEdge_400m + 
                             strata(strata),data = Data_NestSelection)

summary(Macro400m_NestSelect)

#        [Forest 100m Model]                            Close               ####
Forest100m_NestSelect <- clogit(response ~ Forest_100m + 
                              strata(strata),data = Data_NestSelection)

summary(Forest100m_NestSelect)

#        [Forest 200m Model]                            Close               ####
Forest200m_NestSelect <- clogit(response ~ Forest_200m + 
                              strata(strata),data = Data_NestSelection)

summary(Forest200m_NestSelect)

#        [Forest 400m Model]                            Significant         ####
Forest400m_NestSelect <- clogit(response ~ Forest_400m + 
                              strata(strata),data = Data_NestSelection)

summary(Forest400m_NestSelect)

#        [Herb 100m Model]                                                  ####
Herb100m_NestSelect <- clogit(response ~ Herb_100m + 
                            strata(strata),data = Data_NestSelection)

summary(Herb100m_NestSelect)

#        [Herb 200m Model]                                                  ####
Herb200m_NestSelect <- clogit(response ~ Herb_200m + 
                            strata(strata),data = Data_NestSelection)

summary(Herb200m_NestSelect)

#        [Herb 400m Model]                              Close               ####
Herb400m_NestSelect <- clogit(response ~ Herb_400m + 
                            strata(strata),data = Data_NestSelection)

summary(Herb400m_NestSelect)

#        [Crop 100m Model]                              Significant         ####
Crop100m_NestSelect <- clogit(response ~ Crop_100m + 
                            strata(strata),data = Data_NestSelection)

summary(Crop100m_NestSelect)

#        [Crop 200m Model]                              Significant         ####
Crop200m_NestSelect <- clogit(response ~ Crop_200m + 
                            strata(strata),data = Data_NestSelection)

summary(Crop200m_NestSelect)
 
#        [Crop 400m Model]                              Significant         ####
Crop400m_NestSelect <- clogit(response ~ Crop_400m + 
                            strata(strata),data = Data_NestSelection)

summary(Crop400m_NestSelect)

#        [Other 100m Model]                                                 ####
Other100m_NestSelect <- clogit(response ~ Other_100m + 
                             strata(strata),data = Data_NestSelection)

summary(Other100m_NestSelect)

#        [Other 200m Model]                                                 ####
Other200m_NestSelect <- clogit(response ~ Other_200m + 
                             strata(strata),data = Data_NestSelection)

summary(Other200m_NestSelect)

#        [Other 400m Model]                                                 ####
Other400m_NestSelect <- clogit(response ~ Other_400m + 
                             strata(strata),data = Data_NestSelection)

summary(Other400m_NestSelect)

#        [Forest Edge 100m Model]                       Significant         ####
ForestEdge100m_NestSelect <- clogit(response ~ ForestEdge_100m + 
                                  strata(strata),data = Data_NestSelection)

summary(ForestEdge100m_NestSelect)

#        [Forest Edge 200m Model]                       Significant         ####
ForestEdge200m_NestSelect <- clogit(response ~ ForestEdge_200m + 
                                  strata(strata),data = Data_NestSelection)

summary(ForestEdge200m_NestSelect)

#        [Forest Edge 400m Model]                       Significant         ####
ForestEdge400m_NestSelect <- clogit(response ~ ForestEdge_400m + 
                                  strata(strata),data = Data_NestSelection)

summary(ForestEdge400m_NestSelect)

#        [Basal Area Model]                                                 ####

BasalArea_NestSelect <- clogit(response ~ BasalArea + 
                             strata(strata),data = Data_NestSelection)

summary(BasalArea_NestSelect)

#        [Forest Edge Dist Model]                                           ####

ForestEdgeDist_NestSelect <- clogit(response ~ ForestEdgeDist + 
                                  strata(strata),data = Data_NestSelection)

summary(ForestEdgeDist_NestSelect)

#        [Height GC Model]                              Significant         ####

HeightGC_NestSelect <- clogit(response ~ Height_GC + 
                            strata(strata),data = Data_NestSelection)

summary(HeightGC_NestSelect)

#        [Percent Canopy Closure Model]                 Significant         ####

PercentCanopyClosure_NestSelect <- clogit(response ~ Percent_CanopyClosure + 
                                        strata(strata),data = Data_NestSelection)

summary(PercentCanopyClosure_NestSelect)

#        [Percent Bare Ground Litter Model]             Close               ####

PercentBareGroundLitter_NestSelect <- clogit(response ~ Percent_BareGroundLitter + 
                                           strata(strata),data = Data_NestSelection)

summary(PercentBareGroundLitter_NestSelect)

#        [Percent Grasses Model]                        Significant         ####

PercentGrasses_NestSelect <- clogit(response ~ Percent_Grasses + 
                                  strata(strata),data = Data_NestSelection)

summary(PercentGrasses_NestSelect)

#        [Percent Forbs Model]                                              ####

PercentForbs_NestSelect <- clogit(response ~ Percent_Forbs + 
                                strata(strata),data = Data_NestSelection)

summary(PercentForbs_NestSelect)

#        [Percent WoodyVeg Model]                       Significant         ####

PercentWoodyVeg_NestSelect <- clogit(response ~ Percent_WoodyVeg + 
                                   strata(strata),data = Data_NestSelection)

summary(PercentWoodyVeg_NestSelect)

#        [Percent SC1 Model]                            Significant         ####

PercentSC1_NestSelect <- clogit(response ~ Percent_SC1 + 
                              strata(strata),data = Data_NestSelection)

summary(PercentSC1_NestSelect)

#        [Percent SC2 Model]                            Significant         ####

PercentSC2_NestSelect <- clogit(response ~ Percent_SC2 + 
                              strata(strata),data = Data_NestSelection)

summary(PercentSC2_NestSelect)

#        [Percent SC3 Model]                            Significant         ####

PercentSC3_NestSelect <- clogit(response ~ Percent_SC3 + 
                              strata(strata),data = Data_NestSelection)

summary(PercentSC3_NestSelect)

#        [Final Model]                                  Significant         ####

FinalModel <- glm(response ~ Crop_200m + 
            Height_GC + Percent_Grasses,
          data = Data_NestSelection, family = "binomial")

summary(FinalModel)


ggcoef(FinalModel, exclude_intercept = TRUE, exponentiate = TRUE, sort = "ascending")

#        [Micro]                                                            ####

chart.Correlation(Data_NestSelection[,24:34], histogram=TRUE, pch=19)

Micro_NestSelectComp <- glm(response ~ BasalArea + ForestEdgeDist + Percent_BareGroundLitter+ Percent_Forbs +
                            Height_GC + Percent_WoodyVeg, 
                        data = Data_NestSelection, family = "binomial")

summary(Micro_NestSelectComp)

Micro_NestSelect <- glm(response ~ Height_GC + Percent_WoodyVeg, 
                        data = Data_NestSelection, family = "binomial")

summary(Micro_NestSelect)

ggcoef(Micro_NestSelect, exclude_intercept = TRUE, exponentiate = TRUE, sort = "ascending")

#        [Macro]                                                            ####

chart.Correlation(Data_NestSelection[,6:20], histogram=TRUE, pch=19)

Macro_NestSelect <- glm(response ~ Forest_200m + Herb_200m + 
                          Crop_200m + Other_200m + ForestEdge_200m,
                        data = Data_NestSelection, family = "binomial")

summary(Macro_NestSelect)

ggcoef(Macro_NestSelect, exclude_intercept = TRUE, exponentiate = TRUE, sort = "ascending")


###############################################################################
#   Site Success Analysis                                                   ####
#      Data Prep                                                            ####

# Removing Random nest from the data 
Data_CoxHaz <- subset(Data_Weather,Data_Weather$Nest_Type == "Nest")
Data_CoxHaz$Nest_Fate<-ifelse(Data_CoxHaz$Nest_Fate == 0,1,0)
# Making Sure Number of days is a numeric variable 
Data_CoxHaz$NumberOfDays <- as.numeric(Data_CoxHaz$NumberOfDays)
Data_CoxHaz$Nest_Fate <- as.numeric(Data_CoxHaz$Nest_Fate)

#      Variable Prep                                                        ####

#        [Variables]                                                        ####

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

#        [Checking for Correlations]                                        ####

# Checking for Correlated Variables

# All of the Variables
chart.Correlation(Data_CoxHaz[c(6:20,22:32,38:42)], histogram=TRUE, pch=19)

# Separated into Groups

chart.Correlation(Data_CoxHaz[6:20], histogram=TRUE, pch=19)

chart.Correlation(Data_CoxHaz[22:32], histogram=TRUE, pch=19)

chart.Correlation(Data_CoxHaz[38:42], histogram=TRUE, pch=19)

# Matrix
allcor <- cor(Data_CoxHaz[c(6:20,22:32,38:42)])
cor <- data.frame(round(allcor, 2))

write.csv(cor, file = "3.Output/CorrelationMatrix.csv")

#      Model Sets                                                           ####
#        [Global Model]            Significant                              ####

Global_HazCox <- coxph(response ~ HabitatType + 
                         Forest_100m + Forest_200m + Forest_400m +
                         Herb_100m + Herb_200m + Herb_400m +
                         Crop_100m + Crop_200m + Crop_400m +
                         Other_100m + Other_200m + Other_400m +
                         ForestEdge_100m + ForestEdge_200m + ForestEdge_400m +
                         BasalArea + ForestEdgeDist + Height_GC +
                         Percent_CanopyClosure + Percent_BareGroundLitter +
                         Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                         Percent_SC1 + Percent_SC2 + Percent_SC3+
                         PrecipAmount + PrecipDays + MeanTemp + MinTemp + MaxTemp
                         )

summary(Global_HazCox)



#        [Habitat Model]    Significant                                     ####
Habitat_HazCox <- coxph(response ~ HabitatType)

summary(Global_HazCox)
#        [Micro Model]                                                      ####
Micro_HazCox <- coxph(response ~ BasalArea + ForestEdgeDist + Height_GC +
                        Percent_CanopyClosure + Percent_BareGroundLitter +
                        Percent_Grasses + Percent_Forbs + Percent_WoodyVeg +
                        Percent_SC1 + Percent_SC2 + Percent_SC3)

summary(Micro_HazCox)

#        [Macro Model]       Significant                                    ####
Macro_HazCox <- coxph(response ~ Forest_100m + Forest_200m + Forest_400m +
                        Herb_100m + Herb_200m + Herb_400m +
                        Crop_100m + Crop_200m + Crop_400m +
                        Other_100m + Other_200m + Other_400m +
                        ForestEdge_100m + ForestEdge_200m + ForestEdge_400m)

summary(Macro_HazCox)

#        [Weather Model]                                                    ####
Weather_HazCox <- coxph(response ~ PrecipAmount + PrecipDays + MeanTemp + MinTemp + MaxTemp)

summary(Weather_HazCox)


#        [Global 100m Model]                                                ####
Macro100m_HazCox <- coxph(response ~ Forest_100m + Herb_100m + Crop_100m + 
                            Other_100m + ForestEdge_100m)

summary(Macro100m_HazCox)

#        [Global 200m Model]                                                ####
Macro200m_HazCox <- coxph(response ~ Forest_200m + Herb_200m + Crop_200m + 
                            Other_200m + ForestEdge_200m)

summary(Macro200m_HazCox)



#        [Global 400m Model]  Significant                                   ####
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


#        [Crop 200m Model]    Significant                                   ####
Crop200m_HazCox <- coxph(response ~ Crop_200m)

summary(Crop200m_HazCox)



#        [Crop 400m Model]                                                  ####
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



#        [Forest Edge 400m Model]      Close                                ####
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


#      Building Final Model                                                 ####

# Notes: Based on the Correlation Matrix Analysis, I removed any variables that
# had a correlation greater than 0.6. 

# Variable list 

FinalModel1 <- coxph(response ~ Forest_200m + Crop_200m + Other_200m + 
                          ForestEdgeDist+ BasalArea + Height_GC + Percent_Grasses + 
                          Percent_Forbs + HabitatType +
                          MeanTemp + PrecipAmount)

FinalModel2 <- coxph(response ~ Crop_200m +  
                     Percent_Forbs + HabitatType)

summary(FinalModel2)

#        [Macro]                                                            ####

chart.Correlation(Data_NestSelection[,6:20], histogram=TRUE, pch=19)


Macro_HazCoxComp <- coxph(response ~ Forest_200m +
                        Herb_200m +
                        Crop_200m +
                        Other_200m+
                        ForestEdge_200m)

summary(Macro_HazCoxComp)

Macro_HazCox <- coxph(response ~ Forest_200m +
                        Herb_200m)

summary(Macro_HazCox)

#        [Micro]                                                            ####

chart.Correlation(Data_NestSelection[,24:34], histogram=TRUE, pch=19)


Micro_HazCoxComp <- coxph(response ~ BasalArea + ForestEdgeDist + Height_GC +
                        Percent_Grasses + Percent_Forbs + Percent_WoodyVeg)

summary(Micro_HazCoxComp)


Micro_HazCox <- coxph(response ~ 1)

summary(Micro_HazCox)

#      Survival Plots                                                       ####

# Micro
ggsurvplot(survfit(Micro_HazCoxComp,data=Data_CoxHaz), color = "#2E9FDF",
           title = "Micro Model",
           ggtheme = theme_minimal())

# Macro
ggsurvplot(survfit(Macro_HazCoxComp,data=Data_CoxHaz), color = "#2E9FDF",
           title = "Macro Model",
           ggtheme = theme_minimal())

###############################################################################