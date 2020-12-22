#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-11-15 

# Purpose: Data Exploration 

###############################################################################
#   Library / Functions / Data                                              ####

##  Library                                                                 ####
library(tidyverse)
library(ggplot2)

##  Functions                                                               ####

##  Data                                                                    ####
Data <- read.csv("1.Data/Final_MissingCases.csv")

###############################################################################
#                         Site Selection Analysis                           ####
##  Data Prep                                                               ####

# Eliminating Hens with repeat nest
Data_NestSelection <- subset(Data_MC, Data_MC$Nest_Number == 1) %>% .[,c(1:(ncol(Data_MC)-3))]

# Changing the Nest type to a binary variable
Data_NestSelection$Nest_Type <- ifelse(Data_NestSelection$Nest_Type == "Nest",1,0) # 1 = Used ; 0 = Available

##  Variable Prep                                                           ####
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



# Model 

m1 <- clogit(response ~ Micro01 + strata(strata),data = Data_NestSelection)

summary(m1)


##  Model Sets                                                              ####


################################################################################
#                          Nest Success Plotting                            ####

#
##  Data Prep                                                               ####

# Removing Random nest from the data 
Data_CoxHaz <- subset(Data, Data$Nest_Type == "Nest") %>% na.omit()

# Making Sure Number of days is a numeric variable 
Data_CoxHaz$NumberOfDays <- as.numeric(Data_CoxHaz$NumberOfDays)
Data_CoxHaz$Nest_Fate    <- ifelse(Data_CoxHaz$Nest_Fate == 1, "Successful","Failed")
Data_CoxHaz$Habitat_Type <- factor(x = Data_CoxHaz$Habitat_Type, levels = c("Opening","Edge","Forest"))

# Making Long Version of Data Set

Data_CoxHazLong <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                              y = c(Data_CoxHaz$Amount_Forest_100m,
                                    Data_CoxHaz$Amount_Forest_200m,
                                    Data_CoxHaz$Amount_Forest_400m),
                              Variables = c(rep("Forest_100m", nrow( Data_CoxHaz)),
                                            rep("Forest_200m", nrow( Data_CoxHaz)),
                                            rep("Forest_400m", nrow( Data_CoxHaz))))
##  Example with one Variable                                               ####

base <- ggplot(data = Data_CoxHaz)

base + geom_count(aes(base$data$Nest_Fate,base$data$Habitat_Type)) + 
  ggtitle("Habitat Type vs. Successful/Failed") + 
  theme_classic() +
  xlab("Nest Fate") +
  ylab("Habitat Type") 
  

#                          Macro Variables                                  ####


### Forest Buffer                                                           ####
Data <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                   y = c(Data_CoxHaz$Amount_Forest_100m,
                         Data_CoxHaz$Amount_Forest_200m,
                         Data_CoxHaz$Amount_Forest_400m),
                   Variables = c(rep("Forest_100m", nrow( Data_CoxHaz)),
                                 rep("Forest_200m", nrow( Data_CoxHaz)),
                                 rep("Forest_400m", nrow( Data_CoxHaz))))

ggplot(Data, aes(Nest_Fate, y)) +
  geom_boxplot() + 
  facet_grid(Variables ~ .) +
  theme_bw() +
  ylab("Numbers of Hectares") +
  xlab("Nest Fate")

    
### Forest Edge Buffer                                                      ####
Data <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                   y = c(Data_CoxHaz$Amount_ForestEdge_100m,
                         Data_CoxHaz$Amount_ForestEdge_200m,
                         Data_CoxHaz$Amount_ForestEdge_400m),
                   Variables = c(rep("ForestEdge_100m", nrow( Data_CoxHaz)),
                                 rep("ForestEdge_200m", nrow( Data_CoxHaz)),
                                 rep("ForestEdge_400m", nrow( Data_CoxHaz))))

ggplot(Data, aes(Nest_Fate, y)) +
  geom_boxplot() + 
  facet_grid(Variables ~ .) +
  theme_bw() +
  ylab("Amount of Forest Edge (m)") +
  xlab("Nest Fate")



### Crop Buffer                                                             ####
Data <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                   y = c(Data_CoxHaz$Amount_Crop_100m,
                         Data_CoxHaz$Amount_Crop_200m,
                         Data_CoxHaz$Amount_Crop_400m),
                   Variables = c(rep("Crop_100m", nrow( Data_CoxHaz)),
                                 rep("Crop_200m", nrow( Data_CoxHaz)),
                                 rep("Crop_400m", nrow( Data_CoxHaz))))

ggplot(Data, aes(Nest_Fate, y)) +
  geom_boxplot() + 
  facet_grid(Variables ~ .) +
  theme_bw() +
  ylab("Numbers of Hectares") +
  xlab("Nest Fate")
  

### Herb Buffer                                                             ####
Data <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                   y = c(Data_CoxHaz$Amount_Herb_100m,
                         Data_CoxHaz$Amount_Herb_200m,
                         Data_CoxHaz$Amount_Herb_400m),
                   Variables = c(rep("Herb_100m", nrow( Data_CoxHaz)),
                                 rep("Herb_200m", nrow( Data_CoxHaz)),
                                 rep("Herb_400m", nrow( Data_CoxHaz))))

ggplot(Data, aes(Nest_Fate, y)) +
  geom_boxplot() + 
  facet_grid(Variables ~ .) +
  theme_bw() +
  ylab("Numbers of Hectares") +
  xlab("Nest Fate")


### Other Buffer                                                            ####
Data <- data.frame(Nest_Fate = Data_CoxHaz$Nest_Fate,
                   y = c(Data_CoxHaz$Amount_Other_100m,
                         Data_CoxHaz$Amount_Other_200m,
                         Data_CoxHaz$Amount_Other_400m),
                   Variables = c(rep("Other_100m", nrow( Data_CoxHaz)),
                                 rep("Other_200m", nrow( Data_CoxHaz)),
                                 rep("Other_400m", nrow( Data_CoxHaz))))

ggplot(Data, aes(Nest_Fate, y)) +
  geom_boxplot() + 
  facet_grid(Variables ~ .) +
  theme_bw() +
  ylab("Numbers of Hectares") +
  xlab("Nest Fate")


#                          Micro Variables                                  ####



### Habitat Type                                                            ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Habitat_Type)) +
  geom_count() + 
  theme_bw() +
  ylab("Habitat_Type") +
  xlab("Nest Fate")

### Basal_Area                                                              ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Basal_Area)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Basal_Area") +
  xlab("Nest Fate")

### ForestEdgeDistance                                                      ####
ggplot(Data_CoxHaz, aes(Nest_Fate, ForestEdgeDistance)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Distance to Forest Edge (m)") +
  xlab("Nest Fate")

### Height_GC                                                               ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Height_GC)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Height of Ground Cover (m)") +
  xlab("Nest Fate")

### Percent Canopy Closure                                                  ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_CanopyClosure)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Canopy Closure (%)") +
  xlab("Nest Fate")


### Percent Bare Ground to Litter                                           ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_BareGroundtoLiter)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Bare Ground to Litter (%)") +
  xlab("Nest Fate")

### Percent Grasses                                                         ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_Grasses)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Grasses (%)") +
  xlab("Nest Fate")


### Percent Forbs                                                           ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_Forbs)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Forbs (%)") +
  xlab("Nest Fate")

### Percent WoodyVeg                                                        ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_WoodyVeg)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Woody Vegetation (%)") +
  xlab("Nest Fate")

### Percent SC1                                                             ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_ScreeningCover_0_0.5)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Percent Screening Cover (1.0 - 1.8m)") +
  xlab("Nest Fate")

### Percent SC2                                                             ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_ScreeningCover_0.5_1.0)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Percent Screening Cover (0.5 - 1.0m)") + 
  xlab("Nest Fate")

### Percent SC3                                                             ####
ggplot(Data_CoxHaz, aes(Nest_Fate, Percent_ScreeningCover_1.0_1.8)) +
  geom_boxplot() + 
  theme_bw() +
  ylab("Percent Screening Cover (1.0 - 1.8m)") +
  xlab("Nest Fate")


###############################################################################

