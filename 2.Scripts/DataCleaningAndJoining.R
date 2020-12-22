#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-10-28 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####
##  Library                                                                 ####
library(tidyverse)
library(dplyr)

##  Functions                                                               ####

##  Data                                                                    ####
Macro <- read_csv("1.Data\\Data_Landscape_Metrics.csv") %>% as.data.frame()
Micro <- read_csv("1.Data\\Data_Micro_Metrics.csv") %>% as.data.frame()
Incubation <- read_csv("1.Data\\Data_Incubation_Timing.csv")

###############################################################################
#   Adding Nesting Event ID                                                 ####

#   Changing all of the Column Names                                        ####

# Macro Dataframe
Macro <- data.frame(Hen_ID = Macro$`Hen ID`,
                    Nest_Type = Macro$`Plot Type`,
                    Year = Macro$Year,
                    Nest_Number = Macro$`Nest #`,
                    Nest_Fate = Macro$`Nest Fatea`,
                    Buffer_Radius = Macro$`Radius (m) of Buffer`,
                    Amount_Forest = Macro$`Amount Forest (ha)`,
                    Amount_Herb = Macro$`Amount Herb. (ha)`,
                    Amount_Crop = Macro$`Amount Crop. (ha)`,
                    Amount_Other = Macro$`Amount Other (ha)`,
                    Amount_ForestEdge = Macro$`Amount Forest Edge (m)`
)

# Micro Dataframe
Micro <- data.frame(Hen_ID = as.character(Micro$`Hen ID`),
                    Nest_Type = Micro$`Plot Type`,
                    Year = str_match(Micro$Date, pattern = "2016|2017|2018"),
                    Nest_Number = Micro$`Nest #`,
                    Nest_Fate = Micro$`Nest Fate`,
                    Location_X = Micro$`Location X`,
                    Location_Y = Micro$`Location Y`,
                    Habitat_Type = Micro$`Habitat Type`,
                    Basal_Area = Micro$`Basal Area`,
                    ForestEdgeDistance = Micro$`Forest Edge Distance`,
                    Height_GC = Micro$`GC Height`,
                    Percent_CanopyClosure = Micro$`% Canopy Closure`,
                    Percent_BareGroundtoLiter = Micro$`% BG/Litter`,
                    Percent_Grasses = Micro$`% Grasses`,
                    Percent_Forbs = Micro$`% Forbs`,
                    Percent_WoodyVeg = Micro$`% Woody Veg.`,
                    Percent_ScreeningCover_0_0.5 = Micro$`% SC (0.0-0.5 m)`,
                    Percent_ScreeningCover_0.5_1.0 = Micro$`% SC (0.5-1.0 m)`,
                    Percent_ScreeningCover_1.0_1.8 = Micro$`% SC (1.0-1.8 m)`)

# Incubation 
Incubation <- data.frame(Hen_ID = Incubation$`Hen ID`,
                         Nest_Type = Incubation$`Plot Type`,
                         Year = Incubation$Year,
                         Nest_Number = Incubation$`Nest #`,
                         Nest_Fate = Incubation$`Nest Fate`,
                         Incubation_Start = Incubation$`Incubation Start`,
                         Incubation_End = Incubation$`Incubation End`,
                         NumberOfDays = Incubation$n_days)

# Creating the List of Unique Events and adding it into the dataframe
NestingEvent <- rep(1:(nrow(Macro)/3), each=3)

Macro$NestingEvent <- NestingEvent

#   Making a function to automate column join and performing transformation ####

# Function that Performs the column breakdown
df_create<- function(index){
  
  Extracted <- subset(Macro,Macro$NestingEvent == index) # Taking a unique event out 
  
  # Organizing the Data and Changing the Column names 
  BaseData <- Extracted[1,1:5]                                                 # Getting the base information of the nesting event 
  Res100 <- subset(Extracted, Buffer_Radius == 100) %>% .[,7:(ncol(.)-1)] # 100m buffer info 
  colnames(Res100) <- paste(colnames(Res100),"100m", sep = "_")           # Adding Buffer size suffix 
  Res200 <- subset(Extracted, Buffer_Radius == 200) %>% .[,7:(ncol(.)-1)] # 200m buffer info
  colnames(Res200) <- paste(colnames(Res200),"200m", sep = "_")           # Adding Buffer size suffix
  Res400 <- subset(Extracted, Buffer_Radius == 400) %>% .[,7:(ncol(.)-1)] # 400m buffer info
  colnames(Res400) <- paste(colnames(Res400),"400m", sep = "_")           # Adding Buffer size suffix
  
  return(Final_event <- do.call(cbind,c(BaseData,Res100,Res200,Res400)) %>% as.data.frame())
}

# Initializing function and creating dataframe
index <- unique(unique(NestingEvent))
Macro <- lapply(index, df_create)
MacroFinal <- do.call(rbind,Macro)



#   Forming the dataframe and final cleaning/export                         ####

# Changing the Structure to be the same as Micro 
MacroFinal$Hen_ID <- as.numeric(MacroFinal$Hen_ID)
Micro$Hen_ID <- as.numeric(Micro$Hen_ID)
MacroFinal$Nest_Number<- as.numeric(MacroFinal$Nest_Number)

Final <- left_join(MacroFinal,Micro, 
                   by = c("Hen_ID", "Nest_Type", "Year", "Nest_Number","Nest_Fate"),
                   copy = T) 

# Adding Incubation Data 
Incubation$Year <- as.character(Incubation$Year)


Final <- left_join(Final,Incubation,by = c("Hen_ID", "Nest_Type", "Year", "Nest_Number","Nest_Fate"))

# Removing Abandoned Nest sites since they were human induced

Final <- subset(Final, Final$Nest_Fate != "Abandoned")

# Re-coding Nest Succes (1 = Success, 0 = Failed, N/A = Randomized Data Point)

Final$Nest_Fate <- ifelse(Final$Nest_Fate == "Successful", 1,
                          ifelse(Final$Nest_Fate == "Failed", 0,
                                 ifelse(Final$Nest_Fate == "Random", NA,"error")))
# Exporting the Data
write.csv(Final,"1.Data/Final.csv",
          row.names = F)
