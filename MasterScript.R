
# Load packages and data --------------------------------------------------

library(ape)
library(tidyverse)

BaseWD <- "C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R" #Default WD
setwd(BaseWD) #make sure default WD is set correctly 
CleanData <- read.csv("StressDataClean.csv")
tree <- read.nexus("Outputs/StressTree_AllSpecies.nex")

plot(tree)

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(CleanData$Species)))
#To view data structure
str(CleanData)


# Crtstn Model ------------------------------------------------------------

StressData <- CleanData

Label <- "Unfilted Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/CrtstnModel")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)


# Crtstn Wet Corrected Feces ----------------------------------------------

#correct wet samples by dividing by 4
StressData <- CleanData %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC/4, 
      FecesMass == "dry" ~ BasalFGC,
      TRUE ~ BasalFGC/4),
    BasalCortisol = case_when(
      FecesMass == "wet" ~ BasalCortisol/4, 
      FecesMass == "dry" ~ BasalCortisol,
      TRUE ~ BasalCortisol/4),
    BasalCorticosterone = case_when(
      FecesMass == "wet" ~ BasalCorticosterone/4, 
      FecesMass == "dry" ~ BasalCorticosterone,
      TRUE ~ BasalCorticosterone/4),
    ElevCortisol = case_when(
      FecesMass == "wet" ~ ElevCortisol/4, 
      FecesMass == "dry" ~ ElevCortisol,
      TRUE ~ ElevCortisol/4),
    ElevCorticosterone = case_when(
      FecesMass == "wet" ~ ElevCorticosterone/4, 
      FecesMass == "dry" ~ ElevCorticosterone,
      TRUE ~ ElevCorticosterone/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC/4, 
      FecesMass == "dry" ~ ElevFGC,
      TRUE ~ ElevFGC/4))

Label <- "Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/CrtstnWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)

# Crtstn Primate filter -----------------------------------------------------

#Remove primates
StressData <- CleanData %>% 
  filter(Group != "Yes/NWP")

Label <- "Primate Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/CrtstnPrimate")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)


# Primate filter and wet corrected ----------------------------------------

#correct wet samples by dividing by 4
StressData <- CleanData %>%
  filter(Group != "Yes/NWP") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC/4, 
      FecesMass == "dry" ~ BasalFGC,
      TRUE ~ BasalFGC/4),
    BasalCortisol = case_when(
      FecesMass == "wet" ~ BasalCortisol/4, 
      FecesMass == "dry" ~ BasalCortisol,
      TRUE ~ BasalCortisol/4),
    BasalCorticosterone = case_when(
      FecesMass == "wet" ~ BasalCorticosterone/4, 
      FecesMass == "dry" ~ BasalCorticosterone,
      TRUE ~ BasalCorticosterone/4),
    ElevCortisol = case_when(
      FecesMass == "wet" ~ ElevCortisol/4, 
      FecesMass == "dry" ~ ElevCortisol,
      TRUE ~ ElevCortisol/4),
    ElevCorticosterone = case_when(
      FecesMass == "wet" ~ ElevCorticosterone/4, 
      FecesMass == "dry" ~ ElevCorticosterone,
      TRUE ~ ElevCorticosterone/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC/4, 
      FecesMass == "dry" ~ ElevFGC,
      TRUE ~ ElevFGC/4)) 

Label <- "Primate and Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/CrtstnPrimateAndWet")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AnalysisScript2025.R")

setwd(BaseWD)

