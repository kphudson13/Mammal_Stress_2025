
# Load packages and data --------------------------------------------------

library(ape)
library(tidyverse)
library(beepr)

BaseWD <- "C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R" #Default WD
setwd(BaseWD) #make sure default WD is set correctly 


#to view the lists lining up
#cbind(sort(tree$tip.label), sort(unique(CleanData$Species)))
#To view data structure
#str(read.nexus("StressTree.nex"))

# Crtstn Uncorrected ------------------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CrtstnDataClean.csv")

Label <- "Unfilted Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnUncorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)

# Crtstn Wet Corrected Feces ----------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

#correct wet samples by dividing by 4
StressData <- read.csv("CrtstnDataClean.csv") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC/4, 
      FecesMass == "dry" ~ BasalFGC,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC/4, 
      FecesMass == "dry" ~ ElevFGC,
      TRUE ~ ElevFGC/4))

Label <- "Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)

# Crtstn Bat Filter -----------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CrtstnDataClean.csv") %>%
  filter(Order != "Chiroptera") 

Label <- "Bat Filtered Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnBat")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)


# Crtstn Bat Filter and wet corrected -------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CrtstnDataClean.csv") %>%
  filter(Order != "Chiroptera") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC/4, 
      FecesMass == "dry" ~ BasalFGC,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC/4, 
      FecesMass == "dry" ~ ElevFGC,
      TRUE ~ ElevFGC/4)) 

Label <- "Bat and Wet Corrected Model"  

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnBatAndWet")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)


# Cortisol Uncorrected ----------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CortisolDataClean.csv")

Label <- "Unfilted Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol/CortisolUncorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)

# Cortisol Wet Corrected --------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CortisolDataClean.csv")

#correct wet samples by dividing by 4
StressData <- read.csv("CortisolDataClean.csv") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC/4, 
      FecesMass == "dry" ~ BasalFGC,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC/4, 
      FecesMass == "dry" ~ ElevFGC,
      TRUE ~ ElevFGC/4))

Label <- "Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol/CortisolWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")

setwd(BaseWD)
