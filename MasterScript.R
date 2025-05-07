
# Load packages and data --------------------------------------------------

library(ape)
library(tidyverse)
library(beepr)
library(cowsay)

BaseWD <- "C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R" #Default WD
setwd(BaseWD) #make sure default WD is set correctly 

#store shapes so all orders match in geom_point functions
shapes <- c("Artiodactyla" = 0, 
            "Carnivora" = 1,
            "Chiroptera"= 2,
            "Cingulata" = 3,
            "Rodentia" = 4,
            "Lagomorpha" = 11,
            "Perissodactyla" = 15,
            "Proboscidea" = 16,
            "Primates" = 17, 
            "Pilosa" = 19)

#To view data structure
#str(read.nexus("StressTree.nex"))

# Crtstn Uncorrected ------------------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "Corticosterone - Uncorrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnUncorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)

# Crtstn Wet Corrected Feces ----------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

tree <- read.nexus("StressTree.nex")

#correct wet samples by dividing by 4
StressData <- read.csv("CrtstnDataClean.csv") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC, 
      FecesMass == "dry" ~ BasalFGC/4,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC, 
      FecesMass == "dry" ~ ElevFGC/4,
      TRUE ~ ElevFGC/4)) %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "Corticosterone - Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)

# Cortisol Uncorrected ----------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "Cortisol - Uncorrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol/CortisolUncorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)

# Cortisol Wet Corrected --------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol")

tree <- read.nexus("StressTree.nex")

StressData <- read.csv("CortisolDataClean.csv")

#correct wet samples by dividing by 4
StressData <- read.csv("CortisolDataClean.csv") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC, 
      FecesMass == "dry" ~ BasalFGC/4,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC, 
      FecesMass == "dry" ~ ElevFGC/4,
      TRUE ~ ElevFGC/4)) %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "Cortisol - Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol/CortisolWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)

# FGC Uncorrected ---------------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/FGCAnalysis")
      
tree <- read.nexus("StressTree.nex")  

StressData <- read.csv("FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "FGC - Uncorrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/FGCAnalysis/FGCUncorrected")
      
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)      

# FGC Wet Corrected -------------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/FGCAnalysis")

tree <- read.nexus("StressTree.nex")  

StressData <- read.csv("FGCDataClean.csv") %>%
  mutate(
    BasalFGC = case_when(
      FecesMass == "wet" ~ BasalFGC,
      FecesMass == "dry" ~ BasalFGC/4,
      TRUE ~ BasalFGC/4),
    ElevFGC = case_when(
      FecesMass == "wet" ~ ElevFGC, 
      FecesMass == "dry" ~ ElevFGC/4,
      TRUE ~ ElevFGC/4)) %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

Label <- "FGC - Wet Corrected Model"

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/FGCAnalysis/FGCWetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Workingscript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PhyloSigScript.R")
source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AICScript.R")

setwd(BaseWD)      

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
beep(10)


