
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

# Unfiltered Data ---------------------------------------------------------

StressData <- CleanData

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/Unfiltered")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AnalysisScript2025.R")

setwd(BaseWD)

# Primate filter ----------------------------------------------------------


StressData <- CleanData %>% 
  filter(Group != "Old Primate",
         Group != "New Primate")

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/Primate_Filtered")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AnalysisScript2025.R")

setwd(BaseWD)

# Wet Corrected Feces -----------------------------------------------------

StressData <- CleanData %>%
  mutate(BasalCortisol = case_when(
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
      TRUE ~ ElevCorticosterone/4  
    ))

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Outputs/WetCorrected")

source("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/AnalysisScript2025.R")

setwd(BaseWD)

