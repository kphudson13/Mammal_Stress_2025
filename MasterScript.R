
# Load packages and data --------------------------------------------------

library(ape)
library(rotl) #to pull from Open Tree of Life
library(nlme) #for gls
library(tidyverse)
library(geiger) #for name.check
library(rr2) #for the R2 function
library(grid) #to set table themes
library(gridExtra) #to set table themes
library(cowplot) #to combine plots 
library(beepr) #for sound notifications
library(cowsay) #for the fun frog

rm(list=ls())

# #store shapes so all orders match in geom_point functions
# shapes <- c("Artiodactyla" = 0, 
#             "Carnivora" = 1,
#             "Chiroptera"= 2,
#             "Cingulata" = 3,
#             "Rodentia" = 4,
#             "Lagomorpha" = 11,
#             "Perissodactyla" = 15,
#             "Proboscidea" = 16,
#             "Primates" = 17, 
#             "Pilosa" = 19)

#To view data structure
#str(read.nexus("StressTree.nex"))

# Crtstn Uncorrected ------------------------------------------------------------

tree <- read.nexus("Corticosterone/StressTree.nex")

StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
rownames(StressData) = StressData$Species

directory <- "Corticosterone/CrtstnUncorrected/"

Label <- "Corticosterone - Uncorrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
source("AICScript.R")

# Crtstn Wet Corrected Feces ----------------------------------------------

tree <- read.nexus("Corticosterone/StressTree.nex")

#correct wet samples by dividing by 4
StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
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

directory <- "Corticosterone/CrtstnWetCorrected/"

Label <- "Corticosterone - Wet Corrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
source("AICScript.R")

# Cortisol Uncorrected ----------------------------------------------------

tree <- read.nexus("Cortisol/StressTree.nex")

StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "Cortisol/CortisolUncorrected/"

Label <- "Cortisol - Uncorrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
source("AICScript.R")

# Cortisol Wet Corrected --------------------------------------------------

tree <- read.nexus("Cortisol/StressTree.nex")

#correct wet samples by dividing by 4
StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
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

directory <- "Cortisol/CortisolWetCorrected/"

Label <- "Cortisol - Wet Corrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
source("AICScript.R")

# FGC Uncorrected ---------------------------------------------------------
    
tree <- read.nexus("FGCAnalysis/StressTree.nex")  

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "FGCAnalysis/FGCUncorrected/"

Label <- "FGC - Uncorrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
# source("AICScript.R")

# FGC Wet Corrected -------------------------------------------------------

tree <- read.nexus("FGCAnalysis/StressTree.nex")  

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
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

directory <- "FGCAnalysis/FGCWetCorrected/"

Label <- "FGC - Wet Corrected Model"

source("Workingscript.R")
source("PhyloSigScript.R")
# source("AICScript.R")     

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
beep(10)


