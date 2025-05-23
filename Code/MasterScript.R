
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


# Crtstn Uncorrected ------------------------------------------------------------

tree <- read.nexus("Corticosterone/StressTree.nex")

StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
rownames(StressData) = StressData$Species

directory <- "Corticosterone/CrtstnUncorrected/"

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }

Label <- "Corticosterone - Uncorrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

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

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
} else {
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Corticosterone - Wet Corrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# Cortisol Uncorrected ----------------------------------------------------

tree <- read.nexus("Cortisol/StressTree.nex")

StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "Cortisol/CortisolUncorrected/"

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
} else {
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Cortisol - Uncorrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

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

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
} else {
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Cortisol - Wet Corrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# FGC Uncorrected ---------------------------------------------------------
    
tree <- read.nexus("FGCAnalysis/StressTree.nex")  

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "FGCAnalysis/FGCUncorrected/"

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
} else {
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "FGC - Uncorrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
# source("Code/AICScript.R")

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

#make a figure directory for initial pull users
if(file.exists(paste(directory, "Figures", sep = ""))) {
} else {
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "FGC - Wet Corrected Model"

source("Code/Workingscript.R")
source("Code/PhyloSigScript.R")
# source("Code/AICScript.R")     

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
beep(10)


