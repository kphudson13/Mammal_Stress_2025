
# Master script for core data analysis
# This script controls the flow of the analysis and runs the individual scripts
# It is dependent on outputs from CleanAndTree.R, but those are already in the the remote repository
# I realize leaving outputs and binaries is bad practice, but running CleanAndTree requires an API key
# I also want the original .nex files saved, so if taxonomy changes we can still confirm our analysis
# live laugh love -Kyle


# Load packages ------------------------------------------------------------

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
library(phytools) #for use in the phylo signal script

rm(list=ls())


# Crtstn Uncorrected --------------------------------------------------------

#Load tree build by CleanAndTree.R
tree <- read.nexus("Corticosterone/StressTree.nex")

#Load data and clean it up
StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
rownames(StressData) = StressData$Species

#set directory for these data
directory <- "Corticosterone/CrtstnUncorrected/"

#make a figure directory for initial pull users
if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

#label figures
Label <- "Corticosterone - Uncorrected Model"

#run the scripts
source("Code/WorkingScript.R")
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

if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Corticosterone - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# Cortisol Uncorrected ----------------------------------------------------

tree <- read.nexus("Cortisol/StressTree.nex")

StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "Cortisol/CortisolUncorrected/"

if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Cortisol - Uncorrected Model"

source("Code/WorkingScript.R")
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

if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "Cortisol - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# FGC Uncorrected ---------------------------------------------------------

tree <- read.nexus("FGCAnalysis/StressTree.nex")  

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "FGCAnalysis/FGCUncorrected/"

if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "FGC - Uncorrected Model"

source("Code/WorkingScript.R")
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

if(file.exists(directory)) {
  if(file.exists(paste(directory, "Figures", sep = ""))) {
    #do nothing
  } else {
    dir.create(paste(directory, "Figures", sep = ""))
  }
} else {
  dir.create(directory)
  dir.create(paste(directory, "Figures", sep = ""))
}

Label <- "FGC - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
# source("Code/AICScript.R")     

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
beep(10)
