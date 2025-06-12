
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


# Directory function ------------------------------------------------------

#make a figure directory for initial pull users
CreateDR <- function(DR) {
  if(file.exists(DR)) {
    if(file.exists(paste(DR, "Figures", sep = ""))) {
      #do nothing
    } else {
      dir.create(paste(DR, "Figures", sep = ""))
    }
  } else {
    dir.create(DR)
    dir.create(paste(DR, "Figures", sep = ""))
  }
}

# Crtstn Uncorrected --------------------------------------------------------

#Load tree build by CleanAndTree.R
tree <- read.nexus("Corticosterone/StressTree.nex")

#Load data and clean it up
StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
rownames(StressData) = StressData$Species

#set directory for these data
directory <- "Corticosterone/CrtstnUncorrected/"
CreateDR(directory)

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
CreateDR(directory)

Label <- "Corticosterone - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# Crtstn Lifespan Update --------------------------------------------------

tree <- read.nexus("Corticosterone/StressTree.nex")

Lifespan <- read.csv("LifespanData.csv")

#Load data and clean it up
StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
StressData <- merge(StressData, Lifespan, by = "Species", all.x = TRUE) 

#use mean lifespan, if not use 0.8 max lifespan 
for (i in 1:nrow(StressData)) {
  if (is.na(StressData$MeanLifespan[i]) == FALSE) {
    StressData$MaxLifespan[i] <- StressData$MeanLifespan[i]
  } else {
    StressData$MaxLifespan[i] <- StressData$MaxLifespan[i]*0.8
  }
}

rownames(StressData) = StressData$Species

#set directory for these data
directory <- "Corticosterone/CrtstnLifespan/"
CreateDR(directory)

#label figures
Label <- "Corticosterone - Lifespan Update"

#run the scripts
source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# Cortisol Uncorrected ----------------------------------------------------

tree <- read.nexus("Cortisol/StressTree.nex")

StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "Cortisol/CortisolUncorrected/"
CreateDR(directory)

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
CreateDR(directory)

Label <- "Cortisol - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# Cortisol Lifespan Update ------------------------------------------------


tree <- read.nexus("Cortisol/StressTree.nex")

Lifespan <- read.csv("LifespanData.csv")

#Load data and clean it up
StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) 
StressData <- merge(StressData, Lifespan, by = "Species", all.x = TRUE) 

#use mean lifespan, if not use 0.8 max lifespan 
for (i in 1:nrow(StressData)) {
  if (is.na(StressData$MeanLifespan[i]) == FALSE) {
    StressData$MaxLifespan[i] <- StressData$MeanLifespan[i]
  } else {
    StressData$MaxLifespan[i] <- StressData$MaxLifespan[i]*0.8
  }
}

rownames(StressData) = StressData$Species

#set directory for these data
directory <- "Cortisol/CortisolLifespan/"
CreateDR(directory)

#label figures
Label <- "Cortisol - Lifespan Update"

#run the scripts
source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
source("Code/AICScript.R")

# FGC Uncorrected ---------------------------------------------------------

tree <- read.nexus("FGCAnalysis/StressTree.nex")  

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other'))

directory <- "FGCAnalysis/FGCUncorrected/"
CreateDR(directory)

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
CreateDR(directory)

Label <- "FGC - Wet Corrected Model"

source("Code/WorkingScript.R")
source("Code/PhyloSigScript.R")
# source("Code/AICScript.R")     

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
beep(10)
