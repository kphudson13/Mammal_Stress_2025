
################################################################################

# Master script for core data analysis
# This script controls the flow of the analysis and runs the individual scripts
# It is dependent on outputs from CleanAndTree.R, but those are already in the the remote repository
# I realize leaving outputs and binaries is bad practice, but running CleanAndTree requires an API key
# I also want the original .nex files saved, so if taxonomy changes we can still confirm our analysis
# live laugh love -Kyle

################################################################################

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
library(cowsay) #for the fun frog
library(phytools) #for use in the phylo signal script
library(nlme)

rm(list=ls()) #clear environment
source("Code/DirectoryFunction.R") #function to create directories
source("Code/DataAndTreeFunction.R") #function to make trees

#load lifespan data for later use
MyhrvoldData <- read.csv("LifespanData/MyhrvoldDataClean.csv")
# TurbillData <- read.csv("LifespanData/TurbillDataRaw.csv") #seems like Turbill an AZA completely overlap with Myhrvold
# AZAData <- read.csv("LifespanData/AZADataRaw.csv")

theme1 <- theme(axis.title = element_text(size =10)) 

# Crtstn Max Lifespan --------------------------------------------------------
# 
# #Analysis of data where lifespan is the max value reported from AnAge
# 
# #Load tree build by CleanAndTree.R
# tree <- read.nexus("Corticosterone/StressTree.nex")
# 
# #Load data and clean it up
# StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
#   mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>% #simplify stressor into ACTH or Other
#   mutate(Lifespan = MaxLifespan)
# 
# #set directory for these data
# directory <- "Corticosterone/CrtstnMaxLifespan/"
# CreateDR(directory)
# 
# #label figures
# Label <- "Corticosterone - Max Lifespan Model"
# 
# #run the scripts
# source("Code/WorkingScript.R")
# #source("Code/PhyloSigScript.R")
# #source("Code/AICScript.R.R")
# 

# Crtstn Mean Lifespan ----------------------------------------------------

#Analysis of data where mean lifespan is used from various sources 

#Load tree build by CleanAndTree.R
tree <- read.nexus("Corticosterone/StressTree.nex")

#Load data and clean it up
StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>% #simplify stressor into ACTH or Other
  merge(., MyhrvoldData, by = "Species", all.x = TRUE) %>% #merge in lifespan data
  mutate(Lifespan = longevity_y)

summary(StressData)

#set directory for these data
directory <- "Corticosterone/CrtstnMeanLifespan/"
CreateDR(directory)

#label figures
Label <- "Corticosterone - Mean Lifespan Model"

#run the scripts
source("Code/WorkingScript.R")
#source("Code/PhyloSigScript.R")
source("Code/BICScript.R")

# Crtstn Mixed Lifespan --------------------------------------------------
# 
# #Analysis of data where lifespan is the mean if available or max value reported from AnAge times 0.8 if not
# 
# tree <- read.nexus("Corticosterone/StressTree.nex")
# 
# #Load data and clean it up
# StressData <- read.csv("Corticosterone/CrtstnDataClean.csv") %>%
#   mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>%
#   merge(., MyhrvoldData, by = "Species", all.x = TRUE) %>% #merge in lifespan data
#   mutate(MeanLifespan = longevity_y)
# 
# #use mean lifespan, if not use 0.8 max lifespan 
# for (i in 1:nrow(StressData)) {
#   if (is.na(StressData$MeanLifespan[i]) == FALSE) {
#     StressData$Lifespan[i] <- StressData$MeanLifespan[i]
#   } else {
#     StressData$Lifespan[i] <- StressData$MaxLifespan[i]*0.8
#   }
# }
# 
# rownames(StressData) = StressData$Species
# 
# #set directory for these data
# directory <- "Corticosterone/CrtstnMixedLifespan/"
# CreateDR(directory)
# 
# #label figures
# Label <- "Corticosterone - Mixed Lifespan"
# 
# #run the scripts
# source("Code/WorkingScript.R")
# #source("Code/PhyloSigScript.R")
# # source("Code/BICScript.R")
# 

# Cortisol Max Lifespan ----------------------------------------------------
# 
# #Analysis of data where lifespan is the max value reported from AnAge
# 
# tree <- read.nexus("Cortisol/StressTree.nex")
# 
# StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
#   mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>%
#   mutate(Lifespan = MaxLifespan)
# 
# directory <- "Cortisol/CortisolMaxLifespan/"
# CreateDR(directory)
# 
# Label <- "Cortisol - Max Lifespan Model"
# 
# source("Code/WorkingScript.R")
# #source("Code/PhyloSigScript.R")
# #source("Code/AICScript.R.R")

# Cortisol Mean Lifespan --------------------------------------------------

#Analysis of data where mean lifespan is used from various sources 

tree <- read.nexus("Cortisol/StressTree.nex")

StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>% #simplify stressor into ACTH or Other
  merge(., MyhrvoldData, by = "Species", all.x = TRUE) %>% #merge in lifespan data
  mutate(Lifespan = longevity_y)

directory <- "Cortisol/CortisolMeanLifespan/"
CreateDR(directory)

Label <- "Cortisol - Mean Lifespan Model"

source("Code/WorkingScript.R")
#source("Code/PhyloSigScript.R")
source("Code/BICScript.R")

# Cortisol Mixed Lifespan ------------------------------------------------
# 
# #Analysis of data where lifespan is the mean if available or max value reported from AnAge times 0.8 if not
# 
# tree <- read.nexus("Cortisol/StressTree.nex")
# 
# #Load data and clean it up
# StressData <- read.csv("Cortisol/CortisolDataClean.csv") %>%
#   mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>%
#   merge(., MyhrvoldData, by = "Species", all.x = TRUE) %>% #merge in lifespan data
#   mutate(MeanLifespan = longevity_y)
# 
# #use mean lifespan, if not use 0.8 max lifespan 
# for (i in 1:nrow(StressData)) {
#   if (is.na(StressData$MeanLifespan[i]) == FALSE) {
#     StressData$Lifespan[i] <- StressData$MeanLifespan[i]
#   } else {
#     StressData$Lifespan[i] <- StressData$MaxLifespan[i]*0.8
#   }
# }
# 
# rownames(StressData) = StressData$Species
# 
# #set directory for these data
# directory <- "Cortisol/CortisolMixedLifespan/"
# CreateDR(directory)
# 
# #label figures
# Label <- "Cortisol - Mixed Lifespan"
# 
# #run the scripts
# source("Code/WorkingScript.R")
# #source("Code/PhyloSigScript.R")
# #source("Code/AICScript.R.R")
# 
# FGC Mean lifespan ---------------------------------------------------------

#Use cortisol and corticosterone

tree <- read.nexus("FGCAnalysis/StressTree.nex")

StressData <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  mutate(Stressor = ifelse(Stressor == 'ACTH', Stressor, 'Other')) %>%
  merge(., MyhrvoldData, by = "Species", all.x = TRUE) %>% #merge in lifespan data
  mutate(Lifespan = longevity_y)

directory <- "FGCAnalysis/FGCUncorrected/"
CreateDR(directory)

Label <- "FGC - Uncorrected Model"

# source("Code/WorkingScript.R")
#source("Code/PhyloSigScript.R")
# #source("Code/AICScript.R.R")


# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")
