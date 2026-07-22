
################################################################################

# This is just a script to count the amount of observations 
# live laugh love -Kyle

################################################################################

library(tidyverse)
rm(list=ls()) #clear environment

Crtsn <- read.csv("Corticosterone/CrtstnDataClean.csv")
Cort <- read.csv("Cortisol/CortisolDataClean.csv")
FGC <- read.csv("FGCAnalysis/FGCDataClean.csv")

# FGC <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
#   .[ , -c(1, 19)]


combined <- rbind(Crtsn, Cort) # just row bind cort and crst to count 

length(unique(combined$Species)) # number of species total
length(unique(combined$Family)) # number of families total

length(unique(FGC$Species)) # number of species if we were to combine cort and crst with only one point each species
