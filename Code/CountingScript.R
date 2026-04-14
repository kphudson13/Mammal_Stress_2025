
################################################################################

# This is just a script to count the amount of observations 
# live laugh love -Kyle

################################################################################

library(tidyverse)
Crtsn <- read.csv("Corticosterone/CrtstnDataClean.csv")

Cort <- read.csv("Cortisol/CortisolDataClean.csv")

# FGC <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
#   .[ , -c(1, 19)]


combined <- rbind(Crtsn, Cort)

length(unique(combined$Species))

length(unique(combined$Family))
