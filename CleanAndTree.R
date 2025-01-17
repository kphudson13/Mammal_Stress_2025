
library(ape)
library(rotl) #pull from tree of life
library(tidyverse)

rawdata <- read.csv("StressDataRaw.csv")
# summary(rawdata)

# Build Tree --------------------------------------------------------------

StressData <- rawdata %>% 
  filter(Species != "Cebus apella", 
         Species != "Gerbillus andersoni allenbyi",
         Species != "Gerbillus piridium")

StressData$Species[StressData$Species == "Choeropsis liberiensis"] <- "Hexaprotodon liberiensis"
StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

unmatched <- taxa[is.na(taxa$ott_id), ]
if (nrow(unmatched) > 0) {
  print("Unmatched species:")
  print(unmatched$search_string)
}

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

write.nexus(tree, file = "Outputs/StressTree_AllSpecies.nex")

write.csv(StressData, file = "StressDataClean.csv")
