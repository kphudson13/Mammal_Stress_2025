
library(ape)
library(rotl) #pull from tree of life
library(tidyverse)
library(taxize) #pull upstream taxonomic data, package installed from github not CRAN
library(beepr) #beep when done)

BaseWD <- "C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R" #Default WD
setwd(BaseWD)

# summary(rawdata)

# Build Corticosterone Tree ------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone")

StressData <- read.csv("CrtstnDataRaw.csv")

# StressData <- rawdata %>% 
#   filter(Species != "Gerbillus piridium") #Cant find this species 
# 
# #update taxonomy from the data set
# StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"
# StressData$Species[StressData$Species == "Papio hamadryas ursinus"] <- "Papio ursinus" #Represents a species complex
# StressData$Species[StressData$Species == "Cebus apella/ Sapajus apella"] <- "Sapajus apella"
# StressData$Species[StressData$Species == "Cebus apella"] <- "Sapajus apella"
# StressData$Species[StressData$Species == "Capra aegargrus hircus"] <- "Capra hircus"
# StressData$Species[StressData$Species == "Elaphas maximus"] <- "Elephas maximus" 
StressData$Species[StressData$Species == "Suricata suricatta "] <- "Suricata suricatta"
StressData$Species[StressData$Species == "Sturnira parivdens"] <- "Sturnira parvidens"
StressData$Species[StressData$Species == "Equus burchelli"] <- "Equus burchellii"

StressData <- StressData %>% 
     filter(Species != "Gerbillus andersoni") #Got rid of one of them, fix this later
StressData$Species[StressData$Species == "Gerbillus andersoni allenbyi"] <- "Gerbillus andersoni"

#NCBI access is only available with an API key stored in the .Rprofile
rawtaxa <- tax_name(unique(StressData$Species), get = c("family", "order"), db = "ncbi")

#Add order and family column from NCBI data
StressData <- merge(StressData, rawtaxa[, c("query", "family", "order")], by.x = "Species", by.y = "query")

colnames(StressData)[colnames(StressData) == "order"] <- "Order"
colnames(StressData)[colnames(StressData) == "family"] <- "Family"

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
# tree$tip.label[tree$tip.label == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
tree$tip.label[tree$tip.label == "Sapajus apella"] <- "Cebus apella"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "StressTree.nex")

png("TreePic.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()

write.csv(StressData, file = "CrtstnDataClean.csv")

# Cortisol Model ------------------------------------------------------------

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol")

StressData <- read.csv("CortisolDataRaw.csv")

# StressData <- rawdata %>% 
#   filter(Species != "Gerbillus piridium") #Cant find this species 
# 
# #update taxonomy from the data set
# StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"
# StressData$Species[StressData$Species == "Papio hamadryas ursinus"] <- "Papio ursinus" #Represents a species complex
# StressData$Species[StressData$Species == "Cebus apella/ Sapajus apella"] <- "Sapajus apella"
# StressData$Species[StressData$Species == "Cebus apella"] <- "Sapajus apella"
# StressData$Species[StressData$Species == "Capra aegargrus hircus"] <- "Capra hircus"
# StressData$Species[StressData$Species == "Elaphas maximus"] <- "Elephas maximus" 
#StressData$Species[StressData$Species == "Suricata suricatta "] <- "Suricata suricatta"
#StressData$Species[StressData$Species == "Sturnira parivdens"] <- "Sturnira parvidens"
#StressData$Species[StressData$Species == "Equus burchelli"] <- "Equus burchellii"

StressData <- StressData %>% 
  filter(Species != "Gerbillus andersoni") #Got rid of one of them, fix this later
StressData$Species[StressData$Species == "Gerbillus andersoni allenbyi"] <- "Gerbillus andersoni"

#NCBI access is only available with an API key stored in the .Rprofile
rawtaxa <- tax_name(unique(StressData$Species), get = c("family", "order"), db = "ncbi")

#Add order and family column from NCBI data
StressData <- merge(StressData, rawtaxa[, c("query", "family", "order")], by.x = "Species", by.y = "query")

colnames(StressData)[colnames(StressData) == "order"] <- "Order"
colnames(StressData)[colnames(StressData) == "family"] <- "Family"

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
# tree$tip.label[tree$tip.label == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
tree$tip.label[tree$tip.label == "Sapajus apella"] <- "Cebus apella"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "StressTree.nex")

png("TreePic.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()

write.csv(StressData, file = "CortisolDataClean.csv")


beep()
