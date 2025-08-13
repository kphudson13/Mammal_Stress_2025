
library(ape)
library(rotl) #pull from tree of life
library(tidyverse)
library(taxize) #pull upstream taxonomic data, package installed from github not CRAN
library(cowsay)

# summary(rawdata)

# Corticosterone model -----------------------------------------------------

StressData <- read.csv("Corticosterone/CrtstnDataRaw.csv")

StressData$Species[StressData$Species == "Suricata suricatta "] <- "Suricata suricatta"
StressData$Species[StressData$Species == "Sturnira parivdens"] <- "Sturnira parvidens"
StressData$Species[StressData$Species == "Equus burchelli"] <- "Equus burchellii"
StressData$Species[StressData$Species == "Papio hamadryas ursinus"] <- "Papio ursinus"

StressData <- StressData %>% 
     filter(Species != "Gerbillus andersoni") #Got rid of one of them, fix this later
StressData$Species[StressData$Species == "Gerbillus andersoni allenbyi"] <- "Gerbillus andersoni"

#NCBI access is only available with an API key stored in the .Rprofile
rawtaxa <- tax_name(unique(StressData$Species), get = c("family", "order"), db = "ncbi")

#Add order and family column from NCBI data
StressData <- merge(StressData, rawtaxa[, c("query", "family", "order")], by.x = "Species", by.y = "query")
colnames(StressData)[colnames(StressData) == "order"] <- "Order"
colnames(StressData)[colnames(StressData) == "family"] <- "Family"

StressData$Method <- str_trim(StressData$Method) #remove whitespace from method names

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
tree$tip.label[tree$tip.label == "Sapajus apella"] <- "Cebus apella"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "Corticosterone/StressTree.nex")

png("Corticosterone/TreePic.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()

#convert MSMR to mW/g
StressData$MSMR <- StressData$MSMR * 1000 

write.csv(StressData, file = "Corticosterone/CrtstnDataClean.csv")

# Cortisol Model ------------------------------------------------------------

StressData <- read.csv("Cortisol/CortisolDataRaw.csv")

# StressData <- rawdata %>% 
#   filter(Species != "Gerbillus piridium") #Cant find this species 
# 
# #update taxonomy from the data set
StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"
StressData$Species[StressData$Species == "Capra aegargrus hircus"] <- "Capra hircus"
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

StressData$Method <- str_trim(StressData$Method) #remove whitespace from method names

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
tree$tip.label[tree$tip.label == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
tree$tip.label[tree$tip.label == "Mazama gouazoupira"] <- "Mazama gouazoubira"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "Cortisol/StressTree.nex")

png("Cortisol/TreePic.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()

#convert MSMR to mW/g
StressData$MSMR <- StressData$MSMR * 1000

write.csv(StressData, file = "Cortisol/CortisolDataClean.csv")

#beep()


# FGC model ---------------------------------------------------------------

#pull in already cleaned data with orders and familys attached 
CrtsnData <- read.csv("Corticosterone/CrtstnDataClean.csv") 
CortisolData <- read.csv("Cortisol/CortisolDataClean.csv")


#add a column to sort by 
CrtsnData$Hormone <- "Corticosterone"
CortisolData$Hormone <- "Cortisol"

#keep corticosterone for rodents and cortisol for everything else 
StressData <- rbind(CrtsnData, CortisolData) %>% 
  subset(., (Hormone == "Corticosterone" & Order == "Rodentia") | (Hormone == "Cortisol" & Order != "Roentia"))

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
tree$tip.label[tree$tip.label == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
tree$tip.label[tree$tip.label == "Mazama gouazoupira"] <- "Mazama gouazoubira"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "FGCAnalysis/StressTree.nex")

png("FGCAnalysis/TreePic.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()

write.csv(StressData, file = "FGCAnalysis/FGCDataClean.csv")

# Lifespan Data Cleaning --------------------------------------------------

MyhrvoldData <- read.csv("LifespanData/MyhrvoldDataRaw.csv") %>%
  filter(longevity_y != -999 & class == "Mammalia") %>% #-999 is no data for this set, and we only want mammals
  select("genus", "species", "longevity_y") %>% #cut unused columns
  mutate(Species = paste(genus, species, sep = " ")) %>% #combine into binomial names 
  select(Species, longevity_y) 

write.csv(MyhrvoldData, file = "LifespanData/MyhrvoldDataClean.csv")

# Done --------------------------------------------------------------------

say("Done", by = "frog", what_color = "darkgreen")

