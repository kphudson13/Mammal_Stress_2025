
library(ape)
library(rotl) #pull from tree of life
library(tidyverse)
library(taxize) #pull upstream taxonomic data, package installed from github not CRAN

rawdata <- read.csv("StressDataRaw.csv")
# summary(rawdata)

# Build Tree --------------------------------------------------------------

StressData <- rawdata %>% 
  filter(Species != "Gerbillus piridium") #Cant find this species 

#update taxonomy from the data set
StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"
StressData$Species[StressData$Species == "Papio hamadryas ursinus"] <- "Papio ursinus" #Represents a species complex
StressData$Species[StressData$Species == "Cebus apella"] <- "Sapajus apella"
StressData$Species[StressData$Species == "Gerbillus andersoni allenbyi"] <- "Gerbillus andersoni"
StressData$Species[StressData$Species == "Capra aegargrus hircus"] <- "Capra hircus"


taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method

#Clean tree tip labels 
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

#Update taxonomy from tree of life
tree$tip.label[tree$tip.label == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
tree$tip.label[tree$tip.label == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

write.nexus(tree, file = "Outputs/StressTree_AllSpecies.nex")

write.csv(StressData, file = "StressDataClean.csv")

png("Outputs/FullTree_PGLS.png",
    height = 3000,
    width = 1800,
    res = 200)
plot(tree)
dev.off()




#NCBI access is only available with an API key stored in the .Rprofile
rawtaxa <- classification(c("Elephas maximus", "Sapajus apella"), db = "ncbi")


table <- tibble(names = names(rawtaxa), rawtaxa)


# tibble(names = names(rawtaxa), rawtaxa) %>% 
#   unnest() %>% 
#   filter(rank %in% c("phylum","class","order","family","genus")) %>% 
#   select(-id) %>% 
#   spread(rank, name) %>% 
#   select(name = rawtaxa, phylum, class, order, family, genus)
