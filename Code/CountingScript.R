

Crtsn <- read.csv("Corticosterone/CrtstnDataClean.csv")

Cort <- read.csv("Cortisol/CortisolDataClean.csv")

FGC <- read.csv("FGCAnalysis/FGCDataClean.csv") %>%
  .[ , -c(1, 19)]

setwd(BaseWD)

combined <- rbind(Crtsn, Cort, FGC)

length(unique(combined$Species))

length(unique(combined$Family))
