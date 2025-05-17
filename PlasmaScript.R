
library(tidyverse)

#get all the data
rm(list=ls())
setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R")
FecalCortisol <- read.csv("Cortisol/CortisolDataClean.csv")
FecalCrtstn <- read.csv("Corticosterone/CrtstnDataClean.csv")
load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_PGLS.RData") #load corticosterone PGLS 
FecalCrtstn_PGLS <- BasFGCMSMR_PGLS
load("Cortisol/CortisolUncorrected/BasFGCMSMR_PGLS.RData") #load cortisol PGLS
FecalCort_PGLS <- BasFGCMSMR_PGLS
Plasma <- read.csv("HaaseData.csv")

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PublicationFigures")

#set colours weirdly because there is multiple data sets 
legend_colors <- c("Plasma Cortisol" = "red", "Fecal Cortisol" = "blue", "Fecal Corticosterone" = "green")

GCPlot <- ggplot() +
  geom_point(data = Plasma, aes(x = log(MSMR), y = log(Base), color = "Plasma Cortisol")) +
  geom_point(data = FecalCortisol, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Cortisol")) +
  geom_point(data = FecalCrtstn, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Corticosterone")) +
  labs(x = "ln MSMR", y = "ln Glucocorticoid (ng/ml)", color = "Source") +
  scale_color_manual(values = legend_colors) +
  geom_abline(intercept = 3.3, slope = 0.97, colour = "red", linewidth = 1) + #from Haase et al. 2016
  geom_abline(intercept = coefficients(summary(FecalCort_PGLS))[1,1], slope = coefficients(summary(FecalCort_PGLS))[2,1], colour = "blue", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(summary(FecalCrtstn_PGLS))[1,1], slope = coefficients(summary(FecalCrtstn_PGLS))[2,1], colour = "green", linewidth = 1) + #from PGLS
  theme_classic()


GCPlot  

ggsave("PlasmaComparison.png", plot = GCPlot, width = 8, height = 6, dpi = 300)




