
library(tidyverse)
library(cowplot)

rm(list=ls())
setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Cortisol/CortisolUncorrected")

load("BasFGCMSMR_Plot.RData")
load("BasFGCMass_Plot.RData")
load("ElvFGCBasFGC_Plot.RData")
load("LifespanBasFGC_Plot.RData")

BasFGCMSMR_Cortisol <- BasFGCMSMR_Plot +
  labs(x = "MSMR (ln(mW/g))",
       y = "Cortisol (ln(ng/g))") 

BasFGCMass_Cortisol <- BasFGCMass_Plot +
  labs(x = "Body Mass (ln(g))",
       y = "Cortisol (ln(ng/g))") 

ElvFGCBasFGC_Cortisol <- ElvFGCBasFGC_Plot +
  labs(x = "Basal Cortisol (ln(ng/g))",
       y = "Elevated Cortisol (ln(ng/g))") 

LifespanBasFGC_Cortisol <- LifespanBasFGC_Plot +
  labs(x = "Basal Cortisol (ln(ng/g))",
       y = "Lifespan (years)") 

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/Corticosterone/CrtstnUncorrected")

load("BasFGCMSMR_Plot.RData")
load("BasFGCMass_Plot.RData")
load("ElvFGCBasFGC_Plot.RData")
load("LifespanBasFGC_Plot.RData")

BasFGCMSMR_Corticosterone <- BasFGCMSMR_Plot +
  labs(x = "MSMR (ln(mW/g)",
       y = "Corticosterone (ng/g)") 

BasFGCMass_Corticosterone <- BasFGCMass_Plot +
  labs(x = "Body Mass (ln(g))",
       y = "Corticosterone (ln(ng/g))") 

ElvFGCBasFGC_Corticosterone <- ElvFGCBasFGC_Plot +
  labs(x = "Basal Corticosterone (ln(ng/g))",
       y = "Elevated Corticosterone (ln(ng/g))") 

LifespanBasFGC_Corticosterone <- LifespanBasFGC_Plot  +
  labs(x = "Basal Corticosterone (ln(ng/g))",
       y = "Lifespan (years)") 

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PublicationFigures")

save_plot(plot_grid(BasFGCMSMR_Cortisol, BasFGCMSMR_Corticosterone, labels = c("A", "B")),
          filename = "BasFGCMSMR.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(BasFGCMass_Cortisol, BasFGCMass_Corticosterone, labels = c("A", "B")),
          filename = "BasFGCMass.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(ElvFGCBasFGC_Cortisol, ElvFGCBasFGC_Corticosterone, labels = c("A", "B")),
          filename = "ElvFGCBasFGC.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(LifespanBasFGC_Cortisol, LifespanBasFGC_Corticosterone, labels = c("A", "B")),
          filename = "LifespanBasFGC.png",
          base_height = 4, base_width = 8)

setwd("C:/Users/kphud/Documents")






