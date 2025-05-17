
library(tidyverse)

#get all the data
rm(list=ls())
setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R")

load("Cortisol/CortisolUncorrected/BasFGCMSMR_PGLS.RData") #load cortisol PGLS
BasFGCMSMR_Cortisol <- BasFGCMSMR_PGLS
load("Cortisol/CortisolUncorrected/BasFGCMSMR_Reduced.RData") #load cortisol reduced PGLS
BasFGCMSMR_Cortisol_Reduced <- BasFGCMSMR_Reduced

load("Cortisol/CortisolUncorrected/BasFGCMass_PGLS.RData") 
BasFGCMass_Cortisol <- BasFGCMass_PGLS
load("Cortisol/CortisolUncorrected/BasFGCMass_Reduced.RData") 
BasFGCMass_Cortisol_Reduced <- BasFGCMass_Reduced

load("Cortisol/CortisolUncorrected/ElvFGCBasFGC_PGLS.RData") 
ElvFGCBasFGC_Cortisol <- ElvFGCBasFGC_PGLS
load("Cortisol/CortisolUncorrected/ElvFGCBasFGC_Reduced.RData") 
ElvFGCBasFGC_Cortisol_Reduced <- ElvFGCBasFGC_Reduced

load("Cortisol/CortisolUncorrected/LifespanBasFGC_PGLS.RData") 
LifespanBasFGC_Cortisol <- LifespanBasFGC_PGLS
load("Cortisol/CortisolUncorrected/LifespanBasFGC_Reduced.RData") 
LifespanBasFGC_Cortisol_Reduced <- LifespanBasFGC_Reduced

load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_PGLS.RData") 
BasFGCMSMR_Crtstn <- BasFGCMSMR_PGLS
load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_Reduced.RData") 
BasFGCMSMR_Crtstn_Reduced <- BasFGCMSMR_Reduced

load("Corticosterone/CrtstnUncorrected/BasFGCMass_PGLS.RData") 
BasFGCMass_Crtstn <- BasFGCMass_PGLS
load("Corticosterone/CrtstnUncorrected/BasFGCMass_Reduced.RData") 
BasFGCMass_Crtstn_Reduced <- BasFGCMass_Reduced

load("Corticosterone/CrtstnUncorrected/ElvFGCBasFGC_PGLS.RData") 
ElvFGCBasFGC_Crtstn <- ElvFGCBasFGC_PGLS
load("Corticosterone/CrtstnUncorrected/ElvFGCBasFGC_Reduced.RData")
ElvFGCBasFGC_Crtstn_Reduced <- ElvFGCBasFGC_Reduced

load("Corticosterone/CrtstnUncorrected/LifespanBasFGC_PGLS.RData") 
LifespanBasFGC_Crtstn <- LifespanBasFGC_PGLS
load("Corticosterone/CrtstnUncorrected/LifespanBasFGC_Reduced.RData") 
LifespanBasFGC_Crtstn_Reduced <- LifespanBasFGC_Reduced


StatsTab <- rbind(intervals(BasFGCMSMR_Cortisol)[["coef"]][1,],
                  intervals(BasFGCMass_Cortisol)[["coef"]][1,],
                  intervals(ElvFGCBasFGC_Cortisol)[["coef"]][1,],
                  intervals(LifespanBasFGC_Cortisol)[["coef"]][1,],
                  intervals(BasFGCMSMR_Crtstn)[["coef"]][1,],
                  intervals(BasFGCMass_Crtstn)[["coef"]][1,],
                  intervals(ElvFGCBasFGC_Crtstn)[["coef"]][1,],
                  intervals(LifespanBasFGC_Crtstn)[["coef"]][1,]) %>% #intercept coefficients 
  as.data.frame(.) %>% #cut out all the rows of intercept stats
  mutate(across(c(1,2,3), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Intercept (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>%
  select(., -c("est.", "lower", "upper")) %>% 
  cbind(., rbind(intervals(BasFGCMSMR_Cortisol)[["coef"]][2,],
                 intervals(BasFGCMass_Cortisol)[["coef"]][2,],
                 intervals(ElvFGCBasFGC_Cortisol)[["coef"]][2,],
                 intervals(LifespanBasFGC_Cortisol)[["coef"]][2,],
                 intervals(BasFGCMSMR_Crtstn)[["coef"]][2,],
                 intervals(BasFGCMass_Crtstn)[["coef"]][2,],
                 intervals(ElvFGCBasFGC_Crtstn)[["coef"]][2,],
                 intervals(LifespanBasFGC_Crtstn)[["coef"]][2,]) ) %>% #slope coefficients
  mutate(across(c(2,3,4), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Slope (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>%
  select(., -c("est.", "lower", "upper")) %>%
  cbind(., 
        rbind(R2(BasFGCMSMR_Cortisol, BasFGCMSMR_Cortisol_Reduced)[3],
              R2(BasFGCMass_Cortisol, BasFGCMass_Cortisol_Reduced)[3],
              R2(ElvFGCBasFGC_Cortisol, ElvFGCBasFGC_Cortisol_Reduced)[3],
              R2(LifespanBasFGC_Cortisol, LifespanBasFGC_Cortisol_Reduced)[3],
              R2(BasFGCMSMR_Crtstn, BasFGCMSMR_Crtstn_Reduced)[3],
              R2(BasFGCMass_Crtstn, BasFGCMass_Crtstn_Reduced)[3],
              R2(ElvFGCBasFGC_Crtstn, ElvFGCBasFGC_Crtstn_Reduced)[3],
              R2(LifespanBasFGC_Crtstn, LifespanBasFGC_Crtstn_Reduced)[3]), #r squared column
        rbind(coefficients(summary(BasFGCMSMR_Cortisol))[2,4],
              coefficients(summary(BasFGCMass_Cortisol))[2,4],
              coefficients(summary(ElvFGCBasFGC_Cortisol))[2,4],
              coefficients(summary(LifespanBasFGC_Cortisol))[2,4],
              coefficients(summary(BasFGCMSMR_Crtstn))[2,4],
              coefficients(summary(BasFGCMass_Crtstn))[2,4],
              coefficients(summary(ElvFGCBasFGC_Crtstn))[2,4],
              coefficients(summary(LifespanBasFGC_Crtstn))[2,4])) %>% #p value column
  add_row(.before = 1) %>% add_row(.before = 6) %>% #add blank rows to divide cortisol and crtstn
  mutate(Model = c("Cortisol", "Basal FGC vs. MSMR", "Basal FGC vs. Mass", "Elevated FGC vs. Basal FGC", "Lifespan vs. Basal FGC", 
                   "Corticosterone", "Basal FGC vs. MSMR", "Basal FGC vs. Mass", "Elevated FGC vs. Basal FGC", "Lifespan vs. Basal FGC")) %>%
  `colnames<-`(c("Intercept (95% CI)", "Slope (95% CI)", "R2","p-value", "Model")) %>%
  mutate(across(c(3,4), \(x) round(x, digits = 3))) %>%
  mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`)) #change very small p values to < 0.001

#reorder the table and remove NAs
StatsTab <- StatsTab[,c(5,1,2,3,4)]
StatsTab[is.na(StatsTab)] <- " "

tt1 <- ttheme_minimal(core=list(fg_params=list(hjust = 1, x = 0.95)))

png("PublicationFigures/StatsTab_PGLS.png", 
    height = 190*nrow(StatsTab), 
    width = 800*ncol(StatsTab),
    res = 300)
grid.newpage()
g <- tableGrob(StatsTab[,1], cols = "Model", theme = tt1)
g2 <- tableGrob(StatsTab[,2:ncol(StatsTab)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()






