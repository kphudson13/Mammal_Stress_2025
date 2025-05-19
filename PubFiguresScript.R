
library(tidyverse)
library(ape)
library(nlme) #for gls
library(rr2) #for the R2 function
library(grid) #to set table themes
library(gridExtra) #to set table themes
library(rlang)
library(phytools) #for phylosig
library(cowplot) #to combine plots 

rm(list=ls())

# FGC Figures -------------------------------------------------------------

#load in cortisol ggplot objects
load("Cortisol/CortisolUncorrected/BasFGCMSMR_Plot.RData")
load("Cortisol/CortisolUncorrected/BasFGCMass_Plot.RData")
load("Cortisol/CortisolUncorrected/ElvFGCBasFGC_Plot.RData")
load("Cortisol/CortisolUncorrected/LifespanBasFGC_Plot.RData")

BasFGCMSMR_Cortisol <- BasFGCMSMR_Plot +
  labs(x = "MSMR (ln(mW/g))",
       y = "Cortisol (ln(ng/g))") #change axis labels 

BasFGCMass_Cortisol <- BasFGCMass_Plot +
  labs(x = "Body Mass (ln(g))",
       y = "Cortisol (ln(ng/g))") 

ElvFGCBasFGC_Cortisol <- ElvFGCBasFGC_Plot +
  labs(x = "Basal Cortisol (ln(ng/g))",
       y = "Elevated Cortisol (ln(ng/g))") 

LifespanBasFGC_Cortisol <- LifespanBasFGC_Plot +
  labs(x = "Basal Cortisol (ln(ng/g))",
       y = "Lifespan (years)") 

#load in corticosterone ggplot objects
load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_Plot.RData")
load("Corticosterone/CrtstnUncorrected/BasFGCMass_Plot.RData")
load("Corticosterone/CrtstnUncorrected/ElvFGCBasFGC_Plot.RData")
load("Corticosterone/CrtstnUncorrected/LifespanBasFGC_Plot.RData")

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

#export cortisol and corticosterone combined figures 
save_plot(plot_grid(BasFGCMSMR_Cortisol, BasFGCMSMR_Corticosterone, labels = c("A", "B")),
          filename = "PublicationFigures/BasFGCMSMR.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(BasFGCMass_Cortisol, BasFGCMass_Corticosterone, labels = c("A", "B")),
          filename = "PublicationFigures/BasFGCMass.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(ElvFGCBasFGC_Cortisol, ElvFGCBasFGC_Corticosterone, labels = c("A", "B")),
          filename = "PublicationFigures/ElvFGCBasFGC.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(LifespanBasFGC_Cortisol, LifespanBasFGC_Corticosterone, labels = c("A", "B")),
          filename = "PublicationFigures/LifespanBasFGC.png",
          base_height = 4, base_width = 8)



# FGC and Plasma Figures --------------------------------------------------

#load in the data sets
Cortisol_data <- read.csv("Cortisol/CortisolDataClean.csv")
Crtstn_data <- read.csv("Corticosterone/CrtstnDataClean.csv")
Plasma_data <- read.csv("HaaseData.csv")

#load in and rename PGLS objects
load("Cortisol/CortisolUncorrected/BasFGCMSMR_PGLS.RData") #load cortisol PGLS
load("Cortisol/CortisolUncorrected/BasFGCMass_PGLS.RData")
load("Cortisol/CortisolUncorrected/ElvFGCBasFGC_PGLS.RData")
load("Cortisol/CortisolUncorrected/LifespanBasFGC_PGLS.RData")
BasFGCMSMR_FecalCort_PGLS <- BasFGCMSMR_PGLS
BasFGCMass_FecalCort_PGLS <- BasFGCMass_PGLS
ElvFGCBasFGC_FecalCort_PGLS <- ElvFGCBasFGC_PGLS
LifespanBasFGC_FecalCort_PGLS <- LifespanBasFGC_PGLS

load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_PGLS.RData") #load corticosterone PGLS 
load("Corticosterone/CrtstnUncorrected/BasFGCMass_PGLS.RData")
load("Corticosterone/CrtstnUncorrected/ElvFGCBasFGC_PGLS.RData")
load("Corticosterone/CrtstnUncorrected/LifespanBasFGC_PGLS.RData")
BasFGCMSMR_Crtstn_PGLS <- BasFGCMSMR_PGLS
BasFGCMass_Crtstn_PGLS <- BasFGCMass_PGLS
ElvFGCBasFGC_Crtstn_PGLS <- ElvFGCBasFGC_PGLS
LifespanBasFGC_Crtstn_PGLS <- LifespanBasFGC_PGLS

#set shape weirdly because there is multiple data sets 
legend_shapes <- c("Plasma Cortisol" = 15, "Fecal Cortisol" = 1, "Fecal Corticosterone" = 17)
legend_colors <- c("Plasma Cortisol" = "red", "Fecal Cortisol" = "blue", "Fecal Corticosterone" = "green")

BasFGCMSMR_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(MSMR), y = log(Base), shape = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(MSMR), y = log(BasalFGC), shape = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(MSMR), y = log(BasalFGC), shape = "Fecal Corticosterone")) +
  labs(x = "MSMR (ln(mW/g)", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
  scale_shape_manual(values = legend_shapes) +
  geom_abline(intercept = 3.3, slope = 0.97, colour = "red", linewidth = 1) + #from Haase et al. 2016
  geom_abline(intercept = coefficients(BasFGCMSMR_FecalCort_PGLS)[1], slope = coefficients(BasFGCMSMR_FecalCort_PGLS)[2], colour = "blue", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(BasFGCMSMR_Crtstn_PGLS)[1], slope = coefficients(BasFGCMSMR_Crtstn_PGLS)[2], colour = "green", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "none") # Removes the legend

BasFGCMSMR_combined  

BasFGCMass_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(as.numeric(Mass)), y = log(Base), shape = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(BodyMassAnAge), y = log(BasalFGC), shape = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(BodyMassAnAge), y = log(BasalFGC), shape = "Fecal Corticosterone")) +
  labs(x = "Body Mass (ln(g)", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
  scale_shape_manual(values = legend_shapes) +
  geom_abline(intercept = 6.12, slope = -0.22, colour = "red", linewidth = 1) + #from Haase et al. 2016
  geom_abline(intercept = coefficients(BasFGCMass_FecalCort_PGLS)[1], slope = coefficients(BasFGCMass_FecalCort_PGLS)[2], colour = "blue", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(BasFGCMass_Crtstn_PGLS)[1], slope = coefficients(BasFGCMass_Crtstn_PGLS)[2], colour = "green", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "none") # Removes the legend

BasFGCMass_combined

ElvFGCBasFGC_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(Base), y = log(Elev), shape = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(BasalFGC), y = log(ElevFGC), shape = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(BasalFGC), y = log(ElevFGC), shape = "Fecal Corticosterone")) +
  labs(x = "Basal Glucocorticoid (ln(ng/g))", y = "Elevated Glucocorticoid (ln(ng/g))", shape = " ", color = " ") +
  scale_shape_manual(values = legend_shapes) +
  geom_abline(intercept = 3.01, slope = 0.57, linewidth = 1, colour = "red") + #from Haase et al. 2016
  geom_abline(intercept = coefficients(ElvFGCBasFGC_FecalCort_PGLS)[1], slope = coefficients(ElvFGCBasFGC_FecalCort_PGLS)[2], colour = "blue", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(ElvFGCBasFGC_Crtstn_PGLS)[1], slope = coefficients(ElvFGCBasFGC_Crtstn_PGLS)[2], colour = "green", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "none") # Removes the legend

ElvFGCBasFGC_combined  

#save a legend object to share in the combined plot
PlotLegend <- get_legend(BasFGCMSMR_combined +
                           theme(legend.position = "right"))

#save the combined plot
save_plot(plot_grid(BasFGCMSMR_combined, BasFGCMass_combined, ElvFGCBasFGC_combined, PlotLegend,
                    labels = c("A", "B", "C")), 
          filename = "PublicationFigures/FGC_Plasma.png",
          base_height = 8, base_width = 8)


# Stats table -------------------------------------------------------------

#load and rename cortisol reduced PGLS
load("Cortisol/CortisolUncorrected/BasFGCMSMR_Reduced.RData") 
load("Cortisol/CortisolUncorrected/BasFGCMass_Reduced.RData")
load("Cortisol/CortisolUncorrected/ElvFGCBasFGC_Reduced.RData")
load("Cortisol/CortisolUncorrected/LifespanBasFGC_Reduced.RData")
BasFGCMSMR_Cortisol_Reduced <- BasFGCMSMR_Reduced
BasFGCMass_Cortisol_Reduced <- BasFGCMass_Reduced
ElvFGCBasFGC_Cortisol_Reduced <- ElvFGCBasFGC_Reduced
LifespanBasFGC_Cortisol_Reduced <- LifespanBasFGC_Reduced

#load and rename corticosterone reduced PGLS
load("Corticosterone/CrtstnUncorrected/BasFGCMSMR_Reduced.RData") 
load("Corticosterone/CrtstnUncorrected/BasFGCMass_Reduced.RData")
load("Corticosterone/CrtstnUncorrected/ElvFGCBasFGC_Reduced.RData")
load("Corticosterone/CrtstnUncorrected/LifespanBasFGC_Reduced.RData")
BasFGCMSMR_Crtstn_Reduced <- BasFGCMSMR_Reduced
BasFGCMass_Crtstn_Reduced <- BasFGCMass_Reduced
ElvFGCBasFGC_Crtstn_Reduced <- ElvFGCBasFGC_Reduced
LifespanBasFGC_Crtstn_Reduced <- LifespanBasFGC_Reduced


StatsTab <- rbind(intervals(BasFGCMSMR_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasFGCMass_FecalCort_PGLS)[["coef"]][1,],
                  intervals(ElvFGCBasFGC_FecalCort_PGLS)[["coef"]][1,],
                  intervals(LifespanBasFGC_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasFGCMSMR_Crtstn_PGLS)[["coef"]][1,],
                  intervals(BasFGCMass_Crtstn_PGLS)[["coef"]][1,],
                  intervals(ElvFGCBasFGC_Crtstn_PGLS)[["coef"]][1,],
                  intervals(LifespanBasFGC_Crtstn_PGLS)[["coef"]][1,]) %>% #intercept coefficients 
  as.data.frame(.) %>% #cut out all the rows of intercept stats
  mutate(across(c(1,2,3), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Intercept (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge intercept columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., rbind(intervals(BasFGCMSMR_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasFGCMass_FecalCort_PGLS)[["coef"]][2,],
                 intervals(ElvFGCBasFGC_FecalCort_PGLS)[["coef"]][2,],
                 intervals(LifespanBasFGC_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasFGCMSMR_Crtstn_PGLS)[["coef"]][2,],
                 intervals(BasFGCMass_Crtstn_PGLS)[["coef"]][2,],
                 intervals(ElvFGCBasFGC_Crtstn_PGLS)[["coef"]][2,],
                 intervals(LifespanBasFGC_Crtstn_PGLS)[["coef"]][2,]) ) %>% #slope coefficients
  mutate(across(c(2,3,4), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Slope (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge slope columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., 
        rbind(R2(BasFGCMSMR_FecalCort_PGLS, BasFGCMSMR_Cortisol_Reduced)[1],
              R2(BasFGCMass_FecalCort_PGLS, BasFGCMass_Cortisol_Reduced)[1],
              R2(ElvFGCBasFGC_FecalCort_PGLS, ElvFGCBasFGC_Cortisol_Reduced)[1],
              R2(LifespanBasFGC_FecalCort_PGLS, LifespanBasFGC_Cortisol_Reduced)[1],
              R2(BasFGCMSMR_Crtstn_PGLS, BasFGCMSMR_Crtstn_Reduced)[1],
              R2(BasFGCMass_Crtstn_PGLS, BasFGCMass_Crtstn_Reduced)[1],
              R2(ElvFGCBasFGC_Crtstn_PGLS, ElvFGCBasFGC_Crtstn_Reduced)[1],
              R2(LifespanBasFGC_Crtstn_PGLS, LifespanBasFGC_Crtstn_Reduced)[1]), #likelihoof r squared column
        rbind(coefficients(summary(BasFGCMSMR_FecalCort_PGLS))[2,4],
              coefficients(summary(BasFGCMass_FecalCort_PGLS))[2,4],
              coefficients(summary(ElvFGCBasFGC_FecalCort_PGLS))[2,4],
              coefficients(summary(LifespanBasFGC_FecalCort_PGLS))[2,4],
              coefficients(summary(BasFGCMSMR_Crtstn_PGLS))[2,4],
              coefficients(summary(BasFGCMass_Crtstn_PGLS))[2,4],
              coefficients(summary(ElvFGCBasFGC_Crtstn_PGLS))[2,4],
              coefficients(summary(LifespanBasFGC_Crtstn_PGLS))[2,4])) %>% #p value column
  add_row(.before = 1) %>% add_row(.before = 6) %>% #add blank rows to divide cortisol and crtstn
  mutate(Model = c("Cortisol", "Basal FGC vs. MSMR", "Basal FGC vs. Mass", "Elevated FGC vs. Basal FGC", "Lifespan vs. Basal FGC", 
                   "Corticosterone", "Basal FGC vs. MSMR", "Basal FGC vs. Mass", "Elevated FGC vs. Basal FGC", "Lifespan vs. Basal FGC")) %>%
  `colnames<-`(c("Intercept (95% CI)", "Slope (95% CI)", "Likelihood R2","p-value", "Model")) %>%
  mutate(across(c(3,4), \(x) round(x, digits = 3))) %>%
  mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`)) #change very small p values to < 0.001

#reorder the table and remove NAs
StatsTab <- StatsTab[,c(5,1,2,3,4)]
StatsTab[is.na(StatsTab)] <- " "

tt1 <- ttheme_minimal(core=list(fg_params=list(hjust = 1, x = 0.95)))

png("PublicationFigures/StatsTab_PGLS.png", 
    height = 100*nrow(StatsTab), 
    width = 500*ncol(StatsTab),
    res = 300)
grid.newpage()
g <- tableGrob(StatsTab[,1], cols = "Model", theme = tt1)
g2 <- tableGrob(StatsTab[,2:ncol(StatsTab)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()


# Phylo Sig Table ---------------------------------------------------------

Cortisol_tree <- read.nexus("Cortisol/StressTree.nex")
Crtstn_tree <- read.nexus("Corticosterone/StressTree.nex")

PhyloSigTab <- 
cbind(c(phylosig(tree = Cortisol_tree,
           x = setNames(Cortisol_data$BasalFGC, Cortisol_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]], 
  phylosig(tree = Cortisol_tree,
           x = setNames(Cortisol_data$ElevFGC, Cortisol_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]], 
  phylosig(tree = Cortisol_tree,
           x = setNames(Cortisol_data$MSMR, Cortisol_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]],
  phylosig(tree = Cortisol_tree,
           x = setNames(Cortisol_data$BodyMassAnAge, Cortisol_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]],
  phylosig(tree = Crtstn_tree,
           x = setNames(Crtstn_data$BasalFGC, Crtstn_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]], 
  phylosig(tree = Crtstn_tree,
           x = setNames(Crtstn_data$ElevFGC, Crtstn_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]], 
  phylosig(tree = Crtstn_tree,
           x = setNames(Crtstn_data$MSMR, Crtstn_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]],
  phylosig(tree = Crtstn_tree,
           x = setNames(Crtstn_data$BodyMassAnAge, Crtstn_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)[["lambda"]]),
       c(phylosig(tree = Cortisol_tree,
             x = setNames(Cortisol_data$BasalFGC, Cortisol_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]], 
    phylosig(tree = Cortisol_tree,
             x = setNames(Cortisol_data$ElevFGC, Cortisol_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]], 
    phylosig(tree = Cortisol_tree,
             x = setNames(Cortisol_data$MSMR, Cortisol_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]],
    phylosig(tree = Cortisol_tree,
             x = setNames(Cortisol_data$BodyMassAnAge, Cortisol_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]],
    phylosig(tree = Crtstn_tree,
             x = setNames(Crtstn_data$BasalFGC, Crtstn_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]],
    phylosig(tree = Crtstn_tree,
             x = setNames(Crtstn_data$ElevFGC, Crtstn_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]],
    phylosig(tree = Crtstn_tree,
             x = setNames(Crtstn_data$MSMR, Crtstn_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]],
    phylosig(tree = Crtstn_tree,
             x = setNames(Crtstn_data$BodyMassAnAge, Crtstn_data$Species),   
             method = "lambda",
             test = TRUE, 
             nsim = 1000)[["P"]])) %>%
  as.data.frame(.) %>%
  mutate(across(c(1,2), \(x) round(x, digits = 4))) %>%
  `colnames<-`(c("Lambda", "p value")) %>%
  mutate(`p value` = ifelse(`p value` < 0.001, "< 0.001", `p value`)) %>% #change very small p values to < 0.001
  mutate(Lambda = ifelse(Lambda < 0.001, "< 0.001", Lambda)) %>% 
  add_row(.before = 1) %>% add_row(.before = 6) %>% #add blank rows to divide cortisol and crtstn
  mutate(Variable = c("Cortisol", "Basal FGC", "Elevated FGC", "MSMR", "Body Mass (g)", 
                      "Corticosterone", "Basal FGC", "Elevated FGC", "MSMR", "Body Mass (g)")) 
  
PhyloSigTab <- PhyloSigTab[ , c(3,1,2)]
PhyloSigTab[is.na(PhyloSigTab)] <- " "


tt1 <- ttheme_minimal(core=list(fg_params=list(hjust = 1, x = 0.95)))

png("PublicationFigures/PhyloSigTab.png", 
    height = 100*nrow(PhyloSigTab), 
    width = 300*ncol(PhyloSigTab),
    res = 300)
grid.newpage()
g <- tableGrob(PhyloSigTab[,1], cols = "Variable", theme = tt1)
g2 <- tableGrob(PhyloSigTab[,2:ncol(PhyloSigTab)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()































