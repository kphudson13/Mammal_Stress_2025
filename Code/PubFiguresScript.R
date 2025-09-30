
################################################################################

# Have you run MasterScript.R since you cloned this repository?
# If you haven't then all binaries this script needs do not exist in your local repository
# MasterScript.R will configure your repository with the binaries you need
# live laugh love -Kyle

################################################################################

# Load packages and configure ---------------------------------------------


library(tidyverse)
library(ape)
library(nlme) #for gls
library(rr2) #for the R2 function
library(grid) #to set table themes
library(gridExtra) #to set table themes
library(phytools) #for phylosig
library(cowplot) #to combine plots 

rm(list=ls())

if (file.exists("PublicationFigures")) {
  #Do nothing
} else {
  dir.create("PublicationFigures")
}

# FGC Figures -------------------------------------------------------------

#load in cortisol ggplot objects
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_Plot.RData")
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_Plot.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_Plot.RData")
load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_Plot.RData")

#Add titles to the plots
BasalFGCMSMR_Cortisol <- BasalFGCMSMR_Plot + labs(title = "Cortisol Based Assay")
BasalFGCMass_Cortisol <- BasalFGCMass_Plot + labs(title = "Cortisol Based Assay")
ElevFGCBasalFGC_Cortisol <- ElevFGCBasalFGC_Plot + labs(title = "Cortisol Based Assay")
LifespanBasalFGC_Cortisol <- LifespanBasalFGC_Plot + labs(title = "Cortisol Based Assay")

#load in corticosterone ggplot objects
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_Plot.RData")
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_Plot.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_Plot.RData")
load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_Plot.RData")

BasalFGCMSMR_Corticosterone <- BasalFGCMSMR_Plot + labs(title = "Corticosterone Based Assay")
BasalFGCMass_Corticosterone <- BasalFGCMass_Plot + labs(title = "Corticosterone Based Assay")
ElevFGCBasalFGC_Corticosterone <- ElevFGCBasalFGC_Plot + labs(title = "Corticosterone Based Assay")
LifespanBasalFGC_Corticosterone <- LifespanBasalFGC_Plot + labs(title = "Corticosterone Based Assay")

#export cortisol and corticosterone combined figures 
save_plot(plot_grid(BasalFGCMSMR_Cortisol, BasalFGCMSMR_Corticosterone, labels = c("A:", "B:")),
          filename = "PublicationFigures/BasalFGCMSMR.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(BasalFGCMass_Cortisol, BasalFGCMass_Corticosterone, labels = c("A:", "B:")),
          filename = "PublicationFigures/BasalFGCMass.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(ElevFGCBasalFGC_Cortisol, ElevFGCBasalFGC_Corticosterone, labels = c("A:", "B:")),
          filename = "PublicationFigures/ElevFGCBasalFGC.png",
          base_height = 4, base_width = 8)

save_plot(plot_grid(LifespanBasalFGC_Cortisol, LifespanBasalFGC_Corticosterone, labels = c("A:", "B:")),
          filename = "PublicationFigures/LifespanBasalFGC.png",
          base_height = 4, base_width = 8)

# FGC and Plasma Figures --------------------------------------------------

#load in the data sets
Cortisol_data <- read.csv("Cortisol/CortisolDataClean.csv")
Crtstn_data <- read.csv("Corticosterone/CrtstnDataClean.csv")
Plasma_data <- read.csv("HaaseData.csv")

#load in and rename PGLS objects
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_PGLS.RData") #load cortisol PGLS
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_PGLS.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_PGLS.RData")
load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_PGLS.RData")
BasalFGCMSMR_FecalCort_PGLS <- BasalFGCMSMR_PGLS
BasalFGCMass_FecalCort_PGLS <- BasalFGCMass_PGLS
ElevFGCBasalFGC_FecalCort_PGLS <- ElevFGCBasalFGC_PGLS
LifespanBasalFGC_FecalCort_PGLS <- LifespanBasalFGC_PGLS

load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_PGLS.RData") #load corticosterone PGLS 
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_PGLS.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_PGLS.RData")
load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_PGLS.RData")
BasalFGCMSMR_Crtstn_PGLS <- BasalFGCMSMR_PGLS
BasalFGCMass_Crtstn_PGLS <- BasalFGCMass_PGLS
ElevFGCBasalFGC_Crtstn_PGLS <- ElevFGCBasalFGC_PGLS
LifespanBasalFGC_Crtstn_PGLS <- LifespanBasalFGC_PGLS

#set shape weirdly because there is multiple data sets 
legend_shapes <- c("Plasma Cortisol" = 15, "Fecal Cortisol" = 1, "Fecal Corticosterone" = 17)
legend_colors <- c("Plasma Cortisol" = "firebrick", "Fecal Cortisol" = "dodgerblue2", "Fecal Corticosterone" = "seagreen3")

BasalFGCMSMR_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(MSMR), y = log(Base), color = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Corticosterone")) +
  labs(x = "MSMR (ln(mW/g)", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
  scale_color_manual(values = legend_colors) +
  geom_abline(intercept = 3.3, slope = 0.97, colour = "firebrick", linewidth = 1) + #from Haase et al. 2016
  geom_abline(intercept = coefficients(BasalFGCMSMR_FecalCort_PGLS)[1], slope = coefficients(BasalFGCMSMR_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(BasalFGCMSMR_Crtstn_PGLS)[1], slope = coefficients(BasalFGCMSMR_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "none") # Removes the legend

BasalFGCMSMR_combined  

BasalFGCMass_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(as.numeric(Mass)), y = log(Base),  color = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(Mass), y = log(BasalFGC), color = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(Mass), y = log(BasalFGC), color = "Fecal Corticosterone")) +
  labs(x = "Body Mass (ln(g)", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
  scale_color_manual(values = legend_colors) +
  geom_abline(intercept = 6.12, slope = -0.22, colour = "firebrick", linewidth = 1) + #from Haase et al. 2016
  geom_abline(intercept = coefficients(BasalFGCMass_FecalCort_PGLS)[1], slope = coefficients(BasalFGCMass_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(BasalFGCMass_Crtstn_PGLS)[1], slope = coefficients(BasalFGCMass_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "none") # Removes the legend

BasalFGCMass_combined

ElevFGCBasalFGC_combined <- ggplot() +
  geom_point(data = Plasma_data, aes(x = log(Base), y = log(Elev), color = "Plasma Cortisol")) +
  geom_point(data = Cortisol_data, aes(x = log(BasalFGC), y = log(ElevFGC), color = "Fecal Cortisol")) +
  geom_point(data = Crtstn_data, aes(x = log(BasalFGC), y = log(ElevFGC), color = "Fecal Corticosterone")) +
  labs(x = "Baseline Glucocorticoid (ln(ng/g))", y = "Elevated Glucocorticoid (ln(ng/g))", color = " ") +
  scale_color_manual(values = legend_colors) +
  geom_abline(intercept = 3.01, slope = 0.57, linewidth = 1, colour = "firebrick") + #from Haase et al. 2016
  geom_abline(intercept = coefficients(ElevFGCBasalFGC_FecalCort_PGLS)[1], slope = coefficients(ElevFGCBasalFGC_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
  geom_abline(intercept = coefficients(ElevFGCBasalFGC_Crtstn_PGLS)[1], slope = coefficients(ElevFGCBasalFGC_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.2))

ElevFGCBasalFGC_combined  

#save the combined plot
save_plot(plot_grid(BasalFGCMSMR_combined, BasalFGCMass_combined, ElevFGCBasalFGC_combined, 
                    labels = c("A", "B", "C"), nrow = 1), 
          filename = "PublicationFigures/FGC_Plasma.png",
          base_height = 4, base_width = 12)


# Stats table -------------------------------------------------------------

#load and rename cortisol reduced PGLS
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_Reduced.RData") 
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_Reduced.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_Reduced.RData")
load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_Reduced.RData")
BasalFGCMSMR_Cortisol_Reduced <- BasalFGCMSMR_Reduced
BasalFGCMass_Cortisol_Reduced <- BasalFGCMass_Reduced
ElevFGCBasalFGC_Cortisol_Reduced <- ElevFGCBasalFGC_Reduced
LifespanBasalFGC_Cortisol_Reduced <- LifespanBasalFGC_Reduced

#load and rename corticosterone reduced PGLS
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_Reduced.RData") 
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_Reduced.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_Reduced.RData")
load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_Reduced.RData")
BasalFGCMSMR_Crtstn_Reduced <- BasalFGCMSMR_Reduced
BasalFGCMass_Crtstn_Reduced <- BasalFGCMass_Reduced
ElevFGCBasalFGC_Crtstn_Reduced <- ElevFGCBasalFGC_Reduced
LifespanBasalFGC_Crtstn_Reduced <- LifespanBasalFGC_Reduced

StatsTab <- rbind(intervals(BasalFGCMSMR_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMass_FecalCort_PGLS)[["coef"]][1,],
                  intervals(ElevFGCBasalFGC_FecalCort_PGLS)[["coef"]][1,],
                  intervals(LifespanBasalFGC_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMSMR_Crtstn_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMass_Crtstn_PGLS)[["coef"]][1,],
                  intervals(ElevFGCBasalFGC_Crtstn_PGLS)[["coef"]][1,],
                  intervals(LifespanBasalFGC_Crtstn_PGLS)[["coef"]][1,]) %>% #intercept coefficients 
  as.data.frame(.) %>% #cut out all the rows of intercept stats
  mutate(across(c(1,2,3), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Intercept (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge intercept columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., rbind(intervals(BasalFGCMSMR_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMass_FecalCort_PGLS)[["coef"]][2,],
                 intervals(ElevFGCBasalFGC_FecalCort_PGLS)[["coef"]][2,],
                 intervals(LifespanBasalFGC_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMSMR_Crtstn_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMass_Crtstn_PGLS)[["coef"]][2,],
                 intervals(ElevFGCBasalFGC_Crtstn_PGLS)[["coef"]][2,],
                 intervals(LifespanBasalFGC_Crtstn_PGLS)[["coef"]][2,]) ) %>% #slope coefficients
  mutate(across(c(2,3,4), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Slope (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge slope columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., 
        rbind(R2(BasalFGCMSMR_FecalCort_PGLS, BasalFGCMSMR_Cortisol_Reduced)[1],
              R2(BasalFGCMass_FecalCort_PGLS, BasalFGCMass_Cortisol_Reduced)[1],
              R2(ElevFGCBasalFGC_FecalCort_PGLS, ElevFGCBasalFGC_Cortisol_Reduced)[1],
              R2(LifespanBasalFGC_FecalCort_PGLS, LifespanBasalFGC_Cortisol_Reduced)[1],
              R2(BasalFGCMSMR_Crtstn_PGLS, BasalFGCMSMR_Crtstn_Reduced)[1],
              R2(BasalFGCMass_Crtstn_PGLS, BasalFGCMass_Crtstn_Reduced)[1],
              R2(ElevFGCBasalFGC_Crtstn_PGLS, ElevFGCBasalFGC_Crtstn_Reduced)[1],
              R2(LifespanBasalFGC_Crtstn_PGLS, LifespanBasalFGC_Crtstn_Reduced)[1]), #likelihoof r squared column
        rbind(coefficients(summary(BasalFGCMSMR_FecalCort_PGLS))[2,4],
              coefficients(summary(BasalFGCMass_FecalCort_PGLS))[2,4],
              coefficients(summary(ElevFGCBasalFGC_FecalCort_PGLS))[2,4],
              coefficients(summary(LifespanBasalFGC_FecalCort_PGLS))[2,4],
              coefficients(summary(BasalFGCMSMR_Crtstn_PGLS))[2,4],
              coefficients(summary(BasalFGCMass_Crtstn_PGLS))[2,4],
              coefficients(summary(ElevFGCBasalFGC_Crtstn_PGLS))[2,4],
              coefficients(summary(LifespanBasalFGC_Crtstn_PGLS))[2,4])) %>% #p value column
  add_row(.before = 1) %>% add_row(.before = 6) %>% #add blank rows to divide cortisol and crtstn
  mutate(Model = c("Cortisol", "Baseline FGC vs. MSMR", "Baseline FGC vs. Mass", "Elevated FGC vs. Baseline FGC", "Lifespan vs. Baseline FGC", 
                   "Corticosterone", "Baseline FGC vs. MSMR", "Baseline FGC vs. Mass", "Elevated FGC vs. Baseline FGC", "Lifespan vs. Baseline FGC")) %>%
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
  mutate(Variable = c("Cortisol", "Baseline FGC", "Elevated FGC", "MSMR", "Body Mass (g)",
                      "Corticosterone", "Baseline FGC", "Elevated FGC", "MSMR", "Body Mass (g)"))

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


# AIC Table ---------------------------------------------------------------

load("Cortisol/CortisolMeanLifespan/AIC_table.RData")
Cortisol_AIC <- AIC_table %>%
  mutate(Model = row.names(.))
load("Corticosterone/CrtstnMeanLifespan/AIC_table.RData")
Crtstn_AIC <- AIC_table %>%
  mutate(Model = row.names(.))

AICTab <- rbind(Cortisol_AIC, Crtstn_AIC) %>%
  add_row(.before = 1) %>% add_row(.before = 4) %>% add_row(.before = 14) %>% #add blank rows to divide cortisol and crtstn and blank model
  mutate(Model = c("Cortisol", 
                   "Baseline FGC ~ MSMR", "Baseline FGC ~ MSMR + Stressor", "Baseline FGC ~ MSMR + Method",
                   "Baseline FGC ~ Mass", "Baseline FGC ~ Mass + Stressor", "Baseline FGC ~ Mass + Method",
                   "Elevated FGC ~ Baseline FGC", "Elevated FGC ~ Baseline FGC + Stressor", "Elevated FGC ~ Baseline FGC + Method",
                   "Lifespan ~ Baseline FGC", "Lifespan ~ Baseline FGC + Stressor", "Lifespan ~ Baseline FGC + Method",
                   "Corticosterone",
                   "Baseline FGC ~ MSMR", "Baseline FGC ~ MSMR + Stressor", "Baseline FGC ~ MSMR + Method",
                   "Baseline FGC ~ Mass", "Baseline FGC ~ Mass + Stressor", "Baseline FGC ~ Mass + Method",
                   "Elevated FGC ~ Baseline FGC", "Elevated FGC ~ Baseline FGC + Stressor", "Elevated FGC ~ Baseline FGC + Method",
                   "Lifespan ~ Baseline FGC", "Lifespan ~ Baseline FGC + Stressor", "Lifespan ~ Baseline FGC + Method")) %>%
  `rownames<-`(NULL) 

AICTab <- AICTab[ ,c(4,1,2,3)]













