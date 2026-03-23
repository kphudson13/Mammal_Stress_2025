
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
} # make the folder if it doesn't exist

# FGC Figures -------------------------------------------------------------

#load in cortisol ggplot objects
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_Plot.RData")
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_Plot.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_Plot.RData")
# load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_Plot.RData")

#Add titles to the plots
BasalFGCMSMR_Cortisol <- BasalFGCMSMR_Plot
BasalFGCMass_Cortisol <- BasalFGCMass_Plot
ElevFGCBasalFGC_Cortisol <- ElevFGCBasalFGC_Plot
# LifespanBasalFGC_Cortisol <- LifespanBasalFGC_Plot

#load in corticosterone ggplot objects
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_Plot.RData")
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_Plot.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_Plot.RData")
# load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_Plot.RData")

BasalFGCMSMR_Corticosterone <- BasalFGCMSMR_Plot 
BasalFGCMass_Corticosterone <- BasalFGCMass_Plot 
ElevFGCBasalFGC_Corticosterone <- ElevFGCBasalFGC_Plot 
# LifespanBasalFGC_Corticosterone <- LifespanBasalFGC_Plot 

#export cortisol and corticosterone combined figures 
save_plot(plot_grid(BasalFGCMSMR_Cortisol, BasalFGCMSMR_Corticosterone, 
                    labels = c("A", "B"), label_fontface = "plain"),
          filename = "PublicationFigures/Figure_1.png",
          base_height = 3.3, base_width = 6.6)

save_plot(plot_grid(BasalFGCMass_Cortisol, BasalFGCMass_Corticosterone, 
                    labels = c("A", "B"), label_fontface = "plain"),
          filename = "PublicationFigures/Figure_2.png",
          base_height = 3.3, base_width = 6.6)

save_plot(plot_grid(ElevFGCBasalFGC_Cortisol, ElevFGCBasalFGC_Corticosterone, 
                    labels = c("A", "B"), label_fontface = "plain"),
          filename = "PublicationFigures/Figure_3.png",
          base_height = 3.3, base_width = 6.6)

# save_plot(plot_grid(LifespanBasalFGC_Cortisol, LifespanBasalFGC_Corticosterone, labels = c("A", "B")),
#           filename = "PublicationFigures/LifespanBasalFGC.png",
#           base_height = 4, base_width = 8)

# FGC and Plasma Figures --------------------------------------------------

#load in the data sets
Cortisol_data <- read.csv("Cortisol/CortisolDataClean.csv")
Crtstn_data <- read.csv("Corticosterone/CrtstnDataClean.csv")
Plasma_data <- read.csv("HaaseData.csv")

#load in and rename PGLS objects
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_PGLS.RData") #load cortisol PGLS
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_PGLS.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_PGLS.RData")
# load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_PGLS.RData")
BasalFGCMSMR_FecalCort_PGLS <- BasalFGCMSMR_PGLS
BasalFGCMass_FecalCort_PGLS <- BasalFGCMass_PGLS
ElevFGCBasalFGC_FecalCort_PGLS <- ElevFGCBasalFGC_PGLS
# LifespanBasalFGC_FecalCort_PGLS <- LifespanBasalFGC_PGLS

load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_PGLS.RData") #load corticosterone PGLS 
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_PGLS.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_PGLS.RData")
# load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_PGLS.RData")
BasalFGCMSMR_Crtstn_PGLS <- BasalFGCMSMR_PGLS
BasalFGCMass_Crtstn_PGLS <- BasalFGCMass_PGLS
ElevFGCBasalFGC_Crtstn_PGLS <- ElevFGCBasalFGC_PGLS
# LifespanBasalFGC_Crtstn_PGLS <- LifespanBasalFGC_PGLS

#set shape weirdly because there is multiple data sets 
legend_shapes <- c("Plasma Cortisol" = 15, "Fecal Cortisol" = 1, "Fecal Corticosterone" = 17)
legend_colors <- c("Plasma Cortisol" = "firebrick", "Fecal Cortisol" = "dodgerblue2", "Fecal Corticosterone" = "seagreen3")

theme1 <- theme(axis.title = element_text(size =10),
                legend.position = "none")

(BasalFGCMSMR_combined <- ggplot() +
    geom_point(data = Plasma_data, aes(x = log(MSMR), y = log(Base), color = "Plasma Cortisol")) +
    geom_point(data = Cortisol_data, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Cortisol")) +
    geom_point(data = Crtstn_data, aes(x = log(MSMR), y = log(BasalFGC), color = "Fecal Corticosterone")) +
    labs(x = "MSMR (ln(mW/g))", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
    scale_color_manual(values = legend_colors) +
    geom_abline(intercept = 3.3, slope = 0.97, colour = "firebrick", linewidth = 1) + #from Haase et al. 2016
    geom_abline(intercept = coefficients(BasalFGCMSMR_FecalCort_PGLS)[1], slope = coefficients(BasalFGCMSMR_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
    geom_abline(intercept = coefficients(BasalFGCMSMR_Crtstn_PGLS)[1], slope = coefficients(BasalFGCMSMR_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
    theme_classic() +
    theme1)

(BasalFGCMass_combined <- ggplot() +
    geom_point(data = Plasma_data, aes(x = log(as.numeric(Mass)), y = log(Base),  color = "Plasma Cortisol")) +
    geom_point(data = Cortisol_data, aes(x = log(Mass), y = log(BasalFGC), color = "Fecal Cortisol")) +
    geom_point(data = Crtstn_data, aes(x = log(Mass), y = log(BasalFGC), color = "Fecal Corticosterone")) +
    labs(x = "Body Mass (ln(g))", y = "Glucocorticoid (ln(ng/g))", shape = " ") +
    scale_color_manual(values = legend_colors) +
    geom_abline(intercept = 6.12, slope = -0.22, colour = "firebrick", linewidth = 1) + #from Haase et al. 2016
    geom_abline(intercept = coefficients(BasalFGCMass_FecalCort_PGLS)[1], slope = coefficients(BasalFGCMass_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
    geom_abline(intercept = coefficients(BasalFGCMass_Crtstn_PGLS)[1], slope = coefficients(BasalFGCMass_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
    theme_classic() +
    theme1)

(ElevFGCBasalFGC_combined <- ggplot() +
    geom_point(data = Plasma_data, aes(x = log(Base), y = log(Elev), color = "Plasma Cortisol")) +
    geom_point(data = Cortisol_data, aes(x = log(BasalFGC), y = log(ElevFGC), color = "Fecal Cortisol")) +
    geom_point(data = Crtstn_data, aes(x = log(BasalFGC), y = log(ElevFGC), color = "Fecal Corticosterone")) +
    labs(x = "Baseline Glucocorticoid (ln(ng/g))", y = "Elevated Glucocorticoid (ln(ng/g))", color = " ") +
    scale_color_manual(values = legend_colors, labels = c("FGC Corticosterone-Assays", "FGC Cortisol-Assays", "Plasma Cortisol")) + #change the legend labels 
    geom_abline(intercept = 3.01, slope = 0.57, linewidth = 1, colour = "firebrick") + #from Haase et al. 2016
    geom_abline(intercept = coefficients(ElevFGCBasalFGC_FecalCort_PGLS)[1], slope = coefficients(ElevFGCBasalFGC_FecalCort_PGLS)[2], colour = "dodgerblue2", linewidth = 1) + #from PGLS
    geom_abline(intercept = coefficients(ElevFGCBasalFGC_Crtstn_PGLS)[1], slope = coefficients(ElevFGCBasalFGC_Crtstn_PGLS)[2], colour = "seagreen3", linewidth = 1) + #from PGLS
    theme_classic() +
    theme1 +
    theme(legend.position = "inside",
          legend.position.inside = c(0.7,0.16), 
          legend.background = element_blank(),
          legend.key.height = unit(0.6, "lines"))) # move legend labels closer together


save_plot(plot_grid(BasalFGCMSMR_combined, 
          labels = c("A"), label_fontface = "plain", nrow = 1),
          filename = "PublicationFigures/Figure_4A.png",
          base_height = 3.3, base_width = 3.3)

save_plot(plot_grid(BasalFGCMass_combined, 
                    labels = c("B"), label_fontface = "plain", nrow = 1),
          filename = "PublicationFigures/Figure_4B.png",
          base_height = 3.3, base_width = 3.3)

save_plot(plot_grid(ElevFGCBasalFGC_combined, 
                    labels = c("C"), label_fontface = "plain", nrow = 1),
          filename = "PublicationFigures/Figure_4C.png",
          base_height = 3.3, base_width = 3.3)

#save the combined plot
save_plot(plot_grid(BasalFGCMSMR_combined, BasalFGCMass_combined, ElevFGCBasalFGC_combined, 
                    labels = c("A", "B", "C"), label_fontface = "plain", nrow = 1), 
          filename = "PublicationFigures/Figure_4.png",
          base_height = 3.3, base_width = 9)


# Stats table -------------------------------------------------------------

#load and rename cortisol reduced PGLS
load("Cortisol/CortisolMeanLifespan/BasalFGCMSMR_Reduced.RData") 
load("Cortisol/CortisolMeanLifespan/BasalFGCMass_Reduced.RData")
load("Cortisol/CortisolMeanLifespan/ElevFGCBasalFGC_Reduced.RData")
# load("Cortisol/CortisolMeanLifespan/LifespanBasalFGC_Reduced.RData")
BasalFGCMSMR_Cortisol_Reduced <- BasalFGCMSMR_Reduced
BasalFGCMass_Cortisol_Reduced <- BasalFGCMass_Reduced
ElevFGCBasalFGC_Cortisol_Reduced <- ElevFGCBasalFGC_Reduced
# LifespanBasalFGC_Cortisol_Reduced <- LifespanBasalFGC_Reduced

#load and rename corticosterone reduced PGLS
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMSMR_Reduced.RData") 
load("Corticosterone/CrtstnMeanLifespan/BasalFGCMass_Reduced.RData")
load("Corticosterone/CrtstnMeanLifespan/ElevFGCBasalFGC_Reduced.RData")
# load("Corticosterone/CrtstnMeanLifespan/LifespanBasalFGC_Reduced.RData")
BasalFGCMSMR_Crtstn_Reduced <- BasalFGCMSMR_Reduced
BasalFGCMass_Crtstn_Reduced <- BasalFGCMass_Reduced
ElevFGCBasalFGC_Crtstn_Reduced <- ElevFGCBasalFGC_Reduced
# LifespanBasalFGC_Crtstn_Reduced <- LifespanBasalFGC_Reduced

StatsTab <- rbind(intervals(BasalFGCMSMR_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMass_FecalCort_PGLS)[["coef"]][1,],
                  intervals(ElevFGCBasalFGC_FecalCort_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMSMR_Crtstn_PGLS)[["coef"]][1,],
                  intervals(BasalFGCMass_Crtstn_PGLS)[["coef"]][1,],
                  intervals(ElevFGCBasalFGC_Crtstn_PGLS)[["coef"]][1,]) %>% #intercept coefficients 
  as.data.frame(.) %>% #cut out all the rows of intercept stats
  mutate(across(c(1,2,3), \(x) (sprintf("%.2f", round(x, digits = 2))))) %>% #new way to round w/ anonymous function
  mutate(`Intercept (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge intercept columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., rbind(intervals(BasalFGCMSMR_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMass_FecalCort_PGLS)[["coef"]][2,],
                 intervals(ElevFGCBasalFGC_FecalCort_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMSMR_Crtstn_PGLS)[["coef"]][2,],
                 intervals(BasalFGCMass_Crtstn_PGLS)[["coef"]][2,],
                 intervals(ElevFGCBasalFGC_Crtstn_PGLS)[["coef"]][2,]) ) %>% #slope coefficients
  mutate(across(c(2,3,4), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(`Slope (95% CI)` = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>% #merge slope columns
  select(., -c("est.", "lower", "upper")) %>% #remove the columns we don't want
  cbind(., 
        rbind(R2(BasalFGCMSMR_FecalCort_PGLS, BasalFGCMSMR_Cortisol_Reduced)[1],
              R2(BasalFGCMass_FecalCort_PGLS, BasalFGCMass_Cortisol_Reduced)[1],
              R2(ElevFGCBasalFGC_FecalCort_PGLS, ElevFGCBasalFGC_Cortisol_Reduced)[1],
              R2(BasalFGCMSMR_Crtstn_PGLS, BasalFGCMSMR_Crtstn_Reduced)[1],
              R2(BasalFGCMass_Crtstn_PGLS, BasalFGCMass_Crtstn_Reduced)[1],
              R2(ElevFGCBasalFGC_Crtstn_PGLS, ElevFGCBasalFGC_Crtstn_Reduced)[1]), #likelihoof r squared column
        rbind(coefficients(summary(BasalFGCMSMR_FecalCort_PGLS))[2,4],
              coefficients(summary(BasalFGCMass_FecalCort_PGLS))[2,4],
              coefficients(summary(ElevFGCBasalFGC_FecalCort_PGLS))[2,4],
              coefficients(summary(BasalFGCMSMR_Crtstn_PGLS))[2,4],
              coefficients(summary(BasalFGCMass_Crtstn_PGLS))[2,4],
              coefficients(summary(ElevFGCBasalFGC_Crtstn_PGLS))[2,4]),
        rbind(BasalFGCMSMR_FecalCort_PGLS[["modelStruct"]][["corStruct"]],
              BasalFGCMass_FecalCort_PGLS[["modelStruct"]][["corStruct"]],
              ElevFGCBasalFGC_FecalCort_PGLS[["modelStruct"]][["corStruct"]],
              BasalFGCMSMR_Crtstn_PGLS[["modelStruct"]][["corStruct"]],
              BasalFGCMass_Crtstn_PGLS[["modelStruct"]][["corStruct"]],
              ElevFGCBasalFGC_Crtstn_PGLS[["modelStruct"]][["corStruct"]]),
        rbind(BasalFGCMSMR_FecalCort_PGLS[["dims"]][["N"]],
              BasalFGCMass_FecalCort_PGLS[["dims"]][["N"]],
              ElevFGCBasalFGC_FecalCort_PGLS[["dims"]][["N"]],
              BasalFGCMSMR_Crtstn_PGLS[["dims"]][["N"]],
              BasalFGCMass_Crtstn_PGLS[["dims"]][["N"]],
              ElevFGCBasalFGC_Crtstn_PGLS[["dims"]][["N"]])) %>% #p value column
  mutate(across(4, \(x) round(x, digits = 3))) %>%
  mutate(across(c(3,5), \(x) (sprintf("%.2f", round(x, digits = 2))))) %>%
  add_row(.before = 1) %>% add_row(.before = 5) %>% #add blank rows to divide cortisol and crtstn
  mutate(Model = c("Cortisol-Based Assay", "Baseline FGC vs. MSMR", "Baseline FGC vs. Mass", "Elevated FGC vs. Baseline FGC", 
                   "Corticosterone-Based Assay", "Baseline FGC vs. MSMR", "Baseline FGC vs. Mass", "Elevated FGC vs. Baseline FGC")) %>%
  `colnames<-`(c("Intercept (95% CI)", "Slope (95% CI)", "Likelihood R2","p-value", "Lambda", "n", "Model")) %>%
  mutate(`p-value` = ifelse(`p-value` < 0.001, "< 0.001", `p-value`)) %>% #change very small p values to < 0.001
  select(7,1,2,3,4,5,6)
  
StatsTab[is.na(StatsTab)] <- " " # remove NAs

write.csv(StatsTab, "PublicationFigures/StatsTable.csv")

tt1 <- ttheme_minimal(core=list(fg_params=list(hjust = 1, x = 0.95)))
tt2 <- ttheme_minimal(core=list(fg_params=list(hjust = 0, x = 0.05)))

png("PublicationFigures/Table_1.png", 
    height = 100*nrow(StatsTab), 
    width = 400*ncol(StatsTab),
    res = 300)
grid.newpage()
g <- tableGrob(StatsTab[,1], cols = "Model", theme = tt2)
g2 <- tableGrob(StatsTab[,2:ncol(StatsTab)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()


# BIC Table ---------------------------------------------------------------

load("Cortisol/CortisolMeanLifespan/BIC_table.RData")
Cortisol_BIC <- BIC_table %>%
  mutate(Model = row.names(.))
load("Corticosterone/CrtstnMeanLifespan/BIC_table.RData")
Crtstn_BIC <- BIC_table %>%
  mutate(Model = row.names(.))

BICTab <- rbind(Cortisol_BIC, Crtstn_BIC) %>%
  add_row(.before = 1) %>% add_row(.before = 11) %>% #add blank rows to divide cortisol and crtstn and blank model
  mutate(Model = c("Cortisol-Based Assay", 
                   "Baseline FGC ~ MSMR", "Baseline FGC ~ MSMR + Stressor", "Baseline FGC ~ MSMR + Method",
                   "Baseline FGC ~ Mass", "Baseline FGC ~ Mass + Stressor", "Baseline FGC ~ Mass + Method",
                   "Elevated FGC ~ Baseline FGC", "Elevated FGC ~ Baseline FGC + Stressor", "Elevated FGC ~ Baseline FGC + Method",
                   "Corticosterone-Based Assay",
                   "Baseline FGC ~ MSMR", "Baseline FGC ~ MSMR + Stressor", "Baseline FGC ~ MSMR + Method",
                   "Baseline FGC ~ Mass", "Baseline FGC ~ Mass + Stressor", "Baseline FGC ~ Mass + Method",
                   "Elevated FGC ~ Baseline FGC", "Elevated FGC ~ Baseline FGC + Stressor", "Elevated FGC ~ Baseline FGC + Method")) %>%
  `rownames<-`(NULL) %>%
  select(3,1,2)

BICTab[is.na(BICTab)] <- " "  # remove NAs

write.csv(BICTab, "PublicationFigures/BICTable.csv")

png("PublicationFigures/Appendix_2.png", 
    height = 100*nrow(BICTab), 
    width = 500*ncol(BICTab),
    res = 300)
grid.newpage()
g <- tableGrob(BICTab[,1], cols = "Model", theme = tt2)
g2 <- tableGrob(BICTab[,2:ncol(BICTab)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()











