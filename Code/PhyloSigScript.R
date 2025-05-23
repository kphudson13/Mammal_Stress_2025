
# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

library(phytools) 

PhyloSig_table <- 
  data.frame(
    Lambda = c(phylosig(tree = tree, 
                        x = setNames(StressData$BodyMassAnAge, StressData$Species),   
                        method = "lambda",
                        test = TRUE, 
                        nsim = 1000)[["lambda"]], 
               phylosig(tree = tree,
                        x = setNames(StressData$MSMR, StressData$Species),   
                        method = "lambda",
                        test = TRUE, 
                        nsim = 1000)[["lambda"]], 
               phylosig(tree = tree,
                        x = setNames(StressData$BasalFGC, StressData$Species),   
                        method = "lambda",
                        test = TRUE, 
                        nsim = 1000)[["lambda"]], 
               phylosig(tree = tree,
                        x = setNames(StressData$ElevFGC, StressData$Species),   
                        method = "lambda",
                        test = TRUE, 
                        nsim = 1000)[["lambda"]], 
               phylosig(tree = tree,
                        x = setNames(StressData$MaxLifespan, StressData$Species),   
                        method = "lambda",
                        test = TRUE, 
                        nsim = 1000)[["lambda"]]),
    P_value = c(phylosig(tree = tree, 
                         x = setNames(StressData$BodyMassAnAge, StressData$Species),   
                         method = "lambda",
                         test = TRUE, 
                         nsim = 1000)[["P"]], 
                phylosig(tree = tree,
                         x = setNames(StressData$MSMR, StressData$Species),   
                         method = "lambda",
                         test = TRUE, 
                         nsim = 1000)[["P"]], 
                phylosig(tree = tree,
                         x = setNames(StressData$BasalFGC, StressData$Species),   
                         method = "lambda",
                         test = TRUE, 
                         nsim = 1000)[["P"]], 
                phylosig(tree = tree,
                         x = setNames(StressData$ElevFGC, StressData$Species),   
                         method = "lambda",
                         test = TRUE, 
                         nsim = 1000)[["P"]], 
                phylosig(tree = tree,
                         x = setNames(StressData$MaxLifespan, StressData$Species),   
                         method = "lambda",
                         test = TRUE, 
                         nsim = 1000)[["P"]])) %>%
  `rownames<-`(c("Body Mass", "MSMR", "Basal FGC", "Elevated FGC", "lifespan")) %>%
  mutate(across(c(1,2), \(x) round(x, digits = 4)))  %>%
  `colnames<-`(c("Lambda", "p value")) %>%
  mutate(`p value` = ifelse(`p value` < 0.001, "< 0.001", `p value`)) #change very small p values to < 0.001

tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png(paste(directory, "Figures/PhysoSigTable.png", sep = ""),
    height = 190*nrow(PhyloSig_table), 
    width = 700*ncol(PhyloSig_table),
    res = 300)
grid.newpage()
grid.table(PhyloSig_table, theme = tt1)
grid.text(Label, x = 0.4, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()

