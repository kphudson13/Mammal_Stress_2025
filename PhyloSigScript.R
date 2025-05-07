
library(phytools) 

Mass_signal <- 
  phylosig(tree = tree, 
           x = setNames(StressData$BodyMassAnAge, StressData$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

BasFGC_signal <-
  phylosig(tree = tree,
           x = setNames(BasFGCMass_data$BasalFGC, BasFGCMass_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

MSMR_signal <-
  phylosig(tree = tree,
           x = setNames(BasFGCMSMR_data$MSMR, BasFGCMSMR_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

ElvFGC_signal <-
  phylosig(tree = tree,
           x = setNames(ElvFGCBasFGC_data$ElevFGC, ElvFGCBasFGC_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

Lifespan_signal <-
  phylosig(tree = tree,
           x = setNames(LifespanBasFGC_data$MaxLifespan, LifespanBasFGC_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)


PhyloSig_table <- 
  data.frame(
    Lambda = c(Mass_signal[["lambda"]], MSMR_signal[["lambda"]], BasFGC_signal[["lambda"]], ElvFGC_signal[["lambda"]], Lifespan_signal[["lambda"]]),
    P_value = c(Mass_signal[["P"]], MSMR_signal[["P"]], BasFGC_signal[["P"]], ElvFGC_signal[["P"]], Lifespan_signal[["P"]])) %>%
  `rownames<-`(c("Body Mass", "MSMR", "Basal FGC", "Elevated FGC", "lifespan")) %>%
  mutate(across(c(1,2), \(x) round(x, digits = 4)))  %>%
  `colnames<-`(c("Lambda", "p value")) %>%
  mutate(`p value` = ifelse(`p value` < 0.001, "< 0.001", `p value`)) #change very small p values to < 0.001

tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png("PhysoSigTable.png", 
    height = 190*nrow(PhyloSig_table), 
    width = 700*ncol(PhyloSig_table),
    res = 300)
grid.newpage()
grid.table(PhyloSig_table, theme = tt1)
grid.text(Label, x = 0.4, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()

