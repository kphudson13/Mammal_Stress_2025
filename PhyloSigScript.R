
Mass_signal <- 
  phylosig(tree = tree, 
           x = setNames(StressData$BodyMassAnAge, StressData$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

Mass_signal[["lambda"]]

BasFGC_signal <-
  phylosig(tree = tree,
           x = setNames(BasFGCMass_data$BasalFGC, BasFGCMass_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

BasFGC_signal[["lambda"]]

MSMR_signal <-
  phylosig(tree = tree,
           x = setNames(BasFGCMSMR_data$MSMR, BasFGCMSMR_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

MSMR_signal[["lambda"]]

ElvFGC_signal <-
  phylosig(tree = tree,
           x = setNames(ElvFGCBasFGC_data$ElevFGC, ElvFGCBasFGC_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

ElvFGC_signal[["lambda"]]

PhyloSig_table <- 
  data.frame(
    Lambda = c(Mass_signal[["lambda"]], MSMR_signal[["lambda"]], BasFGC_signal[["lambda"]], ElvFGC_signal[["lambda"]]),
    P_value = c(Mass_signal[["P"]], MSMR_signal[["P"]], BasFGC_signal[["P"]], ElvFGC_signal[["P"]])) %>%
  `rownames<-`(c("Body Mass", "MSMR", "Basal FGC", "Elevated FGC")) %>%
  mutate(across(c(1,2), \(x) round(x, digits = 4)))

tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png("PhysoSigTable.png", 
    height = 190*nrow(PhyloSig_table), 
    width = 700*ncol(PhyloSig_table),
    res = 300)
grid.newpage()
grid.table(PhyloSig_table, theme = tt1)
grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()

