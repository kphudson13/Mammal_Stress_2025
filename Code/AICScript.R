
# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

#this includes the if functions because sometimes there is only one method

BasalFGCMSMR_Stressor <- gls(log(BasalFGC) ~ log(MSMR) + Stressor, 
                       data = BasalFGCMSMR_data, 
                       correlation = corPagel(value = BasalFGC_signal1$lambda, phy = BasalFGCMSMR_Tree, form = ~Species))

#Because there is errors when there is only one method
if (length(unique(BasalFGCMSMR_data$Method)) > 1) {
  BasalFGCMSMR_Method <- gls(log(BasalFGC) ~ log(MSMR) + Method, 
                           data = BasalFGCMSMR_data, 
                           correlation = corPagel(value = BasalFGC_signal1$lambda, phy = BasalFGCMSMR_Tree, form = ~Species))
  BasalFGCMSMR_AIC <- AIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Stressor, BasalFGCMSMR_Method) #compare models
  BasalFGCMSMR_BIC <- BIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Stressor, BasalFGCMSMR_Method) 
} else {
  BasalFGCMSMR_AIC <- AIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Stressor) #compare models
  BasalFGCMSMR_BIC <- BIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Stressor) 
}

BasalFGCMass_Stressor <- gls(log(BasalFGC) ~ log(Mass) + Stressor, 
                           data = BasalFGCMass_data, 
                           correlation = corPagel(value = BasalFGC_signal2$lambda, phy = BasalFGCMass_Tree, form = ~Species))

#Because there is errors when there is only one method
if (length(unique(BasalFGCMass_data$Method)) >1 ) {
  BasalFGCMass_Method <- gls(log(BasalFGC) ~ log(Mass) + Method, 
                           data = BasalFGCMass_data, 
                           correlation = corPagel(value = BasalFGC_signal2$lambda, phy = BasalFGCMass_Tree, form = ~Species)) 
  
  BasalFGCMass_AIC <- AIC(BasalFGCMass_PGLS, BasalFGCMass_Stressor, BasalFGCMass_Method) #compare models
  BasalFGCMass_BIC <- BIC(BasalFGCMass_PGLS, BasalFGCMass_Stressor, BasalFGCMass_Method) 
} else {
  BasalFGCMass_AIC <- AIC(BasalFGCMass_PGLS, BasalFGCMass_Stressor) #compare models
  BasalFGCMass_BIC <- BIC(BasalFGCMass_PGLS, BasalFGCMass_Stressor) 
}


ElevFGCBasalFGC_Stressor <- gls(log(ElevFGC) ~ log(BasalFGC) + Stressor,
                         data=ElevFGCBasalFGC_data, 
                         correlation = corPagel(value = ElevFGC_signal$lambda, phy = ElevFGCBasalFGC_Tree, form = ~Species))

#Because there is errors when there is only one method
if (length(unique(ElevFGCBasalFGC_data$Method)) > 1) {
  ElevFGCBasalFGC_Method <- gls(log(ElevFGC) ~ log(BasalFGC) + Method, 
                           data=ElevFGCBasalFGC_data, 
                           correlation = corPagel(value = ElevFGC_signal$lambda,phy = ElevFGCBasalFGC_Tree, form = ~Species))
  
  ElevFGCBasalFGC_AIC <- AIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor, ElevFGCBasalFGC_Method) #compare models
  ElevFGCBasalFGC_BIC <- BIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor, ElevFGCBasalFGC_Method) 
} else {
  ElevFGCBasalFGC_AIC <- AIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor) #compare models
  ElevFGCBasalFGC_BIC <- BIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor) 
}


LifespanBasalFGC_Stressor <- gls(log(Lifespan) ~ log(BasalFGC) + Stressor, 
                           data = LifespanBasalFGC_data, 
                           correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasalFGC_Tree, form = ~Species))

#Because there is errors when there is only one method
if (length(unique(LifespanBasalFGC_data$Method)) > 1) {
  LifespanBasalFGC_Method <- gls(log(Lifespan) ~ log(BasalFGC) + Method, 
                           data = LifespanBasalFGC_data, 
                           correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasalFGC_Tree, form = ~Species))
  
  LifespanBasalFGC_AIC <- AIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor, LifespanBasalFGC_Method) #compare models
  LifespanBasalFGC_BIC <- BIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor, LifespanBasalFGC_Method) 
} else {
  LifespanBasalFGC_AIC <- AIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor) #compare models
  LifespanBasalFGC_BIC <- BIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor) 
}



AIC_table <- as.data.frame(rbind(BasalFGCMSMR_AIC, BasalFGCMass_AIC, ElevFGCBasalFGC_AIC, LifespanBasalFGC_AIC)) %>%
  cbind(., rbind(BasalFGCMSMR_BIC, BasalFGCMass_BIC, ElevFGCBasalFGC_BIC, LifespanBasalFGC_BIC)[ ,2]) %>%
  mutate(across(c(2,3), \(x) round(x, digits = 2))) %>%
  `colnames<-`(c("df", "AIC", "BIC"))

save(AIC_table, file = paste(directory, "AIC_table.RData", sep = ""))

#export stats table 
png(paste(directory, "Figures/AICTable.png", sep = ""),
    height = 190*nrow(AIC_table), 
    width = 1000*ncol(AIC_table),
    res = 300)
grid.newpage()
grid.table(AIC_table, theme = tt1)
grid.text(Label, x = 0.4, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()




