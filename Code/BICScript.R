
################################################################################

# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

################################################################################


# Basal FGC vs. MSMR ------------------------------------------------------

#model without additional effects should already be in environment as xx_PGLS

BasalFGCMSMR_Method <- gls(log(BasalFGC) ~ log(MSMR) + Method, 
                           data = BasalFGCMSMR_data, 
                           correlation = corPagel(value = 1, phy = BasalFGCMSMR_Tree, form = ~Species))

BasalFGCMSMR_Captive <- gls(log(BasalFGC) ~ log(MSMR) + Captive, 
                            data = BasalFGCMSMR_data, 
                            correlation = corPagel(value = 1, phy = BasalFGCMSMR_Tree, form = ~Species))

BasalFGCMSMR_Sex <- gls(log(BasalFGC) ~ log(MSMR) + Sex,
                        data = BasalFGCMSMR_data, 
                        correlation = corPagel(value = 1, phy = BasalFGCMSMR_Tree, form = ~Species))

BasalFGCMSMR_BIC <- BIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Method, BasalFGCMSMR_Captive, BasalFGCMSMR_Sex) 

# #this includes the 'if' functions because sometimes there is only one method
# #Because there is errors when there is only one method
# if (length(unique(BasalFGCMSMR_data$Method)) > 1) {
#   BasalFGCMSMR_Method <- gls(log(BasalFGC) ~ log(MSMR) + Method, 
#                            data = BasalFGCMSMR_data, 
#                            correlation = corPagel(value = 1, phy = BasalFGCMSMR_Tree, form = ~Species))
#   
#   BasalFGCMSMR_BIC <- BIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Method, BasalFGCMSMR_Captive, BasalFGCMSMR_Sex) 
# } else {
#   BasalFGCMSMR_BIC <- BIC(BasalFGCMSMR_PGLS, BasalFGCMSMR_Captive, BasalFGCMSMR_Sex) 
# }

# Basal FGC vs. Mass ------------------------------------------------------

BasalFGCMass_Method <- gls(log(BasalFGC) ~ log(Mass) + Method, 
                           data = BasalFGCMass_data, 
                           correlation = corPagel(value = 1, phy = BasalFGCMass_Tree, form = ~Species))

BasalFGCMass_Captive <- gls(log(BasalFGC) ~ log(Mass) + Captive, 
                             data = BasalFGCMass_data, 
                             correlation = corPagel(value = 1, phy = BasalFGCMass_Tree, form = ~Species))

BasalFGCMass_Sex <- gls(log(BasalFGC) ~ log(Mass) + Sex, 
                        data = BasalFGCMass_data, 
                        correlation = corPagel(value = 1, phy = BasalFGCMass_Tree, form = ~Species))

BasalFGCMass_BIC <- BIC(BasalFGCMass_PGLS, BasalFGCMass_Method, BasalFGCMass_Captive, BasalFGCMass_Sex)

# #Because there is errors when there is only one method
# if (length(unique(BasalFGCMass_data$Method)) >1 ) {
#   BasalFGCMass_Method <- gls(log(BasalFGC) ~ log(Mass) + Method, 
#                            data = BasalFGCMass_data, 
#                            correlation = corPagel(value = 1, phy = BasalFGCMass_Tree, form = ~Species)) 
#   BasalFGCMass_BIC <- BIC(BasalFGCMass_PGLS, BasalFGCMass_Method, BasalFGCMass_Captive, BasalFGCMass_Sex) 
# } else {
#   BasalFGCMass_BIC <- BIC(BasalFGCMass_PGLS, BasalFGCMass_Captive, BasalFGCMass_Sex) 
# }

# Elev. vs. Basal ---------------------------------------------------------

ElevFGCBasalFGC_Method <- gls(log(ElevFGC) ~ log(BasalFGC) + Method, 
                              data=ElevFGCBasalFGC_data, 
                              correlation = corPagel(value = 1,phy = ElevFGCBasalFGC_Tree, form = ~Species))

ElevFGCBasalFGC_Captive <- gls(log(ElevFGC) ~ log(BasalFGC) + Captive,
                               data=ElevFGCBasalFGC_data, 
                               correlation = corPagel(value = 1, phy = ElevFGCBasalFGC_Tree, form = ~Species))

ElevFGCBasalFGC_Sex <- gls(log(ElevFGC) ~ log(BasalFGC) + Sex,
                           data=ElevFGCBasalFGC_data, 
                           correlation = corPagel(value = 1, phy = ElevFGCBasalFGC_Tree, form = ~Species))

ElevFGCBasalFGC_Stressor <- gls(log(ElevFGC) ~ log(BasalFGC) + Stressor,
                                data=ElevFGCBasalFGC_data, 
                                correlation = corPagel(value = 1, phy = ElevFGCBasalFGC_Tree, form = ~Species))

ElevFGCBasalFGC_BIC <- BIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Method, ElevFGCBasalFGC_Captive, ElevFGCBasalFGC_Sex, ElevFGCBasalFGC_Stressor)

# #Because there is errors when there is only one method
# if (length(unique(ElevFGCBasalFGC_data$Method)) > 1) {
#   ElevFGCBasalFGC_Method <- gls(log(ElevFGC) ~ log(BasalFGC) + Method, 
#                            data=ElevFGCBasalFGC_data, 
#                            correlation = corPagel(value = 1,phy = ElevFGCBasalFGC_Tree, form = ~Species))
#   
#   ElevFGCBasalFGC_BIC <- BIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor, ElevFGCBasalFGC_Method, ElevFGCBasalFGC_Captive, ElevFGCBasalFGC_Sex) 
# } else {
#   ElevFGCBasalFGC_BIC <- BIC(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Stressor, ElevFGCBasalFGC_Captive, ElevFGCBasalFGC_Sex) 
# }

# Lifespan vs. Basal ------------------------------------------------------

# LifespanBasalFGC_Stressor <- gls(log(Lifespan) ~ log(BasalFGC) + Stressor, 
#                            data = LifespanBasalFGC_data, 
#                            correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasalFGC_Tree, form = ~Species))
# 
# #Because there is errors when there is only one method
# if (length(unique(LifespanBasalFGC_data$Method)) > 1) {
#   LifespanBasalFGC_Method <- gls(log(Lifespan) ~ log(BasalFGC) + Method, 
#                            data = LifespanBasalFGC_data, 
#                            correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasalFGC_Tree, form = ~Species))
#   
#   LifespanBasalFGC_AIC <- AIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor, LifespanBasalFGC_Method) #compare models
#   LifespanBasalFGC_BIC <- BIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor, LifespanBasalFGC_Method) 
# } else {
#   LifespanBasalFGC_AIC <- AIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor) #compare models
#   LifespanBasalFGC_BIC <- BIC(LifespanBasalFGC_PGLS, LifespanBasalFGC_Stressor) 
# }


BIC_table <- as.data.frame(rbind(BasalFGCMSMR_BIC, BasalFGCMass_BIC, ElevFGCBasalFGC_BIC)) %>%
  mutate(across(2, \(x) round(x, digits = 2))) %>%
  `colnames<-`(c("df",  "BIC"))

save(BIC_table, file = paste(directory, "BIC_table.RData", sep = ""))

#export stats table 
png(paste(directory, "Figures/BICTable.png", sep = ""),
    height = 190*nrow(BIC_table), 
    width = 1000*ncol(BIC_table),
    res = 300)
grid.newpage()
grid.table(BIC_table, theme = tt1)
grid.text(Label, x = 0.4, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()




