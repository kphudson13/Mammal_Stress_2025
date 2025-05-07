



BasFGCMass_Stressor <- gls(log(BasalFGC) ~ log(BodyMassAnAge) + Stressor, 
                       data = BasFGCMass_data, 
                       correlation = corBrownian(phy = BasFGCMass_Tree, form = ~Species), 
                       method = "ML") 
summary(BasFGCMass_Stressor)

BasFGCMass_AIC <- AIC(BasFGCMass_PGLS, BasFGCMass_Stressor) #compare models
BasFGCMass_BIC <- BIC(BasFGCMass_PGLS, BasFGCMass_Stressor) 

BasFGCMSMR_Stressor <- gls(log(BasalFGC) ~ log(MSMR) + Stressor, 
                       data = BasFGCMSMR_data, 
                       correlation = corBrownian(phy = BasFGCMSMR_Tree, form = ~Species), 
                       method = "ML")
summary(BasFGCMSMR_Stressor)

BasFGCMSMR_AIC <- AIC(BasFGCMSMR_PGLS, BasFGCMSMR_Stressor) 
BasFGCMSMR_BIC <- BIC(BasFGCMSMR_PGLS, BasFGCMSMR_Stressor) 

ElvFGCBasFGC_Stressor <- gls(log(ElevFGC) ~ log(BasalFGC) + Stressor,
                         data=ElvFGCBasFGC_data, 
                         correlation = corBrownian(phy = ElvFGCBasFGC_Tree, form = ~Species), 
                         method="ML")
summary(ElvFGCBasFGC_Stressor)

ElvFGCBasFGC_AIC <- AIC(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Stressor) 
ElvFGCBasFGC_BIC <- BIC(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Stressor) 

LifespanBasFGC_Stressor <- gls(log(MaxLifespan) ~ log(BasalFGC) + Stressor, 
                           data = LifespanBasFGC_data, 
                           correlation = corBrownian(phy = LifespanBasFGC_Tree, form = ~Species), 
                           method = "ML")
summary(LifespanBasFGC_Stressor)

LifespanBasFGC_AIC <- AIC(LifespanBasFGC_PGLS, LifespanBasFGC_Stressor)
LifespanBasFGC_BIC <- BIC(LifespanBasFGC_PGLS, LifespanBasFGC_Stressor)

AIC_table <- as.data.frame(rbind(BasFGCMass_AIC, BasFGCMSMR_AIC, ElvFGCBasFGC_AIC, LifespanBasFGC_AIC)) %>%
  cbind(., rbind(BasFGCMass_BIC, BasFGCMSMR_BIC, ElvFGCBasFGC_BIC, LifespanBasFGC_BIC)[ ,2]) %>%
  mutate(across(c(2,3), \(x) round(x, digits = 2))) %>%
  `colnames<-`(c("df", "AIC", "BIC"))

#export stats table 
png("AICTable.png", 
    height = 190*nrow(AIC_table), 
    width = 1000*ncol(AIC_table),
    res = 300)
grid.newpage()
grid.table(AIC_table, theme = tt1)
grid.text(Label, x = 0.4, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()




