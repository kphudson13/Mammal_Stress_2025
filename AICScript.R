

BasFGCMSMR_Stressor <- gls(log(BasalFGC) ~ log(MSMR) + Stressor, 
                       data = BasFGCMSMR_data, 
                       correlation = corPagel(value = BasFGC_signal1$lambda, phy = BasFGCMSMR_Tree, form = ~Species))

BasFGCMSMR_Method <- gls(log(BasalFGC) ~ log(MSMR) + Method, 
                           data = BasFGCMSMR_data, 
                           correlation = corPagel(value = BasFGC_signal1$lambda, phy = BasFGCMSMR_Tree, form = ~Species))

BasFGCMSMR_AIC <- AIC(BasFGCMSMR_PGLS, BasFGCMSMR_Stressor, BasFGCMSMR_Method) #compare models
BasFGCMSMR_BIC <- BIC(BasFGCMSMR_PGLS, BasFGCMSMR_Stressor, BasFGCMSMR_Method) 

BasFGCMass_Stressor <- gls(log(BasalFGC) ~ log(BodyMassAnAge) + Stressor, 
                           data = BasFGCMass_data, 
                           correlation = corPagel(value = BasFGC_signal2$lambda, phy = BasFGCMass_Tree, form = ~Species)                       ) 
BasFGCMass_Method <- gls(log(BasalFGC) ~ log(BodyMassAnAge) + Method, 
                         data = BasFGCMass_data, 
                         correlation = corPagel(value = BasFGC_signal2$lambda, phy = BasFGCMass_Tree, form = ~Species)) 

BasFGCMass_AIC <- AIC(BasFGCMass_PGLS, BasFGCMass_Stressor, BasFGCMass_Method) #compare models
BasFGCMass_BIC <- BIC(BasFGCMass_PGLS, BasFGCMass_Stressor, BasFGCMass_Method) 


ElvFGCBasFGC_Stressor <- gls(log(ElevFGC) ~ log(BasalFGC) + Stressor,
                         data=ElvFGCBasFGC_data, 
                         correlation = corPagel(value = ElvFGC_signal$lambda, phy = ElvFGCBasFGC_Tree, form = ~Species))
ElvFGCBasFGC_Method <- gls(log(ElevFGC) ~ log(BasalFGC) + Method, 
                         data=ElvFGCBasFGC_data, 
                         correlation = corPagel(value = ElvFGC_signal$lambda,phy = ElvFGCBasFGC_Tree, form = ~Species))

ElvFGCBasFGC_AIC <- AIC(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Stressor, ElvFGCBasFGC_Method) #compare models
ElvFGCBasFGC_BIC <- BIC(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Stressor, ElvFGCBasFGC_Method) 

LifespanBasFGC_Stressor <- gls(log(MaxLifespan) ~ log(BasalFGC) + Stressor, 
                           data = LifespanBasFGC_data, 
                           correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasFGC_Tree, form = ~Species))
LifespanBasFGC_Method <- gls(log(MaxLifespan) ~ log(BasalFGC) + Method, 
                           data = LifespanBasFGC_data, 
                           correlation = corPagel(value = Lifespan_signal$lambda,phy = LifespanBasFGC_Tree, form = ~Species))

LifespanBasFGC_AIC <- AIC(LifespanBasFGC_PGLS, LifespanBasFGC_Stressor, LifespanBasFGC_Method) #compare models
LifespanBasFGC_BIC <- BIC(LifespanBasFGC_PGLS, LifespanBasFGC_Stressor, LifespanBasFGC_Method)

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




