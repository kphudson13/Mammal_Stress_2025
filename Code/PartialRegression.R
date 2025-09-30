
################################################################################

# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

################################################################################



LifespanMSMR_Resid <- as.data.frame(lm(log(Lifespan) ~ log(MSMR), 
   data = LifespanMSMR_data)$residuals) %>% 
  `colnames<-`("Residuals") %>% 
  merge(LifespanMSMR_data, ., by = "row.names")


LifespanFGC_Resid_OLS <- lm(Residuals ~ log(BasalFGC), 
  data = LifespanMSMR_Resid)

summary(LifespanFGC_Resid_OLS)


