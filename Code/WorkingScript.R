
# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

# Baseline vs. MSMR model ----------------------------------------------------

#see DataAndTreeFunction
SetDataAndTree("BasalFGC", "MSMR")

name.check(BasalFGCMSMR_Tree, BasalFGCMSMR_data)

#Build gls model 
BasalFGCMSMR_PGLS <- gls(log(BasalFGC) ~ log(MSMR), 
                       data = BasalFGCMSMR_data, 
                       correlation = corPagel(value = 0.1, phy = BasalFGCMSMR_Tree, form = ~Species)) 

#Store the PGLS model
save(BasalFGCMSMR_PGLS, file = paste(directory,"BasalFGCMSMR_PGLS.RData", sep = ""))

#Specify reduced model
BasalFGCMSMR_Reduced <- lm(log(BasalFGC) ~ 1, 
                         data = BasalFGCMSMR_data) 

save(BasalFGCMSMR_Reduced, file = paste(directory,"BasalFGCMSMR_Reduced.RData", sep = ""))

#Build ordinary linear model
BasalFGCMSMR_Ordinary <- lm(log(BasalFGC) ~ log(MSMR),
                          data=BasalFGCMSMR_data)

BasalFGCMSMR_Plot <-
  ggplot(data = BasalFGCMSMR_data,
         aes(x = log(MSMR), y = log(BasalFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(BasalFGCMSMR_PGLS))[1,1], 
              slope = coefficients(summary(BasalFGCMSMR_PGLS))[2,1], 
              ) +
  theme_classic() +
  labs(x = "MSMR (ln(mW/g))",
       y = "Baseline FGC (ln(ng/g))") +
  annotate("text",  x = 2, y = 2.5,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(BasalFGCMSMR_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(BasalFGCMSMR_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2_lik(BasalFGCMSMR_PGLS, BasalFGCMSMR_Reduced)), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(-1.5, 3)) +
  scale_y_continuous(limits = c(2, 8.5))

BasalFGCMSMR_Plot
save(BasalFGCMSMR_Plot, file = paste(directory,"BasalFGCMSMR_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory,"Figures/BasalFGCMSMR_Plot.png",sep = ""),
       width = 5,
       height = 4) #save a picture

# Baseline vs. Mass model ----------------------------------------------------

#see DataAndTreeFunction
SetDataAndTree("BasalFGC", "Mass")

name.check(BasalFGCMass_Tree, BasalFGCMass_data)

#Build gls model 
BasalFGCMass_PGLS <- gls(log(BasalFGC) ~ log(Mass), 
                       data = BasalFGCMass_data, 
                       correlation = corPagel(value = 0.1, phy = BasalFGCMass_Tree, form = ~Species))

#Store the PGLS model
save(BasalFGCMass_PGLS, file = paste(directory,"BasalFGCMass_PGLS.RData", sep = ""))


#Specify reduced model
BasalFGCMass_Reduced <- lm(log(BasalFGC) ~ 1, 
                         data = BasalFGCMass_data)

save(BasalFGCMass_Reduced, file = paste(directory,"BasalFGCMass_Reduced.RData", sep = ""))

#Build ordinary linear model
BasalFGCMass_Ordinary <- lm(log(BasalFGC) ~ log(Mass),
                             data=BasalFGCMass_data)

BasalFGCMass_Plot <-
  ggplot(data = BasalFGCMass_data,
         aes(x = log(Mass), y = log(BasalFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(BasalFGCMass_PGLS))[1,1], 
              slope = coefficients(summary(BasalFGCMass_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Body Mass (ln(g))",
       y = "Baseline FGC (ln(ng/g))") +
  annotate("text",  x = 5, y = 2,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(BasalFGCMass_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(BasalFGCMass_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2_lik(BasalFGCMass_PGLS, BasalFGCMass_Reduced)), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(2, 16)) +
  scale_y_continuous(limits = c(1, 8.5))

BasalFGCMass_Plot
save(BasalFGCMass_Plot, file = paste(directory, "BasalFGCMass_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/BasalFGCMass_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Elevated vs. Baseline FGC --------------------------------------------------

#see DataAndTreeFunction
SetDataAndTree("ElevFGC", "BasalFGC")

name.check(ElevFGCBasalFGC_Tree, ElevFGCBasalFGC_data)

#Build gls model 
ElevFGCBasalFGC_PGLS <- gls(log(ElevFGC) ~ log(BasalFGC),
                         data=ElevFGCBasalFGC_data, 
                         correlation = corPagel(value = 0.1, phy = ElevFGCBasalFGC_Tree, form = ~Species))

#Store the PGLS model
save(ElevFGCBasalFGC_PGLS, file = paste(directory, "ElevFGCBasalFGC_PGLS.RData", sep = ""))

#Specify reduced model
ElevFGCBasalFGC_Reduced <- lm(log(ElevFGC) ~ 1, 
                           data=ElevFGCBasalFGC_data)

save(ElevFGCBasalFGC_Reduced, file = paste(directory, "ElevFGCBasalFGC_Reduced.RData", sep = ""))

#Build ordinary linear model
ElevFGCBasalFGC_Ordinary <- lm(log(ElevFGC) ~ log(BasalFGC),
                                  data=ElevFGCBasalFGC_data)

ElevFGCBasalFGC_Plot <- 
  ggplot(data = ElevFGCBasalFGC_data,
         aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(ElevFGCBasalFGC_PGLS))[1,1], 
              slope = coefficients(summary(ElevFGCBasalFGC_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Baseline FGC (ln(ng/g))",
       y = "Elevated FGC (ln(ng/g))") +
  annotate("text",  x = 6, y = 3,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(ElevFGCBasalFGC_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(ElevFGCBasalFGC_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2_lik(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Reduced)), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(1, 9)) +
  scale_y_continuous(limits = c(2, 10))

ElevFGCBasalFGC_Plot
save(ElevFGCBasalFGC_Plot, file = paste(directory, "ElevFGCBasalFGC_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/ElevFGCBasalFGC_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Lifespan vs. Baseline model ------------------------------------------------

#see DataAndTreeFunction
SetDataAndTree("Lifespan", "BasalFGC")

name.check(LifespanBasalFGC_Tree, LifespanBasalFGC_data)

#Build gls model 
LifespanBasalFGC_PGLS <- gls(log(Lifespan) ~ log(BasalFGC), 
                           data = LifespanBasalFGC_data, 
                           correlation = corPagel(value = 0.1, phy = LifespanBasalFGC_Tree, form = ~Species))

#Store the PGLS model
save(LifespanBasalFGC_PGLS, file = paste(directory, "LifespanBasalFGC_PGLS.RData", sep = ""))

#Specify reduced model
LifespanBasalFGC_Reduced <- lm(log(Lifespan) ~ 1, 
                             data = LifespanBasalFGC_data) 

save(LifespanBasalFGC_Reduced, file = paste(directory, "LifespanBasalFGC_Reduced.RData", sep = ""))

#Build ordinary linear model
LifespanBasalFGC_Ordinary <- lm(log(Lifespan) ~ log(BasalFGC),
                              data=LifespanBasalFGC_data)

LifespanBasalFGC_Plot <-
  ggplot(data = LifespanBasalFGC_data,
         aes(x = log(BasalFGC), y = log(Lifespan))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(LifespanBasalFGC_PGLS))[1,1], 
              slope = coefficients(summary(LifespanBasalFGC_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Baseline FGC (ln(ng/g))",
       y = "Lifespan ln((years))") +
  annotate("text",  x = 3, y = 1.5,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(LifespanBasalFGC_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(LifespanBasalFGC_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2_lik(LifespanBasalFGC_PGLS, LifespanBasalFGC_Reduced)), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(1, 9)) +
  scale_y_continuous(limits = c(1, 5))

LifespanBasalFGC_Plot
save(LifespanBasalFGC_Plot, file = paste(directory, "LifespanBasalFGC_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/LifespanBasalFGC_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Lifespan vs. MSMR model -------------------------------------------------

SetDataAndTree("Lifespan", "MSMR")

name.check(LifespanMSMR_Tree, LifespanMSMR_data)

#Build gls model 
LifespanMSMR_PGLS <- gls(log(Lifespan) ~ log(MSMR), 
                           data = LifespanMSMR_data, 
                           correlation = corPagel(value = 0.1, phy = LifespanMSMR_Tree, form = ~Species))

#Store the PGLS model
save(LifespanMSMR_PGLS, file = paste(directory, "LifespanMSMR_PGLS.RData", sep = ""))

#Specify reduced model
LifespanMSMR_Reduced <- lm(log(Lifespan) ~ 1, 
                             data = LifespanMSMR_data) 

save(LifespanMSMR_Reduced, file = paste(directory, "LifespanMSMR_Reduced.RData", sep = ""))

#Build ordinary linear model 
LifespanMSMR_Ordinary <- lm(log(Lifespan) ~ log(MSMR),
                              data=LifespanMSMR_data)

LifespanMSMR_Plot <-
  ggplot(data = LifespanMSMR_data,
         aes(x = log(MSMR), y = log(Lifespan))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(LifespanMSMR_PGLS))[1,1], 
              slope = coefficients(summary(LifespanMSMR_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "MSMR (ln(mW/g))",
       y = "Lifespan ln((years))") +
  annotate("text",  x = 1, y = 1.2,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(LifespanMSMR_PGLS))[1,1], 2))
                                    ~x^.(round(coefficients(summary(LifespanMSMR_PGLS))[2,1], 2)),
                                    ~R^2 ==~ .(round(as.numeric(R2_lik(LifespanMSMR_PGLS, LifespanMSMR_Reduced)), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_y_continuous(limits = c(1, 5))

LifespanMSMR_Plot
save(LifespanMSMR_Plot, file = paste(directory, "LifespanMSMR_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/LifespanMSMR_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Stats Table ------------------------------------------------------------

#PGLS table
StatsTab_PGLS <- rbind(cbind(coefficients(summary(BasalFGCMSMR_PGLS)), intervals(BasalFGCMSMR_PGLS)[["coef"]]),
                       cbind(coefficients(summary(BasalFGCMass_PGLS)), intervals(BasalFGCMass_PGLS)[["coef"]]),
                       cbind(coefficients(summary(ElevFGCBasalFGC_PGLS)), intervals(ElevFGCBasalFGC_PGLS)[["coef"]]),
                       cbind(coefficients(summary(LifespanBasalFGC_PGLS)), intervals(LifespanBasalFGC_PGLS)[["coef"]])) %>%
  as.data.frame(.) %>%
  slice(-c(1,3,5,7)) %>%  #cut out all the rows of intercept stats
  mutate(across(c(1,5,7), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(Value = str_c(Value, " (", `lower`, ", ", `upper`, ")")) %>% #paste the value and the CI together
  select(., -c("est.", "Std.Error", "t-value", "lower", "upper")) %>% #remove the columns we don't need
  cbind(., rbind(intervals(BasalFGCMSMR_PGLS)[["coef"]][1,],
                 intervals(BasalFGCMass_PGLS)[["coef"]][1,],
                 intervals(ElevFGCBasalFGC_PGLS)[["coef"]][1,],
                 intervals(LifespanBasalFGC_PGLS)[["coef"]][1,]), #add back in a column for the intercept
        rbind(R2_lik(BasalFGCMSMR_PGLS, BasalFGCMSMR_Reduced), 
              R2_lik(BasalFGCMass_PGLS, BasalFGCMass_Reduced), 
              R2_lik(ElevFGCBasalFGC_PGLS, ElevFGCBasalFGC_Reduced), 
              R2_lik(LifespanBasalFGC_PGLS, LifespanBasalFGC_Reduced)),
        rbind(BasalFGCMass_PGLS[["modelStruct"]][["corStruct"]],
              BasalFGCMSMR_PGLS[["modelStruct"]][["corStruct"]],
              ElevFGCBasalFGC_PGLS[["modelStruct"]][["corStruct"]],
              LifespanBasalFGC_PGLS[["modelStruct"]][["corStruct"]])) %>%
  mutate(across(c(3,4,5,7), \(x) round(x, digits = 2))) %>% 
  mutate(est. = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>%
  select(., -c("lower", "upper")) %>%
  `colnames<-`(c("Slope (95% CI)", "p value (slope)", "Intercept  (95% CI)", "Likelihood R2", "Lambda")) %>%
  `rownames<-`(c("Baseline FGC ~ MSMR",
                 "Baseline FGC ~ Body Mass", 
                 "Elevated FGC ~ Baseline FGC",
                 "Lifespan ~ Baseline FGC")) %>%
  mutate(across(c(2,4), \(x) round(x, digits = 3))) %>%
  mutate(`p value (slope)` = ifelse(`p value (slope)` < 0.001, "< 0.001", `p value (slope)`)) #change very small p values to < 0.001

tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))

write.csv(StatsTab_PGLS, file = paste(directory, "Figures/StatsTab_PGLS.csv", sep = ""), row.names = TRUE)

#export stats table 
png(paste(directory, "Figures/StatsTab_PGLS.png", sep = ""),
    height = 190*nrow(StatsTab_PGLS), 
    width = 800*ncol(StatsTab_PGLS),
    res = 300)
grid.newpage()
grid.table(StatsTab_PGLS, theme = tt1)
grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()





