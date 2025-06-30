
# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

# Baseline vs. MSMR model ----------------------------------------------------

#Filter out blank rows of Baseline FGC
BasFGCMSMR_data <- StressData %>% drop_na(c(BasalFGC, MSMR))

#Setting row names to map the tree to
rownames(BasFGCMSMR_data) = BasFGCMSMR_data$Species

#Remove tree species not in the Baseline FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$MSMR)) > 0) {
  BasFGCMSMR_Tree <- drop.tip(tree, name.check(tree, BasFGCMSMR_data)$tree_not_data)
} else {
  BasFGCMSMR_Tree <- tree
}

name.check(BasFGCMSMR_Tree, BasFGCMSMR_data)

BasFGC_signal1 <-
  phylosig(tree = BasFGCMSMR_Tree,
           x = setNames(BasFGCMSMR_data$BasalFGC, BasFGCMSMR_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

#Build gls model 
BasFGCMSMR_PGLS <- gls(log(BasalFGC) ~ log(MSMR), 
                       data = BasFGCMSMR_data, 
                       correlation = corPagel(value = BasFGC_signal1$lambda, phy = BasFGCMSMR_Tree, form = ~Species)) 

#limit lambda to >0 to avoid errors 
if (BasFGCMSMR_PGLS[["modelStruct"]][["corStruct"]][1] < 0) {
  BasFGCMSMR_PGLS[["modelStruct"]][["corStruct"]][1] <- 0
} 

#Store the PGLS model
save(BasFGCMSMR_PGLS, file = paste(directory,"BasFGCMSMR_PGLS.RData", sep = ""))

#Specify reduced model
BasFGCMSMR_Reduced <- lm(log(BasalFGC) ~ 1, 
                         data = BasFGCMSMR_data) 

save(BasFGCMSMR_Reduced, file = paste(directory,"BasFGCMSMR_Reduced.RData", sep = ""))

BasFGCMSMR_RSq_PGLS <- R2(BasFGCMSMR_PGLS, BasFGCMSMR_Reduced)

# #Build ordinary linear model 
# BasFGCMSMR_Ordinary <- lm(log(BasalFGC) ~ log(MSMR),
#                           data=BasFGCMSMR_data)
# 
# BasFGCMSMR_Summ_Ordinary <- summary(BasFGCMSMR_Ordinary)

BasFGCMSMR_Plot <-
  ggplot(data = BasFGCMSMR_data,
         aes(x = log(MSMR), y = log(BasalFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(BasFGCMSMR_PGLS))[1,1], 
              slope = coefficients(summary(BasFGCMSMR_PGLS))[2,1], 
              ) +
  theme_classic() +
  labs(x = "MSMR (ln(mW/g))",
       y = "Baseline FGC (ln(ng/g))") +
  annotate("text",  x = 2, y = 2.5,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(BasFGCMSMR_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(BasFGCMSMR_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2(BasFGCMSMR_PGLS, BasFGCMSMR_Reduced)[1]), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(-1.5, 3)) +
  scale_y_continuous(limits = c(2, 8.5))

BasFGCMSMR_Plot
save(BasFGCMSMR_Plot, file = paste(directory,"BasFGCMSMR_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory,"Figures/BasFGCMSMR_Plot.png",sep = ""),
       width = 5,
       height = 4) #save a picture

# Baseline vs. Mass model ----------------------------------------------------

#Filter out blank rows of Baseline FGC
BasFGCMass_data <- StressData %>% drop_na(BasalFGC)

#Setting row names to map the tree to
rownames(BasFGCMass_data) = BasFGCMass_data$Species

#Remove tree species not in the Baseline FGC data
if (sum(is.na(StressData$BasalFGC)) > 0) {
  BasFGCMass_Tree <- drop.tip(tree, name.check(tree, BasFGCMass_data)$tree_not_data)
} else {
  BasFGCMass_Tree <- tree
}

name.check(BasFGCMass_Tree, BasFGCMass_data)

BasFGC_signal2 <-
  phylosig(tree = BasFGCMass_Tree,
           x = setNames(BasFGCMass_data$BasalFGC, BasFGCMass_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

#Build gls model 
BasFGCMass_PGLS <- gls(log(BasalFGC) ~ log(BodyMassAnAge), 
                       data = BasFGCMass_data, 
                       correlation = corPagel(value = BasFGC_signal2$lambda, phy = BasFGCMass_Tree, form = ~Species))

#limit lambda to >0 to avoid errors 
if (BasFGCMass_PGLS[["modelStruct"]][["corStruct"]][1] < 0) {
  BasFGCMass_PGLS[["modelStruct"]][["corStruct"]][1] <- 0
} 

#Store the PGLS model
save(BasFGCMass_PGLS, file = paste(directory,"BasFGCMass_PGLS.RData", sep = ""))


#Specify reduced model
BasFGCMass_Reduced <- lm(log(BasalFGC) ~ 1, 
                         data = BasFGCMass_data)

save(BasFGCMass_Reduced, file = paste(directory,"BasFGCMass_Reduced.RData", sep = ""))

#Build ordinary linear model 
# BasFGCMass_Ordinary <- lm(log(BasalFGC) ~ log(BodyMassAnAge),
#                              data=BasFGCMass_data)
# 
# BasFGCMass_Summ_Ordinary <- summary(BasFGCMass_Ordinary)

BasFGCMass_Plot <-
  ggplot(data = BasFGCMass_data,
         aes(x = log(BodyMassAnAge), y = log(BasalFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(BasFGCMass_PGLS))[1,1], 
              slope = coefficients(summary(BasFGCMass_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Body Mass (ln(g))",
       y = "Baseline FGC (ln(ng/g))") +
  annotate("text",  x = 5, y = 2,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(BasFGCMass_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(BasFGCMass_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2(BasFGCMass_PGLS, BasFGCMass_Reduced)[1]), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(2, 16)) +
  scale_y_continuous(limits = c(1, 8.5))

BasFGCMass_Plot
save(BasFGCMass_Plot, file = paste(directory, "BasFGCMass_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/BasFGCMass_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Elevated vs. Baseline FGC --------------------------------------------------

#Filter out blank rows of Baseline FGC and Elevated FGC
ElvFGCBasFGC_data <- StressData %>% drop_na(c(BasalFGC, ElevFGC))

#Setting row names to map the tree to
rownames(ElvFGCBasFGC_data) = ElvFGCBasFGC_data$Species

#Remove tree species not in the Baseline FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$ElevFGC)) > 0) {
  ElvFGCBasFGC_Tree <- drop.tip(tree, name.check(tree, ElvFGCBasFGC_data)$tree_not_data)
} else {
  ElvFGCBasFGC_Tree <- tree
}

name.check(ElvFGCBasFGC_Tree, ElvFGCBasFGC_data)

ElvFGC_signal <-
  phylosig(tree = ElvFGCBasFGC_Tree,
           x = setNames(ElvFGCBasFGC_data$ElevFGC, ElvFGCBasFGC_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

#Build gls model 
ElvFGCBasFGC_PGLS <- gls(log(ElevFGC) ~ log(BasalFGC),
                         data=ElvFGCBasFGC_data, 
                         correlation = corPagel(value = ElvFGC_signal$lambda, phy = ElvFGCBasFGC_Tree, form = ~Species))

#limit lambda to >0 to avoid errors 
if (ElvFGCBasFGC_PGLS[["modelStruct"]][["corStruct"]][1] < 0) {
  ElvFGCBasFGC_PGLS[["modelStruct"]][["corStruct"]][1] <- 0
} 

#Store the PGLS model
save(ElvFGCBasFGC_PGLS, file = paste(directory, "ElvFGCBasFGC_PGLS.RData", sep = ""))

#Specify reduced model
ElvFGCBasFGC_Reduced <- lm(log(ElevFGC) ~ 1, 
                           data=ElvFGCBasFGC_data)

save(ElvFGCBasFGC_Reduced, file = paste(directory, "ElvFGCBasFGC_Reduced.RData", sep = ""))

#Build ordinary linear model 
# ElvFGCBasFGC_Ordinary <- lm(log(ElevFGC) ~ log(BasalFGC),
#                                   data=ElvFGCBasFGC_data)
# 
# ElvFGCBasFGC_Summ_Ordinary <- summary(ElvFGCBasFGC_Ordinary)

ElvFGCBasFGC_Plot <- 
  ggplot(data = ElvFGCBasFGC_data,
         aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(ElvFGCBasFGC_PGLS))[1,1], 
              slope = coefficients(summary(ElvFGCBasFGC_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Baseline FGC (ln(ng/g))",
       y = "Elevated FGC (ln(ng/g))") +
  annotate("text",  x = 6, y = 3,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(ElvFGCBasFGC_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(ElvFGCBasFGC_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Reduced)[1]), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(1, 9)) +
  scale_y_continuous(limits = c(2, 10))

ElvFGCBasFGC_Plot
save(ElvFGCBasFGC_Plot, file = paste(directory, "ElvFGCBasFGC_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/ElvFGCBasFGC_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Lifespan vs. Baseline model ------------------------------------------------

#Filter out blank rows of Baseline FGC
LifespanBasFGC_data <- StressData %>% drop_na(c(BasalFGC, Lifespan))

#Setting row names to map the tree to
rownames(LifespanBasFGC_data) = LifespanBasFGC_data$Species

#Remove tree species not in the Baseline FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$Lifespan)) > 0) {
  LifespanBasFGC_Tree <- drop.tip(tree, name.check(tree, LifespanBasFGC_data)$tree_not_data)
} else {
  LifespanBasFGC_Tree <- tree
}

name.check(LifespanBasFGC_Tree, LifespanBasFGC_data)

Lifespan_signal <-
  phylosig(tree = LifespanBasFGC_Tree,
           x = setNames(LifespanBasFGC_data$Lifespan, LifespanBasFGC_data$Species),   
           method = "lambda",
           test = TRUE, 
           nsim = 1000)

#Build gls model 
LifespanBasFGC_PGLS <- gls(log(Lifespan) ~ log(BasalFGC), 
                           data = LifespanBasFGC_data, 
                           correlation = corPagel(value = Lifespan_signal$lambda, phy = LifespanBasFGC_Tree, form = ~Species))

#limit lambda to >0 to avoid errors 
if (LifespanBasFGC_PGLS[["modelStruct"]][["corStruct"]][1] < 0) {
  LifespanBasFGC_PGLS[["modelStruct"]][["corStruct"]][1] <- 0
} 

#Store the PGLS model
save(LifespanBasFGC_PGLS, file = paste(directory, "LifespanBasFGC_PGLS.RData", sep = ""))

#Specify reduced model
LifespanBasFGC_Reduced <- lm(log(Lifespan) ~ 1, 
                             data = LifespanBasFGC_data) 

save(LifespanBasFGC_Reduced, file = paste(directory, "LifespanBasFGC_Reduced.RData", sep = ""))

#Build ordinary linear model 
# LifespanBasFGC_Ordinary <- lm(log(Lifespan) ~ log(BasalFGC),
#                               data=LifespanBasFGC_data)
# 
# LifespanBasFGC_Summ_Ordinary <- summary(LifespanBasFGC_Ordinary)

LifespanBasFGC_Plot <-
  ggplot(data = LifespanBasFGC_data,
         aes(x = log(BasalFGC), y = log(Lifespan))) +
  geom_point() +
  geom_abline(intercept = coefficients(summary(LifespanBasFGC_PGLS))[1,1], 
              slope = coefficients(summary(LifespanBasFGC_PGLS))[2,1]) +
  theme_classic() +
  labs(x = "Baseline FGC (ln(ng/g))",
       y = "Lifespan ln((years))") +
  annotate("text",  x = 3, y = 1.5,
           label = list(bquote(atop(y==~ .(round(coefficients(summary(LifespanBasFGC_PGLS))[1,1], 2))
                               ~x^.(round(coefficients(summary(LifespanBasFGC_PGLS))[2,1], 2)),
                               ~R^2 ==~ .(round(as.numeric(R2(LifespanBasFGC_PGLS, LifespanBasFGC_Reduced)[1]), 2))))),
           parse = TRUE) +
  scale_x_continuous(limits = c(1, 9)) +
  scale_y_continuous(limits = c(1, 5))

LifespanBasFGC_Plot
save(LifespanBasFGC_Plot, file = paste(directory, "LifespanBasFGC_Plot.RData", sep = "")) #save file
ggsave(filename = paste(directory, "Figures/LifespanBasFGC_Plot.png", sep = ""),
       width = 5,
       height = 4) #save a picture

# Stats Table ------------------------------------------------------------

#PGLS table
StatsTab_PGLS <- rbind(cbind(coefficients(summary(BasFGCMSMR_PGLS)), intervals(BasFGCMSMR_PGLS)[["coef"]]),
                       cbind(coefficients(summary(BasFGCMass_PGLS)), intervals(BasFGCMass_PGLS)[["coef"]]),
                       cbind(coefficients(summary(ElvFGCBasFGC_PGLS)), intervals(ElvFGCBasFGC_PGLS)[["coef"]]),
                       cbind(coefficients(summary(LifespanBasFGC_PGLS)), intervals(LifespanBasFGC_PGLS)[["coef"]])) %>%
  as.data.frame(.) %>%
  slice(-c(1,3,5,7)) %>%  #cut out all the rows of intercept stats
  mutate(across(c(1,5,7), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(Value = str_c(Value, " (", `lower`, ", ", `upper`, ")")) %>% #paste the value and the CI together
  select(., -c("est.", "Std.Error", "t-value", "lower", "upper")) %>% #remove the columns we don't need
  cbind(., rbind(intervals(BasFGCMSMR_PGLS)[["coef"]][1,],
                 intervals(BasFGCMass_PGLS)[["coef"]][1,],
                 intervals(ElvFGCBasFGC_PGLS)[["coef"]][1,],
                 intervals(LifespanBasFGC_PGLS)[["coef"]][1,]), #add back in a column for the intercept
        rbind(R2(BasFGCMSMR_PGLS, BasFGCMSMR_Reduced)[1], 
              R2(BasFGCMass_PGLS, BasFGCMass_Reduced)[1], 
              R2(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Reduced)[1], 
              R2(LifespanBasFGC_PGLS, LifespanBasFGC_Reduced)[1])) %>%
  mutate(across(c(3,4,5), \(x) round(x, digits = 2))) %>% 
  mutate(est. = str_c(est., " (", `lower`, ", ", `upper`, ")")) %>%
  select(., -c("lower", "upper")) %>%
  `colnames<-`(c("Slope (95% CI)", "p value (slope)", "Intercept  (95% CI)", "Likelihood R2")) %>%
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





