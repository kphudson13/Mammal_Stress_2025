

library(ape)
library(rotl)
library(nlme) #for gls
library(tidyverse)
library(geiger)
library(rr2) #for the R2.lik function
library(gridExtra) #to set table themes


# Basal vs. Mass model ----------------------------------------------------

#Filter out blank rows of Basal FGC
BasFGCMass_data <- StressData %>% drop_na(BasalFGC)

#Setting row names to map the tree to
rownames(BasFGCMass_data) = BasFGCMass_data$Species

#Remove tree species not in the basal FGC data
BasFGCMass_Tree <- drop.tip(tree, name.check(tree, BasFGCMass_data)$tree_not_data)

cbind(sort(BasFGCMass_Tree$tip.label), sort(unique(BasFGCMass_data$Species)))

#Build gls model 
BasFGCMass_PGLS <- gls(log(BasalFGC) ~ log(BodyMassAnAge), 
                          data = BasFGCMass_data, 
                          correlation = corBrownian(phy = BasFGCMass_Tree, form = ~Species), 
                          method = "ML") #ML = log-likelihood is maximized

#Get values from the model 
BasFGCMass_Summ_PGLS <- summary(BasFGCMass_PGLS)
BasFGCMass_CI_PGLS <- intervals(BasFGCMass_PGLS)
BasFGCMass_RSq_PGLS <- R2_lik(BasFGCMass_PGLS)

#Build ordinary linear model 
BasFGCMass_Ordinary <- lm(log(BasalFGC) ~ log(BodyMassAnAge),
                             data=BasFGCMass_data)

BasFGCMass_Summ_Ordinary <- summary(BasFGCMass_Ordinary)

BasFGCMass_Plot <-
  ggplot(data = BasFGCMass_data,
         aes(x = log(BodyMassAnAge), y = log(BasalFGC))) +
  geom_point(aes(shape = Order)) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(BasFGCMass_Summ_PGLS)[1,1], 
              slope = coefficients(BasFGCMass_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Body Mass (g)",
       y = "ln Basal FGC (ng/g)",
       title = Label) +
  annotate("text",  x = 7, y = 2,
           label = list(bquote(PGLS: y==~ .(round(coefficients(BasFGCMass_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCMass_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCMass_RSq_PGLS, 2)))),
           parse = TRUE) +
  annotate("text",  x = 7, y = 1.5, 
           label = list(bquote(LM: y==~ .(round(coefficients(BasFGCMass_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCMass_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCMass_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

BasFGCMass_Plot
ggsave(filename = "BasFGCMass_Plot.png",
       width = 5,
       height = 4)


# Basal vs. MSMR model ----------------------------------------------------

#Filter out blank rows of Basal FGC
BasFGCMSMR_data <- StressData %>% drop_na(c(BasalFGC, MSMR))

#Setting row names to map the tree to
rownames(BasFGCMSMR_data) = BasFGCMSMR_data$Species

#Remove tree species not in the basal FGC data
BasFGCMSMR_Tree <- drop.tip(tree, name.check(tree, BasFGCMSMR_data)$tree_not_data)

cbind(sort(BasFGCMSMR_Tree$tip.label), sort(unique(BasFGCMSMR_data$Species)))

#Build gls model 
BasFGCMSMR_PGLS <- gls(log(BasalFGC) ~ log(MSMR), 
                       data = BasFGCMSMR_data, 
                       correlation = corBrownian(phy = BasFGCMSMR_Tree, form = ~Species), 
                       method = "ML") #ML = log-likelihood is maximized

#Get values from the model 
BasFGCMSMR_Summ_PGLS <- summary(BasFGCMSMR_PGLS)
BasFGCMSMR_CI_PGLS <- intervals(BasFGCMSMR_PGLS)
BasFGCMSMR_RSq_PGLS <- R2_lik(BasFGCMSMR_PGLS)

#Build ordinary linear model 
BasFGCMSMR_Ordinary <- lm(log(BasalFGC) ~ log(MSMR),
                          data=BasFGCMSMR_data)

BasFGCMSMR_Summ_Ordinary <- summary(BasFGCMSMR_Ordinary)

BasFGCMSMR_Plot <-
  ggplot(data = BasFGCMSMR_data,
         aes(x = log(MSMR), y = log(BasalFGC))) +
  geom_point(aes(shape = Order)) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(BasFGCMSMR_Summ_PGLS)[1,1], 
              slope = coefficients(BasFGCMSMR_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln MSMR",
       y = "ln Basal FGC (ng/g)",
       title = Label) +
  annotate("text",  x = -5.5, y = 3,
           label = list(bquote(PGLS: y==~ .(round(coefficients(BasFGCMSMR_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCMSMR_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCMSMR_RSq_PGLS, 2)))),
           parse = TRUE) +
  annotate("text",  x = -5.5, y = 2.5, 
           label = list(bquote(LM: y==~ .(round(coefficients(BasFGCMSMR_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCMSMR_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCMSMR_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

BasFGCMSMR_Plot
ggsave(filename = "BasFGCMSMR_Plot.png",
       width = 5,
       height = 4)


# Basal vs. Elevated FGC --------------------------------------------------


#Filter out blank rows of Basal FGC and Elevated FGC
ElvFGCBasFGC_data <- StressData %>% drop_na(c(BasalFGC, ElevFGC))

#Setting row names to map the tree to
rownames(ElvFGCBasFGC_data) = ElvFGCBasFGC_data$Species

#Remove tree species not in the basal and elevated FGC data
ElvFGCBasFGC_Tree <- drop.tip(tree, name.check(tree, ElvFGCBasFGC_data)$tree_not_data)

#Remove tree species not in the basal and elevated FGC data
ElvFGCBasFGC_Tree <- drop.tip(tree, name.check(tree, ElvFGCBasFGC_data)$tree_not_data)

ElvFGCBasFGC_PGLS <- gls(log(ElevFGC) ~ log(BasalFGC),
                               data=ElvFGCBasFGC_data, 
                               correlation = corBrownian(phy = ElvFGCBasFGC_Tree, form = ~Species), 
                               method="ML")

#Get values from the model 
ElvFGCBasFGC_Summ_PGLS <- summary(ElvFGCBasFGC_PGLS)
ElvFGCBasFGC_CI_PGLS <- intervals(ElvFGCBasFGC_PGLS)
ElvFGCBasFGC_RSq_PGLS <- R2_lik(ElvFGCBasFGC_PGLS)

#Build ordinary linear model 
ElvFGCBasFGC_Ordinary <- lm(log(ElevFGC) ~ log(BasalFGC),
                                  data=ElvFGCBasFGC_data)

ElvFGCBasFGC_Summ_Ordinary <- summary(ElvFGCBasFGC_Ordinary)

ElvFGCBasFGC_Plot <- 
  ggplot(data = ElvFGCBasFGC_data,
         aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point(aes(shape = Order)) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1], 
              slope = coefficients(ElvFGCBasFGC_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Basal FGC (ng/g)",
       y = "ln Elevated FGC (ng/g)",
       title = Label) +
  annotate("text",  x = 5, y = 2,
           label = list(bquote(PGLS: y==~ .(round(coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(ElvFGCBasFGC_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(ElvFGCBasFGC_RSq_PGLS, 2)))),
           parse = TRUE) +
  annotate("text",  x = 5, y = 1.5, 
           label = list(bquote(LM: y==~ .(round(coefficients(ElvFGCBasFGC_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(ElvFGCBasFGC_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(ElvFGCBasFGC_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

ElvFGCBasFGC_Plot
ggsave(filename = "ElvFGCBasFGC_Plot.png",
       width = 5,
       height = 4)


# Stats Tables ------------------------------------------------------------

#PGLS pic and table first 
StatsTab_PGLS <- rbind(cbind(coefficients(BasFGCMass_Summ_PGLS), BasFGCMass_CI_PGLS[["coef"]]),
                       cbind(coefficients(BasFGCMSMR_Summ_PGLS), BasFGCMSMR_CI_PGLS[["coef"]]),
                       cbind(coefficients(ElvFGCBasFGC_Summ_PGLS), ElvFGCBasFGC_CI_PGLS[["coef"]])) %>%
  as.data.frame(.) %>%
  select(., -"est.") %>% #because we already have an estimate column
  slice(-c(1,3,5)) %>% #cut out all the rows of intercept stats
  cbind(., c(coefficients(BasFGCMass_Summ_PGLS)[1,1],
             coefficients(BasFGCMSMR_Summ_PGLS)[1,1],
             coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1]), #add back in a column for the intercept
        c(BasFGCMass_RSq_PGLS, BasFGCMSMR_RSq_PGLS, ElvFGCBasFGC_RSq_PGLS)) %>% #and a column for RSq
  mutate(across(c(1,2,3,5,6,7,8), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(across((4), \(x) round(x, digits = 6))) %>%
  `colnames<-`(c("Estimate", "SE Est.", "T value",  "p value", "Lower 95 CI", "Upper 95 CI", "Intercept", "R Squared")) %>%
  `rownames<-`(c("Basal FGC ~ Body Mass", 
                 "Basal FGC ~ MSMR",
                 "Elevated FGC ~ Basal FGC"))


tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png("StatsTab_PGLS.png", 
    height = 130*nrow(StatsTab_PGLS), 
    width = 430*ncol(StatsTab_PGLS),
    res = 300)
grid.table(StatsTab_PGLS, theme = tt1)
dev.off()
