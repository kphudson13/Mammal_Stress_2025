
# Load libraries
library(ape)
library(rotl)
library(nlme) #for gls
library(tidyverse)
library(geiger) #for name.check
library(rr2) #for the R2 function
library(grid) #to set table themes
library(gridExtra) #to set table themes

#Models are written in y vs. x format

# Basal vs. Mass model ----------------------------------------------------

#Filter out blank rows of Basal FGC
BasFGCMass_data <- StressData %>% drop_na(BasalFGC)

#Setting row names to map the tree to
rownames(BasFGCMass_data) = BasFGCMass_data$Species

#Remove tree species not in the basal FGC data
if (sum(is.na(StressData$BasalFGC)) > 0) {
  BasFGCMass_Tree <- drop.tip(tree, name.check(tree, BasFGCMass_data)$tree_not_data)
} else {
  BasFGCMass_Tree <- tree
}

#to view them lining up
cbind(sort(BasFGCMass_Tree$tip.label), sort(unique(BasFGCMass_data$Species)))

#Build gls model 
BasFGCMass_PGLS <- gls(log(BasalFGC) ~ log(BodyMassAnAge), 
                          data = BasFGCMass_data, 
                          correlation = corBrownian(phy = BasFGCMass_Tree, form = ~Species), 
                          method = "ML") #ML = log-likelihood is maximized

#Specify reduced model, intercept model but phylogeny is the same
BasFGCMass_Reduced <- lm(log(BasalFGC) ~ 1, 
                          data = BasFGCMass_data)

#Get values from the model 
BasFGCMass_Summ_PGLS <- summary(BasFGCMass_PGLS)
BasFGCMass_CI_PGLS <- intervals(BasFGCMass_PGLS)
BasFGCMass_RSq_PGLS <- R2(BasFGCMass_PGLS, BasFGCMass_Reduced)

#Build ordinary linear model 
BasFGCMass_Ordinary <- lm(log(BasalFGC) ~ log(BodyMassAnAge),
                             data=BasFGCMass_data)

BasFGCMass_Summ_Ordinary <- summary(BasFGCMass_Ordinary)

BasFGCMass_Plot <-
  ggplot(data = BasFGCMass_data,
         aes(x = log(BodyMassAnAge), y = log(BasalFGC))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(BasFGCMass_Summ_PGLS)[1,1], 
              slope = coefficients(BasFGCMass_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Body Mass (g)",
       y = "ln Basal FGC (ng/g)",
       title = Label) +
  annotate("text",  x = 7, y = 1.5,
           label = list(bquote(PGLS: y==~ .(round(coefficients(BasFGCMass_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCMass_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCMass_RSq_PGLS[1], 2)))),
           parse = TRUE) +
  annotate("text",  x = 7, y = 1, 
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
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$MSMR)) > 0) {
  BasFGCMSMR_Tree <- drop.tip(tree, name.check(tree, BasFGCMSMR_data)$tree_not_data)
} else {
  BasFGCMSMR_Tree <- tree
}

cbind(sort(BasFGCMSMR_Tree$tip.label), sort(unique(BasFGCMSMR_data$Species)))

#Build gls model 
BasFGCMSMR_PGLS <- gls(log(BasalFGC) ~ log(MSMR), 
                       data = BasFGCMSMR_data, 
                       correlation = corBrownian(phy = BasFGCMSMR_Tree, form = ~Species), 
                       method = "ML") #ML = log-likelihood is maximized

#Specify reduced model, phylogeny is the same
BasFGCMSMR_Reduced <- lm(log(BasalFGC) ~ 1, 
                          data = BasFGCMSMR_data) 

#Get values from the model 
BasFGCMSMR_Summ_PGLS <- summary(BasFGCMSMR_PGLS)
BasFGCMSMR_CI_PGLS <- intervals(BasFGCMSMR_PGLS)
BasFGCMSMR_RSq_PGLS <- R2(BasFGCMSMR_PGLS, BasFGCMSMR_Reduced)

#Build ordinary linear model 
BasFGCMSMR_Ordinary <- lm(log(BasalFGC) ~ log(MSMR),
                          data=BasFGCMSMR_data)

BasFGCMSMR_Summ_Ordinary <- summary(BasFGCMSMR_Ordinary)

BasFGCMSMR_Plot <-
  ggplot(data = BasFGCMSMR_data,
         aes(x = log(MSMR), y = log(BasalFGC))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
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
                               ~R^2 ==~ .(round(BasFGCMSMR_RSq_PGLS[1], 2)))),
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


# Elevated vs. Basal FGC --------------------------------------------------

#Filter out blank rows of Basal FGC and Elevated FGC
ElvFGCBasFGC_data <- StressData %>% drop_na(c(BasalFGC, ElevFGC))

#Setting row names to map the tree to
rownames(ElvFGCBasFGC_data) = ElvFGCBasFGC_data$Species

#Remove tree species not in the basal FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$ElevFGC)) > 0) {
  ElvFGCBasFGC_Tree <- drop.tip(tree, name.check(tree, ElvFGCBasFGC_data)$tree_not_data)
} else {
  ElvFGCBasFGC_Tree <- tree
}

#Build gls model 
ElvFGCBasFGC_PGLS <- gls(log(ElevFGC) ~ log(BasalFGC),
                               data=ElvFGCBasFGC_data, 
                               correlation = corBrownian(phy = ElvFGCBasFGC_Tree, form = ~Species), 
                               method="ML")

#Specify reduced model, phylogeny is the same
ElvFGCBasFGC_Reduced <- lm(log(ElevFGC) ~ 1, 
                            data=ElvFGCBasFGC_data)

#Get values from the model 
ElvFGCBasFGC_Summ_PGLS <- summary(ElvFGCBasFGC_PGLS)
ElvFGCBasFGC_CI_PGLS <- intervals(ElvFGCBasFGC_PGLS)
ElvFGCBasFGC_RSq_PGLS <- R2(ElvFGCBasFGC_PGLS, ElvFGCBasFGC_Reduced)

#Build ordinary linear model 
ElvFGCBasFGC_Ordinary <- lm(log(ElevFGC) ~ log(BasalFGC),
                                  data=ElvFGCBasFGC_data)

ElvFGCBasFGC_Summ_Ordinary <- summary(ElvFGCBasFGC_Ordinary)

ElvFGCBasFGC_Plot <- 
  ggplot(data = ElvFGCBasFGC_data,
         aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1], 
              slope = coefficients(ElvFGCBasFGC_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Basal FGC (ng/g)",
       y = "ln Elevated FGC (ng/g)",
       title = Label,
       subtitle = "All Stressors") +
  annotate("text",  x = 5, y = 2,
           label = list(bquote(PGLS: y==~ .(round(coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(ElvFGCBasFGC_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(ElvFGCBasFGC_RSq_PGLS[1], 2)))),
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

# ACTH only ---------------------------------------------------------------

#Filter out blank rows of Basal FGC and Elevated FGC
ElvACTH_data <- StressData %>% 
  drop_na(c(BasalFGC, ElevFGC)) %>%
  filter(Stressor == "ACTH")

#Setting row names to map the tree to
rownames(ElvACTH_data) = ElvACTH_data$Species

#Remove tree species not in the basal FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$ElevFGC)) > 0) {
  ElvACTH_Tree <- drop.tip(tree, name.check(tree, ElvACTH_data)$tree_not_data)
} else {
  ElvACTH_Tree <- tree
}

#Build gls model 
ElvACTH_PGLS <- gls(log(ElevFGC) ~ log(BasalFGC),
                         data=ElvACTH_data, 
                         correlation = corBrownian(phy = ElvACTH_Tree, form = ~Species), 
                         method="ML")

#Specify reduced model, phylogeny is the same
ElvACTH_Reduced <- lm(log(ElevFGC) ~ 1, 
                            data=ElvACTH_data)

#Get values from the model 
ElvACTH_Summ_PGLS <- summary(ElvACTH_PGLS)
ElvACTH_CI_PGLS <- intervals(ElvACTH_PGLS)
ElvACTH_RSq_PGLS <- R2(ElvACTH_PGLS, ElvACTH_Reduced)

#Build ordinary linear model 
ElvACTH_Ordinary <- lm(log(ElevFGC) ~ log(BasalFGC),
                            data=ElvACTH_data)

ElvACTH_Summ_Ordinary <- summary(ElvACTH_Ordinary)

ElvACTH_Plot <- 
  ggplot(data = ElvACTH_data,
         aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(ElvACTH_Summ_PGLS)[1,1], 
              slope = coefficients(ElvACTH_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Basal FGC (ng/g)",
       y = "ln Elevated FGC (ng/g)",
       title = Label,
       subtitle = "ACTH Only") +
  annotate("text",  x = 5, y = 2,
           label = list(bquote(PGLS: y==~ .(round(coefficients(ElvACTH_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(ElvACTH_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(ElvACTH_RSq_PGLS[1], 2)))),
           parse = TRUE) +
  annotate("text",  x = 5, y = 1.5, 
           label = list(bquote(LM: y==~ .(round(coefficients(ElvACTH_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(ElvACTH_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(ElvACTH_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

ElvACTH_Plot
ggsave(filename = "ElvACTH_Plot.png",
       width = 5,
       height = 4)


# Lifespan vs. Basal model ------------------------------------------------

#Filter out blank rows of Basal FGC
LifespanBasFGC_data <- StressData %>% drop_na(c(BasalFGC, MaxLifespan))

#Setting row names to map the tree to
rownames(LifespanBasFGC_data) = LifespanBasFGC_data$Species

#Remove tree species not in the basal FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$MaxLifespan)) > 0) {
  LifespanBasFGC_Tree <- drop.tip(tree, name.check(tree, LifespanBasFGC_data)$tree_not_data)
} else {
  LifespanBasFGC_Tree <- tree
}

cbind(sort(LifespanBasFGC_Tree$tip.label), sort(unique(LifespanBasFGC_data$Species)))

#Build gls model 
LifespanBasFGC_PGLS <- gls(log(MaxLifespan) ~ log(BasalFGC), 
                           data = LifespanBasFGC_data, 
                           correlation = corBrownian(phy = LifespanBasFGC_Tree, form = ~Species), 
                           method = "ML") #ML = log-likelihood is maximized

#Specify reduced model, phylogeny is the same
LifespanBasFGC_Reduced <- lm(log(MaxLifespan) ~ 1, 
                              data = LifespanBasFGC_data) 

#Get values from the model 
LifespanBasFGC_Summ_PGLS <- summary(LifespanBasFGC_PGLS)
LifespanBasFGC_CI_PGLS <- intervals(LifespanBasFGC_PGLS)
LifespanBasFGC_RSq_PGLS <- R2(LifespanBasFGC_PGLS, LifespanBasFGC_Reduced)

#Build ordinary linear model 
LifespanBasFGC_Ordinary <- lm(log(MaxLifespan) ~ log(BasalFGC),
                              data=LifespanBasFGC_data)

LifespanBasFGC_Summ_Ordinary <- summary(LifespanBasFGC_Ordinary)

LifespanBasFGC_Plot <-
  ggplot(data = LifespanBasFGC_data,
         aes(x = log(BasalFGC), y = log(MaxLifespan))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(LifespanBasFGC_Summ_PGLS)[1,1], 
              slope = coefficients(LifespanBasFGC_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln Basal FGC (ng/g)",
       y = "ln lifespan (years)",
       title = Label) +
  annotate("text",  x = 5, y = 1.5,
           label = list(bquote(PGLS: y==~ .(round(coefficients(LifespanBasFGC_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(LifespanBasFGC_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(LifespanBasFGC_RSq_PGLS[1], 2)))),
           parse = TRUE) +
  annotate("text",  x = 5, y = 1, 
           label = list(bquote(LM: y==~ .(round(coefficients(LifespanBasFGC_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(LifespanBasFGC_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(LifespanBasFGC_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

LifespanBasFGC_Plot
ggsave(filename = "LifespanBasFGC_Plot.png",
       width = 5,
       height = 4)

# Basal vs. Lifespan Model --------------------------------------------------

#Filter out blank rows of Basal FGC
BasFGCLifespan_data <- StressData %>% drop_na(c(BasalFGC, MaxLifespan))

#Setting row names to map the tree to
rownames(BasFGCLifespan_data) = BasFGCLifespan_data$Species

#Remove tree species not in the basal FGC data
if (sum(is.na(StressData$BasalFGC)) > 0 | sum(is.na(StressData$MaxLifespan)) > 0) {
  BasFGCLifespan_Tree <- drop.tip(tree, name.check(tree, BasFGCLifespan_data)$tree_not_data)
} else {
  BasFGCLifespan_Tree <- tree
}

cbind(sort(BasFGCLifespan_Tree$tip.label), sort(unique(BasFGCLifespan_data$Species)))

#Build gls model 
BasFGCLifespan_PGLS <- gls(log(BasalFGC) ~ log(MaxLifespan), 
                       data = BasFGCLifespan_data, 
                       correlation = corBrownian(phy = BasFGCLifespan_Tree, form = ~Species), 
                       method = "ML") #ML = log-likelihood is maximized

#Specify reduced model, phylogeny is the same
BasFGCLifespan_Reduced <- lm(log(BasalFGC) ~ 1, 
                           data = BasFGCLifespan_data)

#Get values from the model 
BasFGCLifespan_Summ_PGLS <- summary(BasFGCLifespan_PGLS)
BasFGCLifespan_CI_PGLS <- intervals(BasFGCLifespan_PGLS)
BasFGCLifespan_RSq_PGLS <- R2(BasFGCLifespan_PGLS, BasFGCLifespan_Reduced)

#Build ordinary linear model 
BasFGCLifespan_Ordinary <- lm(log(BasalFGC) ~ log(MaxLifespan),
                          data=BasFGCLifespan_data)

BasFGCLifespan_Summ_Ordinary <- summary(BasFGCLifespan_Ordinary)

BasFGCLifespan_Plot <-
  ggplot(data = BasFGCLifespan_data,
         aes(x = log(MaxLifespan), y = log(BasalFGC))) +
  geom_point(aes(shape = Order)) +
  scale_shape_manual(values = shapes) +
  geom_smooth(method=lm, formula = y ~ x, linewidth = 0.5, linetype = 1, colour = "black", se = FALSE) +
  geom_abline(intercept = coefficients(BasFGCLifespan_Summ_PGLS)[1,1], 
              slope = coefficients(BasFGCLifespan_Summ_PGLS)[2,1],
              linetype = 2) +
  theme_classic() +
  labs(x = "ln lifespan (years)",
       y = "ln Basal FGC (ng/g)",
       title = Label) +
  annotate("text",  x = 3, y = 1.5,
           label = list(bquote(PGLS: y==~ .(round(coefficients(BasFGCLifespan_Summ_PGLS)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCLifespan_Summ_PGLS)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCLifespan_RSq_PGLS[1], 2)))),
           parse = TRUE) +
  annotate("text",  x = 3, y = 1, 
           label = list(bquote(LM: y==~ .(round(coefficients(BasFGCLifespan_Summ_Ordinary)[1,1], 2))
                               ~x^.(round(coefficients(BasFGCLifespan_Summ_Ordinary)[2,1], 2))
                               ~R^2 ==~ .(round(BasFGCLifespan_Summ_Ordinary$r.squared, 2)))),
           parse = TRUE) 

BasFGCLifespan_Plot
ggsave(filename = "BasFGCLifespan_Plot.png",
       width = 5,
       height = 4)

# Stats Tables ------------------------------------------------------------

#PGLS table
StatsTab_PGLS <- rbind(cbind(coefficients(BasFGCMass_Summ_PGLS), BasFGCMass_CI_PGLS[["coef"]]),
                       cbind(coefficients(BasFGCMSMR_Summ_PGLS), BasFGCMSMR_CI_PGLS[["coef"]]),
                       cbind(coefficients(ElvFGCBasFGC_Summ_PGLS), ElvFGCBasFGC_CI_PGLS[["coef"]]),
                       cbind(coefficients(BasFGCLifespan_Summ_PGLS), BasFGCLifespan_CI_PGLS[["coef"]]),
                       cbind(coefficients(LifespanBasFGC_Summ_PGLS), LifespanBasFGC_CI_PGLS[["coef"]])) %>%
  as.data.frame(.) %>%
  select(., -"est.") %>% #because we already have an estimate column
  slice(-c(1,3,5,7,9)) %>% #cut out all the rows of intercept stats
  cbind(., c(coefficients(BasFGCMass_Summ_PGLS)[1,1],
             coefficients(BasFGCMSMR_Summ_PGLS)[1,1],
             coefficients(ElvFGCBasFGC_Summ_PGLS)[1,1],
             coefficients(BasFGCLifespan_Summ_PGLS)[1,1],
             coefficients(LifespanBasFGC_Summ_PGLS)[1,1]), #add back in a column for the intercept
        rbind(BasFGCMass_RSq_PGLS, BasFGCMSMR_RSq_PGLS, ElvFGCBasFGC_RSq_PGLS, BasFGCLifespan_RSq_PGLS, LifespanBasFGC_RSq_PGLS)) %>% #and a column for RSq
  mutate(across(c(1,2,3,5,6,7,8,9,10), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(across((4), \(x) round(x, digits = 6))) %>%
  `colnames<-`(c("Estimate", "SE Est.", "T value",  "p value", "Lower 95 CI", "Upper 95 CI", "Intercept", "Likelihood R2", "Residual R2", "Predicted R2")) %>%
  `rownames<-`(c("Basal FGC ~ Body Mass", 
                 "Basal FGC ~ MSMR",
                 "Elevated FGC ~ Basal FGC",
                 "Basal FGC ~ Lifespan",
                 "Lifespan ~ Basal FGC"))


tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png("StatsTab_PGLS.png", 
    height = 190*nrow(StatsTab_PGLS), 
    width = 430*ncol(StatsTab_PGLS),
    res = 300)
grid.newpage()
grid.table(StatsTab_PGLS, theme = tt1)
grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()



