
# Load packages and data --------------------------------------------------

library(ape)
library(rotl)
library(nlme) #for gls
library(tidyverse)
library(geiger)
library(rr2) #for the R2.lik function
library(gridExtra) #to set table themes


StressData <- read.csv("StressDataClean.csv")
tree <- read.nexus("Outputs/StressTree_AllSpecies.nex")

plot(tree)

#to view the lists lining up
cbind(sort(tree$tip.label), sort(unique(StressData$Species)))

# Basal Corticosterone ~ Body Mass ----------------------------------------

#Filter out blank rows of Basal Corticosterone
BasCrtstnMass_data = StressData %>% drop_na(BasalCorticosterone)

#Setting row names to map the tree to
rownames(BasCrtstnMass_data) = BasCrtstnMass_data$Species

#Remove tree species not in the basal corticosterone data
BasCrtstnMass_Tree <- drop.tip(tree, name.check(tree, BasCrtstnMass_data)$tree_not_data)

#Build gls model 
BasCrtstnMass_PGLS <- gls(log(BasalCorticosterone) ~ log(BodyMassAnAge), 
                   data = BasCrtstnMass_data, 
                   correlation = corBrownian(phy = BasCrtstnMass_Tree, form = ~Species), 
                   method = "ML") #ML = log-likelihood is maximized
# residuals(BasCrtstnMass_PGLS)

#Get values from the model 
BasCrtstnMass_Summ_PGLS <- summary(BasCrtstnMass_PGLS)
BasCrtstnMass_CI_PGLS <- intervals(BasCrtstnMass_PGLS)
BasCrtstnMass_RSq_PGLS <- R2_lik(BasCrtstnMass_PGLS)

#Build ordinary linear model 
BasCrtstnMass_Ordinary <- lm(log(BasalCorticosterone) ~ log(BodyMassAnAge),
                      data=BasCrtstnMass_data)

BasCrtstnMass_Summ_Ordinary <- summary(BasCrtstnMass_Ordinary)

BasCrtstnMass_Plot <-
  ggplot(data = BasCrtstnMass_data,
         aes(x = log(BodyMassAnAge), y = log(BasalCorticosterone))) +
  geom_point(aes(colour = Group)) +
  geom_smooth(method=lm, linewidth = 0.5, linetype = 1, colour = "black") +
  theme_classic() +
  labs(x = "Body Mass (g)",
       y = "ln Basal Corticosterone (ng/g)") +
  geom_text(aes(label = list(bquote(y==~ .(round(coefficients(BasCrtstnMass_Summ_PGLS)[1,1], 2))~x^.(round(coefficients(BasCrtstnMass_Summ_PGLS)[2,1], 2)))),
                 x = 6, y = 3), parse = TRUE)

BasCrtstnMass_Plot
ggsave(filename = "Outputs/BasCrtstnMass_Plot.png",
       width = 5,
       height = 4)

# Elevated Corticosterone ~ Basal Corticosterone model ------------------------

#Filter out blank rows of Basal Corticosterone
ElvCrtstnBasCrtstn_data = StressData %>% drop_na(c(BasalCorticosterone, ElevCorticosterone))

#Setting row names to map the tree to
rownames(ElvCrtstnBasCrtstn_data) = ElvCrtstnBasCrtstn_data$Species

#Remove tree species not in the basal and elevated corticosterone data
ElvCrtstnBasCrtstn_Tree <- drop.tip(tree, name.check(tree, ElvCrtstnBasCrtstn_data)$tree_not_data)

ElvCrtstnBasCrtstn_PGLS <- gls(log(ElevCorticosterone) ~ log(BasalCorticosterone),
                  data=ElvCrtstnBasCrtstn_data, 
                  correlation = corBrownian(phy = ElvCrtstnBasCrtstn_Tree, form = ~Species), 
                  method="ML")

#Get values from the model 
ElvCrtstnBasCrtstn_Summ_PGLS <- summary(ElvCrtstnBasCrtstn_PGLS)
ElvCrtstnBasCrtstn_CI_PGLS <- intervals(ElvCrtstnBasCrtstn_PGLS)
ElvCrtstnBasCrtstn_RSq_PGLS <- R2_lik(ElvCrtstnBasCrtstn_PGLS)

#Build ordinary linear model 
ElvCrtstnBasCrtstn_Ordinary <- lm(log(ElevCorticosterone) ~ log(BasalCorticosterone),
                     data=ElvCrtstnBasCrtstn_data)

ElvCrtstnBasCrtstn_Summ_Ordinary <- summary(ElvCrtstnBasCrtstn_Ordinary)

ElvCrtstnBasCrtstn_Plot <- 
  ggplot(data = ElvCrtstnBasCrtstn_data,
         aes(x = log(BasalCorticosterone), y = log(ElevCorticosterone))) +
  geom_point(aes(colour = Group)) +
  geom_smooth(method=lm, linewidth = 0.5, linetype = 1, colour = "black") +
  theme_classic() +
  labs(x = "ln Basal Corticosterone (ng/g)",
       y = "ln Elevated Corticosterone (ng/g)") +
  geom_text(aes(label = list(bquote(y==~ .(round(coefficients(ElvCrtstnBasCrtstn_Summ_PGLS)[1,1], 2))~x^.(round(coefficients(ElvCrtstnBasCrtstn_Summ_PGLS)[2,1], 2)))),
                 x = 5, y = 3), parse = TRUE)

ElvCrtstnBasCrtstn_Plot
ggsave(filename = "Outputs/ElvCrtstnBasCrtstn_Plot.png",
       width = 5,
       height = 4)


# Basal Cortisol ~ Body Mass ----------------------------------------------


#Filter out blank rows of Basal Corticosterone
BasCrtsolMass_data = StressData %>% drop_na(BasalCortisol)

#Setting row names to map the tree to
rownames(BasCrtsolMass_data) = BasCrtsolMass_data$Species

#Remove tree species not in the basal corticosterone data
BasCrtsolMass_Tree <- drop.tip(tree, name.check(tree, BasCrtsolMass_data)$tree_not_data)

#Build gls model 
BasCrtsolMass_PGLS <- gls(log(BasalCortisol) ~ log(BodyMassAnAge), 
                          data = BasCrtsolMass_data, 
                          correlation = corBrownian(phy = BasCrtsolMass_Tree, form = ~Species), 
                          method = "ML") #ML = log-likelihood is maximized

#Get values from the model 
BasCrtsolMass_Summ_PGLS <- summary(BasCrtsolMass_PGLS)
BasCrtsolMass_CI_PGLS <- intervals(BasCrtsolMass_PGLS)
BasCrtsolMass_RSq_PGLS <- R2_lik(BasCrtsolMass_PGLS)

#Build ordinary linear model 
BasCrtsolMass_Ordinary <- lm(log(BasalCortisol) ~ log(BodyMassAnAge),
                             data=BasCrtsolMass_data)

BasCrtsolMass_Summ_Ordinary <- summary(BasCrtsolMass_Ordinary)

BasCrtsolMass_Plot <- 
  ggplot(data = BasCrtsolMass_data,
         aes(x = log(BodyMassAnAge), y = log(BasalCortisol))) +
  geom_point(aes(colour = Group)) +
  geom_smooth(method=lm, linewidth = 0.5, linetype = 1, colour = "black") +
  theme_classic() +
  labs(x = "ln Body mass (g)",
       y = "ln Basal Cortisol (ng/g)") +
  geom_text(aes(label = list(bquote(y==~ .(round(coefficients(BasCrtsolMass_Summ_PGLS)[1,1], 2))~x^.(round(coefficients(BasCrtsolMass_Summ_PGLS)[2,1], 2)))),
                 x = 6, y = 2), parse = TRUE)

BasCrtsolMass_Plot
ggsave(filename = "Outputs/BasCrtsolMass_Plot.png",
       width = 5,
       height = 4)

# Stats Tables -----------------------------------------------------------

#PGLS pic and table first 
StatsTab_PGLS <- rbind(cbind(coefficients(BasCrtstnMass_Summ_PGLS), BasCrtstnMass_CI_PGLS[["coef"]]),
                       cbind(coefficients(ElvCrtstnBasCrtstn_Summ_PGLS), ElvCrtstnBasCrtstn_CI_PGLS[["coef"]]),
                       cbind(coefficients(BasCrtsolMass_Summ_PGLS), BasCrtsolMass_CI_PGLS[["coef"]]))%>%
  as.data.frame(.) %>%
  select(., -"est.") %>% #because we already have an estimate column
  slice(-c(1,3,5)) %>% #cut out all the rows of intercept stats
  cbind(., c(coefficients(BasCrtstnMass_Summ_PGLS)[1,1],
             coefficients(ElvCrtstnBasCrtstn_Summ_PGLS)[1,1],
             coefficients(BasCrtsolMass_Summ_PGLS)[1,1]), #add back in a column for the intercept
        c(BasCrtstnMass_RSq_PGLS, ElvCrtstnBasCrtstn_RSq_PGLS, BasCrtsolMass_RSq_PGLS)) %>% #and a colum for RSq
  mutate(across(c(1,2,3,5,6,7,8), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(across((4), \(x) round(x, digits = 6))) %>%
  `colnames<-`(c("Estimate", "SE Est.", "T value",  "p value", "Lower 95 CI", "Upper 95 CI", "Intercept", "R Squared")) %>%
  `rownames<-`(c("Basal Corticosterone ~ Body Mass", 
                 "Elevated Corticosterone ~ Basal Corticosterone",
                 "Basal Cortisol ~ Body Mass"))

#Ordinary LM pic and table second 
StatsTab_Ordinary <- rbind(cbind(coefficients(BasCrtstnMass_Summ_Ordinary), confint(BasCrtstnMass_Ordinary)),
                           cbind(coefficients(ElvCrtstnBasCrtstn_Summ_Ordinary), confint(ElvCrtstnBasCrtstn_Ordinary)),
                           cbind(coefficients(BasCrtsolMass_Summ_Ordinary), confint(BasCrtsolMass_Ordinary))) %>%
  as.data.frame(.) %>%
  slice(-c(1,3,5)) %>% #cut out all the rows of intercept stats
  cbind(., c(coefficients(BasCrtstnMass_Ordinary)[1],
             coefficients(ElvCrtstnBasCrtstn_Ordinary)[1],
             coefficients(BasCrtsolMass_Ordinary)[1]), #add back in a column for the intercept
        c(BasCrtstnMass_Summ_Ordinary$r.squared,
          ElvCrtstnBasCrtstn_Summ_Ordinary$r.squared,
          BasCrtsolMass_Summ_Ordinary$r.squared)) %>% #and a colum for RSq
  mutate(across(c(1,2,3,5,6,7,8), \(x) round(x, digits = 2))) %>% #new way to round w/ anonymous function
  mutate(across((4), \(x) round(x, digits = 6))) %>%
  `colnames<-`(c("Estimate", "SE Est.", "T value",  "p value", "Lower 95 CI", "Upper 95 CI", "Intercept", "R Squared")) %>%
  `rownames<-`(c("Basal Corticosterone ~ Body Mass", 
                 "Elevated Corticosterone ~ Basal Corticosterone",
                 "Basal Cortisol ~ Body Mass"))

#set the theme for the table
tt1 <- ttheme_default(rowhead=list(fg_params=list(fontface = "bold"),
                                   bg_params=list(fill="grey80")))
#export stats table 
png("Outputs/StatsTab_PGLS.png", 
    height = 130*nrow(StatsTab_PGLS), 
    width = 430*ncol(StatsTab_PGLS),
    res = 300)
grid.table(StatsTab_PGLS, theme = tt1)
dev.off()

#export stats table 
png("Outputs/StatsTab_Ordinary.png", 
    height = 130*nrow(StatsTab_Ordinary), 
    width = 430*ncol(StatsTab_Ordinary),
    res = 300)
grid.table(StatsTab_Ordinary, theme = tt1)
dev.off()

write.csv(StatsTab_PGLS, "Outputs/StatsTab_PGLS.csv")
write.csv(StatsTab_Ordinary, "Outputs/StatsTab_Ordinary.csv")


# Validate Models ---------------------------------------------------------

hist(log(StressData$BasalCorticosterone))
hist(log(StressData$ElevCorticosterone))
hist(log(StressData$BodyMassAnAge))

qqnorm(BasCrtstnMass_PGLS) #qqplot, plot function doesn't work with the PGLS objects
plot(BasCrtstnMass_Ordinary, 2) #qqplot

qqnorm(ElvCrtstnBasCrtstn_PGLS) 
plot(ElvCrtstnBasCrtstn_Ordinary, 2)

qqnorm(BasCrtsolMass_PGLS) 
plot(BasCrtsolMass_Ordinary, 2)

#Build a function for the Fitted vs Residuals plot 
Residual_Fun <- function(m) {
  ggplot() +
    geom_jitter(aes(x = fitted(m), y = residuals(m)),
                width = 0.1,
                height = 0,
                alpha = 0.5) +
    theme_classic() +
    geom_hline(aes(yintercept = 0)) +
    labs(x = "Fitted Values",
         y = "Residuals")
}

Residual_Fun(BasCrtstnMass_PGLS)
Residual_Fun(BasCrtstnMass_Ordinary)
Residual_Fun(ElvCrtstnBasCrtstn_PGLS)
Residual_Fun(ElvCrtstnBasCrtstn_Ordinary)
Residual_Fun(BasCrtsolMass_PGLS)
Residual_Fun(BasCrtsolMass_Ordinary)

