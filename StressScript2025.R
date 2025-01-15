
# Load packages and data --------------------------------------------------

library(ape)
library(nlme) #for gls
library(rotl) #pull from tree of life
library(tidyverse)
library(geiger)
library(rr2) #for the R2.lik function
library(gridExtra) #to set table themes

rawdata <- read.csv("StressDataRaw.csv")
# summary(rawdata)

# Build Tree --------------------------------------------------------------

StressData <- rawdata %>% 
  filter(Species != "Cebus apella", 
         Species != "Gerbillus andersoni allenbyi")

StressData$Species[StressData$Species == "Choeropsis liberiensis"] <- "Hexaprotodon liberiensis"
StressData$Species[StressData$Species == "Spermophilus columbianus"] <- "Urocitellus columbianus"

taxa <- tnrs_match_names(unique(StressData$Species)) #match names with open tree taxonomy 
tree <- tol_induced_subtree(ott_ids = taxa$ott_id) #pull the subtree of the matched names 
# is_in_tree(ott_id(taxa)) #check that only the taxa that are in the synthetic tree
tree <- compute.brlen(tree, method = "Grafen", power=1) #compute branch lengths, grafen method
write.nexus(tree, file = "Outputs/StressTree_AllSpecies.nex")
tree <- read.nexus("Outputs/StressTree_AllSpecies.nex")

# taxon_map_order <- structure(taxa$search_string, names = taxa$unique_name) #creates a character vector of all the names
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)
# 
# #relabel tree tip labels
# tree$tip.label <- taxon_map_order[ otl_tips_order ]
# 
# # #rename some entries so they match
# # otl_tips_order[otl_tips_order == "Capra hircus (species in domain Eukaryota)"] <- "Capra hircus"
# # otl_tips_order[otl_tips_order == "Hexaprotodon liberiensis"] <- "Choeropsis liberiensis"
# # otl_tips_order[otl_tips_order == "Urocitellus columbianus"] <- "Spermophilus columbianus"
# 
# #to view the lists lining up
cbind(tree$tip.label, unique(StressData$Species))

plot(tree) #view the plot with new names


# Basal Corticosterone ~ Body Mass ----------------------------------------

name.check(tree, StressData$Species)
tree.short <-drop.tip(tree, obj$tree_not_data)




#build covariance matrix
brown <- corBrownian(phy = tree, form = ~Species) 


#Build gls model 
BasCrtstnMass_PGLS <- gls(log(BasalCorticosterone) ~ log(BodyMassAnAge), 
                   data = StressData %>% 
                     drop_na(BasalCorticosterone), 
                   correlation = brown, 
                   method = "ML") #ML = log-likelihood is maximized
# residuals(BasCrtstnMass_PGLS)

#Get values from the model 
BasCrtstnMass_Summ_PGLS <- summary(BasCrtstnMass_PGLS)
BasCrtstnMass_CI_PGLS <- intervals(BasCrtstnMass_PGLS)
BasCrtstnMass_RSq_PGLS <- R2_lik(BasCrtstnMass_PGLS)

#Build ordinary linear model 
BasCrtstnMass_Ordinary <- lm(log(BasalCorticosterone) ~ log(BodyMassAnAge),
                      data=StressData %>% 
                        drop_na(BasalCorticosterone))

BasCrtstnMass_Summ_Ordinary <- summary(BasCrtstnMass_Ordinary)

BasCrtstnMass_Plot <-
  ggplot(data = StressData %>%
           drop_na(BasalCorticosterone),
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

ElvCrtstnBasCrtstn_PGLS <- gls(log(ElevCorticosterone) ~ log(BasalCorticosterone),
                  data=StressData %>%
                    drop_na(c(BasalCorticosterone, ElevCorticosterone)), 
                  correlation = brown, 
                  method="ML")

#Get values from the model 
ElvCrtstnBasCrtstn_Summ_PGLS <- summary(ElvCrtstnBasCrtstn_PGLS)
ElvCrtstnBasCrtstn_CI_PGLS <- intervals(ElvCrtstnBasCrtstn_PGLS)
ElvCrtstnBasCrtstn_RSq_PGLS <- R2_lik(ElvCrtstnBasCrtstn_PGLS)

#Build ordinary linear model 
ElvCrtstnBasCrtstn_Ordinary <- lm(log(ElevCorticosterone) ~ log(BasalCorticosterone),
                     data=StressData %>%
                       drop_na(c(BasalCorticosterone, ElevCorticosterone)))

ElvCrtstnBasCrtstn_Summ_Ordinary <- summary(ElvCrtstnBasCrtstn_Ordinary)

ElvCrtstnBasCrtstn_Plot <- 
  ggplot(data = StressData %>% drop_na(c(BasalCorticosterone, ElevCorticosterone)),
         aes(x = log(BasalCorticosterone), y = log(ElevCorticosterone))) +
  geom_point(aes(colour = Group)) +
  geom_smooth(method=lm, linewidth = 0.5, linetype = 1, colour = "black") +
  theme_classic() +
  labs(x = "Basal Corticosterone (ng/g)",
       y = "Elevated Corticosterone (ng/g)") +
  geom_text(aes(label = list(bquote(y==~ .(round(coefficients(ElvCrtstnBasCrtstn_Summ_PGLS)[1,1], 2))~x^.(round(coefficients(ElvCrtstnBasCrtstn_Summ_PGLS)[2,1], 2)))),
                 x = 500, y = 3), parse = TRUE)

ElvCrtstnBasCrtstn_Plot
ggsave(filename = "Outputs/ElvCrtstnBasCrtstn_Plot.png",
       width = 5,
       height = 4)


# Basal Cortisol ~ Body Mass ----------------------------------------------

#Build gls model 
BasCrtsolMass_PGLS <- gls(log(BasalCortisol) ~ log(BodyMassAnAge), 
                          data = StressData %>% 
                            drop_na(BasalCortisol), 
                          correlation = brown, 
                          method = "ML") #ML = log-likelihood is maximized

#Get values from the model 
BasCrtsolMass_Summ_PGLS <- summary(BasCrtsolMass_PGLS)
BasCrtsolMass_CI_PGLS <- intervals(BasCrtsolMass_PGLS)
BasCrtsolMass_RSq_PGLS <- R2_lik(BasCrtsolMass_PGLS)

#Build ordinary linear model 
BasCrtsolMass_Ordinary <- lm(log(BasalCortisol) ~ log(BodyMassAnAge),
                             data=StressData %>% 
                               drop_na(BasalCortisol))

BasCrtsolMass_Summ_Ordinary <- summary(BasCrtsolMass_Ordinary)

BasCrtsolMass_Plot <- 
  ggplot(data = StressData %>% 
           drop_na(BasalCortisol),
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

