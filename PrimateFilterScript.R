
##Filtering out primates

# Basal Corticosterone ~ Body Mass ----------------------------------------

#Filter out blank rows of Basal Corticosterone
BasCrtstnMass_data <- StressData %>% 
  drop_na(BasalCorticosterone) %>% 
  filter(Group != "Old Primate",
         Group != "New Primate")

#Setting row names to map the tree to
rownames(BasCrtstnMass_data) = BasCrtstnMass_data$Species

#Remove tree species not in the basal corticosterone data
BasCrtstnMass_Tree <- drop.tip(tree, name.check(tree, BasCrtstnMass_data)$tree_not_data)

#Build gls model 
BasCrtstnMass_PGLS <- gls(log(BasalCorticosterone) ~ log(BodyMassAnAge), 
                          data = BasCrtstnMass_data, 
                          correlation = corBrownian(phy = BasCrtstnMass_Tree, form = ~Species), 
                          method = "ML") #ML = log-likelihood is maximized

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
  labs(x = "ln Body Mass (g)",
       y = "ln Basal Corticosterone (ng/g)") +
  geom_text(aes(label = list(bquote(y==~ .(round(coefficients(BasCrtstnMass_Summ_PGLS)[1,1], 2))~x^.(round(coefficients(BasCrtstnMass_Summ_PGLS)[2,1], 2)))),
                x = 6, y = 3), parse = TRUE)

BasCrtstnMass_Plot
ggsave(filename = "Outputs/BasCrtstnMass_Plot.png",
       width = 5,
       height = 4)

# Elevated Corticosterone ~ Basal Corticosterone model ------------------------

#Filter out blank rows of Basal Corticosterone
ElvCrtstnBasCrtstn_data <- StressData %>% 
  drop_na(c(BasalCorticosterone, ElevCorticosterone)) %>% 
  filter(Group != "Old Primate",
         Group != "New Primate")

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


# Basal Cortisol ~ Body Mass ----------------------------------------------


#Filter out blank rows of Basal Corticosterone
BasCrtsolMass_data <- StressData %>% drop_na(BasalCortisol)

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