
library(tidyverse)

rm(list=ls())
setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R")
Fecal <- read.csv("Cortisol/CortisolDataClean.csv")
Corticosterone <- read.csv("Corticosterone/CrtstnDataClean.csv")

setwd("C:/Users/kphud/Documents/Mammal_Stress/Mammal_Stress_R/PlasmaCombined")

Plasma <- read.csv("HaaseData.csv")

legend_colors <- c("Plasma" = "red", "Fecal" = "blue", "Corticosterone" = "green")

GCPlot <- ggplot() +
  geom_point(data = Plasma, aes(x = log(MSMR), y = log(Base), color = "Plasma")) +
  geom_point(data = Fecal, aes(x = log(MSMR * 1000), y = log(BasalFGC), color = "Fecal")) +
  geom_point(data = Corticosterone, aes(x = log(MSMR * 1000), y = log(BasalFGC), color = "Corticosterone")) +
  geom_smooth(data = Plasma, aes(x = log(MSMR), y = log(Base), color = "Plasma"), method = "lm", se = FALSE) +
  geom_smooth(data = Fecal, aes(x = log(MSMR * 1000), y = log(BasalFGC), color = "Fecal"), method = "lm", se = FALSE) +
  geom_smooth(data = Corticosterone, aes(x = log(MSMR * 1000), y = log(BasalFGC), color = "Corticosterone"), method = "lm", se = FALSE) +
  labs(title = "Cortisol Levels in Mammals", x = "ln MSMR", y = "ln Cortisol (ng/ml)", color = "Source") +
  scale_color_manual(values = legend_colors) +
  theme_classic()


GCPlot  
ggsave("CortisolLevels.png", plot = GCPlot, width = 8, height = 6, dpi = 300)





