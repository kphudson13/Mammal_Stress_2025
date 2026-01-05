
################################################################################

# This is just a script to chech if gerbil data look weird
# live laugh love -Kyle

################################################################################

library(tidyverse)

df <- read.csv("Cortisol/CortisolDataClean.csv")

df <- df %>%
  mutate(is_gerbillus = grepl("^Cavia", Species, ignore.case = TRUE))

ggplot(df, aes(x = log(MSMR), y = log(BasalFGC))) +
  geom_point(aes(color = is_gerbillus)) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black"))

ggplot(df, aes(x = log(Mass), y = log(BasalFGC))) +
  geom_point(aes(color = is_gerbillus)) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black"))

ggplot(df, aes(x = log(BasalFGC), y = log(ElevFGC))) +
  geom_point(aes(color = is_gerbillus)) +
  scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black"))

