

Cortisol <- read.csv("Cortisol/CortisolUncorrected/StatsTab_PGLS.csv", stringsAsFactors = FALSE)

Crtstn <- read.csv("Corticosterone/CrtstnUncorrected/StatsTab_PGLS.csv", stringsAsFactors = FALSE)

Table <- rbind(c("Cortisol", " ", " ", " ", " "), Cortisol, c("Corticosterone", " ", " ", " ", " "), Crtstn) %>%
  `colnames<-`(c("Model", "Slope (95% CI)", "p value (slope)", "Intercept  (95% CI)", "Predicted R2")) 




tt1 <- ttheme_minimal(core=list(fg_params=list(hjust = 1, x = 0.95)))



png("StatsTab_PGLS.png", 
    height = 190*nrow(Table), 
    width = 800*ncol(Table),
    res = 300)
grid.newpage()
g <- tableGrob(Table[,1], cols = "Model", theme = tt1)
g2 <- tableGrob(Table[,2:ncol(Table)], rows = NULL, theme = ttheme_minimal())
g3 <- gtable_combine(g,g2, along=1)
grid.draw(g3)
# grid.text(Label, x = 0.2, y = 0.9, gp = gpar(fontface = "bold"))
dev.off()








