
# This script is meant to be ran as a function from MasterScript.R
# If you wish to troubleshoot first load inputs from MasterScript.R for the dataset of choice
# Models are written in y vs. x format
# live laugh love -Kyle

SetDataAndTree <- function (y, x) {
    temp <- StressData %>% drop_na(all_of(c(y, x)))
    rownames(temp) = temp$Species
    assign(paste(y, x, "_data", sep = ""), temp, envir = .GlobalEnv)
  if (sum(is.na(StressData[,y])) > 0 | sum(is.na(StressData[,x])) > 0) {
    assign(paste(y, x,"_Tree", sep = ""), drop.tip(tree, name.check(tree, temp)$tree_not_data), envir = .GlobalEnv)
  } else {
    assign(paste(y, x,"_Tree", sep = ""), tree, envir = .GlobalEnv)
  }
}


