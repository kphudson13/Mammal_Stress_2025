
# Directory function ------------------------------------------------------

#make a figure directory for initial pull users
CreateDR <- function(DR) {
  if(file.exists(DR)) {
    if(file.exists(paste(DR, "Figures", sep = ""))) {
      #do nothing
    } else {
      dir.create(paste(DR, "Figures", sep = ""))
    }
  } else {
    dir.create(DR)
    dir.create(paste(DR, "Figures", sep = ""))
  }
}


#Pull model 

PullModel <- function(mod1) {
  if(file.exists(paste(DR, mod1, sep = ""))) {
    load(paste(DR, mod1, sep = ""))
  } else {
    stop("Model not found. Run 'MasterScript' first")
  }
}

