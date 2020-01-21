loadLibraries <- function(){
  if (!require("chron",character.only = TRUE))
  {
    install.packages("chron",dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

simplePlot <- function(filename){
  data <- read.csv(file = filename,header = TRUE, sep = ',')
  
  elapsedTime <- times(data[[1]])
  elapsedSeconds <- sapply(elapsedTime,function(x) hours(x)*60*60+minutes(x)*60+seconds(x))
  
  plot(elapsedSeconds,data[["SensorValue"]])
}