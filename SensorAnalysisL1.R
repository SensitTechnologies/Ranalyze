# Ensures all needed libraries are loaded
loadLibraries <- function(){
  if (!require("chron",character.only = TRUE))
  {
    install.packages("chron",dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Reads the data given a specific file name
dataRead <- function(filename){
  data <- read.csv(file = filename,header = TRUE, sep = ',')
  
  elapsedTime <- times(data[[1]])
  elapsedSeconds <- sapply(elapsedTime,function(x) hours(x)*60*60+minutes(x)*60+seconds(x))
  
  if (max(diff(elapsedSeconds)) > 60){
    ind = match(max(diff(elapsedSeconds)),diff(elapsedSeconds))+1
    elapsedSeconds[ind:length(elapsedSeconds)] = elapsedSeconds[ind:length(elapsedSeconds)] - max(diff(elapsedSeconds))
  }
  
  data$elapsedSeconds = elapsedSeconds
  
  data$dSet = deriv(data$elapsedSeconds,data$Setpoint)
  
  plot(data$elapsedSeconds,data$SensorValue)
  
  return(data)
}

# Takes first derivative
deriv <- function(x,y){
  if (length(x)==length(y)){
    index <- c(1:length(x)-1)
    deriv <- sapply(index,function(x1){
      calc = (y[x1+1]-y[x1])/(x[x1+1]-x[x1])
      return(calc[1])
    })
    
    return(deriv)
  } else {
    print("Not the same length arrays")
  }
}

# Finds all the csv files within the working directory.
findTestDevices <- function(){
  listing = dir()
  devices <- c()
  for (i in 1:length(listing)){
    if (grepl(".csv",listing[i])){
      devices = append(devices,listing[i])
    }
  }
  return(devices)
}