## functions included here are:
# loadLibraries - ensures all needed libraries are loaded
# dataRead - reads the data given a specific file name
# deriv - takes first derivative
# findTestDevices - finds all the csv files within the working directory

loadLibraries <- function(){
  if (!require("chron",character.only = TRUE))
  {
    install.packages("chron",dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

dataRead <- function(filename){
  data <- read.csv(file = filename,header = TRUE, sep = ',')

  elapsedTime <- times(data[[1]])
  elapsedSeconds <- sapply(elapsedTime,function(x) hours(x)*60*60+minutes(x)*60+seconds(x))

  if (max(diff(elapsedSeconds)) > 60){
    ind = match(max(diff(elapsedSeconds)),diff(elapsedSeconds))+1
    elapsedSeconds[ind:length(elapsedSeconds)] = elapsedSeconds[ind:length(elapsedSeconds)] - max(diff(elapsedSeconds))
  }

  if ((grepl("ref",tolower(filename)) == TRUE) || (grepl("sense",tolower(filename)) == TRUE)){
    file_split = strsplit(filename," ")

    attr(data,"device") <- file_split[[1]][1]
    attr(data,"type") <- file_split[[1]][2]
    attr(data,"dnum") <- substr(file_split[[1]][1],nchar(file_split[[1]][1]),nchar(file_split[[1]][1]))
  }

  data$elapsedSeconds = elapsedSeconds

  data$dSet = deriv(data$elapsedSeconds,data$Setpoint)

  plot(data$elapsedSeconds,data$SensorValue,main=filename)

  return(data)
}

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
