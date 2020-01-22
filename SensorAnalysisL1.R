## Lowest-level functions for CSV file analysis.

#' Ensure all needed libraries are loaded.
loadLibraries <- function(){
  # Load the chron package.
  # If it is not installed...
  if (!require("chron", character.only = TRUE))
  {
    # Install chron package and any dependencies.
    install.packages("chron", dep=TRUE)
  }
}

#' Read the data given a specific file name.
#' 
#' This function expects a CSV file with these columns of data:
#' Elapsed Time - independent variable, a timestamp in hh:mm:ss format
#' Setpoint - independent variable, a number
#' Reference - dependent variable, a number
#' SensorValue - dependent variable, a number
#' 
#' @param filename - file of interest.
#' @return data from the file.
dataRead <- function(filename){
  # Open the specified file and read it as a CSV.
  # BTW, read.csv2 is used if the separator is ';' instead of ','.
  # Not sure if header = TRUE and sep = ',' are necessary.
  data <- read.csv(file = filename, header = TRUE, sep = ',')

  # Parse the timestamp data into a vector.
  elapsedTime <- times(data[[1]])
  
  # Convert timestamp from hh:mm:ss into seconds.
  elapsedSeconds <- sapply(elapsedTime, function(x) hours(x)*60*60 + minutes(x)*60 + seconds(x))

  # If there's a big delay in the middle of the data...
  # (If the maximum difference between timestamps is more than 60 seconds...)
  if (max(diff(elapsedSeconds)) > 60)
  {
    # Find the index in the elapsedSeconds vector after the max difference occurs.
    index = match(max(diff(elapsedSeconds)), diff(elapsedSeconds)) + 1
    
    # Remove the big delay.
    # (For all elements in elapsedSeconds after the max difference occurs,
    # subtract the max difference.)
    elapsedSeconds[index:length(elapsedSeconds)] =
      elapsedSeconds[index:length(elapsedSeconds)] - max(diff(elapsedSeconds))
  }

  # If the file we're loading contains "ref" or "sense" (not case sensitive)
  # in it's filename...
  if ((grepl("ref", tolower(filename)) == TRUE) ||
      (grepl("sense", tolower(filename)) == TRUE))
  {
    # Split the filename (wherever there is a space) into a list of words
    file_split = strsplit(filename," ")

    # The first word is the device (e.g. "DUT1").
    attr(data,"device") <- file_split[[1]][1]
    
    # The second word is the type (e.g. "ref," "sense").
    attr(data,"type") <- file_split[[1]][2]
    
    # The number at the end of the first word is the sequence of device (e.g. "1).
    attr(data,"dnum") <- substr(file_split[[1]][1],nchar(file_split[[1]][1]),nchar(file_split[[1]][1]))
  }

  # Add elapsedSeconds to data.
  data$elapsedSeconds = elapsedSeconds

  # Calculate the derivative of the Setpoint with respect to elapsedSeconds.
  # Add that to data.
  data$dSet = deriv(data$elapsedSeconds,data$Setpoint)

  # Generate a scatterplot of SensorValue vs elapsedSeconds.
  plot(data$elapsedSeconds, data$SensorValue,
       main = paste(filename, "Output vs. Time"),
       xlab = "Elapsed Time [s]",
       ylab = "Sensor Output [V]")

  return(data)
}

#' Take the first derivative of a vector x with respect to a vector y.
#' 
#' @param x - independent variable
#' @parem y - dependent variable
#' @return vector of the derivative
deriv <- function(x,y){
  # If x and y vectors have equal length...
  if (length(x)==length(y))
  {
    #
    index <- c(1:length(x)-1)
    
    # Calculate the derivative at each element of the vectors.
    deriv <- sapply(index, function(x1){
      calc =
        (y[x1+1]-y[x1])/
        (x[x1+1]-x[x1])
        
      return(calc[1])
    })

    return(deriv)
  }
  # If x and y have unequal length...
  else
  {
    # Alert the user of an error.
    print("Not the same length arrays")
  }
}

#' Find all the csv files within the working directory.
#' 
#' @return atomic vector with all csv filenames in the working directory
findTestDevices <- function(){
  # List all the files in the working directory.
  listing = dir()
  
  # Create an empty vector.
  devices <- c()
  
  # For all the elements in the file list...
  for (i in 1:length(listing))
  {
    # If the file has "csv" in the name...
    if (grepl(".csv",listing[i]))
    {
      # Add it to our vector of devices.
      devices = append(devices,listing[i])
    }
  }
  
  # Return the vector of devices.
  return(devices)
}
