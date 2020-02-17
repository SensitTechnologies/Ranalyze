## Lowest-level functions for CSV file analysis.

#' Check if a library is loaded; install it if necessary.
CheckLibrary <- function(libraryName){
  # If the package is not installed...
  if (!require(libraryName, character.only = TRUE)){
    # Install it and any dependencies.
    install.packages(libraryName, dep = TRUE)
  }
}

#' Ensure all needed libraries are loaded.
loadLibraries <- function(){
  # Load the chron package.
  CheckLibrary("chron")
  
  # Load the colorspace package (for plotting in color).
  CheckLibrary("colorspace")
}

#' Remove contiguous duplicated elements in a vector.
#' 
#' Inspiration:  https://stackoverflow.com/questions/10769640/how-to-remove-repeated-elements-in-a-vector-similar-to-set-in-python
#' @param vector - the data to parse
#' @return vector, but without contiguous duplicated elements
#' @examples
#' v <- c(1, 1, 5, 5, 5, 5, 2, 2, 6, 6, 1, 3, 3)
#' RemoveContiguousDuplicated(v)
#' [1] 1 5 2 6 1 3
RemoveContiguousDuplicated <- function(vector){
  vector <- vector[c(TRUE, !vector[-length(vector)] == vector[-1])]
  
  return(vector)
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
#' @return structure of data from the file.
dataRead <- function(filename){
  print(paste("Processing ", filename, "..."))
  
  # Open the specified file and read it as a CSV.
  # BTW, read.csv2 is used if the separator is ';' instead of ','.
  # Not sure if header = TRUE and sep = ',' are necessary.
  data <- read.csv(file = filename, header = TRUE, sep = ',')

  # Parse the timestamp data into a vector.
  print("  Parsing Elapsed Time...")
  elapsedTime <- times(data[[1]])
  
  # Convert timestamp from hh:mm:ss into seconds.
  elapsedSeconds <- sapply(elapsedTime, function(x) hours(x)*60*60 + minutes(x)*60 + seconds(x))

  # If there's a big delay in the middle of the data...
  # (If the maximum difference between timestamps is more than 60 seconds...)
  if (max(diff(elapsedSeconds)) > 60)
  {
    print("  Found a delay.  Removing it...")
    
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
    print("  Found Ref/Sense data...")
    
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
  print("  Calculating setpoint derivative...")
  data$derivSetpoint = deriv(data$elapsedSeconds,data$Setpoint)

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
    deriv <- sapply(index, function(i){
      calc =
        (y[i+1]-y[i])/
        (x[i+1]-x[i])
        
      return(calc[1])
    })

    return(deriv)
  }
  # If x and y have unequal length...
  else
  {
    # Alert the user of an error.
    print("Error:  Arrays of unequal length")
  }
}

#' Find all the csv files within the working directory.
#' 
#' @return atomic vector with all csv filenames in the working directory
findTestDevices <- function(){
  print("Searching for CSV files...")
  
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
      
      # Alert the user.
      print(paste("  ", listing[i]))
    }
  }
  
  # Return the vector of devices.
  return(devices)
}
