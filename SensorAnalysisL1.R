## Lowest-level functions for CSV file analysis.

#' Check if a library is loaded; install it if necessary.
CheckLibrary <- function(libraryName){
  # If the package is not installed...
  if (!require(libraryName, character.only = TRUE)){
    # Install it and any dependencies.
    install.packages(libraryName, dep = TRUE)
  }
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
#' @parem largeDelay - time delays greater than this will be parsed out
#' @param printFlag - TRUE to print progress; false to omit
#' @return structure of data from the file.
ReadTestFile <- function(filename, largeDelay = 60, printFlag = FALSE){
  if (printFlag == TRUE){
    print(paste("Processing ", filename, "..."))
  }
  
  # Open the specified file and read it as a CSV.
  # BTW, read.csv2 is used if the separator is ';' instead of ','.
  # Not sure if header = TRUE and sep = ',' are necessary.
  data <- read.csv(file = filename, header = TRUE, sep = ',')

  # Parse the timestamp data into a vector.
  if (printFlag == TRUE){
    print("  Parsing Elapsed Time...")
  }
  elapsedTime <- times(data[[1]])
  
  # Convert timestamp from hh:mm:ss into seconds.
  elapsedSeconds <- sapply(elapsedTime, function(x) hours(x)*60*60 + minutes(x)*60 + seconds(x))

  # The timestamps are of the form day.hour:min:second, so when they roll to
  # 1.00:00:00 R interprets this the same as 1:00:00, so the elapsed seconds
  # will sharply decrease.  In our tests, this happens when a test gets left on
  # overnight.  This detects that and fixes the elapsed seconds data.
  # (If the maximum difference between timestamps is more than largeDelay seconds...)
  while (max(abs(diff(elapsedSeconds))) > largeDelay)
  {
    if (printFlag == TRUE){
      print("  Found a timestamp rollover.  Removing it...")
    }
    
    # Find the index in the elapsedSeconds vector after the biggest difference occurs.
    index = match(max(abs(diff(elapsedSeconds))), abs(diff(elapsedSeconds))) + 1
    
    # Remove the delay. If the delay is positive...
    if (max(diff(elapsedSeconds)) > largeDelay){
      # For all elements after the max difference occurs, subtract the difference.
      elapsedSeconds[index:length(elapsedSeconds)] =
        elapsedSeconds[index:length(elapsedSeconds)] - max(diff(elapsedSeconds)) + 1
    }
    # If the delay is negative...
    else {
      # For all elements after the min difference occurs, subtract the difference.
      elapsedSeconds[index:length(elapsedSeconds)] =
        elapsedSeconds[index:length(elapsedSeconds)] - min(diff(elapsedSeconds)) + 1
    }
  }

  # If the file we're loading contains "ref" or "sense" (not case sensitive)
  # in its filename...
  if ((grepl("ref", tolower(filename)) == TRUE) ||
      (grepl("sense", tolower(filename)) == TRUE))
  {
    if (printFlag == TRUE){
      print("  Found Ref/Sense data...")
    }
    
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
  if (printFlag == TRUE){
    print("  Calculating setpoint derivative...")
  }
  data$derivSetpoint = Derive(data$elapsedSeconds,data$Setpoint)

  return(data)
}

#' Take the first derivative of a vector x with respect to a vector y.
#' 
#' @param x - independent variable
#' @parem y - dependent variable
#' @return vector of the derivative
Derive <- function(x,y){
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
#' @param printFlag - TRUE to print progress; false to omit
#' @return atomic vector with all csv filenames in the working directory
FindCSVFiles <- function(printFlag = FALSE){
  if (printFlag == TRUE){
    print("Searching for CSV files...")
  }
  
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
      if (printFlag == TRUE){
        print(paste("  ", listing[i]))
      }
    }
  }
  
  # Return the vector of devices.
  return(devices)
}
