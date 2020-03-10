## Mid-level functions

#' Ensure all needed libraries are loaded.
LoadLibraries <- function(){
  # Load the chron package.
  CheckLibrary("chron")
  
  # Load the colorspace package (for plotting in color).
  CheckLibrary("colorspace")
}

#' Process data from differential sensors (where a reference reading needs to be
#' subtracted from a sense reading).
#' 
#' @param data - output from 
#' @return ???
ProcessReference <- function(data){
  testIndex <- c()
  for (j in 1:length(data)){
    testIndex = append(testIndex,attr(data[[j]],"dnum"))
  }

  Sense <- list()
  Ref <- list()
  processed_struct <- list()

  for (i in 1:max(testIndex)){
    inds <- which(testIndex == i,arr.ind=TRUE)
    error = 0

    # determine if the first data set is Sense or Ref
    if (grepl("sense",tolower(attr(data[[inds[1]]],"type"))) == TRUE){
      Sense = data[[inds[1]]]
    } else if (grepl("ref",tolower(attr(data[[inds[1]]],"type"))) == TRUE){
      Ref = data[[inds[1]]]
    } else {
      print(paste("TC parse error: DUT",i))
      error = 1
    }

    # determine if the second data set is Sense or Ref
    if (grepl("sense",tolower(attr(data[[inds[2]]],"type"))) == TRUE){
      Sense = data[[inds[2]]]
    } else if (grepl("ref",tolower(attr(data[[inds[2]]],"type"))) == TRUE){
      Ref = data[[inds[2]]]
    } else {
      print(paste("TC parse error: DUT",i))
      error = 1
    }

    processed <- list()
    if (error == 0){
      processed = Sense
      processed$SenseVal = Sense$SensorValue
      processed$RefVal = Ref$SensorValue
      processed$SensorValue = Sense$SensorValue - Ref$SensorValue
      attr(processed,"type") <- "differential"
      attr(processed, "filename") <- attr(processed, "device")

      processed_struct[[i]] <- processed
    }
  }

  return(processed_struct)
}

#' Find average sensor value in each plateau automatically.
FindPlateau <- function(data, device, test){

  plateau_index <- c(which(data[[1]]$derivSetpoint != 0,arr.ind=TRUE),length(data[[1]]$derivSetpoint))
  if (plateau_index[1] != 1){
    plateau_index <- c(1,which(data[[1]]$derivSetpoint != 0,arr.ind=TRUE),length(data[[1]]$derivSetpoint))
  }

  plateauAvg <- list()
  for (i in 1:(length(plateau_index)-1)){
    # Find the index in data where the plateau starts.
    start_plateau = plateau_index[i] + (plateau_index[i+1]-plateau_index[i])/2
    
    # Parse out the plateau data.
    sensorValue = data[[1]]$SensorValue[start_plateau:plateau_index[i]]
    setpoint = data[[1]]$Setpoint[start_plateau:plateau_index[i]]
    reference = data[[1]]$Reference[start_plateau:plateau_index[i]]

    # Average the sensor values.
    avgSensorValue = mean(sensorValue)
    avgSetpoint = mean(setpoint)
    avgReference = mean(reference)

    # Create a structure with the average sensor value, 
    plateauAvg$SensorValue = append(plateauAvg$SensorValue, avgSensorValue)
    plateauAvg$Setpoint = append(plateauAvg$Setpoint, avgSetpoint)
    plateauAvg$Reference = append(plateauAvg$Reference, avgReference)
  }

  return(plateauAvg)
}

#' Separate multiple sensor sweeps.
SeparateSweeps <- function(data){
  zeroIndex = which(data$Setpoint == 0,arr.ind=TRUE)
  zeroJumps = which(diff(zeroIndex) != 1,arr.ind=TRUE)

  num = length(zeroJumps)

  sweepBounds <- c()
  data_struct <- list()
  if (num > 1){
    for(i in 1:num){
      if (i == 1){
        bounds <- c(1,zeroIndex[zeroJumps[i+1]])
      } else if (i == num){
        bounds <- c(zeroIndex[zeroJumps[i-1]+1],length(data$Setpoint))
      } else {
        bounds <- c(zeroIndex[zeroJumps[i-1]+1],zeroIndex[zeroJumps[i+1]])
      }

      sweepBounds = append(sweepBounds,bounds)
    }
    
    for (i in 1:num){
      data_struct[[i]] <- data[sweepBounds[2*i-1]:sweepBounds[2*i],]
      attr(data_struct[[i]],"test") <- paste("test ", i)
    }
  } else {
    sweepBounds <- c(1,length(data$Setpoint))
    
    data_struct[[1]] <- data[sweepBounds[1]:sweepBounds[2],]
    attr(data_struct[[1]],"test") <- paste("test ", 1)
  }

  return(data_struct)
}

#' Read all of the test data within the folder.
#' @param print - TRUE to print progress; false to omit
ReadTestData <- function(print = FALSE){
  # Find all the csv files within the working directory.
  files <- FindCSVFiles(print)

  # Create an empty structure.
  data_struct <- list()

  # For each file...
  for (i in 1:length(files)){
    # Read the data from the file.
    data_struct[[i]] <- ReadTestFile(files[i], printFlag = print)
    
    # Add the filename as an attribute to the data.
    filename <- unlist(strsplit(files[i], "\\."))
    attr(data_struct[[i]],"filename") <- filename[1]
  }

  return(data_struct)
}
