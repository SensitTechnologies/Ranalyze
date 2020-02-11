## High-level functions

#' Plot sensor value vs. elapsed time.
PlotOutputVsTime <- function(data){
  seriesNames <- c()
  rangeX <- 0
  rangeY <- 0
  for (i in 1:length(data)){
    # Find the names of each data series.
    seriesNames <- append(seriesNames, attr(data[[i]],"filename"))
    
    # Find how big the x-axis needs to be.
    if (max(data[[i]]$SensorValue) > rangeX){
      rangeX <- max(data[[1]]$elapsedSeconds)
    }
    
    # Find how big the y-axis needs to be.
    if (max(data[[i]]$SensorValue) > rangeY){
      rangeY <- max(data[[i]]$SensorValue)
    }
  }
  
  # Make the y-axis 10% larger to fit the legend.
  rangeY <- rangeY * 1.1
  
  # Create the plot (with no data).
  plot(NULL, main = "Output vs. Time",
       xlim = c(0, rangeX),
       ylim = c(0.0, rangeY),
       xlab = "Elapsed Time [s]",
       ylab = "Sensor Output [V]")
  
  # Add a grid.
  grid()
  
  # Create an array of colors to use for each set of data.
  pal = rainbow(length(data))
  
  # For each set of data, generate a plot of SensorValue vs elapsedSeconds.
  for (i in 1:length(data)){
    lines(data[[i]]$elapsedSeconds, data[[i]]$SensorValue,
          col = pal[i], lty = i)
  }
  
  # Add a legend to the plot.
  legend("top", seriesNames, fill = pal, horiz = TRUE)
}

#' Calculate average values of the plateaus for all test data in the working directory.
analyzeTest <- function(){
  # Ensure required libraries are loaded.
  loadLibraries()
  
  # Read all of the test data within the working folder.
  data = readTestData()

  # If the data has "ref" and "sense" files, process as differential sensors.
  if ((grepl("ref",tolower(attr(data[[1]],"filename"))) == TRUE) ||
      (grepl("sense",tolower(attr(data[[1]],"filename"))) == TRUE)){
    data = ref_process(data)
  }
  
  # Plot output vs. time.
  PlotOutputVsTime(data)
  
  # Create a structure of the plateau values at each test.
  data_struct <- list()
  for (i in 1:length(data)){
    print(paste("processing DUT",i))

    test_struct <- separateSweeps(data[[i]])

    device_struct <- list()

    for (j in 1:length(test_struct)){
      print(paste("  test #",j))

      device_struct[[j]] = findPlateau(test_struct[j],i,j)
      attr(device_struct[[j]],"device") <- paste("DUT ",i)
      attr(device_struct[[j]],"test") <- paste("test ",j)
    }

    data_struct[[i]] = device_struct
  }

  return(data_struct)
}
