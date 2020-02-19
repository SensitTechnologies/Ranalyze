## High-level functions

#' Plot sensor value vs. gas concentration.
#' @param dataAveraged - custom structure
PlotConcentrationVsOutput <- function(dataAveraged){
  devices <- vector(mode = "list", length = length(dataAveraged))
  rangeX <- 0
  rangeY <- 0
  for (i in 1:length(dataAveraged)){
    # Add an empty list to the series.
    devices[[i]] <- list()

    for (j in 1:length(dataAveraged[[i]])){
      # Find the names of each data series.
      attr(devices[[i]],"name") <- attr(dataAveraged[[i]][[j]],"device")

      # Find how big the x-axis needs to be.
      if (max(dataAveraged[[i]][[j]]$SensorValue) > rangeX){
        rangeX <- max(dataAveraged[[i]][[j]]$SensorValue)
      }
      
      # Find how big the y-axis needs to be.
      if (max(dataAveraged[[i]][[j]]$Reference) > rangeY){
        rangeY <- max(dataAveraged[[i]][[j]]$Reference)
      }
    }
  }
  
  # Aggregate all the sensor output and gas concentration values.
  for (i in 1:length(dataAveraged)){
    for (j in 1:length(dataAveraged[[i]])){
      # Sort the data by device:
      for (k in 1:length(devices)){
        if (grepl(attr(devices[[k]],"name"), attr(dataAveraged[[i]][[j]],"device"), fixed = TRUE)){
          devices[[k]]$gasConcentration = append(devices[[k]]$gasConcentration, dataAveraged[[i]][[j]]$Reference)
          devices[[k]]$output = append(devices[[k]]$output, dataAveraged[[i]][[j]]$SensorValue)
          devices[[k]]$stdDev = append(devices[[k]]$stDev, dataAveraged[[i]][[j]]$StdDev)
        }
      }
    }
  }
  
  # Make a vector of the device names (used for the plot's legend).
  seriesNames <- c()
  for (i in 1:length(devices)){
    # Find the names of each data series.
    seriesNames <- append(seriesNames, attr(devices[[i]],"name"))
  }
  
  # Make the y-axis 10% larger to fit the legend.
  rangeY <- rangeY * 1.25
  
  # Create the plot (with no data).
  plot(NULL, main = "Concentration vs. Output",
       xlim = c(0, rangeX),
       ylim = c(0.0, rangeY),
       xlab = "Signal [V]",
       ylab = "Gas [%V]")
  
  # Add a grid.
  grid()
  
  # Create an array of colors to use for each set of data.
  pal = rainbow(length(devices))
  
  # Plot Output vs. Concentration.
  equationString <- ""
  for (i in 1:length(devices)){
    # Plotting the points.
    points(devices[[i]]$output, devices[[i]]$gasConcentration,
           col = pal[i], pch = 20)
    
    # Connect those points with lines (but probably comment this out if you're
    # planning to plot best fit).
    #lines(devices[[i]]$output, devices[[i]]$gasConcentration, col = pal[i])
    
    # Create a best-fit line and plot that.
    fit <- lm(gasConcentration ~ output, data=devices[[i]])
    abline(fit, col = pal[i])
    
    # Write fit summary string.
    s<-summary(fit)
    equationString <- append(equationString,
                             paste(attr(devices[[i]],"name"),
                                   ": y = ",
                                   round(coef(s)[2], 2),
                                   "x + ",
                                   round(coef(s)[1], 2),
                                   " r^2=",round(s$r.squared,2),
                                   "\n"))
  }
  
  # Write the fit equations on the chart.
  legend('bottomright',bty = 'n',legend = equationString, pt.cex = 1, cex = 0.75)
  
  # Add a legend to the plot.
  legend("top", seriesNames, fill = pal, horiz = TRUE)
  
  return (equationString)
}

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

FindPlateus <- function(data){
  # Create a structure of the plateau values at each test.
  data_struct <- list()
  for (i in 1:length(data)){
    print(paste("processing DUT", i))
    
    # Create a struct of all the "tests" associated with a device.
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
  
  # Calculate plateau values.
  dataAveraged <- FindPlateus(data)
  
  # Plot concentration vs. output for each sensor.
  PlotConcentrationVsOutput(dataAveraged)
}
