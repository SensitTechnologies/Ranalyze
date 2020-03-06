## High-level functions

#' Plot sensor value vs. gas concentration.
#' @param dataAveraged - custom structure
#' @return coefficients - custom structure with best-fit coefficients for each
#' sensor
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
  
  # Create the plot (with no data).
  # Make the x-axis 20% larger to fit the legend.
  plot(NULL, main = "Concentration vs. Output",
       xlim = c(0, rangeX * 1.2),
       ylim = c(0.0, rangeY),
       xlab = "Signal [V]",
       ylab = "Gas [%V]")
  
  # Add a grid.
  grid()
  
  # Create an array of colors to use for each set of data.
  pal = rainbow(length(devices))
  
  # Plot Output vs. Concentration.
  equationString <- ""
  coefficients <- list()
  for (i in 1:length(devices)){
    # Plotting the points.
    points(devices[[i]]$output, devices[[i]]$gasConcentration,
           col = pal[i], pch = 20)
    
    # Create a best-fit line and plot that.
    linearFit <- lm(gasConcentration ~ output, data = devices[[i]])
    abline(linearFit, col = pal[i])
    
    # Write fit summary string.
    s<-summary(linearFit)
    equationString <- append(equationString,
                             paste(attr(devices[[i]],"name"),
                                   ": y = ",
                                   round(coef(s)[2], 2),
                                   "x + ",
                                   round(coef(s)[1], 2),
                                   " r^2=",round(s$r.squared,2),
                                   "\n"))
    
    # Create a quadratic fit and plot that.
    # output2 <- devices[[i]]$output^2
    # quadraticFit <- lm(gasConcentration ~ output + output2, data = devices[[i]])
    # abline(quadraticFit, col = pal[i])
    
    # Calculate mean, std dev of the equations.
    coefficients$slope = append(coefficients$slope, coef(s)[2])
    coefficients$intercept = append(coefficients$intercept, coef(s)[1])
    coefficients$interceptPr = append(coefficients$interceptPr, coef(s)[7])
  }
  
  # Finish calculating the mean of the coefficients for each device.
  coefficients$meanSlope = mean(coefficients$slope)
  coefficients$meanIntercept = mean(coefficients$intercept)
  coefficients$meanInterceptPr = mean(coefficients$interceptPr)
  coefficients$stdDevSlope = sd(coefficients$slope)
  coefficients$stdDevIntercept = sd(coefficients$intercept)
  
  # Write the fit equations on the chart (in smallish font).
  #legend('bottomright',bty = 'n',legend = equationString, pt.cex = 1, cex = 0.75)

  # Add a legend to the plot (in smallish font).
  legend("topright", seriesNames, fill = pal, pt.cex = 1, cex = 0.75)
  
  # Return the coefficients.
  return (coefficients)
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
  
  # Create the plot (with no data).
  # Make the x-axis 20% larger to fit the legend.
  plot(NULL, main = "Output vs. Time",
       xlim = c(0, rangeX * 1.25),
       ylim = c(0.2, rangeY),
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
  
  # Add a legend to the plot (in a smallish font).
  legend("topright", seriesNames, fill = pal, pt.cex = 1, cex = 0.75, ncol = 2)
}

FindPlateus <- function(data, printFlag = FALSE){
  # Create a structure of the plateau values at each test.
  data_struct <- list()
  for (i in 1:length(data)){
    if (printFlag == TRUE){
      print(paste("processing DUT", i))
    }
    
    # Create a struct of all the "tests" associated with a device.
    test_struct <- SeparateSweeps(data[[i]])
    
    device_struct <- list()
    for (j in 1:length(test_struct)){
      if (printFlag == TRUE){
        print(paste("  test #",j))
      }
      
      device_struct[[j]] = FindPlateau(test_struct[j],i,j)
      attr(device_struct[[j]],"device") <- attr(data[[i]],"filename")
      attr(device_struct[[j]],"test") <- paste("test ",j)
    }
    
    data_struct[[i]] = device_struct
  }
  
  return(data_struct)
}

#' Calculate average values of the plateaus for all test data in the working directory.
AnalyzeTest <- function(){
  # Ensure required libraries are loaded.
  LoadLibraries()
  
  # Read all of the test data within the working folder.
  data = ReadTestData(print = TRUE)
  
  # Plot output vs. time.
  PlotOutputVsTime(data)
  
  # Calculate plateau values.
  dataAveraged <- FindPlateus(data)
  
  # Plot concentration vs. output for each sensor.
  coefficients = PlotConcentrationVsOutput(dataAveraged)
}
