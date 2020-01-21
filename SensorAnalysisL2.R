## functions included here are:
# findPlateau - finds average sensor value in each plateau automatically
# separateSweeps - seperates out the different sweeps that if more than one sensor sweep was ran
# readTestData - reads all of the test data within the folder
# separateSweeps_v0_DEPRECIATED - seperates out the different sweeps that if more than one sensor sweep was ran
# findPlateau_v0_DEPRECIATED - finds average sensor value in each plateau automatically

findPlateau <- function(data){
  
  plateau_index <- c(which(data[[1]]$dSet != 0,arr.ind=TRUE),length(data[[1]]$dSet))
  if (plateau_index[1] != 1){
    plateau_index <- c(1,which(data[[1]]$dSet != 0,arr.ind=TRUE),length(data[[1]]$dSet))
  }
  
  plateauAvg <- c()
  plateaus <- c()
  for (i in 1:(length(plateau_index)-1)){
    start_plateau = plateau_index[i] + (plateau_index[i+1]-plateau_index[i])/2
    plateau = data[[1]]$SensorValue[start_plateau:plateau_index[i+1]]
    
    plateaus = append(plateaus,plateau)
    avg = mean(plateau)
    plateauAvg = append(plateauAvg,avg)
  }
  plot(plateaus)
  
  for (i in 1:length(plateauAvg)){
    abline(h=plateauAvg[i])
  }
  
  return(plateauAvg)
}

separateSweeps <- function(data){
  zeroIndex = which(data$Setpoint == 0,arr.ind=TRUE)
  zeroJumps = which(diff(zeroIndex) != 1,arr.ind=TRUE)
  
  num = length(zeroJumps)

  sweepBounds <- c()
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
  
  #separate tests
  data_struct <- list()
  for (i in 1:num){
    data_struct[[i]] <- data[sweepBounds[2*i-1]:sweepBounds[2*i],]
    attr(data_struct[[i]],"test") <- paste("test ",i)
  }
  
  return(data_struct)
}

readTestData <- function(){
  devices <- findTestDevices()
  
  data_struct <- list()
  
  for (i in 1:length(devices)){
    data_struct[[i]] <- dataRead(devices[i])
    attr(data_struct[[i]],"filename") <- devices[i]
  }
  
  return(data_struct)
  
}

separateSweeps_v0_DEPRECIATED <- function(data,tests){
  th1 = 1
  th2 = 1
  avg = mean(data$deriv[!is.infinite(data$deriv)],na.rm = TRUE)
  std = sd(data$deriv[!is.infinite(data$deriv)],na.rm = TRUE)
  
  #find upper bounds
  detected = 0
  while(detected != tests){
    upper = avg + std * th1
    
    testData = data$deriv
    testData[testData < upper] = 0
    
    upperIndex = which(testData!=0,arr.ind = TRUE)
    detected = length(which(diff(upperIndex)>120,arr.ind=TRUE))
    
    th1 = th1 + 0.01
  }
  posIndex <- c(upperIndex[diff(upperIndex)>600],length(data$deriv))
  
  #find lower bounds
  detected = 0
  while(detected != tests){
    lower = avg - std * th2
    
    testData = data$deriv
    testData[testData > lower] = 0
    
    lowerIndex = which(testData!=0,arr.ind = TRUE)
    detected = length(which(diff(lowerIndex)>120,arr.ind=TRUE))
    
    th2 = th2 + 0.01
  }
  negIndex <- c(lowerIndex[diff(lowerIndex)>600],lowerIndex[length(lowerIndex)],length(data$deriv))
  negIndex = negIndex[2:length(negIndex)]
  
  #find test bounds
  testBounds <- c(1)
  for (i in 1:tests){
    testBounds = append(testBounds,round((negIndex[i]+posIndex[i])/2,digits=0))
  }
  
  #separate tests
  data_struct <- list()
  for (i in 1:tests){
    data_struct[[i]] <- data[testBounds[i]:testBounds[i+1],]
    attr(data_struct[[i]],"test") <- paste("test ",i)
  }
  
  return(data_struct)
}

findPlateau_v0_DEPRECIATED <- function(data){
  num = 2*(length(unique(data$Setpoint))-1)
  th = 1
  detected = 0
  
  avg = mean(data$deriv,na.rm = TRUE)
  std = sd(data$deriv,na.rm = TRUE)
  
  # find what the threshold value should be to find the plateaus.
  # this iteratively goes through the derivative until it finds the same number of derivative peaks
  # as the number of tested gas exposures
  while (detected != num){
    upper = avg+th*std
    lower = avg-th*std
    
    testData = data$deriv
    testData[testData < upper & testData > lower] = 0
    
    testIndex = which(testData!=0,arr.ind = TRUE)
    detected = length(which(diff(testIndex)>10,arr.ind=TRUE))
    
    if (detected < num){
      th = th - 0.01
    } else if (detected > num){
      th = th + 0.01
    }
    
    if (th < 0){
      return("No threshold found")
      break
    }
  }
  
  # the previous threshold calculation is the maximum threshold.  
  # the threshold is decreased by 50%, and then the upper and lower bounds are recalculated
  upper = avg+th*std
  lower = avg-th*std
  
  # calculate plateau ranges
  peakDeriv = data$deriv
  peakDeriv[peakDeriv > upper] = -1
  peakDeriv[peakDeriv < lower] = -1
  
  #calculates the end of the plateau
  plateauIndex <- c(which(peakDeriv!=-1,arr.ind=TRUE))
  plateauEnd = plateauIndex[which(diff(plateauIndex)>15)]
  
  plateauBounds <- c(1)
  for (i in 1:length(plateauEnd)){
    #adds end of plateau
    plateauBounds = append(plateauBounds,plateauEnd[i]) 
    #adds start of the next plateau
    plateauBounds = append(plateauBounds,plateauIndex[match(plateauEnd[i],plateauIndex)+1])
  }
  plateauBounds = append(plateauBounds,length(data$SensorValue))
  
  # calculate averages of plateaus
  plateauAvgs <- c()
  
  for (i in seq(from=1,to=(length(plateauBounds)-1),by=2)){
    if (length(data$SensorValue[plateauBounds[i]:plateauBounds[i+1]]) > 25){
      plateauAvgs = append(plateauAvgs,mean(data$SensorValue[plateauBounds[i]:plateauBounds[i+1]]))
    }
  }
  
  # plot plateaus
  plateau = data$SensorValue[plateauIndex]
  plot(data$SensorValue)
  
  for (i in 1:length(plateauAvgs)){
    abline(h=plateauAvgs[i])
  }
  
  return(plateauAvgs)
  
}