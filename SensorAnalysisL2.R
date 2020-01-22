## functions included here are:
# ref_process - processes data from sensors with a reference that needs to be subtracted
# findPlateau - finds average sensor value in each plateau automatically
# separateSweeps - seperates out the different sweeps that if more than one sensor sweep was ran
# readTestData - reads all of the test data within the folder

ref_process <- function(data){
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
      attr(processed,"type") <- "processed"

      processed_struct[[i]] <- processed
    }
  }

  return(processed_struct)
}

findPlateau <- function(data,device,test){

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
  plot(plateaus,main=paste("DUT",device," Test #",test))

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
  } else {
    sweepBounds <- c(1,length(data$Setpoint))
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
