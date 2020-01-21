## functions included here are:
# analyzeTest - calculatees average values of the plateaus for all test data within the folder
# analyzeTest_v0_DEPRECIATED - calculatees average values of the plateaus for all test data within the folder

analyzeTest <- function(){
  data = readTestData()
  
  data_struct <-list()
  
  for (i in 1:length(data)){
    print(paste("processing #",i))
    test_struct <- separateSweeps(data[[i]])
    
    device_struct <- list()
    
    for (j in 1:length(test_struct)){
      print(paste("test #",j))
      
      device_struct[[j]] = findPlateau(test_struct[j])
      attr(device_struct[[j]],"device") <- paste("DUT ",i)
      attr(device_struct[[j]],"test") <- paste("test ",j)
    }
    
    data_struct[[i]] = device_struct
  }
  
  return(data_struct)
}

analyzeTest_v0_DEPRECIATED <- function(data,tests){
  #data = readTestData()
  
  data_struct <- list()
  
  if (tests == 1){
    for (i in 1:length(data)){
      data_struct[[i]] <- findPlateau(data[[i]])
      attr(data_struct[[i]],"device") <- devices[i]
    }
  } else {
    for (i in 1:length(data)){
      print(paste("processing",i))

      test_struct <- separateSweeps(data[[i]])
      for (j in 1:tests){
        testInd = (i-1)*tests+j
        
        #indicate progress
        print(paste("test #",testInd))
        
        data_struct[[testInd]] <- findPlateau(test_struct[[j]])
        attr(data_struct[[testInd]],"device") <- paste("DUT ",i)
        attr(data_struct[[testInd]],"test") <- paste("test ",j)
      }
    }
  }
  
  return(data_struct)
}