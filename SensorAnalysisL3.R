## High-level functions

#' Calculate average values of the plateaus for all test data in the working directory.
analyzeTest <- function(){
  data = readTestData()

  data_struct <- list()

  # If the data has "ref" and "sense" files, process as differential sensors.
  if ((grepl("ref",tolower(attr(data[[1]],"filename"))) == TRUE) ||
      (grepl("sense",tolower(attr(data[[1]],"filename"))) == TRUE)){
    data = ref_process(data)
  }

  for (i in 1:length(data)){
    print(paste("processing #",i))
	
    test_struct <- separateSweeps(data[[i]])

    device_struct <- list()

    for (j in 1:length(test_struct)){
      print(paste("test #",j))

      device_struct[[j]] = findPlateau(test_struct[j],i,j)
      attr(device_struct[[j]],"device") <- paste("DUT ",i)
      attr(device_struct[[j]],"test") <- paste("test ",j)
    }

    data_struct[[i]] = device_struct
  }

  return(data_struct)
}
