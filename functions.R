data_import <- function(){
  
  # Pull in data list from current working directory
  ## You must set the working directory to where your data is stored
  ## You can have other files in the folder.  This will only look for .csv files
  
  ## List all the files in the working directory.
  listing <- dir()
  
  ## Search the directory for CSV files.
  test_data <- listing[grep(".csv",listing)]
  
  ## Convert the elapsed time data into something that we can process.
  all_data <- lapply(test_data,function(x1){
    current_raw <- read.csv(file=x1,header=TRUE,",",stringsAsFactors=FALSE)
    
    # Sets the first time that is subracted from all times in ElapsedTime
    first_time = as.numeric(as.POSIXct(strptime(current_raw[[1]][1],"%H:%M:%OS")))*10
    
    # loops through ElapsedTime and creates adj_time, then subtracts first_time
    current_raw["adj_time"] <- lapply(current_raw["ElapsedTime"],function(x2){
      return ((as.numeric(as.POSIXct(strptime(x2,"%H:%M:%OS")))*10 - first_time)/10)
    })
    
    # Create a list with columns for name and data.
    current_set<-list()
    current_set["name"] = x1
    current_set["data"]<-list(current_raw)
    
    return(current_set)
  })

  # return value
  return(all_data)
}

generate_time_series <- function(data_set,param1,param2){
  print(typeof(data_set[[1]]["SensorValue"][[1]][1]))
  plot(data_set[[1]]["adj_time"][[1]],data_set[[1]]["SensorValue"][[1]],xlab = 'time [seconds]',ylab='ADC Reading [V]')
  grid(nx=NULL,ny=NULL)
}

take_deriv <- function(x_vals,y_vals){
  deriv<-seq_along(x_vals)
  deriv<-lapply(deriv,function(x){
    if (x==0){
      return(c(0))
    } else {
      curr_deriv<-c((y_vals[x]-y_vals[x-1])/(x_vals[x]-x_vals[x-1]))
      
      return(curr_deriv)
    }
  })
  
  plot(x_vals,deriv,xlim=c(0,max(x_vals)))
  
  return (unlist(deriv))
}

find_plateaus <- function(x_vals,deriv,cf){
  mu = mean(deriv)
  std = sd(deriv)
  
  upper = mu + cf*std*std
  lower = mu - cf*std*std
  
  return(which(deriv<upper & deriv>lower))
}

average_plateaus <- function(data_set,plateaus){
  averages<-lapply(unique(data_set[["Setpoint"]]),function(x){
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    return(mean(data_set[["SensorValue"]][points]))
  })
  sd<-lapply(unique(data_set[["Setpoint"]]),function(x){
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    return(sd(data_set[["SensorValue"]][points]))
  })
  gases<-lapply(unique(data_set[["Setpoint"]]),function(x){
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    return(mean(data_set[["Reference"]][points]))
  })
  acceptable<-lapply(unique(data_set[["Setpoint"]]),function(x){
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    return(0.1*mean(data_set[["SensorValue"]][points]))
  })
  error<-lapply(unique(data_set[["Setpoint"]]),function(x){
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    return((mean(data_set[["SensorValue"]][points])-x))
  })
  
  output <- list()
  output["gases"]<-list(unlist(gases))
  output["averages"]<-list(unlist(averages))
  output["sd"]<-list(unlist(sd))
  output["acceptable"]<-list(unlist(acceptable))
  output["error"]<-list(unlist(error))
  
  return(output)
}

generate_plots <- function(device,test_set){
  deriv<-take_deriv(test_set[[device]]["data"][[1]]["adj_time"][[1]],test_set[[device]]["data"][[1]]["SensorValue"][[1]])
  
  plateaus<-find_plateaus(test_set[[device]]["data"][[1]]["adj_time"][[1]],deriv,2)
  
  output<-average_plateaus(test_set[[device]]["data"][[1]],plateaus)
  
  print(output)
  
  # plot overview of data
  plot(test_set[[device]]["data"][[1]]["adj_time"][[1]],test_set[[device]]["data"][[1]]["SensorValue"][[1]])
  
  # general standard deviation plot
  plot(output$gases,output$averages,lwd=2,pch=5,xlab="Gas [vol%]",ylab="Signal [V]")
  
  arrows(output$gases,output$averages-2*output$sd,output$gases,output$averages+2*output$sd,length=0.05,angle=90,code=3,col="blue",lwd=2)
  
  # plot lm plot
  plot(output$averages,output$gases,lwd=2,pch=5,xlab="Signal [V]",ylab="Gases [vol.%]")
  fit<-lm(gases ~ averages,data=output)
  abline(fit)
}