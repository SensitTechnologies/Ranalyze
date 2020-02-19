# Pull in data list from current working directory
## You must set the working directory to where your data is stored
## You can have other files in the folder.  This will only look for .csv files
data_import <- function(){
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

take_deriv <- function(x,y){
  deriv<-seq_along(x)
  deriv<-lapply(deriv, function(i){
    if (i == 0){
      return(c(0))
    } else {
      curr_deriv <- c((y[i]-y[i-1])/
					  (x[i]-x[i-1]))
      
      return(curr_deriv)
    }
  })
  
  #plot(x,deriv,xlim=c(0,max(x)))
  
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
  setpoints <- unique(data_set[["Setpoint"]])
  
  averages<-lapply(setpoints,function(x){
    # Find the plateaus with setpoint == x.
    points = intersect(plateaus,which(data_set[["Setpoint"]]==x))
    
    # Average those plateaus.
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
  
  output <- list()
  output["gases"]<-list(unlist(gases))
  output["averages"]<-list(unlist(averages))
  output["sd"]<-list(unlist(sd))
  
  return(output)
}

generate_plots <- function(device,dut_name,test_set){
  deriv<-take_deriv(test_set[[device]]["data"][[1]]["adj_time"][[1]],test_set[[device]]["data"][[1]]["SensorValue"][[1]])
  
  plateaus<-find_plateaus(test_set[[device]]["data"][[1]]["adj_time"][[1]],deriv,2)
  
  output<-average_plateaus(test_set[[device]]["data"][[1]],plateaus)
  
  # Plot output vs. time.
  # (I commented out the lines that save the plot as a png file.)
  #png(paste(dut_name," Survey of Data.png"))
  
  plot(test_set[[device]]["data"][[1]]["adj_time"][[1]],test_set[[device]]["data"][[1]]["SensorValue"][[1]],'l',xlab="Time [s]",ylab="Signal [V]",main=paste("Survey of ",dut_name," Data"))
  grid(nx=NULL,ny=NULL)
  
  #dev.off()
  
  # general standard deviation plot
  # (I commented out the lines that save the plot as a png file.)
  #png(paste(dut_name," Average and STD.png"))
  plot(output$gases, output$averages,
       lwd = 2, pch = 20,
       xlab = "Gas [vol%]",
       ylab="Signal [V]",
       xlim=c(-1,27),
       main = paste("Overview of ",dut_name," Plateau Averages with Standard Deviation"))
  
  arrows(output$gases,
         output$averages - 2*output$sd,
         output$gases,
         output$averages + 2*output$sd,
         length = 0.05,# length of edges of arrow head 
         angle = 90,   # angle from shaft of arrow to edge of arrowhead
         code = 3,     # Draw arrowhead at both ends of line.
         col = "blue", # Make the arrow blue.
         lwd=2)
  
  with(output,
       text(output$gases, output$averages,
            paste("avg=", round(output$averages, 2),
                  "V\n std=", round(output$sd, 6)),
            pos = 4, cex = 0.75, adj = 0))
  
  grid(nx=NULL,ny=NULL)
  
  #dev.off()
  
  # plot lm plot
  # (I commented out the lines that save the plot as a png file.)
  #png(paste(dut_name," Overview of Calibration.png"))
  plot(output$averages, output$gases,
       lwd = 2, pch = 20,
       xlab = "Signal [V]",
       ylab="Gases [vol.%]",
       ylim=c(-0.7,27),
       main=paste("Overview of ",dut_name," Calibration"))
  
  fit<-lm(gases ~ averages, data=output)
  
  abline(fit, col="red")
  
  s<-summary(fit)
  lgd_str<-paste("(intercept) estimate=",round(coef(s)[1],2),
                 "| Std.Err=",round(coef(s)[3],2),
                 "| Pr(>|t|)=",round(coef(s)[7],2),
                 "\n Signal[V] estimate=",round(coef(s)[2],2),
                 "| Std.Err=",round(coef(s)[4],2),
                 "| Pr(>|t|)=",round(coef(s)[8],2),
                 "\n r^2=",round(s$r.squared,2))
  
  legend('topleft',bty='n',legend=lgd_str)
  
  
  predicted = predict(fit,output)
  output$predicted <- list(unlist(predicted))
  errors <- lapply(seq_along(output$predicted[[1]]),function(x){
    err = round((output$predicted[[1]][x] - output$gases[x])/output$predicted[[1]][x]*100,2)
    return (err)
  })
  output$errors<-unlist(errors)
  
  with(output,text(output$averages,output$gases,paste("err=",output$errors,"%"),pos=1,cex=0.75,adj=0))
  grid(nx=NULL,ny=NULL)
  
  #dev.off()
  
}