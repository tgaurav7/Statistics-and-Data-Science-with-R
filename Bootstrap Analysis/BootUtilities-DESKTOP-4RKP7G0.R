# BootUltilities.R
# file containing the functions used with the driver file 

# recData - to read the file and return the dataframe
recData <- function(file.name){
  #' recData
  #'
  #' Loads the file and prints the name of the file being processed
  #'
  #' @param file.name - name of the file from commandline argument
  #'
  #' @returns data structure of the data loaded from above file
  
  # check if the file exists
  if(file.exists(file.name)==FALSE){
    stop("Error in receiving file: file does not exist")
  }
  # printing the file name being processed
  cat("Processing data from file: ", file.name, "\n")
  
  # put the csv file in dataframe
  lottery.df<-read.csv(file.name)

  # returning the choosen dataframe
  return (lottery.df)
}


# std.Find - function to find standard deviation for a given dataframe and a vector of indices "i"
std.Find<-function(lottery.df, i){
  #' std.Find
  #'
  #' calculates the standard deviation of the data at a given number of indices "i"
  #'
  #' @param dataframe - dataframe from which the data is to be selected
  #'
  #' @param i - vector of indices to select the data for calculating the standard deviation
  #'
  #' @returns returns the standard deviation
  
  # calculate the standard deviation of data for i indices
  sd(lottery.df[i, ])
  
  # return the standard deviation
  return (sd)
}

# boot.sd - function to perform the non-parametric bootstrap analysis on the dataframe
boot.sd<-function(lottery.df){
  #' boot.sd
  #'
  #' Performs the non-parametric bootstrap analysis on the dataframe and prints the 95% confidence intervals of the analysis
  #'
  #' @param dataframe - dataframe used to perform the  non-parametric bootstrap analysis
  #'
  #' @returns returns the outputof the boot command
  
  # perform the bootstrap analysis and save the data in bootsd
  bootsd<- boot(data=lottery.df$return, statistic = std.Find, R=2000)
  
  # print out the 95% confidence intervals
  print(boot.ci(bootsd))
}

# plot.hist - plots the histogram of the standard deviation samples
plot.hist<-function(boot.sd){
  #' plot.hist
  #'
  #' Plots the standard deviation of samples from result of the non-parametric bootstrap analysis on the dataframe
  #' 
  #' @param boot.sd - result of the non-parametric bootstrap analysis
  #'
  #' @returns plots the hostogram of the standard deviation of samples
  
  
  # plotting the histogram of the standard deviation of the samples
  hist(bootsd$t, xlab="Standard Deviation of samples")
}


##

# Functions for the second part
# recData2 - receives data from the link and separates without burned area and returns the dataframe
recData2 <- function(){
  #' recData
  #'
  #' Loads the file and prints the name of the file being processed
  #'
  #' @param file.name - name of the file from commandline argument
  #'
  #' @returns data structure of the data loaded from above file
  
  # check if the file exists
  if(file.exists(file.name)==FALSE){
    stop("Error in receiving file: file does not exist")
  }
  # printing the file name being processed
  cat("Processing data from file: ", file.name, "\n")
  
  # put the csv file in dataframe
  forestfire.df<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")
  
  # removing the data with no area
  ff.df<- forestfire.df[forestfire.df$area!="0", ]
  
  # returning the choosen dataframe
  return (ff.df)
}

