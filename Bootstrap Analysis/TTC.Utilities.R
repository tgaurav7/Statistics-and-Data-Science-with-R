# TTC.Utilities.R

# functions to perform following actions

# filerecv - Receives the argument file, puts in dataframe ttc.df and prints the file name returning the data

filerecv <- function(ttc.str){
        # check if the file exists
        if(file.exists(ttc.str)==FALSE){
                stop("Error in receiving file: file does not exist")
        }

        #' filerecv
        #'
        #' Puts the received file in dataframe
        #'
        #' prints the file name and returns the data
        #'
        #' @param str with the file name
        #'
        #' @returns the data

        #put the csv file in dataframe
        TTC.df<-read.csv(ttc.str)

        # dataframe check
        # check if the dataframe is not empty and has observations
        if(dim(TTC.df[1])==0){
          stop("Error in data: there are no observations")
        }

        # printing the file name being processed
        cat("Processing data from file: ", ttc.str, "\n")

# return the dataframe
return (TTC.df)
}

# function to calculate the total number of delays for each incident type
cal.Delay <- function(TTC.df){
# function receives the dataframe
        #' cal.Delay
        #'
        #' Calculates the total number of delays per incident
        #'
        #' @param the dataframe
        #'
        #' @return no return - prints out the values

        # the dataframe was checked for empty in the function called before
        # find all the unique incidents
        inci.unique <- unique(TTC.df$Incident)

        # printing total number of delays for each incident types
        cat("Total number of delays per incident: \n")
        # for each incident printing out the total number of instances
        for (i in 1:length(inci.unique)){
          # printing all the incident instances
          cat(as.character(inci.unique[i]), " -- ", dim(TTC.df[TTC.df$Incident==inci.unique[i], ])[1], "\n")
          }
}

# function to print the average minimum delay of streetcars due to mechanical incident
averdel.mechanical <- function(TTC.df){
    # printing average minimum delay for mechanical incident
    cat("The average minimum delay of the streetcars due to a mechanical incident, ignoring unreported data,  is ", (mean(TTC.df$Min.Delay[TTC.df$Incident == "Mechanical"], na.rm=TRUE)) , "minutes\n")
}

# function to find the route with most delays in December
most.delays<- function(TTC.df)
{
  #' most.delays
  #'
  #' function to print the route with most delays in December
  #'
  #' @param the dataframe
  #'
  #' @return prints the route name

  # separate the dates for December
  dec.dates<-TTC.df[substr(TTC.df$Report.Date, 4, 6)=="Dec", ]

  # get the table for frequencies of all the routes
  decroute.table<-table(dec.dates$Route)

  # find the one with maximum frequency and print the name of the route
  cat("The route with the most delays in December was route ", names(decroute.table[decroute.table==max(decroute.table)]), ".\n")
}
