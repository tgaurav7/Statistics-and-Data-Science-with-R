# process.TTC.R
# file runs all the functions in TTC.Utilities.R

# source the TTC.Utilities.R file containing the required functions
source("TTC.Utilities.R")

# input of the commandline argument i.e. the file name
args<-commandArgs(trailingOnly=TRUE)

# calling function to receive and make the dataframe
TTC.df<-filerecv(args[1])

# calling function to calculate the total number of delays for each incident type
cal.Delay(TTC.df)

# calling function to print the average minimum delay of streetcars due to mechanical incident
averdel.mechanical(TTC.df)

# Calling function to find the route with most delays in December
most.delays(TTC.df)

# creating space between subsequent data
cat("\n")
