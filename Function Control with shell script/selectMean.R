# selectMean.R

# declaring vector x
x<- c(-10, -3, 2, 6, 4, 5, 0, -1, 2, 3, 12, -12, -1)
# source the myFuncs file containing the functions OddMean and LehmerMean
source("myfuncs.R")
# input the commandline argument 
args<-commandArgs(trailingOnly=TRUE)
# condition from commandline to perform either odd mean or Lehmer mean
if (args=="Lehmer"){
	#printing Lehmer mean with p=4 for Lehmer commandline argument
	# call LehmerMean function with argument x and p
	# LehmerMean function returns the lehmer mean printed out
	cat("The Lehmer mean of the data, with p=4 is", LehmerMean(x, 4), "\n")
} else if(args=="Odd"){
	# for Odd commandline argument
	# printing result with the odd mean
	# call OddMean function with x as argument which returns the mean 
	cat("The Odd mean of the data is", OddMean(x), "\n" )
} else { 
	# response for incorrect commandline argument 
	cat("Error, incorrect command line argument")}

