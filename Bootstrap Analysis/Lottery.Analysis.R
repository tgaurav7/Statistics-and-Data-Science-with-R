# Lottery.Analysis.R

# source the utilities file
source("Lottery.Utilities.R")

# get the commandline argument
args<-commandArgs(trailingOnly=TRUE)

# check for length of argument
if(length(args)!=1){stop("Only one argument is needed")}


# if the argument is "Normal" or "Rolldown" go to either if statements and choose awardlist, if otherwise report error
if(args[1]=="Normal"){
    # creating the non-rolldown award list
    A<-c(0, 2, 5, 150, 4000, 500000)
    cat("Using the normal awards vector.\n")
} else if (args[1]=="Rolldown") {
    # creating the rolldown award list
    cat("Using the rolldown awards vector.\n")
    A<-c(0, 2, 27, 807, 22096, 2000000)

} else {
    stop("Error in argument: Only accepts Normal or Rolldown arguments")
}
# cost of each ticket
C<-2

# total number of numbers
N<-46

# number in draw
k<-6

# calculate the average ticket return for this example using the retoninvestment function
cat("The average return for a ticket is ", retoninvestment(k, N, A, C), "\n" )

# number of tickets
X<-1000

# vector with 1000, thousand times
input<-rep(X, X)

# calculate profits for the 1000 tickets bought 1000 times
profits<-sapply(input, profitontickets, k=k, N=N, A=A, C=C)

# give the mean of the profits as result
cat("The average return for 1000 tickets is ", mean(profits), " \n")
hist(profits, breaks=50)
