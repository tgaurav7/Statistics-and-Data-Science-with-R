# Lottery.Analysis.R

# including the libraries required
library(microbenchmark) # for benchmarking

library(parallel) # for parallel running

library(compiler) # for byte-compiled code

# source the utilities file
source("Lottery.Utilities.R")

# get the commandline argument
args<-commandArgs(trailingOnly=TRUE)

# check for length of argument
if(length(args)!=2){stop("Two arguments are needed")}


# if the argument is "Normal" or "Rolldown" and second is among the given list, go to either if statements and choose awardlist, if otherwise report error
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

# number of cores
nc=4

# vector with 1000, thousand times
input<-rep(X, X)
	if(args[2]=="nonopt"){
		# calculate profits for the 1000 tickets bought 1000 times using for loop
		profits<-sapply(input, profitontickets, k=k, N=N, A=A, C=C)
	} else if(args[2]=="nonoptBC"){
		oldJIT<-enableJIT(0)
		# calculate profits for the 1000 tickets bought 1000 times using the byte-compiled function that uses the for loop
		profits<-sapply(input, profitwithfor_BC, k=k, N=N, A=A, C=C)
	} else if(args[2]=="apply"){
		# calculate profits for the 1000 tickets bought 1000 times using *apply functions
		profits<-sapply(input, profitonticketswofor, k=k, N=N, A=A, C=C)
	} else if(args[2]=="applyBC"){
		oldJIT<-enableJIT(0)
		# calculate profits for the 1000 tickets bought 1000 times using byte compiled function that uses *apply functions
		profits<-sapply(input, profitwofor_BC, k=k, N=N, A=A, C=C)
	} else if(args[2]=="parallel"){
		# calculate profits for the 1000 tickets bought 1000 times using mclapply functions
		profits<-sapply(input, profitontickets_paralled, k=k, N=N, A=A, C=C, nc=nc)
	} else if(args[2]=="parallelBC"){
		oldJIT<-enableJIT(0)
		# calculate profits for the 1000 tickets bought 1000 times using byte-compiled functio with mclapply functions
		profits<-sapply(input, profit_parallel_BC, k=k, N=N, A=A, C=C, nc=nc)
	} else if (args[2]=="benchmark"){
		oldJIT<-enableJIT(0)
		microbenchmark(profitontickets(X, k, N, A, C), profitwithfor_BC(X, k, N, A, C),  profitonticketswofor(X, k, N, A, C), profitwofor_BC(X, k, N, A, C),  profitontickets_paralled(X, k, N, A, C, nc=1), profitontickets_paralled(X, k, N, A, C, nc=2), profitontickets_paralled(X, k, N, A, C, nc=4), profit_parallel_BC(X, k, N, A, C, nc=1), profit_parallel_BC(X, k, N, A, C, nc=2), profit_parallel_BC(X, k, N, A, C, nc=4), times=10)
	} else {
		stop("Error in argument: Only accepts Normal or Rolldown arguments")
	}

# give the mean of the profits as result
cat("The average return for 1000 tickets is ", mean(profits), " \n")
hist(profits, breaks=50)
