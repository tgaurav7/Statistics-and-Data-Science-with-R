#BootstrapAnalysis.R

# source the utilities file
source("BootUtilities.R")

# include the bootstrap library
library(boot)


# get the commandline argument
args<-commandArgs(trailingOnly=TRUE)

# check for length of argument
if(length(args)!=1){stop("Only one argument is needed")}


# if the argument is "lottery" or "forest" go to either if statements and choose method, if otherwise report error
if(args[1]=="lottery"){
    # performing the non-parametric bootstrap analysis on lottery data
    cat("Performing the non-parametric bootstrap analysis on lottery data.\n")
	
	# reading the file 
	file.name="lottery.data.csv"
	
	lottery.df<-recData(file.name)
	
	# performing the bootstrap analysis and printing the Confidence Intervals
	sdboot<-boot.sd(lottery.df)
	
	# plotting the histogram of standard deviation of the samples
	plot.hist(sdboot)
	
}  else if (args[1]=="forest") {
    # performing the parametric bootstrap analysis on forest fire data
    cat("Performing the parametric bootstrap analysis on forest fire data.\n")

	# receive the forest fire data 
	forest.df<-recData2()
	
	# running the function to perform parametric bootstrap analysis with a list of different NS values
	ns.boot(forest.df$area)
		
} else {
    stop("Error in argument: Only accepts lottery or forest as arguments")
}

# comments - it is observed that with increase in N, the results become stable and converge to a value for average median absolute deviations
#			the parametric bootstrapping works well for a higher number of samples(>500 for present case), while the initial increase in accuracy is large(~500 iterations), it converges slowly with furhter increase in number of iterations
	