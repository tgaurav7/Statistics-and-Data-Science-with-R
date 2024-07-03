# myFuncs.R

#OddMean function to find mean of all odd values of the vector

OddMean <- function(x, posonly=FALSE){

# check for x is numeric and posonly is boolean
if(is.numeric(x)==FALSE){
	stop("Error in OddMean: x must be numeric.")
} else if (is.logical(posonly)==FALSE){
	stop("Error in OddMean: posonly must be boolean.")
}

# if posonly is true use only values greater than zero


	#' OddMean
	#'
	#' Calculate the mean of odd values in the vector based on boolean parameter
	#'
	#' Calculates mean of odd positive values if optional logical argument is TRUE
	#' Calculates mean of odd values if logical operator is FALSE
	#'
	#' @param a vector of numeric values (x)
	#' @param optional - boolean value - Default at FALSE (posonly)
	#'
	#' @return the odd mean of selected values from given vector

	if(posonly==TRUE){
		# separating indices with condition
		# odd numbers
		# and positive numbers
		bool.indices=(x%%2==1)&(x>=0)

		# check if length of bool.indices is greater than 0
		if(!(length(x[bool.indices])>1e-10)){
			stop("Error in OddMean: no valid values to average.")
		}

		# Calculate mean of indices with condition TRUE
		oddpositive.xmean=mean(x[bool.indices])

		# return mean
		return (oddpositive.xmean)

	# if false use all
	} else {
		# separate the indices with odd numbers

		# condition for separating odd numbers
		bool.indices=(x%%2==1)

		# check if length of bool.indices is greater than 0
		if(!(length(x[bool.indices])>1e-10)){
			stop("Error in OddMean: no valid values to average.")
		}

		# calculating mean of x
		odd.xmean=mean(x[bool.indices])

		# return OddMean
		return (odd.xmean)
	}
}


# Lehmer Mean function to calculate sum of x to power p divided by sum of elements x to power p-1

LehmerMean<- function(x, p){
	# x is the input vector to the function
	# p is the exponent
	# separating indices for positive x


	#' LehmerMean
	#'
	#' Calculates the Lehmer mean of positive values in the vector, for value of p
	#'
	#' Calculates Lehmer mean of positive values using p as the exponent
	#'
	#' @param a vector of numeric values (x)
	#' @param a numeric value (p)
	#'
	#' @return the Lehmer mean of positive values from given vector

	# Check for x and p to be numeric and length of p is 1
	if(is.numeric(x)==FALSE){
		stop("Error in LehmerMean: x must be numeric.")
	} else if (is.numeric(p)==FALSE){
		stop("Error in LehmerMean: p must be numeric.")
	} else if (!(length(p)-1<1e-10)){
		stop("Error in LehmerMean: p must be single numeric value.")
	}

	# select the positive values from x
	bool.indices=(x>0)

	# check for valid values to average
	if(!(length(x[bool.indices])>1e-10)){
		stop("Error in OddMean: no valid values to average.")
	}

	# calculate Lehmer mean
	# fraction of sum of x vector to power p and x to power p-1

	lehmermean=sum(x[bool.indices]^p)/sum(x[bool.indices]^(p-1))

	# return lehmermean
	return (lehmermean)
}
