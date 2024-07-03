# myFuncs.R

#OddMean function to find mean of all odd values of the vector
OddMean <- function(x,posonly=FALSE){
	#if posonly is true use only values greater than zero
	if(posonly==TRUE){
		# separating indices with condition
		# odd numbers
		# AND positive numbers
		bool.indices=(x%%2==1)&(x>=0)
		#calculate mean of indices with condition TRUE
		oddpositive.xmean=mean(x[bool.indices])
		# return mean
		return (oddpositive.xmean)
	#if false use all 	
	} else {
		# separate the indices with odd numbers
		# condition odd numbers
		bool.indices=(x%%2==1)
		# calculating mean of x
		odd.xmean=mean(x[bool.indices])
		# return lehmermean 
		return (odd.xmean)
	}
}


# Lehmer Mean function to calculate sum of x to power p divided by sum of elements x to power p-1

LehmerMean<- function(x, p){
	# x is the input vector to the function
	# p is the exponent
	# separating indices for positive x
	bool.indices=(x>=0)
	# calculate Lehmermean 
	# fraction of sum of x vector to power p and x to power p-1
	lehmermean=sum(x[bool.indices]^p)/sum(x[bool.indices]^(p-1))
	# return lehmermean
	return (lehmermean)
}

