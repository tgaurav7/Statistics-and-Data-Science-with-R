# Lottery.Utilities.R

# functions to perform different actions

# pobabcalc - calculates probability for getting m out of k numbers correct in N numbers

probabcalc <- function(m , k , N){

	  #' probabcalc
	  #'
	  #' calculates the probabiltiy of  for getting m out of k numbers correct in N numbers
	  #'
	  #' @param m - number - number of numbers correct
	  #' @param k - number - number of numbers drawn
	  #' @param N - number - total number of numbers
	  #'
	  #' @returns returns the probability

	# returns the number probability
	return ((choose(k, m)*choose(N-k, k-m))/choose(N, k))
}
# retoninvestment - calculates the return on investment for a given vector of awards

retoninvestment<-function(k, N, A, C){
	  #' retoninvestment
	  #'
	  #' calculates the return on investment i.e. the amount of money awarded multiplied by the probability of winning minus the cost of ticket
	  #'
	  #' @param k - number - number of numbers drawn
	  #' @param N - number - total number of numbers
	  #' @param A - vector of size k of numbers - award prize for each number in k
	  #' @param C - cost of ticket
	  #'
	  #' @returns return for a given lottery ticket
	# define m such that it goes from 1 to k
	m<-1:k

	# calculate the probability for each value of m
	probabs <- sapply(m, probabcalc, k=k, N=N)

	# return the sum of awards mulitplied with individual probabilities minus cost
return (sum(probabs*A)-C)
}

# randnums - randomly draw k numbers from N
randnums<- function(k,N){
	  #' randnums
	  #'
	  #' draws k random numbers from 1 through N
	  #'
	  #' @param k - number - number of numbers to be drawn
	  #' @param N - number - maximum for range of numbers to draw from
	  #'
	  #' @returns vector of random numbers drawn

	# return the random numbers
	return (sample(1:N, k))
}

# commonnums - returns the number of common numbers between two vectors
commonnums<- function(vec1, vec2){

	  #' commonnums
	  #'
	  #' calculate the number of common numbers in two vectors
	  #'
	  #' @param two vectors of numbers
	  #'
	  #' returns number of common numbers
	return (sum(vec1%in%vec2))
}

# profitontickets - calculates profit on X tickets bought with the lottery numbers 1 to k
profitontickets<-function(X, k, N, A, C){
	  #' profitontickets
	  #'
	  #' Calculates profit on X tickets bought with the lottery numbers 1 to k
	  #'
	  #' @param X - number - number of tickets bought
	  #' @param k - number - number of numbers to be drawn
	  #' @param N - number - maximum for range of numbers to draw from
	  #' @param A - vector of numbers - awards
	  #' @param C - cost of each ticket
	  #'
	  #' @returns profit from X tickets
	# winning numbers
	draw <- 1:k
	profit<-0
	for (i in 1:X){
	  # generate the X tickets
	  ticket<-randnums(k, N)
	  # find number of common numbers with draw
	  t<-commonnums(ticket, draw)
	  # use if to remove values with no matching
	  if(t!=0){
	  # add the profit for this ticket to sum
	  profit<-profit+A[t]
	  }
	}

	# return the profit
	return (profit-C*X)
}
