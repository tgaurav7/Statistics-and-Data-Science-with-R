# Fitting.Utilities.R

# creating the function to read the csv file and loads the observations
recData <- function(file.name){
  #'recData
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
  fulldata.df<-read.csv(file.name, sep="\t")

  # choosing x and y data from the csv file
  Concussion.df<-fulldata.df[, c("Perc..of.TBIs", "Perc..of.concussions")]

  # returning the choosen dataframe
  return (Concussion.df)

}

# function to print correlation estimators
corrEstimates<- function(Concussion.df){
  #' corrEstimates
  #'
  #' Function to print the correlation estimators
  #'
  #' @param dataframe with the x and y data
  #'
  #' @returns only prints the correlation estimators

  # printing correlation estimators
  cat("Computing correlation indicators.....", "\n" , "Covariance: ", cov(Concussion.df$Perc..of.TBIs, Concussion.df$Perc..of.concussions), "\n", "Correlation coefficient: ", cor(Concussion.df$Perc..of.TBIs, Concussion.df$Perc..of.concussions), "\n", "Correlation Test:", "\n")
  print(cor.test(Concussion.df$Perc..of.TBIs, Concussion.df$Perc..of.concussions))
}


# function to fit the linear model
linModel<- function(Concussion.df){
	#' linModel
	#'
	#' Performs the linear model fitting and prints the model details
	#'
	#' @param dataframe with x and y data
	#'
	#' @returns returns the linear model

	# printing the model
	cat("Fitting a Linear Model", "\n")

	# performing the linear model fitting with the data and printing the details
	lmodel<-lm(Concussion.df$Perc..of.TBIs ~ Concussion.df$Perc..of.concussions)
	print(summary(lmodel))

	# return the model
	return (lmodel)
}

# function to fit the quadratic model
quadModel<- function(Concussion.df){
	#' quadModel
	#'
	#' Performs the quadratic model fitting and prints the model details
	#'
	#' @param dataframe with x and y data
	#'
	#' @returns returns the quadractic model

	# printing the model
	cat("Fitting a Quadratic Model", "\n")

	# performing the quadratic model fitting with the data and printing the details
	concussionperc2<-Concussion.df$Perc..of.concussions^2
	qmodel<-lm(Perc..of.TBIs ~ Perc..of.concussions + concussionperc2, data=Concussion.df)
	print(summary(qmodel))

	# return the model
	return (qmodel)
}

# function to plot the model
plotModel <- function(Concussion.df, modelinput,  modeldata, num=2){
	#' plotModel
	#'
	#' Plots the given model data
	#'
	#' @param dataframe with x and y data
	#' @param the data from model for plotting
    #' @param the result file for data
    #' @param num to change the color
	#'
	#' @return plots the given model
	lines(modelinput, modeldata, lwd=2, col= num)
}

# function to analyze model 
AnalyseModel<-function(model, Concussion.df, tolLevel=0.25){
	
	#' AnalyseModel
	#'
	#' Function to analyse model for suspicious points for given tolerance level 
	#' Plots the diagnostic plots with suspicious points in red
	#'
	#' @param model made in the previous function
	#' @param data used to create the model
	#' @param Tolerance level for Cook's distance default at 25%
	#'
	#' return returns the list of suspicious points
	
	
	# finding leverage
	lev<-hat(model.matrix(model))

	# finding cook's distance
	cookDist<-cooks.distance(model)

	# finding suspicious points with Cook's Distance>tolLevel
	sus.pts<-which(cookDist>tolLevel)

	# plotting different plots - Histogram, Leverage, Cook's Distance, qqplot, residuals with points outside tolerance in red
	
	# histogram of the percent of TBIs
	hist(Concussion.df$Perc..of.TBIs)
	
	# histogram plot of Percent of Concussions
	hist(Concussion.df$Perc..of.concussions)
	
	# plotting Leverade
	plot(lev, ylab="Leverage")
	points(sus.pts, lev[sus.pts], col='red')
	
	# plotting Cook's Distance
	plot(cookDist, ylab="Cook's Distance")
	points(sus.pts, cookDist[sus.pts], col='red')	

	# plotting qqplot
	qqplot(model$res)
	qqline(model$res)

	# finding teh studentized residuals
	model.rs <- rstudent(model)
	
	# plotting residuals and explanatory variabless
	par(mfrow =c(1 ,3))	
	plot(Concussion.df$Perc..of.TBIs ,model$res)
	plot(Concussion.df$Perc..of.concussions ,model$res)
	plot(model$fitted , model$res)
	points(sus.pts, model$res[sus.pts], col='red')
	
	# plotting studentized residuals and explanatory variabless
	par(mfrow =c(1 ,3))	
	plot(Concussion.df$Perc..of.TBIs ,model.rs)
	plot(Concussion.df$Perc..of.concussions ,model.rs)
	plot(model$fitted , model.rs)
	points(sus.pts, model.rs[sus.pts], col='red')
	
	# returing the list of suspiciuous points after printing
	cat("The suspicious points are: ", sus.pts, "\n")
	
	return (sus.pts)
}