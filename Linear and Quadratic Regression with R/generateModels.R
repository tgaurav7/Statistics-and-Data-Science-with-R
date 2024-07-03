# generateModels.R

# source the utility file
source("Fitting.Utilities.R")

# get the commandline arguments
args<-commandArgs(trailingOnly=TRUE)

# check for length of argument
if(length(args)!=2){stop("Two arguments are needed")}

# check for the second argument
if(!((args[2]==1)|(args[2]==2)|(args[2]==3))){{stop(" The second argument should be either 1, 2 or 3")}}

# getting the x-y dataframe from the file
Concussion.df<- recData(args[1])

# printing the correlation estimators
corrEstimates(Concussion.df)

# based on the value of second argument defining the model
if((args[2]==1)|(args[2]==11)|(args[2]==21)){
	# for linear model
	lm<-linModel(Concussion.df)

	# getting the sequence for inner product
	modelinput <- seq(min(Concussion.df$Perc..of.concussions), max(Concussion.df$Perc..of.concussions), len=length(Concussion.df$Perc..of.concussions))

	# using the model coefficients to calculate corresponding result data
	modeldata <- lm$coef %*% rbind(1, modelinput)

	# plotting the points
	plot(Concussion.df$Perc..of.concussions, Concussion.df$Perc..of.TBIs)

	# plotting the model
	plotModel(lm, modelinput, modeldata)
	
	# for analysing and performing modelling on clean data based on argument
	if((args[2]==11)|(args[2]==21)){
		# analyze model 
		sus.pts = AnalyseModel(lm, Concussion.df, 0.15)
	
		if(args[2]==21){
			clean.Data<-Concussion.df[-sus.pts, ]
			
			# for linear model
			lm<-linModel(clean.Data)

			# getting the sequence for inner product
			lmodelinput <- seq(min(clean.Data$Perc..of.concussions), max(clean.Data$Perc..of.concussions), len=length(clean.Data$Perc..of.concussions))

			# using the model coefficients to calculate corresponding result data
			lmodeldata <- lm$coef %*% rbind(1, lmodelinput)

			# plotting the points
			plot(clean.Data$Perc..of.concussions, clean.Data$Perc..of.TBIs, ylab="Model with clean data")

			# plotting the model
			plotModel(lm, lmodelinput, lmodeldata)			
		}
	}
	
} else if ((args[2]==2)|(args[2]==12)|(args[2]==22)) {
	# for quadratic model
	qm<-quadModel(Concussion.df)

	# getting the sequence for inner product
	modelinput <- seq(min(Concussion.df$Perc..of.concussions), max(Concussion.df$Perc..of.concussions), len=length(Concussion.df$Perc..of.concussions))

	# using the model coefficients to calculate corresponding result data
	modeldata <- qm$coef %*% rbind(1, modelinput, modelinput^2)


	# plotting points if only quadratic model was called
	plot(Concussion.df$Perc..of.concussions, Concussion.df$Perc..of.TBIs)

	# plotting the model
	plotModel(qm, modelinput, modeldata, 2)
	
	# for analysing and performing modelling on clean data based on argument
	if((args[2]==11)|(args[2]==21)){
		
		# analyze model 
		sus.pts = AnalyseModel(qm, Concussion.df, 0.15)
		
		if(args[2]==22){
			clean.Data<-Concussion.df[-sus.pts, ]
			# for quadratic model
			qm<-quadModel(cleanData.df)

			# getting the sequence for inner product
			qmodelinput <- seq(min(clean.Data$Perc..of.concussions), max(clean.Data$Perc..of.concussions), len=length(clean.Data$Perc..of.concussions))

			# using the model coefficients to calculate corresponding result data
			qmodeldata <- qm$coef %*% rbind(1, qmodelinput, qmodelinput^2)

			# using the model coefficients to calculate corresponding result data
			qmodeldata <- qm$coef %*% rbind(1, qmodelinput, qmodelinput^2)
			
			# plotting the points
			plot(clean.Data$Perc..of.concussions, clean.Data$Perc..of.TBIs, ylab="Model with Clean Data")
			
			# plotting the model
			plotModel(qm, qmodelinput, qmodeldata, 3)
			
		}
	}
	
	
} else if(args[2]==3) {

	# for linear model
	lm<-linModel(Concussion.df)

	# getting the sequence for inner product
	lmodelinput <- seq(min(Concussion.df$Perc..of.concussions), max(Concussion.df$Perc..of.concussions), len=length(Concussion.df$Perc..of.concussions))

	# using the model coefficients to calculate corresponding result data
	lmodeldata <- lm$coef %*% rbind(1, lmodelinput)

	# plotting the points
	plot(Concussion.df$Perc..of.concussions, Concussion.df$Perc..of.TBIs)

	# plotting the model
	plotModel(lm, lmodelinput, lmodeldata)

	# for quadratic model
	qm<-quadModel(Concussion.df)

	# getting the sequence for inner product
	qmodelinput <- seq(min(Concussion.df$Perc..of.concussions), max(Concussion.df$Perc..of.concussions), len=length(Concussion.df$Perc..of.concussions))

	# using the model coefficients to calculate corresponding result data
	qmodeldata <- qm$coef %*% rbind(1, qmodelinput, qmodelinput^2)


	# plotting the model
	plotModel(qm, qmodelinput, qmodeldata, 3)

} 

# since the Percent of TBIs and Percent of Concussion data is being used, the same set of functions are called again. 
