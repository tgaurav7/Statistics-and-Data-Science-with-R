	#' plot2d
	#'
	#' Loads the data and plots it in 2D format
	#' Here the data has x, y positions and velocities in x and y directions at each of these points
	#'
	#' @param dataframe - containing the data
	#'
	#' @return plots the 2D plots i.e. the line plot and the box plot of y-velocity with x near mid of the reactor (y-yavg)

	# setting axis label font size
	pdf(file="2dPlots.pdf", 3.375, 2, pointsize=6)
	# removing margins
	par(mai=c(0.28, 0.28, 0.03, 0.03))
	
	# defining the data for plotting
	xdata<- vdata.df$Positionx[vdata.df$Positiony=="0.7"]
	ydata<- vdata.df$Velocityy[vdata.df$Positiony=="0.7"]
	

	# plotting with symbol size and type
	plot(xdata, ydata, ann=F, axes=F, xlim=c(0.04, 0.145), pch=19, ylim=c(0, 0.9), cex=1.5)
	
	box()
	
	# defining the data for box plots
	xdata<- vdata.df$Positionx["0.6"<vdata.df$Positiony & vdata.df$Positiony<"0.9"]
	ydata<- vdata.df$Velocityy["0.6"<vdata.df$Positiony & vdata.df$Positiony<"0.9"]
	

	# adjust axes labels 
	axis(side=1, labels=NA)
	axis(side=1, lwd=0, line=-0.3)
	title(line=1.8, cex.lab=1.1, xlab=expression(paste(Radial_Distance, " [m]")))
	
	axis(side=2, labels=NA)
	axis(side=2, lwd=0, line=-0.4)
	title(line=1.5, cex.lab=1.1, ylab=expression(paste(Axial_Velocity, " [m/s]")))

	# plot the box plots with variation near the center of the reactor	
	boxplot(ydata~xdata, xlab=expression(paste(Radial_Distance, " [m]")), ylab=expression(paste(Axial_Velocity, " [m/s]")), col="lightgray")	
	dev.off()


