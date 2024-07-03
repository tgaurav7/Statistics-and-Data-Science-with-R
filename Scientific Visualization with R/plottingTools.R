# plot.Utilities.R
# functions to make the 2D, 3D and interactive plots given here

# the dataset includes velocity at different positions, both axial (along y axis) and radial (along x axis). Here, I'll only use the axial velocity data and the positions given. For 2D plot, The point plot will be at a line through the center of the reactor, and a box plot for studying variation in axial velocity data. For 3D plot, the contour of axial velocities is plotted to show the regions of low and high axial velocity 

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
  fulldata.df<-read.csv(file.name)

  # choosing x and y data from the csv file
  velocity.df<-fulldata.df[, c("Positionx", "Positiony", "Velocityy")]

  # returning the choosen dataframe
  return (velocity.df)
}

# function to plot the 2D plot with individual data along a line and box plot to observe the variation in axial velocity over a range in the middle of the reactor
plot2D <- function(vdata.df, line="0.5", range=c(0.6, 0.9)){
	#' plot2D
	#'
	#' Loads the data and plots it in 2D format
	#' Here the data has x, y positions and velocities in x and y directions at each of these points
	#'
	#' @param dataframe - containing the data
	#'
	#' @param value of y at which the line-data is to be plotted
	#'
	#' @param range of values for box plot
	#'
	#' @return plots the 2D plots i.e. the line plot and the box plot of y-velocity with x near mid of the reactor (y-yavg)

	# setting axis label font size
	pdf(file="2dPlots.pdf", 3.375, 2, pointsize=6)
	# removing margins
	par(mai=c(0.28, 0.28, 0.03, 0.03))
	
	# defining the data for plotting
	xdata<- vdata.df$Positionx[vdata.df$Positiony==line]
	ydata<- vdata.df$Velocityy[vdata.df$Positiony==line]
	

	# plotting with symbol size and type
	plot(xdata, ydata, ann=F, axes=F, xlim=c(0., 0.145), pch=19, ylim=c(-0.4, 0.4), cex=1.5)
	
	box()

	# defining the data for box plots
	xdata<- vdata.df$Positionx[range[1]<vdata.df$Positiony & vdata.df$Positiony<range[2]]
	ydata<- vdata.df$Velocityy[range[1]<vdata.df$Positiony & vdata.df$Positiony<range[2]]
	

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

}

plotCont<-function(vdata.df){
	#' plotCont
	#'
	#' Loads the data and plots the contour of data
	#' Data with x, y positions and velocities y direction at each of these points is plotted as contour 
	#'
	#' @param dataframe - containing the data
	#'
	#' @return contour of the axial velocity 
	
	# separatung the data out from the given dataframe for contour plots
	xdata<-unique(vdata.df$Positionx) # all x points
	ydata<-unique(vdata.df$Positiony) # all y points 
		cat("pt1")
	# at all nrow*ncol points 
	zdata<-matrix(vdata.df$Velocityy, nrow=length(ydata), ncol=length(xdata))
	
	nx<-length(xdata)
	ny<-length(ydata)
	
	# defining the number of levels
	nlevels<-25

	# saving the plot as pdf file
	pdf(file="contourPlot.pdf", 3.375, 1.8, pointsize=6)

	# defining the margins  and color palette for contour plot
	par(mai=c(0.32, 0.38, 0.05, 0.35))
	col<-colorRampPalette(c('orange', 'blue'))(nlevels)

	# plotting the data`
	filled.contour(ydata, xdata,  zdata, col=col)

	# defining the labels
	xlab=xdata[seq(1,nx, 10)]
	ylab=ydata[seq(1, ny, 10)]
	zlab=seq(0,1,length=11)

  cat("pt2")
	# function to associate the axes to the plot
	d.ax<-function(s, l1, lab, l2, f1, f2){
		axis(s, line=l1, label=NA, at=lab)
		axis(s, lwd=0, at=lab, line=12, label=as.character(lab*f1+f2))
	}
cat("pt3")
	# adding the y axis in the plot
	d.ax(1, -0.2, xlab, -0.9, 10*(nx-1), 0)
	title(xlab="Axial Length, [m]", line=2.2, cex=1.1)
	
	# adding the x axis in the plot
	d.ax(2, -1, ylab, -0.2, 10*(ny-1), 0)
	title(ylab="Raidal Length, [m]", line=2.8, cex=1.1)
	
	
	# adding the z axis in the plot - defining the data range for color palette
	d.ax(4, 0, zlab, 0, 100, 95)
	mtext("Axial Velocity [m/s]", side=4, srt=90, cex=1.1, line=2.1)
  cat("pt4")	
	dev.off()
}

plotInteractive<-function(vdata.df, line="0.7", range=c(0.6, 0.9)){
	#' plotInteractive
	#'
	#' Loads the data and makes interactive 2D and contour plots of data
	#' 
	#' @param dataframe - containing the data
	#'
	#' @param value of y at which the line-data is to be plotted
	#'
	#' @param range of values for box plot
	#'
	#' @return interactive 2D - scatter and box-plots and contour plots of the axial velocity 
	
	
    # defining the data for plotting
	xdata<- vdata.df$Positionx[vdata.df$Positiony==line]
	ydata<- vdata.df$Velocityy[vdata.df$Positiony==line]
	vdata<-data.frame(xdata, ydata)

	
	# 2D scatter plot with plotly of Velocityy varying along xdata
	library(plotly)
	p1handler<-plot_ly(data=vdata, x=~xdata, y=~ydata)

	# saving the plot as html
	htmlwidgets::saveWidget(as.widget(p1handler), "scatterPlot_Velocity.html")
	
	# defining the data for box plots
	xdata<- vdata.df$Positionx[range[1]<vdata.df$Positiony & vdata.df$Positiony<range[2]]
	ydata<- vdata.df$Velocityy[range[1]<vdata.df$Positiony & vdata.df$Positiony<range[2]]
	
	vdata<-data.frame(xdata, ydata)

	# plot boxplot 
	p2handler <- plot_ly(vdata, x = ~ydata, color = ~xdata, type = "box")
	
	# saving the plot as html
	htmlwidgets::saveWidget(as.widget(p2handler), "boxPlot_Velocity.html")
	
	# contour plots
	# separatung the data out from the given dataframe for contour plots
	xdata<-unique(vdata.df$Positionx) # all x points
	ydata<-unique(vdata.df$Positiony) # all y points 
		
	# at all nrow*ncol points 
	zdata<-matrix(vdata.df$Velocityy, nrow=length(ydata), ncol=length(xdata))
	
	nx<-length(xdata)
	ny<-length(ydata)
	
	p3handler<-plot_ly(x=~xdata, y=~ydata, z=~zdata, type="contour")
	
	# saving the contour plot as html
	htmlwidgets::saveWidget(as.widget(p3handler), "ContourPlot_Velocity.html")
	
}
