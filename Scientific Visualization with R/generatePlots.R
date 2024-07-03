# generatePlots.R
# Plots the data based on the argument provided.

# the dataset includes velocity at different positions, both axial (along y axis) and radial (along x axis). 
# Here, I'll only use the axial velocity data and the positions given. For 2D plot, The point plot will be at a line through the center of the reactor, 
# and a box plot for studying variation in axial velocity data. 
# For 3D plot, the contour of axial velocities is plotted to show the regions of low and high axial velocity 

# source the utility file
source("plottingTools.R")

# get the commandline arguments
args<-commandArgs(trailingOnly=TRUE)

# check for length of argument
if(length(args)!=2){stop("Only two arguments are needed")}

# check for the second argument
if(!((args[2]=="2D")|(args[2]=="3D")|(args[2]=="interactive"))){{stop(" The second argument should be either 2D, 3D or Interactive")}}

# getting the x-y dataframe from the file
vdata.df<- recData(args[1])
# defining the parameters of line and range for 2D plotting
	line="0.7"
	range=c(0.55, 0.85)
# based on the value of second argument defining the model
if(args[2]=="2D"){
	
	
	# calling the plotting function with given line and range values 
	plot2D(vdata.df, line, range)
} else if (args[2]=="3D"){
	# Calling the contour plotting function with the dataframe
	plotCont(vdata.df)
} else if (args[2]=="interactive") {
	# defining the parameters of line and range for 2D plotting
	line=0.75
	range=c(0.55, 0.85)
	
	# plotting both the interactive 2D and contour plots
	plotInteractive(vdata.df, line, range)
}