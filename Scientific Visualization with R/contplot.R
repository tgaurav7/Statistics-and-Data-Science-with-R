
xdata<-unique(vdata.df$Positionx)
ydata<-unique(vdata.df$Positiony)
zdata<-matrix(vdata.df$Velocityy, nrow=length(ydata), ncol=length(xdata))


nx<-length(xdata)
ny<-length(ydata)
nlevels<-25

pdf(file="ContourPlot.pdf", 3.375, 2, pointsize=6)

par(mai=c(0.45, 0.2, 0.05, 0.3))
col<-colorRampPalette(c('orange', 'blue'))(nlevels)

filled.contour(ydata, xdata,  zdata, col=col)


xlab=x[seq(1,nx, 10)]
ylab=y[seq(1, ny, 10)]
zlab=seq(0,1,length=9)


d.ax<-function(s, l1, lab, l2, f1, f2){
	axis(s, line=l1, label=NA, at=lab)
	axis(s, lwd=0, at=lab, line=12, label=as.character(lab*f1+f2))
}

d.ax(1, -0.65, xlab, -0.9, 10*(nx-1), 0)
title(xlab="metres", line=1.2, cex=1.1)
d.ax(2, -1, ylab, -1.4, 10*(ny-1), 0)
title(ylab="metres", line=0.7, cex=1.1)


#colorbar.plot(1.07, 0., levels, hor=F, strip.width=0.1, strip.length = 0.56, col=col, adj.y=0)

d.ax(4, 0, zlab, 0, 100, 95)
mtext("Height [m]", side=4, srt=90, cex=1.1, line=2.1)

dev.off()
