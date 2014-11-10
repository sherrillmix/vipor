##File Name: violinPoint.R
##Creation Date: Jun 04, 2009
##Last Modified: Fri 05 Jun 2009 07:28:16 PM EDT
##Created By: scott
##Summary: Function to take a vector of points and calculate an adjustment to x so they can be plotted cleanly

#spreadY: function to spread out points in a group for plotting based on kernel density
#y: data points
#x: groupings that will be jiggered
#maxOffset: maximum amount to spread (usually not reached)
#stepNum: number of breaks to use in density estimation
#compare: use normal runif for comparison to low discrepancy
#...: additional arguments to density
spreadY<-function(y,x=rep(1,length(y)),maxOffset=.4,stepNum=100,compare=FALSE,...){
	#thisNum<-20;groups<-6;x<-rep(1:groups,each=thisNum);y<-rnorm(thisNum*groups,rep(rnorm(groups),each=thisNum))+rep(c(0,2),length.out=groups*thisNum);plot(x+spreadY(y,x,maxOffset=.8),y)
	output<-rep(NA,length(y))
	#low discrepancy "random"
	maxDense<--Inf
	for(j in unique(x)){
		groupSelector<-x==j
		if(sum(groupSelector)==1){
			output[groupSelector]<-0
			next()
		}
		dense<-density(y[groupSelector],n=stepNum,...)
		dense$y<-dense$y/max(dense$y)
		#maxDense<-max(maxDense,dense$y)
		if(!compare) randomJitter<-vanDerCorput(sum(groupSelector))[rank(y[groupSelector],ties.method='first')]
		else randomJitter<-runif(sum(groupSelector),0,1)
		for(i in 1:stepNum){
			selector<-y[groupSelector]<=dense$x[i]&is.na(output[groupSelector])
			output[groupSelector][selector]<-(randomJitter[selector]*maxOffset*2-maxOffset)*dense$y[i]
		}
	}
	#output<-output/maxDense
	return(output)
}

#simple.violinplot from UsingR package
violinPoint<-function(values,samples,sampleOrder=unique(samples),xlab='Samples',ylab='Values',addRects=TRUE,addMedians=TRUE,...){
	idLookup<-1:length(sampleOrder)
	names(idLookup)<-sampleOrder
	dots<-list(...)
	plotPars<-names(par()) #this can create a new device but that should be ok since this is going to plot in a second anyway
	isPlotArg <- names(dots) %in% plotPars
	spreadXPos<-idLookup[samples]+do.call(spreadY,c(list(values),list(samples),dots[!isPlotArg]))
	do.call(plot,c(list(spreadXPos),list(values),dots[isPlotArg],xlab=xlab,ylab=ylab,xaxt='n'))
	axis(1,idLookup,names(idLookup),las=2)
	if(addRects)rect(seq(1,length(sampleOrder),2)-.5,par('usr')[3],seq(1,length(sampleOrder),2)+.5,par('usr')[4],col='#00000009',border=NA)
	if(addMedians){
		medians<-tapply(values,samples,median)
		maxOffset<-ifelse('maxOffset' %in% names(dots),dots[['maxOffset']],.4)
		segments(xPos[names(medians)]-maxOffset,medians,xPos[names(medians)]+maxOffset,medians)
	}
	invisible(idLookup)
}




#helper functions from https://stat.ethz.ch/pipermail/r-help/2008-May/162911.html
number2digits=function(n,base){
  #first digit in output is the least significant
  digit<-n%%base
  if (n<base)
    return(digit)
  else
    return(c(digit,number2digits((n-digit)/base,base)))
}

digits2number=function(digits,base){
  #first digit in input should be the most significant
  output<-0
  for (digit in digits) output<-(base*output)+digit
  return(output)
}

vanDerCorput=function(n,base=2){
  #generate n first digits of the van der Corput sequence
  output=rep(NA,n)
  for(i in 1:n){
    digits<-number2digits(i,base)
    output[i]<-digits2number(digits,base)/base^length(digits)
  }
  return(output)
}



