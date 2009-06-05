##File Name: violinPoint.R
##Creation Date: Jun 04, 2009
##Last Modified: Fri 05 Jun 2009 10:12:13 AM EDT
##Created By: scott
##Summary: Function to take a vector of points and calculate an adjustment to x so they can be plotted cleanly

require(fOptions)
spreadY<-function(y,x=rep(1,length(y)),maxOffset=.2,stepNum=100,compare=FALSE,...){
#thisNum<-20;groups<-6;x<-rep(1:groups,each=thisNum);y<-rnorm(thisNum*groups,rep(rnorm(groups),each=thisNum));jitter<-rep(NA,thisNum*groups);for(i in 1:groups){jitter[x==i]<-spreadY(y[x==i],maxOffset=.4)};plot(x+jitter,y)
	output<-rep(NA,length(y))
	require(fOptions)
	#low discrepancy "random"
	for(j in unique(x)){
		dense<-density(y,n=stepNum,...)
		groupSelector<-x==j
		if(!compare) randomJitter<-runif.halton(sum(groupSelector),1)[rank(y[groupSelector])]
		else randomJitter<-runif(sum(groupSelector),0,1)
		for(i in 1:stepNum){
			selector<-y[groupSelector]<=dense$x[i]&is.na(output[groupSelector])
			output[groupSelector][selector]<-(randomJitter[selector]*maxOffset*2-maxOffset)*dense$y[i]
		}
	}
	return(output)
}
