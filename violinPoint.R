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
	for(j in unique(x)){
		dense<-density(y,n=stepNum,...)
		groupSelector<-x==j
		if(!compare) randomJitter<-vanDerCorput(sum(groupSelector))[rank(y[groupSelector])]
		else randomJitter<-runif(sum(groupSelector),0,1)
		for(i in 1:stepNum){
			selector<-y[groupSelector]<=dense$x[i]&is.na(output[groupSelector])
			output[groupSelector][selector]<-(randomJitter[selector]*maxOffset*2-maxOffset)*dense$y[i]
		}
	}
	return(output)
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



