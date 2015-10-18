#' Offset data to avoid overplotting. 
#' 
#' Arranges data points using a van der Corput sequence to form "beeswarm" style
#' plots. Returns a vector of the offsets to be used in plotting.
#' 
#' @param y vector of data points 
#' @param x a grouping factor for y (optional)
#' @param width the maximum spacing away from center for each group of points. Since points are spaced to left and right, the maximum width of the cluster will be approximately width*2 (0 = no offset, default = 0.4)
#' @param varwidth adjust the width of each group based on the number of points in the group
#' @param ... additional arguments to offsetSingleGroup
#' @return a vector with of x-offsets of the same length as y
#' @export
#' @examples 
#' ## Generate fake data
#' dat <- list(rnorm(50), rnorm(500), c(rnorm(100), rnorm(100,5)), rcauchy(100))
#' names(dat) <- c("Normal", "Dense Normal", "Bimodal", "Extremes")
#' 
#' ## Plot each distribution with a variety of parameters
#' par(mfrow=c(4,1), mar=c(2,4, 0.5, 0.5))
#' sapply(names(dat),function(label) {
#'   y<-dat[[label]]
#'   ids <- rep(1:4, each=length(y))
#'   
#'   offsets <- c(
#'     offsetX(y),  # Default
#'     offsetX(y, adjust=2),    # More smoothing
#'     offsetX(y, adjust=0.1),  # Tighter fit
#'     offsetX(y, width=0.1))   # Less wide
#'   
#'   plot(offsets + ids, rep(y, 4), ylab=label, xlab='', xaxt='n', pch=21, las=1)
#'   axis(1, 1:4, c("Default", "Adjust=2", "Adjust=0.1", "Width=10%"))
#' })
#' 
offsetX <- function(y, x=rep(1, length(y)), width=0.4, varwidth=FALSE,...) {
  
  if (length(x)!=length(y)) stop(simpleError('x and y not the same length in offsetX'))
  
  maxLength<-max(table(x))

  # Apply the van der Corput noise to each x group to create offsets
  new_x <- aveWithArgs(y,x, FUN=offsetSingleGroup,maxLength=if(varwidth){maxLength}else{NULL},...)
  new_x <- new_x*width
  
  return(new_x)
}

aveWithArgs<-function(x, y, FUN = mean,...){
	if (missing(y)) 
		x[] <- FUN(x,...)
	else {
		g <- interaction(y)
		split(x, g) <- lapply(split(x, g), FUN,...)
	}
	x
}


#' Offset data to avoid overplotting for a single subgroup of data
#' 
#' Arranges data points using a van der Corput sequence, pseudorandom noise or
#' alternatively positioning extreme values within a band to the left and right to
#' form "beeswarm" style plots. Returns a vector of the offsets to be used in
#' plotting.
#' @param y_subgroup y values for a single group for which offsets should be calculated
#' @param maxLength multiply the offset by sqrt(length(y_subgroup)/maxLength) if not NULL. The sqrt is to match boxplot (allows comparison of order of magnitude different ns, scale with standard error)
#' @param method method used to distribute the points
#' @param nbins the number of points used to calculate density
#' @param adjust bandwidth used to adjust the density
#' @return a vector with of x-offsets between -1 and 1 of the same length as y
offsetSingleGroup<-function(y_subgroup,maxLength=NULL,method=c('quasirandom','pseudorandom','smiley','frowney'),nbins=1000,adjust=.5) {
	method<-match.arg(method)
	#catch 0 length inputs
	if (length(y_subgroup) == 0) return(NULL) 
	# If there's only one value in this group, leave it alone
	if (length(y_subgroup) == 1) return(0) 

	#sqrt to match boxplot (allows comparison of order of magnitude different ns, scale with standard error)
	if(is.null(maxLength)||maxLength<=0)subgroup_width <- 1
	else subgroup_width <- sqrt(length(y_subgroup)/maxLength)

	dens <- stats::density(y_subgroup, n = nbins, adjust = adjust)
	dens$y <- dens$y / max(dens$y)
	offset <- switch(method,
		'quasirandom'=vanDerCorput(length(y_subgroup))[rank(y_subgroup, ties.method="first")],
		'pseudorandom'=runif(length(y_subgroup)),
		stop(simpleError('Unrecognized method in offsetSingleGroup'))
	)

	pointDensities<-stats::approx(dens$x,dens$y,y_subgroup)$y

	#*2 to get -1 to 1
	out<-(offset-.5)*2*pointDensities*subgroup_width

	return(out)
}


#' Generate van der Corput sequences
#' 
#' Generates the first (or an arbitrary offset) n elements of the van der Corput low-discrepancy sequence for a given base
#' 
#' @param n the first n elements of the van der Corput sequence
#' @param base the base to use for calculating the van der Corput sequence
#' @param start start at this position in the sequence
#' @return a vector of length n with values ranging between 0 and 1
#' @references \url{https://en.wikipedia.org/wiki/Van_der_Corput_sequence}
#' @export
#' @examples
#' vanDerCorput(100)
vanDerCorput <- function(n, base=2,start=1){
  #generate n first digits of the van der Corput sequence
  if(n==0)return(c())
  if(n<0)stop(simpleError('n < 0 in vanDerCorput'))
  if(base<=1)stop(simpleError('base <=1 in vanDerCorput'))
  if(start<1)stop(simpleError('start < 1 in vanDerCorput'))
  out<-sapply(1:n+start-1,function(ii)digits2number(rev(number2digits(ii,base)),base,TRUE))
  return(out)
}

#' Convert an integer to an arbitrary base
#' 
#' Takes an integer and converts it into an arbitrary base e.g. binary or octal. Note that the first digit in the output is the least significant.
#' 
#' @param n the integer to be converted
#' @param base the base for the numeral system (e.g. 2 for binary or 8 for octal)
#' @return a vector of length ceiling(log(n+1,base)) respresenting each digit for that numeral system
#' @references \url{https://en.wikipedia.org/wiki/Radix}
#' @export
#' @examples
#' number2digits(100)
#' number2digits(100,8)
number2digits <- function(n, base=2){
  if(n==0)return(c())
  if(n<0)stop(simpleError('negative number in number2digits'))
  if(base<=1)stop(simpleError('base <=1 in number2digits'))
  nDigits<-ceiling(log(n+1,base))
  powers<-base^(0:nDigits)
  out<-diff(n %% powers)/powers[-length(powers)]
  return(out)
}

#' Convert a vector of integers representing digits in an arbitrary base to an integer
#' 
#' Takes a vector of integers representing digits in an arbitrary base e.g. binary or octal and converts it into an integer (or the integer divided by base^length(digits) for the number of digits if fractional is TRUE). Note that the first digit in the input is the least significant.
#' 
#' @param digits a vector of integers representing digits in an arbitrary base
#' @param base the base for the numeral system (e.g. 2 for binary or 8 for octal)
#' @param fractional divide the 
#' @return an integer
#' @references \url{https://en.wikipedia.org/wiki/Radix}
#' @export
#' @examples
#' digits2number(c(4,4,1),8)
#' digits2number(number2digits(100))
digits2number<-function(digits,base=2,fractional=FALSE){
  if(length(digits)==0)return(0)
  if(base<=0)stop(simpleError('base <= 0 in digits2number'))
  if(any(digits<0))stop(simpleError('digit < 0 in digits2number'))
  powers<-0:(length(digits)-1)
  out<-sum(digits*base^powers)
  if(fractional)out<-out/base^(length(digits))
  return(out)
}
