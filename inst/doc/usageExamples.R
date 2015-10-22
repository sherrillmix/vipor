### R code from vignette source 'usageExamples.Rnw'

###################################################
### code chunk number 1: package
###################################################
options(keep.source = TRUE, width = 60)
packageInfo <- packageDescription("violinpoint")
library(violinpoint)
packageKeywords<-"visualization, display, one dimensional, grouped, groups, violin, scatter, points, quasirandom, beeswarm, van der Corput"


###################################################
### code chunk number 2: vpPlot (eval = FALSE)
###################################################
##   set.seed(1234)
##   n<-100
##   dat<-rnorm(n*2)
##   labs<-rep(c('a','b'),n)
##   vpPlot(labs,dat)


###################################################
### code chunk number 3: showVpPlot
###################################################
  set.seed(1234)
  n<-100
  dat<-rnorm(n*2)
  labs<-rep(c('a','b'),n)
  vpPlot(labs,dat)


###################################################
### code chunk number 4: vpOpts (eval = FALSE)
###################################################
##   vpPlot(labs,dat,las=1,ylab='Data',col=rep(1:2,n))
##   abline(h=0,lty=2)


###################################################
### code chunk number 5: showVpOpts
###################################################
  vpPlot(labs,dat,las=1,ylab='Data',col=rep(1:2,n))
  abline(h=0,lty=2)


###################################################
### code chunk number 6: vpFactors (eval = FALSE)
###################################################
##   labs2<-factor(labs,levels=c('b','a'))
##   vpPlot(labs2,dat,las=1,ylab='Data',col=rep(1:2,n))
##   abline(h=0,lty=2)


###################################################
### code chunk number 7: showVpFactors
###################################################
  labs2<-factor(labs,levels=c('b','a'))
  vpPlot(labs2,dat,las=1,ylab='Data',col=rep(1:2,n))
  abline(h=0,lty=2)


###################################################
### code chunk number 8: offsetX
###################################################
	offsets<-offsetX(dat,labs)
	head(offsets,4)
	xPos<-vpPlot(labs,dat)
	head(xPos,4)
	xPos2<-rep(1:2,n)+offsets
	head(xPos2,4)
	all(xPos==xPos2)


