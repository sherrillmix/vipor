### R code from vignette source 'methodComparison.Rnw'

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
##   dat<-rnorm(100)
##   vpPlot(rep(c('a','b'),50),dat)


###################################################
### code chunk number 3: showVpPlot
###################################################
  set.seed(1234)
  dat<-rnorm(100)
  vpPlot(rep(c('a','b'),50),dat)


