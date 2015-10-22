### R code from vignette source 'methodComparison.Rnw'

###################################################
### code chunk number 1: package
###################################################
options(keep.source = TRUE, width = 60)
packageInfo <- packageDescription("violinpoint")
library(violinpoint)
packageKeywords<-"visualization, display, one dimensional, grouped, groups, violin, scatter, points, quasirandom, beeswarm, van der Corput, beanplot"


###################################################
### code chunk number 2: simData
###################################################
  library(beeswarm)
  library(violinpoint)
  set.seed(12345)

  dat <- list(rnorm(50), rnorm(500), c(rnorm(100), rnorm(100,5)), rcauchy(100))
  names(dat) <- c("Normal", "Dense Normal", "Bimodal", "Extremes")


###################################################
### code chunk number 3: simmed (eval = FALSE)
###################################################
##   par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
##   dummy<-sapply(names(dat),function(label) {
##     y<-dat[[label]]
##     offsets <- list(
##       'Quasi'=offsetX(y),  # Default
##       'Pseudo'=offsetX(y, method='pseudorandom',nbins=100),
##       'Frown'=offsetX(y, method='frowney',nbins=20),
##       'Smile\n20 bin'=offsetX(y, method='smiley',nbins=20),
##       'Smile\n100 bin'=offsetX(y, method='smiley',nbins=100),
##       'Smile\nn/5 bin'=offsetX(y, method='smiley',nbins=round(length(y)/5)),
##       'Beeswarm'=swarmx(rep(0,length(y)),y)$x
##     )
##     ids <- rep(1:length(offsets), each=length(y))
##     plot(unlist(offsets) + ids, rep(y, length(offsets)),
##         xlab='', xaxt='n', pch=21,las=1,main=label, ylab='y value',
##         col='#00000099',bg='#00000033')
##   par(lheight=.8)
##   axis(1, 1:length(offsets), names(offsets),padj=1,mgp=c(0,-.3,0),tcl=-.5)
## })


###################################################
### code chunk number 4: plotSimmed
###################################################
  par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
  dummy<-sapply(names(dat),function(label) {
    y<-dat[[label]]
    offsets <- list(
      'Quasi'=offsetX(y),  # Default
      'Pseudo'=offsetX(y, method='pseudorandom',nbins=100),
      'Frown'=offsetX(y, method='frowney',nbins=20),
      'Smile\n20 bin'=offsetX(y, method='smiley',nbins=20),
      'Smile\n100 bin'=offsetX(y, method='smiley',nbins=100),
      'Smile\nn/5 bin'=offsetX(y, method='smiley',nbins=round(length(y)/5)),
      'Beeswarm'=swarmx(rep(0,length(y)),y)$x
    )
    ids <- rep(1:length(offsets), each=length(y))
    plot(unlist(offsets) + ids, rep(y, length(offsets)),
        xlab='', xaxt='n', pch=21,las=1,main=label, ylab='y value',
        col='#00000099',bg='#00000033')
  par(lheight=.8)
  axis(1, 1:length(offsets), names(offsets),padj=1,mgp=c(0,-.3,0),tcl=-.5)
})


###################################################
### code chunk number 5: vpOrchard (eval = FALSE)
###################################################
##   vpPlot(
##     OrchardSprays$treatment,log(OrchardSprays$decrease),
## 	 las=1, ylab='Log treatment effect',
## 	 offsetXArgs=list(width=.2),
##     pch=21, col='#00000099',bg='#00000033'
##   )
##   means<-tapply(log(OrchardSprays$decrease),OrchardSprays$treatment,mean)
##   segments(
##     1:length(means)-.3,means,1:length(means)+.3,means,
##     col='#FF000099',lwd=2
##   )
##   title(xlab='Treatment')


###################################################
### code chunk number 6: showVpOrchard
###################################################
  vpPlot(
    OrchardSprays$treatment,log(OrchardSprays$decrease),
	 las=1, ylab='Log treatment effect',
	 offsetXArgs=list(width=.2),
    pch=21, col='#00000099',bg='#00000033'
  )
  means<-tapply(log(OrchardSprays$decrease),OrchardSprays$treatment,mean)
  segments(
    1:length(means)-.3,means,1:length(means)+.3,means,
    col='#FF000099',lwd=2
  )
  title(xlab='Treatment')


###################################################
### code chunk number 7: vpSinger (eval = FALSE)
###################################################
##   data('singer',package='lattice')
##   part<-sub(' [0-9]+$','',singer$voice)
##   vpPlot(
##     part,singer$height,
## 	 las=1, ylab='Height',
##     pch=21, col='#00000099',bg='#00000033'
##   )
##   means<-tapply(singer$height,part,mean)
##   segments(
##     1:length(means)-.3,means,1:length(means)+.3,means,
##     col='#FF000099',lwd=2
##   )
##   title(xlab='Treatment')


###################################################
### code chunk number 8: showVpSinger
###################################################
  data('singer',package='lattice')
  part<-sub(' [0-9]+$','',singer$voice)
  vpPlot(
    part,singer$height,
	 las=1, ylab='Height',
    pch=21, col='#00000099',bg='#00000033'
  )
  means<-tapply(singer$height,part,mean)
  segments(
    1:length(means)-.3,means,1:length(means)+.3,means,
    col='#FF000099',lwd=2
  )
  title(xlab='Treatment')


###################################################
### code chunk number 9: vpBeaver (eval = FALSE)
###################################################
##   y<-c(beaver1$temp,beaver2$temp)
##   x<-rep(
##     c('Beaver 1','Beaver 2'),
##     c(nrow(beaver1),nrow(beaver2))
##   )
##   vpPlot(x,y,las=1, ylab='Body temperature',
##     pch=21, col='#00000099',bg='#00000033'
##   )


###################################################
### code chunk number 10: showBeaver
###################################################
  y<-c(beaver1$temp,beaver2$temp)
  x<-rep(
    c('Beaver 1','Beaver 2'),
    c(nrow(beaver1),nrow(beaver2))
  )
  vpPlot(x,y,las=1, ylab='Body temperature',
    pch=21, col='#00000099',bg='#00000033'
  )


