### R code from vignette source 'methodComparison.Rnw'

###################################################
### code chunk number 1: package
###################################################
options(keep.source = TRUE, width = 60)
packageInfo <- packageDescription("vipor")
library(vipor)
packageKeywords<-"visualization, display, one dimensional, grouped, groups, violin, scatter, points, quasirandom, beeswarm, van der Corput, beanplot"


###################################################
### code chunk number 2: simData
###################################################
  library(vipor)
  library(beeswarm)
  library(beanplot)
  library(vioplot)
  set.seed(12345)

  dat <- list(rnorm(50), rnorm(500), c(rnorm(100),
    rnorm(100,5)), rcauchy(100))
  names(dat) <- c("Normal", "Dense Normal", "Bimodal", "Extremes")


###################################################
### code chunk number 3: simmed (eval = FALSE)
###################################################
##   par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
##   dummy<-sapply(names(dat),function(label) {
##     y<-dat[[label]]
##     # need to plot first so beeswarm can figure out pars
##     # xlim is a magic number due to needing plot for beeswarm
##     plot(1,1,type='n',xlab='',xaxt='n',ylab='y value',las=1,main=label,
##       xlim=c(0.5,9.5),ylim=range(y))
##     offsets <- list(
##       'Quasi'=offsetX(y),  # Default
##       'Pseudo'=offsetX(y, method='pseudorandom',nbins=100),
##       'Min out'=offsetX(y, method='minout',nbins=20),
##       'Max out\n20 bin'=offsetX(y, method='maxout',nbins=20),
##       'Max out\n100 bin'=offsetX(y, method='maxout',nbins=100),
##       'Max out\nn/5 bin'=offsetX(y, method='maxout',nbins=round(length(y)/5)),
##       'Beeswarm'=swarmx(rep(0,length(y)),y)$x,
##       'Tukey'=offsetX(y,method='tukey'),
##       'Tukey +\ndensity'=offsetX(y,method='tukeyDense')
##     )
##     ids <- rep(1:length(offsets), each=length(y))
##     points(unlist(offsets) + ids, rep(y, length(offsets)),
##           pch=21,col='#00000099',bg='#00000033')
##   par(lheight=.8)
##   axis(1, 1:length(offsets), names(offsets),padj=1,
##     mgp=c(0,-.1,0),tcl=-.5,cex.axis=1.1)
## })


###################################################
### code chunk number 4: plotSimmed
###################################################
  par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
  dummy<-sapply(names(dat),function(label) {
    y<-dat[[label]]
    # need to plot first so beeswarm can figure out pars
    # xlim is a magic number due to needing plot for beeswarm
    plot(1,1,type='n',xlab='',xaxt='n',ylab='y value',las=1,main=label,
      xlim=c(0.5,9.5),ylim=range(y))
    offsets <- list(
      'Quasi'=offsetX(y),  # Default
      'Pseudo'=offsetX(y, method='pseudorandom',nbins=100),
      'Min out'=offsetX(y, method='minout',nbins=20),
      'Max out\n20 bin'=offsetX(y, method='maxout',nbins=20),
      'Max out\n100 bin'=offsetX(y, method='maxout',nbins=100),
      'Max out\nn/5 bin'=offsetX(y, method='maxout',nbins=round(length(y)/5)),
      'Beeswarm'=swarmx(rep(0,length(y)),y)$x,
      'Tukey'=offsetX(y,method='tukey'),
      'Tukey +\ndensity'=offsetX(y,method='tukeyDense')
    )
    ids <- rep(1:length(offsets), each=length(y))
    points(unlist(offsets) + ids, rep(y, length(offsets)),
          pch=21,col='#00000099',bg='#00000033')
  par(lheight=.8)
  axis(1, 1:length(offsets), names(offsets),padj=1,
    mgp=c(0,-.1,0),tcl=-.5,cex.axis=1.1)
})


###################################################
### code chunk number 5: simmedBox (eval = FALSE)
###################################################
##   x<-rep(names(dat),sapply(dat,length))
##   y<-unlist(lapply(dat,function(x)x/max(abs(x))))
##   par(mfrow=c(4,1), mar=c(6,4.5, 1.2, 0.5),mgp=c(3.3,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
##   vpPlot(x,y, ylab='',cex=.7, pch=21,
##     col='#00000044',bg='#00000011')
##   boxplot(y~x,main='Boxplot',ylab='')
##   beanplot(y~x,main='Beanplot',ylab='')
##   vioInput<-split(y,x)
##   labs<-names(vioInput)
##   names(vioInput)[1]<-'x'
##   do.call(vioplot,c(vioInput,list(names=labs,col='white')))
##   title(main='Vioplot')


###################################################
### code chunk number 6: plotSimmedBox
###################################################
  x<-rep(names(dat),sapply(dat,length))
  y<-unlist(lapply(dat,function(x)x/max(abs(x))))
  par(mfrow=c(4,1), mar=c(6,4.5, 1.2, 0.5),mgp=c(3.3,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
  vpPlot(x,y, ylab='',cex=.7, pch=21,
    col='#00000044',bg='#00000011')
  boxplot(y~x,main='Boxplot',ylab='')
  beanplot(y~x,main='Beanplot',ylab='')
  vioInput<-split(y,x)
  labs<-names(vioInput)
  names(vioInput)[1]<-'x'
  do.call(vioplot,c(vioInput,list(names=labs,col='white')))
  title(main='Vioplot')


###################################################
### code chunk number 7: vpCounties
###################################################
  y<-log10(counties$landArea)
  offsets <- list(
    'Quasi'=offsetX(y),  # Default
    'Pseudo'=offsetX(y, method='pseudorandom',nbins=100),
    'Min out'=offsetX(y, method='minout',nbins=20),
    'Max out\n20 bin'=offsetX(y, method='maxout',nbins=20),
    'Max out\n100 bin'=offsetX(y, method='maxout',nbins=100),
    'Max out\nn/5 bin'=offsetX(y, method='maxout',nbins=round(length(y)/5)),
    'Beeswarm'=swarmx(rep(0,length(y)),y)$x,
    'Tukey'=offsetX(y,method='tukey'),
    'Tukey +\ndensity'=offsetX(y,method='tukeyDense')
  )
  ids <- rep(1:length(offsets), each=length(y))

  #reduce file size by rendering to raster
  tmpPng<-tempfile(fileext='.png')
  png(tmpPng,height=1200,width=1800,res=300)
  par(mar=c(2.5,3.5,.2,0.2))
  plot(
    unlist(offsets) + ids, rep(y, length(offsets)),
    xlab='', xaxt='n', yaxt='n',pch='.',
    ylab='Land area (square miles)',mgp=c(2.7,1,0),
    col='#00000077'
  )
  par(lheight=.8)
  axis(1, 1:length(offsets), names(offsets),padj=1,
    mgp=c(0,-.3,0),tcl=-.3,cex.axis=.65)
  axis(2, pretty(y), format(10^pretty(y),scientific=FALSE,big.mark=','),
    mgp=c(0,.5,0),tcl=-.3,las=1,cex.axis=.75)
  dev.off()


###################################################
### code chunk number 8: vpOrchard (eval = FALSE)
###################################################
##   par(mfrow=c(5,1), mar=c(3.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
##   #simple function to avoid repeating code
##   plotFunc<-function(x,y,offsetXArgs){
##     vpPlot(x,y, ylab='Log treatment effect', pch=21,
##       col='#00000099',bg='#00000033', offsetXArgs=offsetXArgs)
##     title(xlab='Treatment')
##     addMeanLines(x,y)
##   }
##   addMeanLines<-function(x,y,col='#FF000099'){
##     means<-tapply(y,x,mean)
##     segments(
##      1:length(means)-.25,means,1:length(means)+.25,means,
##      col=col,lwd=2
##     )
##   }
##   #quasirandom
##   plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
##     list(width=.2))
##   title(main='Quasirandom')
##   #pseudorandom
##   plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
##     list(method='pseudo',width=.2))
##   title(main='Pseudorandom')
##   #smiley
##   plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
##     list(method='maxout',width=.2))
##   title(main='Max outside')
##   #beeswarm
##   beeInput<-split(log(OrchardSprays$decrease), OrchardSprays$treatment)
##   beeswarm(beeInput,las=1,ylab='Log treatment effect',xlab='Treatment',
##     pch=21, col='#00000099',bg='#00000033', main='Beeswarm')
##   addMeanLines(OrchardSprays$treatment,log(OrchardSprays$decrease))
##   plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
##     list(method='tukey',width=.2))
##   title(main='Tukey')


###################################################
### code chunk number 9: showVpOrchard
###################################################
  par(mfrow=c(5,1), mar=c(3.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
  #simple function to avoid repeating code
  plotFunc<-function(x,y,offsetXArgs){
    vpPlot(x,y, ylab='Log treatment effect', pch=21,
      col='#00000099',bg='#00000033', offsetXArgs=offsetXArgs)
    title(xlab='Treatment')
    addMeanLines(x,y)
  }
  addMeanLines<-function(x,y,col='#FF000099'){
    means<-tapply(y,x,mean)
    segments(
     1:length(means)-.25,means,1:length(means)+.25,means,
     col=col,lwd=2
    )
  }
  #quasirandom
  plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
    list(width=.2))
  title(main='Quasirandom')
  #pseudorandom
  plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
    list(method='pseudo',width=.2))
  title(main='Pseudorandom')
  #smiley
  plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
    list(method='maxout',width=.2))
  title(main='Max outside')
  #beeswarm
  beeInput<-split(log(OrchardSprays$decrease), OrchardSprays$treatment)
  beeswarm(beeInput,las=1,ylab='Log treatment effect',xlab='Treatment',
    pch=21, col='#00000099',bg='#00000033', main='Beeswarm')
  addMeanLines(OrchardSprays$treatment,log(OrchardSprays$decrease))
  plotFunc(OrchardSprays$treatment,log(OrchardSprays$decrease),
    list(method='tukey',width=.2))
  title(main='Tukey')


###################################################
### code chunk number 10: vpSinger (eval = FALSE)
###################################################
##   data('singer',package='lattice')
##   parts<-sub(' [0-9]+$','',singer$voice)
##   par(mfrow=c(5,1), mar=c(3.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
##   #simple function to avoid repeating code
##   plotFunc<-function(x,y,...){
##     vpPlot(x,y, ylab='Height',pch=21,col='#00000099',bg='#00000033',...)
##     addMeanLines(x,y)
##   }
##   #quasirandom
##   plotFunc(parts,singer$height,
##     main='Quasirandom')
##   #pseudorandom
##   plotFunc(parts,singer$height,offsetXArgs=list(method='pseudo'),
##     main='Pseudorandom')
##   #smiley
##   plotFunc(parts,singer$height,offsetXArgs=list(method='maxout'),
##     main='Max outside')
##   #beeswarm
##   beeInput<-split(singer$height, parts)
##   beeswarm(beeInput,ylab='Height',main='Beeswarm',
##     pch=21, col='#00000099',bg='#00000033')
##   addMeanLines(parts,singer$height)
##   #tukey
##   plotFunc(parts,singer$height,offsetXArgs=list(method='tukey'),
##     main='Tukey')


###################################################
### code chunk number 11: showVpSinger
###################################################
  data('singer',package='lattice')
  parts<-sub(' [0-9]+$','',singer$voice)
  par(mfrow=c(5,1), mar=c(3.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
  #simple function to avoid repeating code
  plotFunc<-function(x,y,...){
    vpPlot(x,y, ylab='Height',pch=21,col='#00000099',bg='#00000033',...)
    addMeanLines(x,y)
  }
  #quasirandom
  plotFunc(parts,singer$height,
    main='Quasirandom')
  #pseudorandom
  plotFunc(parts,singer$height,offsetXArgs=list(method='pseudo'),
    main='Pseudorandom')
  #smiley
  plotFunc(parts,singer$height,offsetXArgs=list(method='maxout'),
    main='Max outside')
  #beeswarm
  beeInput<-split(singer$height, parts)
  beeswarm(beeInput,ylab='Height',main='Beeswarm',
    pch=21, col='#00000099',bg='#00000033')
  addMeanLines(parts,singer$height)
  #tukey
  plotFunc(parts,singer$height,offsetXArgs=list(method='tukey'),
    main='Tukey')


###################################################
### code chunk number 12: vpBeaver (eval = FALSE)
###################################################
##   y<-c(beaver1$temp,beaver2$temp)
##   x<-rep(c('Beaver 1','Beaver 2'), c(nrow(beaver1),nrow(beaver2)))
##   par(mfrow=c(3,2), mar=c(3.5,4.5, 1.2, 0.5),mgp=c(3,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
##   #simple function to avoid repeating code
##   plotFunc<-function(x,y,...){
##     vpPlot(x,y, las=1, ylab='Body temperature',pch=21,
##       col='#00000099',bg='#00000033',...)
##     addMeanLines(x,y)
##   }
##   #quasirandom
##   plotFunc(x,y,main='Quasirandom')
##   #pseudorandom
##   plotFunc(x,y,offsetXArgs=list(method='pseudo'),main='Pseudorandom')
##   #smiley
##   plotFunc(x,y,offsetXArgs=list(method='maxout'),main='Max outside')
##   #beeswarm
##   beeInput<-split(y,x)
##   beeswarm(beeInput,las=1,ylab='Body temperature',main='Beeswarm',
##     pch=21, col='#00000099',bg='#00000033')
##   addMeanLines(x,y)
##   #tukey
##   plotFunc(x,y,offsetXArgs=list(method='tukey'),main='Tukey')


###################################################
### code chunk number 13: showBeaver
###################################################
  y<-c(beaver1$temp,beaver2$temp)
  x<-rep(c('Beaver 1','Beaver 2'), c(nrow(beaver1),nrow(beaver2)))
  par(mfrow=c(3,2), mar=c(3.5,4.5, 1.2, 0.5),mgp=c(3,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
  #simple function to avoid repeating code
  plotFunc<-function(x,y,...){
    vpPlot(x,y, las=1, ylab='Body temperature',pch=21,
      col='#00000099',bg='#00000033',...)
    addMeanLines(x,y)
  }
  #quasirandom
  plotFunc(x,y,main='Quasirandom')
  #pseudorandom
  plotFunc(x,y,offsetXArgs=list(method='pseudo'),main='Pseudorandom')
  #smiley
  plotFunc(x,y,offsetXArgs=list(method='maxout'),main='Max outside')
  #beeswarm
  beeInput<-split(y,x)
  beeswarm(beeInput,las=1,ylab='Body temperature',main='Beeswarm',
    pch=21, col='#00000099',bg='#00000033')
  addMeanLines(x,y)
  #tukey
  plotFunc(x,y,offsetXArgs=list(method='tukey'),main='Tukey')


###################################################
### code chunk number 14: vpStock (eval = FALSE)
###################################################
##   y<-as.vector(EuStockMarkets)
##   x<-rep(colnames(EuStockMarkets), each=nrow(EuStockMarkets))
##   par(mfrow=c(3,2), mar=c(4,4.3, 1.2, 0.5),mgp=c(3.3,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
##   #simple function to avoid repeating code
##   plotFunc<-function(x,y,...){
##     vpPlot(x,y, ylab='Price',cex=.7,cex.axis=.7,
##       mgp=c(2.5,.75,0),tcl=-.4, pch=21,
##       col='#00000011',bg='#00000011',...)
##     addMeanLines(x,y)
##   }
##   #quasirandom
##   plotFunc(x,y,main='Quasirandom')
##   #pseudorandom
##   plotFunc(x,y,offsetXArgs=list(method='pseudo'),main='Pseudorandom')
##   #smiley
##   plotFunc(x,y,offsetXArgs=list(method='maxout'),main='Max outside')
##   #beeswarm
##   #beeInput<-split(y,x)
##   beeswarm(EuStockMarkets[,'DAX',drop=FALSE],cex=.7, ylab='Price',
##     main='Beeswarm',pch=21, col='#00000099',bg='#00000033',cex.axis=.7)
##   #tukey
##   plotFunc(x,y,offsetXArgs=list(method='tukey'),main='Tukey')


###################################################
### code chunk number 15: showStock
###################################################
  y<-as.vector(EuStockMarkets)
  x<-rep(colnames(EuStockMarkets), each=nrow(EuStockMarkets))
  par(mfrow=c(3,2), mar=c(4,4.3, 1.2, 0.5),mgp=c(3.3,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
  #simple function to avoid repeating code
  plotFunc<-function(x,y,...){
    vpPlot(x,y, ylab='Price',cex=.7,cex.axis=.7,
      mgp=c(2.5,.75,0),tcl=-.4, pch=21,
      col='#00000011',bg='#00000011',...)
    addMeanLines(x,y)
  }
  #quasirandom
  plotFunc(x,y,main='Quasirandom')
  #pseudorandom
  plotFunc(x,y,offsetXArgs=list(method='pseudo'),main='Pseudorandom')
  #smiley
  plotFunc(x,y,offsetXArgs=list(method='maxout'),main='Max outside')
  #beeswarm
  #beeInput<-split(y,x)
  beeswarm(EuStockMarkets[,'DAX',drop=FALSE],cex=.7, ylab='Price',
    main='Beeswarm',pch=21, col='#00000099',bg='#00000033',cex.axis=.7)
  #tukey
  plotFunc(x,y,offsetXArgs=list(method='tukey'),main='Tukey')


###################################################
### code chunk number 16: vpInts
###################################################
  ints<-integrations[integrations$nearestGene>0,]
  y<-log10(ints$nearestGene)
  x<-paste(ints$latent,ints$study,sep='\n')

  #reduce file size by rendering to raster
  tmpPng<-tempfile(fileext='.png')
  png(tmpPng,height=2400,width=1500,res=300)
  par(mfrow=c(4,1), mar=c(7.5,3.5, 1.2, 0.5),mgp=c(2.5,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
  #simple function to avoid repeating code
  plotFunc<-function(x,y,...){
    cols<-ifelse(grepl('Expressed',x),'#FF000033','#0000FF33')
    vpPlot(x,y,las=2, ylab='Distance to gene',cex=.7,yaxt='n',
      pch=21, col=NA,bg=cols,lheight=.4,...)
    prettyY<-pretty(y)
    yLabs<-sapply(prettyY,function(x)as.expression(bquote(10^.(x))))
    axis(2,prettyY,yLabs,las=1)
    addMeanLines(x,y,col='#000000AA')
  }
  #quasirandom
  plotFunc(x,y,main='Quasirandom')
  #pseudorandom
  plotFunc(x,y,offsetXArgs=list(method='pseudo'),main='Pseudorandom')
  #smiley
  plotFunc(x,y,offsetXArgs=list(method='maxout'),main='Max outside')
  #tukey
  plotFunc(x,y,offsetXArgs=list(method='tukey'),main='Tukey')
  #beeswarm
  #beeInput<-split(y,x)
  #beeswarm(beeInput,las=1,cex=.7, ylab='Log distance to gene',
    #main='Beeswarm',pch=21, col='#00000099',bg='#00000033')
  #addMeanLines(x,y)
  dev.off()


###################################################
### code chunk number 17: vpDiamond (eval = FALSE)
###################################################
##   select<-sample(1:nrow(ggplot2::diamonds),3000)
##   y<-unlist(log10(ggplot2::diamonds[select,'price']))
##   x<-unlist(ggplot2::diamonds[select,'cut'])
##   par(mfrow=c(5,1), mar=c(6,4.5, 1.2, 0.5),mgp=c(3.3,.75,0),
##     cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
##   #simple function to avoid repeating code
##   prettyYAxis<-function(y){
##     prettyY<-pretty(y)
##     yLabs<-sapply(prettyY,function(x)as.expression(bquote(10^.(x))))
##     axis(2,prettyY,yLabs)
##   }
##   #quasirandom
##   vpPlot(x,y,offsetXArgs=list(varwidth=TRUE), 
##     ylab='Price',cex=.7,pch=21, col='#00000044',
##     bg='#00000011',yaxt='n',main='Quasirandom')
##   prettyYAxis(y)
##   #tukey
##   vpPlot(x,y,offsetXArgs=list(method='tukey'), 
##     ylab='Price',cex=.7,pch=21, col='#00000044',
##     bg='#00000011',yaxt='n',main='Tukey')
##   prettyYAxis(y)
##   #boxplot
##   boxplot(y~x,main='Boxplot',ylab='Price',yaxt='n')
##   prettyYAxis(y)
##   #beanplot
##   beanplot(y~x,main='Beanplot',ylab='Price',yaxt='n')
##   prettyYAxis(y)
##   vioInput<-split(y,x)
##   labs<-names(vioInput)
##   names(vioInput)[1]<-'x'
##   #vioplot
##   do.call(vioplot,c(vioInput,list(names=labs,col='white')))
##   title(ylab='Price', main='Vioplot')


###################################################
### code chunk number 18: showDiamond
###################################################
  select<-sample(1:nrow(ggplot2::diamonds),3000)
  y<-unlist(log10(ggplot2::diamonds[select,'price']))
  x<-unlist(ggplot2::diamonds[select,'cut'])
  par(mfrow=c(5,1), mar=c(6,4.5, 1.2, 0.5),mgp=c(3.3,.75,0),
    cex.axis=1.2,cex.lab=1.2,cex.main=1.2,las=1)
  #simple function to avoid repeating code
  prettyYAxis<-function(y){
    prettyY<-pretty(y)
    yLabs<-sapply(prettyY,function(x)as.expression(bquote(10^.(x))))
    axis(2,prettyY,yLabs)
  }
  #quasirandom
  vpPlot(x,y,offsetXArgs=list(varwidth=TRUE), 
    ylab='Price',cex=.7,pch=21, col='#00000044',
    bg='#00000011',yaxt='n',main='Quasirandom')
  prettyYAxis(y)
  #tukey
  vpPlot(x,y,offsetXArgs=list(method='tukey'), 
    ylab='Price',cex=.7,pch=21, col='#00000044',
    bg='#00000011',yaxt='n',main='Tukey')
  prettyYAxis(y)
  #boxplot
  boxplot(y~x,main='Boxplot',ylab='Price',yaxt='n')
  prettyYAxis(y)
  #beanplot
  beanplot(y~x,main='Beanplot',ylab='Price',yaxt='n')
  prettyYAxis(y)
  vioInput<-split(y,x)
  labs<-names(vioInput)
  names(vioInput)[1]<-'x'
  #vioplot
  do.call(vioplot,c(vioInput,list(names=labs,col='white')))
  title(ylab='Price', main='Vioplot')


