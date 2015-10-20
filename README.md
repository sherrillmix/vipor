# Plot one-dimensional data using quasirandom noise and kernel density

## Introduction

`violinPointR` provides a way to plot one-dimensional data (perhaps divided into several categories) by spreading the data points to fill the kernel density. It uses a [van der Corput sequence](http://en.wikipedia.org/wiki/Van_der_Corput_sequence) to space the dots and avoid generating distracting patterns in the data. See the examples below.

Beeswarm plots (aka column scatter plots or violin scatter plots) are a way of plotting points that would ordinarily overlap so that they fall next to each other instead. In addition to reducing overplotting, it helps visualize the density of the data at each point (similar to a violin plot), while still showing each data point individually.

## Installation


```r
devtools::install_github("sherrillmix/violinPointR")
```

## Examples

### Violin point examples

We use the provided function `offsetX` to generate the x-offsets for plotting.

```r
library(violinPointR)
# Generate data
set.seed(12345)
dat <- list(rnorm(50), rnorm(500), c(rnorm(100), rnorm(100,5)), rcauchy(100))
names(dat) <- c("Normal", "Dense Normal", "Bimodal", "Extremes")

# Violin points of several distributions
par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
sapply(names(dat),function(label) {
	y<-dat[[label]]
	offsets <- list(
		offsetX(y),  # Default
		offsetX(y, adjust=2),    # More smoothing
		offsetX(y, adjust=0.1),  # Tighter fit
		offsetX(y, width=0.1)    # Less wide
	)  
	ids <- rep(1:length(offsets), each=length(y))
	plot(unlist(offsets) + ids, rep(y, length(offsets)), ylab='y value', xlab='', xaxt='n', pch=21,col='#00000099',bg='#00000033',las=1,main=label)
	axis(1, 1:length(offsets), c("Default", "Adjust=2", "Adjust=0.1", "Width=10%"))
})
```

![plot of chunk adjust-examples](README_files/adjust-examples-1.png) 


### Comparison with other methods

```r
library(beeswarm)
par(mfrow=c(4,1), mar=c(2.5,3.1, 1.2, 0.5),mgp=c(2.1,.75,0),cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
sapply(names(dat),function(label) {
	y<-dat[[label]]
	offsets <- list(
		offsetX(y),  # Default
		offsetX(y, method='pseudorandom',nbins=100),
		offsetX(y, method='frowney',nbins=20),
		offsetX(y, method='smiley',nbins=20),
		offsetX(y, method='smiley',nbins=100),
		offsetX(y, method='smiley',nbins=round(length(y)/5)),
		swarmx(rep(0,length(y)),y)$x
	)
	ids <- rep(1:length(offsets), each=length(y))

	plot(unlist(offsets) + ids, rep(y, length(offsets)), ylab='y value', xlab='', xaxt='n', pch=21,col='#00000099',bg='#00000033',las=1,main=label)
	axis(1, 1:length(offsets), c("Quasi", "Pseudo", "Frown", "Smile 20 bin","Smile 100 bin","Smile n/5 bin","Beeswarm"))
})
```

![plot of chunk other-methods](README_files/other-methods-1.png) 

------
Authors: Scott Sherrill-Mix and Erik Clarke

