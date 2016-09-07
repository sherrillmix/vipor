context("violinscatter functions")
test_that("Test offsetting",{
  expect_equal(offsetX(1,1), 0)
  expect_equal(offsetX(rep(1,10),1:10), rep(0,10))
  expect_error(offsetX(1,1:2), "not the same length")
  expect_error(offsetX(1:2,1), "not the same length")
  expect_equal(length(unique(offsetX(rep(1,100)))), 100)
  expect_equal(offsetX(rep(1,100)), offsetX(rep(1,100)))
  expect_equal(offsetX(rep(1,100),width=.2)*2, offsetX(rep(1,100),width=.4))
  expect_equal(
    offsetX(rep(1:2,c(10,1000)),rep(1:2,c(10,1000)),varwidth=TRUE)[1:10]*10,
    offsetX(rep(1:2,c(1000,1000)),rep(1:2,c(1000,1000)),varwidth=TRUE)[1:10]
  )
})

test_that("Test single group offsetting",{
  expect_equal(offsetSingleGroup(1), 0)
  expect_equal(offsetSingleGroup(NULL), NULL)
  expect_equal(offsetSingleGroup(rep(1,100)), offsetSingleGroup(rep(1,100)))
  expect_equal(length(offsetSingleGroup(rnorm(1000))), 1000)
  x<-rnorm(1000)
  expect_equal(offsetSingleGroup(x), offsetSingleGroup(x,maxLength=1000))
  expect_equal(offsetSingleGroup(x)*10, offsetSingleGroup(x,maxLength=10))
  expect_equal(offsetSingleGroup(x)/10, offsetSingleGroup(x,maxLength=100000))
  for(ii in c('quasirandom','pseudorandom','smiley','frowney','tukey','tukeyDense')){
    expect_lte(max(offsetX(rnorm(100),method=ii,width=.1)),.1)
    expect_gte(min(offsetX(rnorm(100),method=ii,width=.1)),-.1)
    expect_equal(length(offsetSingleGroup(rnorm(100),method=ii)),100)
    expect_equal(length(offsetSingleGroup(rnorm(2),method=ii)),2)
    expect_equal(length(offsetSingleGroup(rnorm(1),method=ii)),1)
    #make sure no errors with small groups
    expect_error(offsetSingleGroup(1,method=ii),NA)
    expect_error(offsetSingleGroup(1:2,method=ii),NA)
    expect_error(offsetSingleGroup(1:3,method=ii),NA)
  }
})

test_that("Test ave with args",{
  expect_equal(aveWithArgs(1:10,rep(1:5,2)), ave(1:10,rep(1:5,2)))
  expect_equal(aveWithArgs(1:10,rep(1:5,2),FUN=median), ave(1:10,rep(1:5,2),FUN=median))
  expect_equal(aveWithArgs(100:1+.01,rep(1:5,20),FUN=median), ave(100:1+.01,rep(1:5,20),FUN=median))
  expect_equal(aveWithArgs(100:1+.01,rep(1:5,20),FUN=max), ave(100:1+.01,rep(1:5,20),FUN=max))
  expect_equal(aveWithArgs(100:1+.01,rep(1:5,20),FUN=max), ave(100:1+.01,rep(1:5,20),FUN=max))
  expect_equal(aveWithArgs(c(1:5,NA),rep(1:3,2),FUN=max,na.rm=TRUE), rep(c(4,5,3),2))
  expect_equal(aveWithArgs(c(1:6),rep(1:3,2),FUN=function(x,y)sum(x)+y,3), rep(c(8,10,12),2))
  expect_equal(aveWithArgs(c(1:6)),rep(mean(1:6),6))
  expect_equal(aveWithArgs(c(1:6),rep(1:3,2),FUN=function(x,y)x+y,3), 1:6+3)
  expect_equal(aveWithArgs(c(6:1),rep(1:3,2),FUN=function(x,y)sqrt(x)+y,3), sqrt(6:1)+3)
  expect_error(aveWithArgs(c(6:1),rep(1:3,2),FUN=function(x,y)sqrt(x)+y), 'argument.*missing')
})

test_that("Test top bottom distribute",{
  expect_equal(topBottomDistribute(1:10), topBottomDistribute(1:10+100))
  expect_equal(topBottomDistribute(-1:-10), topBottomDistribute(1:10,TRUE))
  expect_equal(topBottomDistribute(1000), .5)
  expect_equal(topBottomDistribute(-1000), .5)
  expect_equal(topBottomDistribute(1:3)[1], .5) #could do left to right or right to left
  expect_equal(topBottomDistribute(-1:-3)[3], .5) #could do left to right or right to left
  expect_equal(length(topBottomDistribute(1:100)), 100) 
  expect_equal(sort(tail(topBottomDistribute(1:100),2)), c(0,1))
  expect_equal(sort(tail(topBottomDistribute(1:1000),2)), c(0,1))
  expect_equal(sort(head(topBottomDistribute(1:1000,TRUE),2)), c(0,1))
  expect_equal(sort(head(topBottomDistribute(1:100,TRUE),2)), c(0,1))
  expect_equal(sort(head(topBottomDistribute(1:100,TRUE,FALSE),2)), c(1,100))
  expect_equal(sort(tail(topBottomDistribute(1:100,FALSE,FALSE),2)), c(1,100))
  expect_equal(sort(tail(topBottomDistribute(1:1000,prop=FALSE),2)), c(1,1000))
  expect_equal(sort(head(topBottomDistribute(1:1000,prop=FALSE),5)), 498:502)
  expect_equal(sort(tail(topBottomDistribute(1:1000,TRUE,prop=FALSE),5)), 498:502)
})

test_that("Test van der Corput generation",{
  expect_equal(vanDerCorput(3), c(1/2,1/4,3/4))
  expect_equal(vanDerCorput(8), c(1/2,1/4,3/4,1/8,5/8,6/16,14/16,1/16))
  expect_equal(vanDerCorput(8,start=1), c(1/2,1/4,3/4,1/8,5/8,6/16,14/16,1/16))
  expect_equal(vanDerCorput(5,start=4), c(1/8,5/8,6/16,14/16,1/16))
  expect_equal(vanDerCorput(3,3), c(1/3,2/3,1/9))
  expect_equal(vanDerCorput(3,10), c(1/10,2/10,3/10))
  expect_equal(vanDerCorput(0,10), c())
  expect_error(vanDerCorput(3,1), 'base')
  expect_error(vanDerCorput(3,-10), 'base')
  expect_error(vanDerCorput(10,0), 'base')
  expect_error(vanDerCorput(-10,10), 'n ')
  expect_error(vanDerCorput(10,10,-100), 'start')
  expect_equal(length(vanDerCorput(10000)), 10000)
  expect_equal(length(unique(vanDerCorput(10000))), 10000)
})

test_that("Test number splitting",{
  expect_equal(number2digits(0), c())
  expect_error(number2digits(-1), 'negative')
  expect_error(number2digits(10,1), 'base')
  expect_error(number2digits(10,0), 'base')
  expect_error(number2digits(10,-1), 'base')
  expect_equal(number2digits(1,123), 1)
  expect_equal(number2digits(1,10), 1)
  expect_equal(number2digits(101,102), 101)
  expect_equal(number2digits(5,4), c(1,1))
  expect_equal(number2digits(255), rep(1,8))
  expect_equal(number2digits(65535), rep(1,16))
  expect_equal(number2digits(65535,16), rep(15,4))
  expect_equal(number2digits(65534,16), c(14,15,15,15))
  expect_equal(number2digits(65533,16), c(13,15,15,15))
  expect_equal(number2digits(4095,8), rep(7,4))
  expect_equal(length(number2digits(4095,8)), 4)
  expect_equal(length(number2digits(4096,8)), 5)
  expect_equal(length(number2digits(4096,2)), 13)
})

test_that("Test digit combining",{
  expect_equal(digits2number(c(1,1)), 3)
  expect_equal(digits2number(c(1,1,1)), 7)
  expect_equal(digits2number(rep(15,4),16), 65535)
  expect_equal(digits2number(c(14,15,15,15),16), 65534)
  expect_equal(digits2number(c(0,1,1)), 6)
  expect_equal(digits2number(c(1,1,rep(0,100))), 3)
  expect_equal(digits2number(c(rep(0,15),1)), 2^15)
  expect_equal(digits2number(c(rep(0,15),1),5), 5^15)
  expect_equal(digits2number(c(rep(0,5),1),5), 5^5)
  expect_equal(digits2number(c(1,1,1),1), 3)
  expect_equal(digits2number(rep(0,16),2), 0)
  expect_equal(digits2number(c()), 0)
  expect_equal(digits2number(c(),11), 0)
  expect_error(digits2number(c(1,1,1),0), 'base') #doesn't really require an error but probably does not produce a desired result
  expect_error(digits2number(1,-1), 'base') #doesn't really require an error but probably does not produce a desired result
  expect_error(digits2number(-1,10), 'digit') #doesn't really require an error but probably does not produce a desired result
  #test fractional. Note never reaches 1
  expect_equal(digits2number(1,base=2,fractional=TRUE),1/2)
  expect_equal(digits2number(7,base=8,fractional=TRUE),7/8)
  expect_equal(digits2number(c(0,1),base=2,fractional=TRUE),2/4)
  #not really well formed but might as well handle
  expect_equal(digits2number(20,base=2),20)
  expect_equal(digits2number(c(1,20),base=2),41)
  expect_equal(digits2number(20,base=8,fractional=TRUE),20/8)
})

test_that("Test consistency",{
  expect_equal(digits2number(number2digits(100)), 100)
  expect_equal(digits2number(number2digits(100,5),5), 100)
  expect_equal(digits2number(number2digits(123456,23),23), 123456)
})

dat<-rnorm(1000)
labs<-rep(1:4,250)
test_that("vpPlot returns x positions",{
  expect_equal(length(vpPlot(y=rnorm(100))), 100)
  expect_equal(vpPlot(y=dat), 1+offsetX(dat))
  expect_equal(vpPlot(labs,dat), labs+offsetX(dat,labs))
  expect_silent(vpPlot(labs,dat,col='black'))
})
