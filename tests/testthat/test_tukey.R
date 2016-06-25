context("violinscatter functions")
test_that("Test permuting",{
  expect_equal(permute(c()),NULL)
  expect_equal(permute(1),list(1))
  expect_equal(permute(1:2),list(1:2,2:1))
  expect_equal(permute(2:1),list(2:1,1:2))
  expect_equal(permute(c('a','b')),list(c('a','b'),c('b','a')))
  expect_equal(permute(1:3),list(1:3,c(1,3,2),c(2,1,3),c(2,3,1),c(3,1,2),3:1))
  expect_equal(length(permute(1:5)),factorial(5))
  expect_equal(length(permute(1:6)),factorial(6))
  expect_equal(sort(unlist(permute(1:6))),rep(1:6,each=factorial(6)))
  expect_equal(unique(sapply(permute(2:7),function(x)length(unique(x)))),6)
})

test_that("Test Tukey permutes",{
  expect_equal(length(tukeyPermutes()),32)
  expect_equal(length(tukeyPermutes(5,2)),32)
  expect_equal(length(tukeyPermutes(3,2)),4)
  expect_equal(length(tukeyPermutes(3,3)),6)
  expect_equal(sort(unlist(tukeyPermutes(5,2))),rep(1:5,each=32))
  expect_false(any(sapply(tukeyPermutes(6),function(x)x[1]<x[2]&&x[2]<x[3])))
  expect_false(any(sapply(tukeyPermutes(6),function(x)x[2]<x[3]&&x[3]<x[4])))
  expect_false(any(sapply(tukeyPermutes(6),function(x)x[5]>x[4]&&x[4]>x[3])))
  expect_false(any(sapply(tukeyPermutes(6),function(x)x[6]>x[5]&&x[5]>x[4])))
})

test_that("Test Tukey permute string",{
  expect_equal(sort(generatePermuteString(20)),rep(1:5,each=20))
  expect_equal(sort(generatePermuteString(10,7)),rep(1:7,each=10))
  expect_lt(max(rle(diff(generatePermuteString(20,7))>0)$lengths),3)
  expect_lt(max(rle(diff(generatePermuteString(100,5))>0)$lengths),3)
})

test_that("Test Tukey offset positioning",{
  expect_equal(sort(tukeyT()),rep(seq(1,97,4),each=2))
  expect_equal(sort(tukeyT(10,5)),rep(seq(1,97,4),each=2))
  expect_equal(sort(tukeyT(20,5)),rep(seq(1,97,4),each=4))
  expect_equal(length(tukeyT(10,6)),60)
  expect_lt(max(rle(diff(tukeyT())>0)$lengths),3)
  expect_lt(max(rle(diff(tukeyT(50))>0)$lengths),3)
})

test_that("Test Tukey algorithm",{
  expect_equal(length(tukeyTexture(1:200)),200)
  expect_equal(length(tukeyTexture(1:1234)),1234)
  expect_lte(max(tukeyTexture(1:1234)),100)
  expect_gte(min(tukeyTexture(1:1234)),0)
  expect_equal(length(unique(tukeyTexture(1:100))),100) #assuming jitter will not overlap perfectly
  expect_equal(length(unique(tukeyTexture(1:100,jitter=FALSE))),50)
  expect_equal(tukeyTexture(c(-100,1:100,101.1),delta=1,thin=TRUE)[c(1,102)],c(50,50))
  expect_equal(tukeyTexture(c(-100,1:100,101.1),delta=10,thin=TRUE)[1],50)
  expect_true(tukeyTexture(c(-100,1:100,101.1),delta=10,thin=TRUE)[102]!=50) #assuming jitter and algorithm will not exactly equal 50
  expect_equal(tukeyTexture(1:100,delta=.9,thin=TRUE),rep(50,100))
  expect_equal(range(tukeyTexture(c(1,2,101,102),delta=10,hollow=TRUE)),c(0,100))
  expect_equal(sum(tukeyTexture(c(1:5,101:105),delta=.1,hollow=TRUE) %in% c(0,100)),4)
  expect_equal(sum(tukeyTexture(c(1:5,101:105),delta=1,hollow=TRUE) %in% c(0,100)),2)
  expect_true(all(range(tukeyTexture(c(1,2,101,102),delta=10.1,hollow=TRUE))!=c(0,100))) #assuming jitter and algorithm will not exactly equal 0,100
})
