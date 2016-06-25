#' Return all permutations of a vector
#'
#' Recursively generates all permutations of a vector. The result will be \code{factorial(length(vals))} long so be careful with any longer vectors (e.g. longer than 10).
#'
#' @param vals a vector of elements to be permuted
#' @return A list of vectors containing all permutation of the values
#' @export
#' @seealso \code{\link{sample}}
#' @examples
#' permute(letters[1:3])
#' permute(1:5)
permute<-function(vals){
  if(length(vals)==1)return(list(vals))
  if(length(vals)==0)return(NULL)
  permList<-lapply(1:length(vals),function(ii){
      lapply(permute(vals[-ii]),function(y)c(vals[ii],y))
  })
  return(unlist(permList,recursive=FALSE))
}

#' Find permutations meeting Tukey criteria
#' 
#' Find all permutations of 1:n fulfilling Tukey's criteria that there are no runs of 3 or more increases or decreases in a row. Tukey just uses the default n=5 and limit=2.
#'
#' @param n permutations from 1 to n
#' @param limit the maximum number of increases or decreases in a row
#' @return a list of vectors containing valid permutations
#' @export
#' @examples
#' tukeyPermutes()
#' tukeyPermutes(6,3)
tukeyPermutes<-function(n=5,limit=2){
  allPermutes<-permute(1:n)
  nSameDir<-sapply(allPermutes,function(x)max(rle(diff(x)>0)$lengths))
  okPermutes<-allPermutes[nSameDir<limit]
  return(okPermutes)
}

#' Generate a permutation string meeting Tukey criteria
#' 
#' Find a random string of concatenated permutations of 1:n fulfilling Tukey's criteria that there are no runs of 3 or more increases or decreases in a row. Tukey just uses the default n=5.
#'
#' @param nReps number of permutations to concatenate
#' @param n permutations from 1 to n
#' @return a vector of nReps*n integers giving concatenated permutations
#' @export
#' @examples
#' tukeyPermutes()
#' tukeyPermutes(6,3)
generatePermuteString<-function(nReps=20,n=5){
  permutes<-tukeyPermutes(n)
  indexed<-tapply(permutes,sapply(permutes,'[',1),c)
  out<-rep(c(NULL),nReps)
  out[[1]]<-sample(permutes,1)[[1]]
  if(nReps>1){
    for(ii in 2:nReps){
      if(diff(out[[ii-1]][n-1:0])>0)targets<-unlist(indexed[1:(out[[ii-1]][n]-1)],recursive=FALSE)
      else targets<-unlist(indexed[(out[[ii-1]][n]+1):n],recursive=FALSE)
      out[[ii]]<-sample(targets,1)[[1]]
    }
  }
  return(unlist(out))
}


#' Combine multiple permutation strings into one
#'
#' Combine base+1 permutation strings to generate offsets
#'
#' @param nReps number of permutations to paste together
#' @param base generate permutations of integers 1:base
#' @return A nReps*base length vector giving offset positions based on Tukey's algorithm
#' @export
#' @examples
#' tukeyT()
#' tukeyT()
#' tukeyT(5,4)
tukeyT<-function(nReps=10,base=5){
  T<-generatePermuteString(nReps,base)
  ti<-lapply(1:base,function(x)generatePermuteString(nReps,base))
  indexs<-1:length(T)
  ts<-sapply(indexs,function(fiveGPlusI)ti[[T[fiveGPlusI]]][ceiling(fiveGPlusI/5)])
  #final -1 to deal with using 1:base instead of 0:(base-1)
  out<-2+4*(ts-1)+20*(T-1)-1
  return(out)
}

#' Generate random positions based on Tukey texture algorithm
#'
#' Generate partly random, partly constrained lateral displacements based on Tukey texture algorithm from Tukey and Tukey 1990
#'
#' @param x the points to be jittered. really only used to calculate length
#' @param jitter if TRUE add random jitter to each point
#' @param thin if TRUE then push points to the center in thin regions
#' @param hollow if TRUE then expand points outward to avoid ``hollowness''
#' @param delta a ``reasonably small value'' used in edge straightening and thinning
#' @return a vector of length length(x) giving displacements for each corresponding point in x
#' @export
#' @examples
#' x<-rnorm(200)
#' plot(tukeyTexture(x),x)
#' x<-1:100
#' plot(tukeyTexture(x),x)
#' plot(tukeyTexture(log10(counties$landArea),TRUE,TRUE),log10(counties$landArea),cex=.25)
tukeyTexture<-function(x,jitter=TRUE,thin=FALSE,hollow=FALSE,delta=diff(stats::quantile(x,c(.25,.75)))*.03){
  n<-length(x)
  orderX<-order(x)
  x<-x[orderX]
  offset<-tukeyT(10)
  offset[26:50]<-offset[26:50]+2
  spread<-rep(offset,length.out=n)
  if(jitter)spread<-spread+stats::runif(n,-1,1)
  #deal with thin regions
  if(thin){
    diffLeft<-c(Inf,diff(x))
    diffRight<-c(diff(x),Inf)
    spread[diffLeft>delta&diffRight>delta]<-50
  }
  #deal with 'hollow' regions
  if(hollow){
    current<-1
    fiveStarts<-seq(1,n,5)
    fiveEnds<-fiveStarts+4
    fiveEnds[fiveEnds>n]<-n
    for(ii in fiveStarts){
      breakPoint<-fiveStarts[which(x[fiveEnds]-x[ii]>delta*10)[1]] #sorted so can just take [1] instead of min
      if(is.na(breakPoint))break()
      rightMost<-min(breakPoint+4,n)
      spread[ii:rightMost]<-(spread[ii:rightMost]-min(spread[ii:rightMost]))/diff(range(spread[ii:rightMost]))*100
    }
  }
  return(spread[order(orderX)])
}

#' Census ata on US counties
#'
#' A dataset containing data from the US census burea
#'
#' @format A data frame with 3143 rows and 8 variables:
#' \describe{
#'   \item{id}{GEO.id from original data}
#'   \item{state}{state in which the county is located}
#'   \item{county}{name of the county}
#'   \item{population}{population of the county}
#'   \item{housingUnits}{housing units in the county}
#'   \item{totalArea}{Area in square miles - Total area}
#'   \item{waterArea}{Area in square miles - Water area}
#'   \item{landArea}{Area in square miles - Land area}
#' }
#' @references \url{https://www.census.gov/prod/cen2010/cph-2-1.pdf}
#' @source \url{http://factfinder.census.gov/bkmk/table/1.0/en/DEC/10_SF1/GCTPH1.US05PR}, system.file("data-raw", "makeCounties.R", package = "vipor")
"counties"
