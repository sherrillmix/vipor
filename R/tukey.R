#' Return all permutations of a vector
#'
#' Recursively generates all permutations of a vector. The result will be \code{factorial(length(vals))} long so be careful with any longer vectors (e.g. longer than 10).
#'
#' @param vals a vector of elements to be permuted
#' @return A list of all permutation of the values
#' @export
#' @seealso \code{\link{sample}}
#' @examples
#' permute(letters[1:3])
#' permute(1:5)
#
permute<-function(vals){
  if(length(vals)==1)return(list(vals))
  if(length(vals)==0)return(NULL)
  permList<-lapply(1:length(vals),function(ii){
      lapply(permute(vals[-ii]),function(y)c(vals[ii],y))
  })
  return(unlist(permList,recursive=FALSE))
}

goodPermutes<-function(n,limit=2){
  allPermutes<-permute(1:n)
  nSameDir<-sapply(allPermutes,function(x)max(rle(diff(x)>0)$lengths))
  okPermutes<-allPermutes[nSameDir<limit]
  return(okPermutes)
}

generatePermuteString<-function(nReps=20,n=5){
  permutes<-goodPermutes(n)
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


tukeyT<-function(nReps,base=5){
  T<-generatePermuteString(nReps,base)
  ti<-lapply(1:base,function(x)generatePermuteString(nReps,base))
  indexs<-1:length(T)
  ts<-sapply(indexs,function(fiveGPlusI)ti[[T[fiveGPlusI]]][ceiling(fiveGPlusI/5)])
  out<-2+4*(ts-1)+20*(T-1)
  return(out)
}

texture<-function(x,jitter=TRUE){
  n<-length(x)
  offset<-tukeyT(10)
  offset[26:50]<-offset[26:50]+2
  spread<-rep(offset,length.out=n)
  if(jitter)spread<-spread+runif(n,-1,1)
  return(spread)
}

