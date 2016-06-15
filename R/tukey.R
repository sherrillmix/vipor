
textur(yy,shft,option,del){
  ny<-length(yy) 
  if(ny==0)return(NULL)
  if(ny==1)return(50)
  yy<-sort(yy)
  lohnge<-yy[round(ny*.25)]
  hihnge<-yy[round(ny*.75)]
  delta<-del*.3*(hihnge-lohnge)
  nny<-ny
  if(option==2)nny<-min(nny,50)
  for(ii in 1:nny){
    ii<-(i-1)%%25+1
    if(ii==1)next25(tsmall,tlarge,tmp5x5) ####FIX THIS
    shft[ii]<-20*tlarge[ii]+4*tsmall[ii]+2
  }
  if(option==1){
    for(ii in 1:ny)shift[ii]<-shift[ii]+runif(1)*4-2
  }
  if(option==2){
    for(i in 1:min(25,ny))shft[ii]<-shft[ii]-2
    if(ny>50){
      for(ii in 51:ny)shft[ii]<-shft[ii-50]
    }
  }
  window<-5
  min<-(window+1)/2
  h<-mid
  while(h-mid+window<=ny){
    g<-h
    while(g-mid+window<=ny){ ####OPTIMIZE THIS
      if(yy[g-mid+window]>yy[h]+10*delta)break
      g<-g+window
    }
    if(g-mid+window>ny)break
    hh<-h-mid+1
    gg<-g-h+window
    slo<-5
    shi<-0
    for(ii in 1:(hh+gg-1)){ ####OPTIMIZE THIS
      if(shft[ii]<slo)slo<-shft[ii]
      if(shft[ii]>shi)shi<-shft[ii]
    }
    srnge<-shi-slo
    for(ii in hh:gg)shft[ii]<-100*(shft[i]-slo)/srnge
    h<-g+window
    for(ii in 1:5)last[ii]<-xlast[ii] ####FIX THIS
    if(lastt[1]==0&&last[2]==0){
      #j=irnd(nperms)
      j<-sample(nperms,1)
      for(ii in 1:5)last[ii]<-perms(j,i)
    }
  }
  return(shft)
}

next25<-function(tt,bigt,smallt){
  if(bigt[1]==0 && bigt[2]==0){
    bigt[21:22]<-0
    for(ii in 1:2){
      for(jj in 1:5)smallt[ii,jj]<-0
    }
  }
  next5(bigt[21],bigt[1]) ####FIX THIS
  for(jj in seq(6,21,5))next5(bigt[jj-5],bigt[jj])
  for(jj in 1:5)next5(smallt[1,jj],smallt[1,jj])
  for(ii in 1:5){
    for(jj in 1:5){
      k<-5*(i-1)+j
      tt[k]<-smallt[i,1+bigt[k]]
    }
  }
  return(next25

}

