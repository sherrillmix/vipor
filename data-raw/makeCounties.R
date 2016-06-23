Sys.setlocale('LC_ALL','C') 
#downloaded from http://factfinder.census.gov/bkmk/table/1.0/en/DEC/10_SF1/GCTPH1.US05PR and added column names
counties<-read.csv('DEC_10_SF1_GCTPH1.US05PR_with_ann.csv',stringsAsFactors=FALSE)
nParts<-sapply(strsplit(counties$geographicArea,' - '),length)
counties<-counties[nParts==3,]
counties$state<-sapply(strsplit(counties$geographicArea,' - '),'[',2)
counties$county<-counties$geographicArea2
out<-counties[,c('id','state','county','population','housingUnits','totalArea','waterArea','landArea')]
save(out,file='../data/counties.RData')
tools::resaveRdaFiles('../data/counties.RData')


