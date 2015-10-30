#http://www.retrovirology.com/content/10/1/90/additional
#something messing up read directly so just download and delete
download.file('http://www.retrovirology.com/content/supplementary/1742-4690-10-90-s2.gz','ints.csv.gz')
ints<-read.csv('ints.csv.gz',stringsAsFactors=FALSE)
file.remove('ints.csv.gz')
desiredCols<-c('study'='sample','latent'='isLatent','nearestGene'='logDist_nearest','H4K12ac'='H4K12ac_50000')
integrations<-ints[,desiredCols]
colnames(integrations)<-names(desiredCols)
integrations$latent<-ifelse(integrations$latent,'Unexpressed','Expressed')
integrations$nearestGene<-ifelse(ints$inGene,0,round(exp(integrations$nearestGene)))
integrations$study<-sub(' .*$','',integrations$study)


save(integrations,file='../data/integrations.RData')
tools::resaveRdaFiles('../data/integrations.RData')
