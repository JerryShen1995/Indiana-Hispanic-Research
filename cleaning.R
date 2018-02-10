hisp.data<-read.csv(file="2010 Hispanic.csv")
hisp.data$hisp/(hisp.data$hisp+hisp.data$non.hisp)

id<-hisp.data$Id2               
#ID
name<-hisp.data$geog            
#Name of the census tract
hisp<-hisp.data$hisp            
#Hispanic population
non.hisp<-hisp.data$non.hisp
#Non-Hispanic population
pop<-(hisp.data$hisp+hisp.data$non.hisp)
#Total population
zonal.hisp<-0
#Indicator of Hispanic majority areas
zonal.almost.hisp<-0
#Indicator of Hispanic influence areas
zonal.non.hisp<-0
#Indicator of places without a significant Hispanic influence

for(i in 1:1511){
  if(pop[i]==0){
    zonal.hisp[i]=0
    zonal.almost.hisp[i]=0
    zonal.non.hisp[i]=1
    
  }else if(hisp[i]/pop[i]>0.5){
    zonal.hisp[i]=1
    zonal.almost.hisp[i]=0
    zonal.non.hisp[i]=0
  }else if(hisp[i]/pop[i]>0.25){
    zonal.hisp[i]=0
    zonal.almost.hisp[i]=1
    zonal.non.hisp[i]=0
  }else{
    zonal.hisp[i]=0
    zonal.almost.hisp[i]=0
    zonal.non.hisp[i]=1
  }
}
ct<-data.frame(id,name,hisp,non.hisp,pop,zonal.hisp,zonal.almost.hisp,zonal.non.hisp)

income.data<-read.csv(file="2010 Income Estimate edited.csv",stringsAsFactors=FALSE)
for(i in 2:11){
  income.data[,i]<-as.numeric(income.data[,i])
}
ct<-merge(ct,income.data,by="id")

t.test(ct$per.capita[ct$zonal.non.hisp==0 & ct$hisp>0],ct$per.capita[ct$zonal.non.hisp==1 & ct$zonal.non.hisp>0], alternative = "less")

pov<-read.csv(file="2010 Poverty Estimate.csv",stringsAsFactors=FALSE)

ct<-merge(ct,pov,by="id")

edu<-read.csv(file="2010 Education Estimate.csv",stringsAsFactors=FALSE)

ct<-merge(ct,edu,by="id")

hisp.tracts<-ct[ct$zonal.non.hisp==0 & ct$hisp>0,]
non.hisp.tracts<-ct[ct$zonal.non.hisp==1 & ct$hisp>0,]

t.test(hisp.tracts$pov/(hisp.tracts$pov+hisp.tracts$non.pov),non.hisp.tracts$pov/(non.hisp.tracts$pov+non.hisp.tracts$non.pov), alternative = "greater")
t.test((hisp.tracts$bach.or.high.f+hisp.tracts$bach.or.high.m)/hisp.tracts$above.25.est,(non.hisp.tracts$bach.or.high.f+non.hisp.tracts$bach.or.high.m)/non.hisp.tracts$above.25.est, alternative = "less")

write.csv(ct, file="2010 Comprehensive Data - Indiana.csv")
