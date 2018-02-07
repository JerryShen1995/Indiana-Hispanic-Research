x=1
pop=1
hisp=1
for (i in LookingFor$Id2){
  if(length(Source[Source$Id2==i,]$HC01_VC55)>0){
    pop[x]<-Source[Source$Id2==i,]$HC01_VC55
    hisp[x]<-Source[Source$Id2==i,]$HC01_VC56
  }
  x=x+1
}

output<-data.frame(LookingFor$Id2[1:length(pop)],pop,hisp)
write.csv(output,file="output.csv")
