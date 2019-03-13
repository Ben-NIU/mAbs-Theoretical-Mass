Calc.Mass.all.R<-function(HC, LC, Items, Ab.format, FP.ngly){
  
  
  source("Calc.Mass.R.R")
  source("report.item.R.R")
  
  Formu.withDSB.HC<-NULL
  Formu.fullR.HC<-NULL
  Formu.withDSB.LC<-NULL
  Formu.fullR.LC<-NULL
  
  for(j in 1:nrow(Items)){
    
    Formu0<-Calc.Mass.R(HC, LC, as.character(Items[j,1]), as.character(Items[j,2]), as.character(Items[j,3]), as.character(Items[j,4]), Items[j,5], Items[j,6],Ab.format, FP.ngly)
    ## Forum0 will be a list with 4 components: HC.withDSB", "LC.withDSB", "HC.fullR", "LC.fullR".
    
    Formu.withDSB.HC<-rbind(Formu.withDSB.HC, Formu0$HC.withDSB)
    Formu.fullR.HC<-rbind(Formu.fullR.HC, Formu0$HC.fullR)
    Formu.withDSB.LC<-rbind(Formu.withDSB.LC, Formu0$LC.withDSB)
    Formu.fullR.LC<-rbind(Formu.fullR.LC, Formu0$LC.fullR)
    
  }
  
 List<-list("HC.withDSB"=Formu.withDSB.HC, "HC.fullR"=Formu.fullR.HC, "LC.withDSB"=Formu.withDSB.LC, "LC.fullR"=Formu.fullR.LC)
 
 FFF<-list("HC.fullR"=cbind(Items[,-c(2,5,6)], List$HC.fullR), "HC.withDSB"=cbind(Items[,-c(2,6)], List$HC.withDSB), "LC.fullR"=cbind(Items[,c(2,6)], List$LC.fullR)[,-2], "LC.withDSB"=cbind(Items[,c(2,6)], List$LC.withDSB))
  
 no.dup<-function(x) {x[!duplicated(x), ]}
 
 U<-do.call("list", lapply(FFF, no.dup))
 
  return(U)
}