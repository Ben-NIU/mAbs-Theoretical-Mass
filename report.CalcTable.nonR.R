Calc.Mass.all.nonR<-function(HC, LC, Items,Ab.format, FP.ngly){
  
  ## List contains (1) pE status (2) CtermK status, (3) Nglycan status ,(4) SS #
  ## where: pE can be "Yes", "No", and NULL
  ##        CtermK can be "Yes", "No", and NULL
  ##        Glycan can be "Aglycosylated","Deglycosylated",'G0', "G0F", "G1", "G1F", "G2F","G0-GN"
  ##        DS can be one number from 0 to 100.
  
  
  source("Calc.Mass.nonR.R")
  source("report.item.nonR.R")
  
  Formu<-NULL
  
  for(j in 1:nrow(Items)){
 
     Formu0<-Calc.Mass.nonR(HC, LC, as.character(Items[j,1]), as.character(Items[j,2]), as.character(Items[j,3]), as.character(Items[j,4]), Items[j,5],Ab.format, FP.ngly)
    
     Formu<-rbind(Formu, Formu0)   
  
     }
  
  return(cbind(Items,Formu))
  
  
}