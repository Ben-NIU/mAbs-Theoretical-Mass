# Return the LC formula depending on PTM input.

Calc.Mass.LC.Red<-function(LC, PyroE.lc, AorB, Others, howmany, where, ds.lcun){
  
  source("formula.R")

  if(is.null(PyroE.lc)){
    pyroe<-0
  } else {
    if(PyroE.lc=="No"){
      pyroe<-0}
    if(PyroE.lc=="Yes"){
      if(substr(LC, 1,1)=="E"){  
        pyroe<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(LC, 1,1)=="Q") {
        pyroe<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
    }
  }
  
  
  
  OTHERS<-list("+Lys"=c("C"=6, "H"=12,"N"=2, "O"=1, "S"=0), "none"=c(C=0, H=0, N=0, O=0, S=0), "+Glycation"=c(C=6, H=10, N=0, O=5, S=0), "Oxidation"=c(C=0, H=0, N=0, O=1, S=0), "Water-Loss"=c(C=0, H=-2, N=0, O=-1, S=0), "NH3-Loss"=c(C=0, H=-3, N=-1, O=0, S=0), "Deamidation"=c(C=0, H=-1, N=-1, O=1, S=0), "Cysteinylation"=c(C=3, H=5, N=1, O=2,S=1))
  
  F<-if(where=="HC"){0} else if(where=="LC"){1}
  
  if(AorB=="Pre-defined"){  
    
    
    total<-unlist(ConvertPeptide(LC, IAA=FALSE)) + pyroe + OTHERS[[Others]]*howmany*F + c(C=0, H=as.numeric(ds.lcun)*(-2), N=0, O=0, S=0) 
    
  } 
  else if(AorB=="Customized"){
    
    
    X<-Turn(Others)
    
    
    total<-unlist(ConvertPeptide(LC, IAA=FALSE)) + pyroe + X*howmany*F + c(C=0, H=as.numeric(ds.lcun)*(-2), N=0, O=0, S=0) 
    
    
  }

  return(total)
}