## Return the HC formula depending on PTM input.

Calc.Mass.HC.Red<-function(HC, PyroE.hc, Lys, r.Glycan, r.Others, howmany, where.other, r.cust, fml.cust, where.cust, howmany.cust,ds.hcun=3, EXTRA_R){
 
  source("formula.R")
  
  if(is.null(PyroE.hc)){
    pyroe<-0
  } else {
    if(PyroE.hc=="No"){
      pyroe<-0}
    if(PyroE.hc=="Yes"){
      if(substr(HC, 1,1)=="E"){  
        pyroe<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(HC, 1,1)=="Q") {
        pyroe<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
    }
  }
  
  if(is.null(Lys)){
    lys<-0
  } else {
    if(Lys=="No"){
      lys<-0
    } else if(Lys=="Yes") {
      lys<-c(C=-6, H=-12, N=-2, O=-1, S=0)}
  }

  
  
  if(is.null(fml.cust)){Fml.cust<-"C0"} else{Fml.cust<-fml.cust}
  if(is.null(howmany.cust)){Howmany.cust<-0} else{Howmany.cust<-howmany.cust}
  if(is.null(where.cust)){where.Cust<-"XXX"} else{where.Cust<-where.cust}
  if(is.null(r.Others)){RO<-"none"} else{RO<-r.Others}
  
  
  
  RGLYCAN<-list("Aglycosylated"=c(C=0, H=0, N=0, O=0, S=0),"Deglycosylated"=c(C=0, H=-1, N=-1, O=1,S=0),'G0'=c("C"=50, "H"=82, "N"=4, "O"=35, "S"=0),"G0F"=c(C=56, H=92, N=4, O=39, S=0),"G1"=c(C=56, H=92, N=4, O=40, S=0),"G2"=c(C=62, H=102, N=4, O=45, S=0),"G1F"=c(C=62, H=102, N=4, O=44, S=0),"G2F"=c(C=68, H=112, N=4, O=49, S=0),"G0-GN"=c(C=42, H=69, N=3, O=30, S=0), "M5"=c(C=46, H=76, N=2, O=35, S=0), "M6"=c(C=52, H=86, N=2, O=40, S=0))
  
  ROTHERS<-list("+Lys"=c("C"=6, "H"=12,"N"=2, "O"=1, "S"=0), "none"=c(C=0, H=0, N=0, O=0, S=0), "+Glycation"=c(C=6, H=10, N=0, O=5, S=0), "Oxidation"=c(C=0, H=0, N=0, O=1, S=0), "Water-Loss"=c(C=0, H=-2, N=0, O=-1, S=0), "NH3-Loss"=c(C=0, H=-3, N=-1, O=0, S=0), "Deamidation"=c(C=0, H=-1, N=-1, O=1, S=0), "Cysteinylation"=c(C=3, H=5, N=1, O=2,S=1))
  
  EE<-if(where.other=="HC"){1} else if(where.other=="LC"){0}
  D<-if(where.Cust=="HC"){1} else if(where.Cust=="LC"){0} else {0}

  

    
    total<-unlist(ConvertPeptide(HC, IAA=FALSE)) + pyroe + lys + RGLYCAN[[r.Glycan]] + ROTHERS[[RO]]*howmany*EE + Turn(Fml.cust,r.cust)*Howmany.cust*D + c(C=0, H=as.numeric(ds.hcun)*(-2), N=0, O=0, S=0) +EXTRA_R
    


  return(total)
}