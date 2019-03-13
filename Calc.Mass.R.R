Calc.Mass.R<-function(HC, LC, PyroE.hc, PyroE.lc, Lys, Glycan, unR.HC, unR.LC,Ab.format, FP.ngly){
  
  
  if(is.null(PyroE.hc)){
    pyroe.hc<-0
  } else {
    if(PyroE.hc=="No"){
      pyroe.hc<-0}
    if(PyroE.hc=="Yes"){
      if(substr(HC, 1,1)=="E"){  
        pyroe.hc<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(HC, 1,1)=="Q") {
        pyroe.hc<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
    }
  }
  
  
  
  if(is.null(PyroE.lc)){
    pyroe.lc<-0
  } else {
    if(PyroE.lc=="No"){
      pyroe.lc<-0}
    if(PyroE.lc=="Yes"){
      if(substr(HC, 1,1)=="E"){  
        pyroe.lc<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(HC, 1,1)=="Q") {
        pyroe.lc<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
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
  
  if(Ab.format!="Fusion protein"){
    FP.Ngly<-1
  } else {FP.Ngly<-FP.ngly}
  
  if(is.null(FP.ngly)){FP.Ngly<-1}else{FP.Ngly<-FP.ngly}
  
  GLYCAN<-list("Agly"=c(C=0, H=0, N=0, O=0, S=0),"Degly"=c(C=0, H=-1, N=-1, O=1,S=0),'G0'=c("C"=50, "H"=82, "N"=4, "O"=35, "S"=0),"G0F"=c(C=56, H=92, N=4, O=39, S=0),"G1"=c(C=56, H=92, N=4, O=40, S=0),"G2"=c(C=62, H=102, N=4, O=45, S=0),"G1F"=c(C=62, H=102, N=4, O=44, S=0),"G2F"=c(C=68, H=112, N=4, O=49, S=0),"G0-GN"=c(C=42, H=69, N=3, O=30, S=0), "M5"=c(C=46, H=76, N=2, O=35, S=0), "M6"=c(C=52, H=86, N=2, O=40, S=0))
  
  
## with un-reduced DSB in HC and LC:  
HC_1<-unlist(ConvertPeptide(HC, IAA=FALSE)) + pyroe.hc +lys + GLYCAN[[Glycan]]*FP.Ngly  + c(C=0, H=as.numeric(unR.HC)*(-2), N=0, O=0, S=0)

LC_1<-unlist(ConvertPeptide(LC, IAA=FALSE)) + pyroe.lc  + c(C=0, H=as.numeric(unR.LC)*(-2), N=0, O=0, S=0)

HC_2<-unlist(ConvertPeptide(HC, IAA=FALSE)) + pyroe.hc +lys + GLYCAN[[Glycan]]*FP.Ngly  + c(C=0, H=as.numeric(unR.HC)*(0), N=0, O=0, S=0)

LC_2<-unlist(ConvertPeptide(LC, IAA=FALSE)) + pyroe.lc  + c(C=0, H=as.numeric(unR.LC)*(0), N=0, O=0, S=0)

Reduced<-list("HC.withDSB"=HC_1, "LC.withDSB"=LC_1, "HC.fullR"=HC_2, "LC.fullR"=LC_2)

return(Reduced)
}