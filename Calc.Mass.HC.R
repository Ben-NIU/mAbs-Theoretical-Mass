## Return the HC formula depending on PTM input.

Calc.Mass.HC<-function(HC, PyroE.hc, Lys, Glycan, AorB, Others, howmany, where, ds.hing, ds.lchc, ds.hchc){
 

  
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

  
  GLYCAN<-list("Aglycosylated"=c(C=0, H=0, N=0, O=0, S=0),"Deglycosylated"=c(C=0, H=-1, N=-1, O=1,S=0),'G0'=c("C"=50, "H"=82, "N"=4, "O"=35, "S"=0),"G0F"=c(C=56, H=92, N=4, O=39, S=0),"G1"=c(C=56, H=92, N=4, O=40, S=0),"G1F"=c(C=62, H=102, N=4, O=44, S=0),"G2F"=c(C=68, H=112, N=4, O=49, S=0),"G0-GN"=c(C=42, H=69, N=3, O=30, S=0))
 
  total<-unlist(ConvertPeptide(HC, IAA=FALSE)) + pyroe + lys + GLYCAN[[Glycan]] + c(C=0, H=as.numeric(ds.hing)*(-1), N=0, O=0, S=0) + c(C=0, H=as.numeric(ds.lchc)*(-1), N=0, O=0,S=0) + c(C=0, H=as.numeric(ds.hchc)*(-2), N=0, O=0, S=0)
  return(total)
}