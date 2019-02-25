## For report only: report the mAb formula depending on PTMs input.

report.Calc.Mass.nonR<-function(HC, LC, PyroE.hc, PyroE.lc, Lys, Glycan, ds.hing, ds.lchc, ds.hchc, ds.lclc){
 
  
  if(is.null(PyroE.hc)){
    pyroeHC<-0
  } else {
    if(PyroE.hc=="No"){
      pyroeHC<-0}
    if(PyroE.hc=="Yes"){
      if(substr(HC, 1,1)=="E"){  
        pyroeHC<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(HC, 1,1)=="Q") {
        pyroeHC<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
    }
  }
  
  
  
  if(is.null(PyroE.lc)){
    pyroeLC<-0
  } else {
    if(PyroE.lc=="No"){
      pyroeLC<-0}
    if(PyroE.lc=="Yes"){
      if(substr(LC, 1,1)=="E"){  
        pyroeLC<-c(C=0, H=-2, N=0, O=-1, S=0)} ## -18 Da
      else if(substr(LC, 1,1)=="Q") {
        pyroeLC<-c(C=0, H=-3, N=-1, O=0, S=0)} ## -17 Da
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
  

  
  GLYCAN<-list("Agly"=c(C=0, H=0, N=0, O=0, S=0),"Degly"=c(C=0, H=-1, N=-1, O=1,S=0),'G0/G0'=c("C"=50, "H"=82, "N"=4, "O"=35, "S"=0),'G0/G0-GN'=c("C"=(50+42)/2, "H"=(82+69)/2, "N"=(4+3)/2, "O"=(35+30)/2,"S"=0),"G0F/G0F"=c(C=56, H=92, N=4, O=39, S=0),"G0F/G1F"=c(C=(56+62)/2, H=(92+102)/2, N=(4+4)/2, O=(39+44)/2, S=0),"G1/G1"=c(C=56, H=92, N=4, O=40, S=0),"G1F/G1F"=c(C=62, H=102, N=4, O=44, S=0),"G2F/G2F"=c(C=68, H=112, N=4, O=49, S=0),"G0-GN/G0-GN"=c(C=42, H=69, N=3, O=30, S=0), "G1F/G2F"=c(C=(62+68)/2, H=(102+112)/2, N=4, O=(44+49)/2, S=0), "G0/G1"=c(C=(50+56)/2, H=(82+92)/2, N=(4+4)/2, O=(35+40)/2, S=0), "G1/G2"=c(C=(56+62)/2, H=(92+102)/2, N=(4+4)/2, O=(40+45)/2, S=0), "G2/G2"=c(C=62, H=102, N=4, O=45, S=0))
  

  total<-unlist(ConvertPeptide(HC, IAA=FALSE))*2 + unlist(ConvertPeptide(LC, IAA=FALSE))*2 + pyroeHC*2 +pyroeLC*2 + lys*2 + GLYCAN[[Glycan]]*2  + c(C=0, H=as.numeric(ds.hing)*(-2), N=0, O=0, S=0) + c(C=0, H=as.numeric(ds.lchc)*(-2), N=0, O=0,S=0) + c(C=0, H=as.numeric(ds.hchc)*(-2), N=0, O=0, S=0) + c(C=0, H=as.numeric(ds.lclc)*(-2), N=0, O=0, S=0)
  return(total)
}