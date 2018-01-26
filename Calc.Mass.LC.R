# Return the LC formula depending on PTM input.

Calc.Mass.LC<-function(LC, PyroE.lc,ds.lchc, ds.lclc){
  


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
  
  total<-unlist(ConvertPeptide(LC, IAA=FALSE)) + pyroe + c(C=0, H=as.numeric(ds.lchc)*(-1), N=0, O=0,S=0) + c(C=0, H=as.numeric(ds.lclc)*(-2), N=0, O=0, S=0)
  return(total)
}