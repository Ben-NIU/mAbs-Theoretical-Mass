harnessNewUI_NR<-function(new.glycan, new.site){
  
if(is.null(new.glycan) | is.null(new.site)){g<-0
} else{
 
  
  GLYCAN<-list("Aglycosylated"=c(C=0, H=0, N=0, O=0, S=0),"Deglycosylated"=c(C=0, H=-1, N=-1, O=1,S=0),'G0'=c("C"=50, "H"=82, "N"=4, "O"=35, "S"=0),"G0F"=c(C=56, H=92, N=4, O=39, S=0),"G1"=c(C=56, H=92, N=4, O=40, S=0),"G2"=c(C=62, H=102, N=4, O=45, S=0),"G1F"=c(C=62, H=102, N=4, O=44, S=0),"G2F"=c(C=68, H=112, N=4, O=49, S=0),"G0-GN"=c(C=42, H=69, N=3, O=30, S=0), "M5"=c(C=46, H=76, N=2, O=35, S=0), "M6"=c(C=52, H=86, N=2, O=40, S=0))

 g<-c(C=0, H=0, N=0,O=0,S=0)
 for(i in 1:length(new.glycan)){
g0<-GLYCAN[[new.glycan[i]]]*(as.numeric(new.site[i])/2)
 g<-g+g0
 }
}

 return(g)
  
  
}



  
  

