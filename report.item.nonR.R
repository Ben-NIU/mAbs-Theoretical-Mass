report.item.nonR<-function(rpt.pEhc, rpt.pElc, rpt.lys, rpt.glycan, rpt.tds){
  
  if(is.null(rpt.pEhc)){
    rpt.pEhc<-"No"
  }
  
  if(is.null(rpt.pElc)){
    rpt.pElc<-"No"
  }
  
  if(is.null(rpt.lys)){
    rpt.lys<-"No"
    }
  
  if(is.null(rpt.glycan)){
    rpt.glycan<-"Agly"
  }
  
  
  L<-expand.grid("HC-PyroE"=rpt.pEhc, "LC-PyroE"=rpt.pElc, "Lys"=rpt.lys, "Glycan"=rpt.glycan, "DSB"=rpt.tds)
  return(L)
  
}