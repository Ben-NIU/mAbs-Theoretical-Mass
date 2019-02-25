report.item.R<-function(rpt.pEhc, rpt.pElc, rpt.lys, rpt.glycan, rpt.uds.hc, rpt.uds.lc){
  
  source("Split.glycans.R")
  
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
  } else (rpt.glycan<-Split.glycans(rpt.glycan))
  
  
  
  
  Q<-expand.grid("HC-PyroE"=rpt.pEhc, "LC-PyroE"=rpt.pElc, "Lys"=rpt.lys, "Glycan"=rpt.glycan, "unR.HC"=rpt.uds.hc, "unR.LC"=rpt.uds.lc)
  return(Q)
  
}