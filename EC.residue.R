EC.residue<-function(hc, lc){
  Trp<-sum(strsplit(as.character(hc), split = "")[[1]] %in% "W")*2 + sum(strsplit(as.character(lc), split = "")[[1]] %in% "W")*2
  
  Tyr<-sum(strsplit(as.character(hc), split = "")[[1]] %in% "Y")*2 + sum(strsplit(as.character(lc), split = "")[[1]] %in% "Y")*2
  
  Cystine<-sum(strsplit(as.character(hc), split = "")[[1]] %in% "C") + sum(strsplit(as.character(lc), split = "")[[1]] %in% "C")
  
  return(data.frame("Component"=c("Tryptophan", "Tyrosine", "Cystine"), "Counts"=c(Trp, Tyr, Cystine)))
}