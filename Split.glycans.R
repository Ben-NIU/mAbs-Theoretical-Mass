Split.glycans<-function(nonR.selected){
  
  h<-nonR.selected[grepl("/", nonR.selected)]
  
  splt<-function(x) {strsplit(x, split = "/")[[1]]}
  
 uni.glycan<- unique(do.call("c",lapply(h, splt)))

  hx<-nonR.selected[!grepl("/", nonR.selected)]
  return(c(hx, uni.glycan))
}