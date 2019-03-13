## A function to add one space in the peptide sequence for every y residues.

Addspace<-function(x, y){
  aa<-strsplit(as.character(x), split = "")[[1]]
  l.aa<-length(aa)
  sec<-l.aa %/% y
  if(l.aa<=y){
    return(as.character(x))
  } else {  
    
    S<-NULL
    for(i in 0:sec){
      
      AA<-aa[(1+(i*y)):(y+(i*y))]
      AA<-AA[!is.na(AA)]
      AA[length(AA)+1]<-" "
      S0<-paste(AA, collapse = "")
      S<-c(S,S0)
      
    }
    return(paste(S, collapse = ""))
    
  }
}