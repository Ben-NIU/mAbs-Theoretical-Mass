read.fasta<-function(x){
  r<-suppressWarnings(readLines(x))
  LH<-which(substr(r,1,1)==">")
  chain1<-gsub(" ","", paste(r[2:(LH[2]-1)], collapse=""))
  chain2<-gsub(" ","", paste(r[(LH[2]+1):length(r)], collapse=""))
  Len1<-length(strsplit(chain1, split="")[[1]])
  Len2<-length(strsplit(chain2, split="")[[1]])

  if(Len1<Len2){
    HC<-chain2
    LC<-chain1} else {
    HC<-chain1
    LC<-chain2}
  
  Seq<-data.frame("HC"=c(max(Len1, Len2), HC), "LC"=c(min(Len1,Len2), LC))
  return(Seq)
}