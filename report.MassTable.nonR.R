Report.MassTable.nonR<-function(from.CalcTable){
  
  comp<-from.CalcTable[,6:10]
  
  atomic<-c(C=12.0107, H=1.00794, N=14.0067, O=15.9994, S=32.065)
  
  M<-NULL
  
  for(h in 1:nrow(comp)){
    
   M0<-format(round(sum(comp[h,]*atomic),2), nsmall=2)
   M<-c(M, M0)
  }
  
  from.CalcTable$Mass<-M
  



  return(from.CalcTable)
}