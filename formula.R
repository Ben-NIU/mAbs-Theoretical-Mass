
Turn<-function(composition, cust){
  
if(cust == "No") {return(c("C"=0, "H"=0, "N"=0, "O"=0, "S"=0))}
  else if(cust == "Yes") {
  
if(composition==""| is.null(composition)){num<-c(C=0, H=0, N=0, O=0, S=0)
}else {
  
  SPL<-strsplit(composition,"")[[1]]
  SPL<-SPL[!grepl(" ", SPL)]

tr<-function(x){## transform a vector of numbers, so that each number of the new vector equals to the old number plus its position within that vector.
  for(j in 1:length(x)) {
    x[j]<-x[j]+j}
  return(x)
  }

tr2<-function(composition){

  K<-NULL
  for(i in 1:length(SPL)){
    if((SPL[i] %in% c("C","H","N","O","S")) & (SPL[i+1] %in% c("C","H","N","O","S") | is.na(SPL[i+1]))  ){
      K0<-i
      K<-c(K, K0)}
  }
  
  f<-rep("x", length(SPL)+length(K))
  f[tr(K)]<-"1"
  f[which(f!="1")]<-SPL
  return(f)
  
  
}

SPL<-tr2(composition)


x<-suppressWarnings(as.numeric(SPL))

y<-(is.na(x))
x[y]<-","
z<-gsub(","," ",x[-1])

a<-gsub(" ",",",paste(z, collapse = ""))

D<-!is.na(as.numeric(strsplit(a, split = ",")[[1]]))
num<-as.numeric(strsplit(a, split = ",")[[1]])[D]


names(num)<-SPL[which(SPL %in% c("C","H","N","O","S"))]

}
  judge<-function(x){
    C<-if("C" %in% names(x)){x[["C"]]} else {0}
    N<-if("N" %in% names(x)){x[["N"]]} else {0}
    H<-if("H" %in% names(x)){x[["H"]]} else {0}
    O<-if("O" %in% names(x)){x[["O"]]} else {0}
    S<-if("S" %in% names(x)){x[["S"]]} else {0}
    return(c("C"=C, "H"=H, "N"=N, "O"=O, "S"=S))
  }
  
    
return(judge(num))
  }
}