EC.280<-function(HC, LC, SWITCH, payload.MA, payload.n){
  
  
 EC.thr<- if(SWITCH=="No") {sum(strsplit(HC, split = "")[[1]] %in% "Y")*1490*2 + sum(strsplit(LC, split = "")[[1]] %in% "Y")*1490*2 + sum(strsplit(HC, split = "")[[1]] %in% "W")*5500*2 + sum(strsplit(LC, split = "")[[1]] %in% "W")*5500*2 + sum(strsplit(HC, split = "")[[1]] %in% "C")*125 + sum(strsplit(LC, split = "")[[1]] %in% "C")*125 } else if (SWITCH=="Yes"){
   sum(strsplit(HC, split = "")[[1]] %in% "Y")*1490*2 + sum(strsplit(LC, split = "")[[1]] %in% "Y")*1490*2 + sum(strsplit(HC, split = "")[[1]] %in% "W")*5500*2 + sum(strsplit(LC, split = "")[[1]] %in% "W")*5500*2 + sum(strsplit(HC, split = "")[[1]] %in% "C")*125 + sum(strsplit(LC, split = "")[[1]] %in% "C")*125 + payload.MA*payload.n
   
 }
 
 
 
  return(format(EC.thr, scientific = FALSE))
}