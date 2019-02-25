ResCount<-function(seq) {
  AA<-strsplit("ACDEFGHIKLMNPQRSTVWY","")[[1]]
  AAA<-c("Ala","Cys","Asp","Glu","Phe","Gly","His","Ile","Lys","Leu","Met","Asn","Pro","Gln","Arg","Ser","Thr","Val","Trp","Tyr")
	l<-lapply(AA, function(x) sum(strsplit(seq,"")[[1]] %in% x))
  count<-do.call("c",l)
  tabl<-data.frame("Counts"=count,check.names = FALSE)
  tabl$'Percent %'<-round(tabl$Counts * 100/ sum(tabl$Counts),1)
  row.names(tabl)<-AAA
  t(subset(tabl, Counts>0))
  } 
	