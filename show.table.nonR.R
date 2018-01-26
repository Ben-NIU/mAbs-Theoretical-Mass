show.table.nonR<-function(Seq.info, PyroE.hc, PyroE.lc, Lys, Glycan, ds.hing, ds.lchc, ds.hchc, ds.lclc, nonRds.intraHC=3, nonRds.intraLC=1){
  source("Calc.Mass.HC.nonR.R")
  source("Calc.Mass.LC.R")
  
  ## compute the theoretical formula depending on PTM input.
  HC.form.thr<-Calc.Mass.HC.nonR(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan, ds.hing, ds.lchc, ds.hchc)
  LC.form.thr<-Calc.Mass.LC(as.character(Seq.info$LC[2]), PyroE.lc, ds.lchc, ds.lclc)
  
  mAb.form.thr<-(HC.form.thr+LC.form.thr)*2  ## REPORT
  ## compute the theoretical formula of polypeptide chain.
  HC.poly<-unlist(ConvertPeptide(as.character(Seq.info$HC[2]), IAA=FALSE))
  LC.poly<-unlist(ConvertPeptide(as.character(Seq.info$LC[2]), IAA=FALSE))
  mAb.poly<-(HC.poly+LC.poly)*2
  #===========================================================#
  Standard<-c("C"=12.01078, "H"=1.007947, "N"=14.00672, "O"=15.99943, "S"=32.0655)
  ## compute the AVERAGE mass of theoretical formula depending on PTM input.
  mAb.mass.thr<-format(round(sum(mAb.form.thr*Standard),1), nsmall = 1)  ## REPORT
  
  HC.mass.poly<-format(round(sum(HC.poly*Standard),1), nsmall = 1)  ## REPORT
  LC.mass.poly<-format(round(sum(LC.poly*Standard),1), nsmall = 1)  ## REPORT
  mAb.mass.poly<-format(round(sum(mAb.poly*Standard),1), nsmall = 1)  ## REPORT
  
  #===============================================================================#
  HC.info<-data.frame( "C"=HC.poly[["C"]],"H"=HC.poly[["H"]],"N"=HC.poly[["N"]],"O"=HC.poly[["O"]],"S"=HC.poly[["S"]], "Mass (Da)"=HC.mass.poly, "Description"="Polypeptide", check.names = FALSE)
  LC.info<-data.frame( "C"=LC.poly[["C"]],"H"=LC.poly[["H"]],"N"=LC.poly[["N"]],"O"=LC.poly[["O"]],"S"=LC.poly[["S"]], "Mass (Da)"=LC.mass.poly,"Description"="Polypeptide", check.names = FALSE)
  mAb.info<-data.frame( "C"=c(mAb.form.thr[["C"]], mAb.poly[["C"]]),"H"=c(mAb.form.thr[["H"]], mAb.poly[["H"]]), "N"=c(mAb.form.thr[["N"]], mAb.poly[["N"]]),"O"=c(mAb.form.thr[["O"]], mAb.poly[["O"]]),"S"=c(mAb.form.thr[["S"]], mAb.poly[["S"]]), "Mass (Da)"=c(mAb.mass.thr, mAb.mass.poly), "Description"=c("Expected","Polypeptide"), check.names = FALSE )
  Overall<-list(HC=HC.info, LC=LC.info, mAb=mAb.info)
  return(Overall)
}
