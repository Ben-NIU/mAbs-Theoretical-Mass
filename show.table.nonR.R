show.table.nonR<-function(Seq.info, PyroE.hc, PyroE.lc, Lys, Glycan,  Others, howmany, cust,cust.ptm, c.howmany,ds.hing, ds.lchc, ds.hchc, ds.lclc, nonRds.intraHC=3, nonRds.intraLC=1, extra){
  source("Calc.Mass.HC.nonR.R")
  source("Calc.Mass.LC.R")
  
  ## compute the theoretical formula depending on PTM input.
  HC.form.thr<-Calc.Mass.HC.nonR(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan,  Others, howmany, cust, cust.ptm, c.howmany,ds.hing, ds.lchc, ds.hchc, extra)
  LC.form.thr<-Calc.Mass.LC(as.character(Seq.info$LC[2]), PyroE.lc, ds.lclc)
  
  mAb.form.thr<-(HC.form.thr+LC.form.thr)*2  ## REPORT
  ## compute the theoretical formula of polypeptide chain.
  HC.poly<-unlist(ConvertPeptide(as.character(Seq.info$HC[2]), IAA=FALSE))
  LC.poly<-unlist(ConvertPeptide(as.character(Seq.info$LC[2]), IAA=FALSE))
 
  #===========================================================#
  Standard<-c("C"=12.0107, "H"=1.00794, "N"=14.0067, "O"=15.9994, "S"=32.065)
  ## compute the AVERAGE mass of theoretical formula depending on PTM input.
  mAb.mass.thr<-format(round(sum(mAb.form.thr*Standard),2), nsmall = 2)  ## REPORT
  
  HC.mass.poly<-format(round(sum(HC.poly*Standard),2), nsmall = 2)  ## REPORT
  LC.mass.poly<-format(round(sum(LC.poly*Standard),2), nsmall = 2)  ## REPORT

  
  #===============================================================================#
  HC.fml<-data.frame( "C"=HC.poly[["C"]],"H"=HC.poly[["H"]],"N"=HC.poly[["N"]],"O"=HC.poly[["O"]],"S"=HC.poly[["S"]], check.names = FALSE)
  LC.fml<-data.frame( "C"=LC.poly[["C"]],"H"=LC.poly[["H"]],"N"=LC.poly[["N"]],"O"=LC.poly[["O"]],"S"=LC.poly[["S"]], check.names = FALSE)

  
  mAb.fml<-data.frame("C"=mAb.form.thr[["C"]], "H"= mAb.form.thr[["H"]], "N"= mAb.form.thr[["N"]], "O"=mAb.form.thr[["O"]], "S" =mAb.form.thr[["S"]], check.names = FALSE )
  HC.ms<-HC.mass.poly
  LC.ms<-LC.mass.poly
  mAb.ms<-mAb.mass.thr
  
  Overall<-list(HC.fml=HC.fml, LC.fml=LC.fml, mAb.fml=mAb.fml, HC.ms=HC.ms, LC.ms=LC.ms, mAb.ms=mAb.mass.thr)
  return(Overall)
}
