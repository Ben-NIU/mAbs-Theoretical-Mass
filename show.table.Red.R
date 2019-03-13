show.table.Red<-function(Seq.info, PyroE.hc, PyroE.lc, Lys, Glycan.Red, Others, howmany1, where1, r.cust, fml.cust, where2, howmany2, ds.unhc=3, ds.unlc=2, EXTRA_R){
  source("Calc.Mass.HC.Red.R")
  source("Calc.Mass.LC.Red.R")
  
  ## compute the theoretical formula depending on PTM input.
  HC.form.thr<-Calc.Mass.HC.Red(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan.Red, Others, howmany1, where1, r.cust, fml.cust, where2, howmany2,0, EXTRA_R)  ## REPORT
  LC.form.thr<-Calc.Mass.LC.Red(as.character(Seq.info$LC[2]), PyroE.lc, Others, howmany1,where1,  r.cust, fml.cust, where2, howmany2,0)  ## REPORT


  #===========================================================#
  Standard<-c("C"=12.0107, "H"=1.00794, "N"=14.0067, "O"=15.9994, "S"=32.065)
  ## compute the AVERAGE mass of theoretical formula depending on PTM input.
  HC.mass.thr<-round(sum(HC.form.thr*Standard),2)  ## REPORT
  LC.mass.thr<-round(sum(LC.form.thr*Standard),2)  ## REPORT

  
  HC.form.expect<-Calc.Mass.HC.Red(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan.Red, Others, howmany1, where1, r.cust, fml.cust, where2, howmany2, ds.unhc,EXTRA_R)  ## REPORT
  LC.form.expect<-Calc.Mass.LC.Red(as.character(Seq.info$LC[2]), PyroE.lc, Others, howmany1,where1,  r.cust, fml.cust, where2, howmany2, ds.unlc)  ## REPORT
  HC.mass.expect<-round(sum(HC.form.expect*Standard),2)  ## REPORT
  LC.mass.expect<-round(sum(LC.form.expect*Standard),2)  ## REPORT

  
  #===============================================================================#
  HC.info<-data.frame( "C"=as.integer(c(HC.form.expect[["C"]], HC.form.thr[["C"]])),"H"=as.integer(c(HC.form.expect[["H"]], HC.form.thr[["H"]])),"N"=as.integer(c(HC.form.expect[["N"]],HC.form.thr[["N"]])),"O"=as.integer(c(HC.form.expect[["O"]], HC.form.thr[["O"]])),"S"=as.integer(c(HC.form.expect[["S"]],HC.form.thr[["S"]])), "Mass (Da)"=c(HC.mass.expect,HC.mass.thr), "Description"=c("Partially reduced","Fully reduced"), check.names = FALSE)
  LC.info<-data.frame( "C"=as.integer(c(LC.form.expect[["C"]], LC.form.thr[["C"]])),"H"=as.integer(c(LC.form.expect[["H"]], LC.form.thr[["H"]])),"N"=as.integer(c(LC.form.expect[["N"]],LC.form.thr[["N"]])),"O"=as.integer(c(LC.form.expect[["O"]], LC.form.thr[["O"]])),"S"=as.integer(c(LC.form.expect[["S"]],LC.form.thr[["S"]])), "Mass (Da)"=c(LC.mass.expect,LC.mass.thr), "Description"=c("Partially reduced","Fully reduced"), check.names = FALSE)
  
  Overall<-list(HC=HC.info, LC=LC.info)
  return(Overall)
}
