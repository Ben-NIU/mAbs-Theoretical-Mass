show.table.Red<-function(Seq.info, PyroE.hc, PyroE.lc, Lys, Glycan.Red, nonRds.intraHC=3, nonRds.intraLC=1){
  source("Calc.Mass.HC.R")
  source("Calc.Mass.LC.R")
  
  ## compute the theoretical formula depending on PTM input.
  HC.form.thr<-Calc.Mass.HC(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan.Red, 0, 0, 0)  ## REPORT
  LC.form.thr<-Calc.Mass.LC(as.character(Seq.info$LC[2]), PyroE.lc, 0, 0)  ## REPORT


  #===========================================================#
  Standard<-c("C"=12.01078, "H"=1.007947, "N"=14.00672, "O"=15.99943, "S"=32.0655)
  ## compute the AVERAGE mass of theoretical formula depending on PTM input.
  HC.mass.thr<-round(sum(HC.form.thr*Standard),1)  ## REPORT
  LC.mass.thr<-round(sum(LC.form.thr*Standard),1)  ## REPORT

  
  HC.form.expect<-Calc.Mass.HC(as.character(Seq.info$HC[2]), PyroE.hc, Lys, Glycan.Red, 0, 0, nonRds.intraHC) ## REPORT
  LC.form.expect<-Calc.Mass.LC(as.character(Seq.info$LC[2]), PyroE.lc, 0, nonRds.intraLC)  ## REPORT
  HC.mass.expect<-round(sum(HC.form.expect*Standard),1)  ## REPORT
  LC.mass.expect<-round(sum(LC.form.expect*Standard),1)  ## REPORT

  
  #===============================================================================#
  HC.info<-data.frame( "C"=as.integer(c(HC.form.expect[["C"]], HC.form.thr[["C"]])),"H"=as.integer(c(HC.form.expect[["H"]], HC.form.thr[["H"]])),"N"=as.integer(c(HC.form.expect[["N"]],HC.form.thr[["N"]])),"O"=as.integer(c(HC.form.expect[["O"]], HC.form.thr[["O"]])),"S"=as.integer(c(HC.form.expect[["S"]],HC.form.thr[["S"]])), "Mass (Da)"=c(HC.mass.expect,HC.mass.thr), "Description"=c("Expected","Theoretical"), check.names = FALSE)
  LC.info<-data.frame( "C"=as.integer(c(LC.form.expect[["C"]], LC.form.thr[["C"]])),"H"=as.integer(c(LC.form.expect[["H"]], LC.form.thr[["H"]])),"N"=as.integer(c(LC.form.expect[["N"]],LC.form.thr[["N"]])),"O"=as.integer(c(LC.form.expect[["O"]], LC.form.thr[["O"]])),"S"=as.integer(c(LC.form.expect[["S"]],LC.form.thr[["S"]])), "Mass (Da)"=c(LC.mass.expect,LC.mass.thr), "Description"=c("Expected","Theoretical"), check.names = FALSE)
  
  Overall<-list(HC=HC.info, LC=LC.info)
  return(Overall)
}
