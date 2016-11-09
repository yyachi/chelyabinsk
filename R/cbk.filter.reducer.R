#' Convert major-element concentration as oxide to one as metal
#' @param tbl0 A dataframe with columns of stone and rows of chem
#' @return A dataframe with converted columns
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' tbl0    <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' tbl1    <- cbk.filter.reducer(tbl0)
cbk.filter.reducer <- function(tbl0) {

  comment.datain <- "
#+TBLNAME: detoxtable
#+ORGTBL: SEND detoxtable:datain orgtbl-to-R :no-escape t :dataframe t
|       | metal | noxygen |
|-------+-------+---------|
| SiO2  | Si    |       2 |
| Al2O3 | Al    |     1.5 |
| CaO   | Ca    |       1 |
| MgO   | Mg    |       1 |
| Fe2O3 | Fe    |     1.5 |
| FeO   | Fe    |       1 |
| Na2O  | Na    |     0.5 |
| H2O+  | H     |     0.5 |
| TiO2  | Ti    |       2 |
| K2O   | K     |     0.5 |
| P2O5  | P     |     2.5 |
| MnO   | Mn    |       1 |
"
  ## BEGIN RECEIVE ORGTBL detoxtable:datain
  detoxtable <- data.frame(row.names=c("SiO2","Al2O3","CaO","MgO","Fe2O3","FeO","Na2O","H2O+","TiO2","K2O","P2O5","MnO"))
  detoxtable[,'metal'] <- c("Si","Al","Ca","Mg","Fe","Fe","Na","H","Ti","K","P","Mn")
  detoxtable[,'noxygen'] <- c(2,1.5,1,1,1.5,1,0.5,0.5,2,0.5,2.5,1)
  ## END RECEIVE ORGTBL detoxtable:datain

  periodic             <- cbk.periodic()
  oxidelist <- rownames(detoxtable)    # "SiO2","Al2O3","CaO","MgO","Fe2O3","FeO","Na2O","H2O+","TiO2","K2O","P2O5","MnO"
  chemlist  <- colnames(tbl0)          # "SiO2","Al2O3","La","Ce"
  oxygenweight <- 15.9994
  for(ii in 1:length(oxidelist)) {
    ### replace columns of "SiO2" and "Al2O3" by "Si" and "Al"
    oxide  <- oxidelist[ii]                   # SiO2
    if (oxide %in% chemlist) {
      metal   <- detoxtable[oxide,"metal"]    # Si
      noxygen <- detoxtable[oxide,"noxygen"]  # 2

      metalweight <- periodic[metal,"atomicmass"] # 28.0855
      oxideweight <- metalweight + oxygenweight * noxygen # 60.0843
      chem_in_oxide <- tbl0[,oxide]
      chem_in_metal <- chem_in_oxide * metalweight / oxideweight

      ## colnames(tbl0)[grep(oxide,chemlist)] <- metal # rename col
      colnames(tbl0)[which(chemlist == oxide)] <- metal # rename col
      tbl0[,metal]  <- chem_in_metal # replace value of oxide by metal
    }
  }
  return(tbl0)
}
