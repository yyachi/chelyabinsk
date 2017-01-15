#' Convert major-element concentration as oxide to one as metal
#' @param pmlame A pmlame of element abundances (with columns of stone and rows of chem)
#' @return A pmlame with converted columns
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' pmlame1 <- cbk.filter.reduce(pmlame)
cbk.filter.reduce <- function(pmlame) {

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
  chemlist  <- colnames(pmlame)        # "SiO2","Al2O3","La","Ce"
  oxygenweight <- 15.9994
  for(ii in 1:length(oxidelist)) {
    ### replace columns of "SiO2" and "Al2O3" by "Si" and "Al"
    oxide  <- oxidelist[ii]                   # SiO2
    if (oxide %in% chemlist) {
      metal   <- detoxtable[oxide,"metal"]    # Si
      noxygen <- detoxtable[oxide,"noxygen"]  # 2

      metalweight <- periodic[metal,"mass"]   # 28.0855
      oxideweight <- metalweight + oxygenweight * noxygen # 60.0843
      chem_in_oxide <- pmlame[,oxide]
      chem_in_metal <- chem_in_oxide * metalweight / oxideweight

      ## colnames(pmlame)[grep(oxide,chemlist)] <- metal # rename col
      colnames(pmlame)[which(chemlist == oxide)] <- metal # rename col
      pmlame[,metal]  <- chem_in_metal # replace value of oxide by metal
    }
  }
  return(pmlame)
}
