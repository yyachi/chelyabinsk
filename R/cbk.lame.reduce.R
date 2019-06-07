#' Convert major-element concentration as oxide to one as metal
#' @param pmlame A pmlame of element abundances (with columns of stone and rows of chem)
#' @return A pmlame with converted columns
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL,force=TRUE)
#' pmlame1 <- cbk.lame.reduce(pmlame)
#'
#' pmlame <- structure(list(Si = c(NA, NA, NA, 0.233, NA, NA, NA, NA), SiO2 = c(0.5195, 0.5359, 0.5341, 0.499, 0.5133, 0.5899, 0.5305, 0.5)), .Names = c("Si", "SiO2"), class = "data.frame", row.names = c("ref-cpx-klb1", "ref-cpx-sax33", "ref-cpx-sax39c", "ref-gl-bhvo2", "ref-gl-dr1a1", "ref-gl-tahiti", "ref-pl-bytownite-c", "ref_dl_blank"))
#' cbk.lame.reduce(pmlame)
cbk.lame.reduce <- function(pmlame) {

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
| Cr2O3 | Cr    |     1.5 |
| NiO   | Ni    |       1 |
"
  ## BEGIN RECEIVE ORGTBL detoxtable:datain
  detoxtable <- data.frame(row.names=c("SiO2","Al2O3","CaO","MgO","Fe2O3","FeO","Na2O","H2O+","TiO2","K2O","P2O5","MnO","Cr2O3","NiO"))
  detoxtable[,'metal'] <- c("Si","Al","Ca","Mg","Fe","Fe","Na","H","Ti","K","P","Mn","Cr","Ni")
  detoxtable[,'noxygen'] <- c(2,1.5,1,1,1.5,1,0.5,0.5,2,0.5,2.5,1,1.5,1)
  ## END RECEIVE ORGTBL detoxtable:datain

  meanlame0    <- cbk.lame.regulate(pmlame,mean=T,error=F,extra=F)
  errorlame0   <- cbk.lame.fetch.error(pmlame)

  periodic     <- cbk.periodic()
  oxidelist    <- rownames(detoxtable)    # "SiO2","Al2O3","CaO","MgO","Fe2O3","FeO","Na2O","H2O+","TiO2","K2O","P2O5","MnO"
  chemlist     <- colnames(meanlame0)        # "SiO2","Al2O3","La","Ce"
  errorlist    <- colnames(errorlame0)        # "SiO2","Al2O3","La","Ce"
  oxygenweight <- 15.9994
  for(ii in 1:length(oxidelist)) {
    ### replace columns of "SiO2" and "Al2O3" by "Si" and "Al"
    oxide  <- oxidelist[ii]                      # SiO2
    if (oxide %in% chemlist) {
      metal       <- detoxtable[oxide,"metal"]   # Si
      noxygen     <- detoxtable[oxide,"noxygen"] # 2
      metalweight <- periodic[metal,"mass"]      # 28.0855
      oxideweight <- metalweight + oxygenweight * noxygen # 60.0843

      chem_in_oxide      <- meanlame0[,oxide]
      chem_in_metal      <- chem_in_oxide * metalweight / oxideweight
      ## colnames(pmlame)[grep(oxide,chemlist)] <- metal # rename col
      if (metal %in% chemlist) {
        value_i   <- is.na(meanlame0[,metal])
        meanlame0[value_i,metal] <- chem_in_metal[value_i]
        meanlame0 <- meanlame0[,setdiff(chemlist,oxide),drop=FALSE]
      } else {
        meanlame0[,oxide]  <- chem_in_metal # replace value of oxide by metal
        colnames(meanlame0)[which(chemlist == oxide)]  <- metal # rename col
      }

      if (oxide %in% errorlist) {
        error_in_oxide <- errorlame0[,oxide]
        error_in_metal <- error_in_oxide * metalweight / oxideweight
        if (metal %in% errorlist) {
          error_i    <- is.na(errorlame0[,metal])
          errorlame0[error_i,metal] <- chem_in_metal[error_i]
          errorlame0 <- errorlame0[,setdiff(errorlist,oxide),drop=FALSE]
        } else {
          errorlame0[,oxide] <- error_in_metal # replace value of oxide by metal
          colnames(errorlame0)[which(errorlist == oxide)] <- metal # rename col
        }
      }
    }
    chemlist  <- colnames(meanlame0)
    errorlist <- colnames(errorlame0)
  }
  ## colnames(meanlame0) <- chemlist
  if (ncol(errorlame0) > 0) {
    pmlame <- cbk.lame.merge.error(meanlame0,errorlame0)
  } else {
    pmlame <- meanlame0
  }
  return(pmlame)
}
