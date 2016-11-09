#' Convert major-element concentration as oxide to one as metal
#' @param tbl0 A dataframe with columns of stone and rows of
#' element abundances
#' @param oxides A list of oxides to be converted
#' @return A dataframe with converted columns
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' tbl0    <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' tbl1    <- cbk.reducer(tbl0)
cbk.reducer <- function(tbl0,oxides=NULL) {
### extract "Si" and element numbers
  periodic            <- cbk.periodic()
  oxidelist           <- c("SiO2", "Al2O3", "CaO", "MgO", "Fe2O3", "FeO", "Na2O", "H2O+", "TiO2", "K2O", "P2O5", "MnO")
  convector           <- c("Si"  , "Al"   , "Ca",  "Mg" , "Fe"   , "Fe" , "Na"  , "H"   , "Ti"  , "K"  , "P"   , "Mn")
  noxygen             <- c( 2,      1.5,     1,     1,     1.5,     1,     0.5,    0.5,    2,      0.5,   2.5,    1)
  convector           <- data.frame(metal=I(convector),noxygen)
  rownames(convector) <- oxidelist
  
### replace "SiO2" column with "Si"
  oxygen <- 15.9994
  if(!is.null(oxides)){
    oxidelist     <- oxides
  }
  for(ii in 1:length(oxidelist)) {
    if (oxidelist[ii] %in% colnames(tbl0)) {
    metal   <- convector[oxidelist[ii],"metal"]  # "Si"
    noxygen <- convector[oxidelist[ii],"noxygen"]  # 2 # "O" number 

    metalmass    <- periodic[metal,"atomicmass"]
    oxideweight <- metalmass + oxygen * noxygen
    ## tbl0[,metal]  <- tbl0[,oxidelist[ii]] * metalmass / oxideweight
    tbl0[,oxidelist[ii]]  <- tbl0[,oxidelist[ii]] * metalmass / oxideweight # replace value
    colnames(tbl0)[grep(oxidelist[ii],colnames(tbl0))] <- metal # replace colname
    }
  }
  return(tbl0)
}
