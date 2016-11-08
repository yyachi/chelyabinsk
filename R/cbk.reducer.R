#' Convert major-element concentration as oxide to one as metal
#' @param tbl0 A dataframe with columns of stone and rows of
#' element abundances
#' @param oxide A list of oxide to be converted
#' @return A dataframe with converted columns
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' tbl0    <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' tbl1    <- cbk.reducer(tbl0)
cbk.reducer <- function(tbl0,oxides=NULL) {
### extract "Si" and element numbers
  periodic         <- cbk.periodic()
  oxidelist        <- c("SiO2", "Al2O3", "CaO", "MgO", "Fe2O3", "FeO", "Na2O", "H2O+", "TiO2", "K2O", "P2O5", "MnO")
  convector        <- c("Si"  , "Al"   , "Ca" , "Mg" , "Fe"   , "Fe" , "Na"  , "H"   , "Ti"  , "K"  , "P"   , "MnO")
  names(convector) <- oxidelist
  convector        <- rbind(convector,c(  1,    2,    1,    1,    2,    1,    2,   2,    1,   2,   2,     1))
  convector        <- rbind(convector,c(  2,    3,    1,    1,    3,    1,    1,   1,    2,   1,   5,     1))

### replace "SiO2" column with "Si"
  oxygen <- 15.9994
  if(!is.null(oxides)){
    oxidelist     <- oxides
  }
  for(ii in 1:length(oxidelist)) {
    if (oxidelist[ii] %in% colnames(tbl0)) {
    obj         <- convector[,oxidelist[ii]][1]  # "Si"
    objnum      <- as.numeric(convector[,oxidelist[ii]][2])  # 1 # "Si" number
    oxynum      <- as.numeric(convector[,oxidelist[ii]][3])  # 2 # "O" number 

    objmass     <- periodic[obj,"atomicmass"]
    oxideweight <- objmass * objnum + oxygen * oxynum
    ## tbl0[,obj]  <- tbl0[,oxidelist[ii]] * objmass * objnum / oxideweight
    tbl0[,oxidelist[ii]]  <- tbl0[,oxidelist[ii]] * objmass * objnum / oxideweight # replace value
    colnames(tbl0)[grep(oxidelist[ii],colnames(tbl0))] <- obj # replace colname
    }
  }
  return(tbl0)
}
