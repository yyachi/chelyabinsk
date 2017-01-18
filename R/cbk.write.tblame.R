#' @title Write pmlame as tblame.csv
#'
#' @param pmlame A pmlame with rows of stone and columns of chem
#' @param tblame Path of tblame, which is a csvfile with rows of stone
#'   and columns of chem.
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.read.dflame}},
#'   \url{https://github.com/misasa/casteml}, and
#'   \code{\link{cbk.convector}}
#' @export
cbk.write.tblame <- function(pmlame,tblame){
  write.csv(pmlame,tblame)
  return(tblame)
}
