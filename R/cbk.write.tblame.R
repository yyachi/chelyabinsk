#' @title Write pmlame as tblame.csv
#'
#' @param pmlame A pmlame with rows of stone and columns of chem
#' @param tblame Path of tblame, which is a csvfile with rows of stone
#'   and columns of chem.
#' @return A dataframe with unit organized
#' @seealso \code{\link{write.csv}}, \code{\link{cbk.read.tblame}},
#'   and \url{https://github.com/misasa/casteml}
#' @export
cbk.write.tblame <- function(pmlame,tblame){
  write.csv(pmlame,tblame)
  return(tblame)
}
