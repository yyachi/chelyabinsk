#' Write pmlame as TBLAME.csv
#'
#' @param pmlame A pmlame with row of stone and column of chem.
#' @param tblame Path of TBLAME.csv, which is a csvfile with rows of stone
#'   and columns of chem.
#' @return Path of TBLAME.csv
#' @seealso \code{\link{write.csv}}, \code{\link{cbk.read.tblame}},
#'   and \url{https://github.com/misasa/casteml}
#' @export
cbk.write.tblame <- function(pmlame,tblame){
  write.csv(pmlame,tblame)
  return(tblame)
}
