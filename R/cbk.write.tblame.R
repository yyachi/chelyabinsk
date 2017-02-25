#' Write pmlame as TBLAME.csv
#'
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param outfile Path of TBLAME.csv, which is a csvfile with rows of
#'   stone and columns of chem.
#' @param verbose Output debug info (default: FALSE).
#' @return Path of TBLAME.csv
#' @seealso \code{\link{write.csv}}, \code{\link{cbk.read.tblame}},
#'   and \url{https://github.com/misasa/casteml}
#' @export
cbk.write.tblame <- function(pmlame,outfile,verbose=TRUE){

  ##* Round number
  ## if(!is.null(digit)){
  ##   pmlame <- signif(pmlame,digit)
  ## }

  write.csv(pmlame,outfile)
  return(outfile)
}
