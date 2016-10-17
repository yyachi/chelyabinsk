#' @title Read CASTEML file and return a dataframe
#'
#' @description Read CASTEML file and return a dataframe.
#'
#' @details This function converts a CASTEML file to a csvfile by
#'   `cbk.convert.casteml()' and read it by `cbk.read.dataframe()'.
#' @param pmlfile A CASTEML file that was download by `casteml
#'   download'
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @param category category to pass to `cbk.convert.casteml'
#' @return A dataframe with unit organized
#' @seealso \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dataframe}},
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.kitagawa.pml")
#' tbl0 <- cbk.read.casteml(pmlfile,"ppm","trace")
#'
cbk.read.casteml <- function(pmlfile,tableunit="none",category=NULL){
  cbkfile <- cbk.convert.casteml(pmlfile,category)
  tbl0    <- cbk.read.dataframe(cbkfile,tableunit)
  return(tbl0)
}
