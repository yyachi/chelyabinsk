#' @title Return a dataframe from CASTEML file
#'
#' @description Return a dataframe from CASTEML file.
#'
#' @details This function convert CASTEML file to csvfile by
#'   `cbk.casteml.convert()' and read it by `cbk.read.dataframe()'.
#' @param pmlfile CASTEML file download by `casteml download'
#' @param tableunit Output unit that will be resolved by
#'   `cbk.convector()' (default="none")
#' @param category category to pass to `cbk.casteml.convert'
#' @return A dataframe with unit organized
#' @seealso
#'   \code{\link{cbk.casteml.convert}},
#'   \code{\link{cbk.read.datafram}},
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.kitagawa.pml")
#' tbl0 <- cbk.read.casteml(pmlfile,"ppm","trace")
#'
cbk.read.casteml <- function(pmlfile,tableunit="none",category="trace"){
  cbkfile <- cbk.casteml.convert(pmlfile,category)
  tbl0    <- cbk.read.dataframe(cbkfile,tableunit)
  return(tbl0)
}
