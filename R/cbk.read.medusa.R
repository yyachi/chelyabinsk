#' @title Return analyses records stored in Medusa in a form of a
#'   dataframe
#'
#' @description Return analyses records stored in Medusa in a form of
#'   a dataframe.  This function downloads CASTEML file by external
#'   command `casteml' then reads it as data.frame.
#'
#' @details This function downloads a CASTEML file by
#'   `cbk.download.casteml()', converts it to csvfile by
#'   `cbk.convert.casteml()', and reads the file by
#'   `cbk.read.dataframe()'.
#' @param stone Unique indentification number of stones in Medusa.
#'   Really, those will pass to cbk.download.casteml().
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default="none")
#' @param category category to pass to `casteml convert'
#' @return A dataframe with unit organized
#' @seealso \url{https://github.com/misasa/casteml},
#'   \code{\link{cbk.download.casteml}},
#'   \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dataframe}}
#' @export
#' @examples
#' tbl0 <- cbk.read.medusa("20081202172326.hkitagawa","ppm","trace")
#'
cbk.read.medusa <- function(stone,tableunit="none",category=NULL){
  pmlfile <- cbk.download.casteml(stone)
  tbl0    <- cbk.read.casteml(pmlfile,tableunit,category)
  return(tbl0)
}
