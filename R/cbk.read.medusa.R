#' @title Return analyses records stored in Medusa in a form of a
#'   dataframe
#'
#' @description Return analyses records stored in Medusa in a form of
#'   a dataframe.  This function downloads CASTEML file by external
#'   command `casteml' then reads it as data.frame.
#'
#' @details This function downloads CASTEML file by
#'   `cbk.casteml.download()', convert it to csvfile by
#'   `cbk.casteml.convert()', and read the file by
#'   `cbk.read.dataframe()'.
#' @param stone Unique indentification number of stones in Medusa.
#'   Really, those will pass to `casteml download'.
#' @param tableunit Output unit that will be resolved by
#'   `cbk.convector()' (default="none")
#' @param category category to pass to `casteml convert'
#' @return A dataframe with unit organized
#' @seealso \url{https://github.com/misasa/casteml},
#'   \code{\link{cbk.casteml.download}},
#'   \code{\link{cbk.casteml.convert}},
#'   \code{\link{cbk.read.dataframe}}
#' @export
#' @examples
#' tbl0 <- cbk.read.medusa("20081202172326.hkitagawa","ppm","trace")
#'
cbk.read.medusa <- function(stone,tableunit="none",category="trace"){
  pmlfile <- cbk.casteml.download(stone)
  tbl0    <- cbk.read.casteml(pmlfile,tableunit,category)
  return(tbl0)
}
