#' @title Return a dataframe stored in Medusa
#'
#' @description Return a dataframe stored in Medusa.  This function
#'   downloads CASTEML file by external command `casteml' then read
#'   it as data.frame.
#'
#' @details This function downloads CASTEML file by `casteml
#'   download', convert it to csvfile by `casteml convert', and read
#'   the file by `cbk.read.dataframe()'.
#' @param stone Unique indentification number of stones in
#'   Medusa.  Really, those will pass to `casteml download'.
#' @param tableunit Output unit that will be resolved by
#'   `cbk.convector' (default="none")
#' @param category category to pass to `casteml convert'
#' @return A dataframe with unit organized
#' @seealso \code{casteml download}, \code{casteml convert},
#'   \url{https://github.com/misasa/casteml},
#'   \code{\link{cbk.casteml.download}}, \code{\link{cbk.casteml.convert}},
#'   \code{\link{cbk.read.datafram}}
#' @export
#' @examples
#' tbl0 <- cbk.read("20081202172326.hkitagawa","ppm","trace")
#'
cbk.read <- function(stone,tableunit="none",category="trace"){
  pmlfile <- cbk.casteml.download(stone)
  cbkfile <- cbk.casteml.convert(pmlfile,category)
  tbl0    <- cbk.read.dataframe(cbkfile,tableunit)
  return(tbl0)
}
