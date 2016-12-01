#' Remove rows where all values are NA
#' @param pmlame A dataframe with rows of stone and columns of chem
#' @param column Flag to remove columns
#' @return A dataframe without removed rows
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' pmlame1 <- cbk.filter.drop.dharma(pmlame)
cbk.filter.drop.dharma <- function(pmlame,column=FALSE) {
  ## repalce space by NA
  pmlame0   <- apply(pmlame,  2, function(x) gsub("^$|^ $", NA, x))

  ## find lines only with NA
  blank_row <- apply(pmlame0, 1, function(x) all(is.na(x)))

  ## remove useless lines
  pmlame1   <- pmlame[!blank_row, ]

  ## filter columns
  if (column) {
    blank_col <- apply(pmlame0, 2, function(x) all(is.na(x)))
    pmlame1   <- pmlame1[ ,!blank_col]
  }
  return(pmlame1)
}
