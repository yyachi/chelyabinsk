#' Remove rows where all values are NA
#' @param pmlame A dataframe with rows of stone and rows of chem
#' @return A dataframe without removed rows
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' pmlame1 <- cbk.filter.drop.dharma(pmlame)
cbk.filter.drop.dharma <- function(pmlame) {
  ## repalce space by NA
  pmlame0  <- apply(pmlame,  2, function(x) gsub("^$|^ $", NA, x))

  ## find lines only with NA
  blank    <- apply(pmlame0, 1, function(x) all(is.na(x)))

  ## remove useless lines
  pmlame1  <- pmlame[!blank, ]
  return(pmlame1)
}
