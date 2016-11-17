#' Remove rows with all values are NA
#' @param pmlame A dataframe with rows of stone and rows of chem
#' @return A dataframe without removed rows
#' @export
#' @examples
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' pmlame1 <- cbk.filter.drop.dharma(pmlame)
cbk.filter.drop.dharma <- function(pmlame) {
  blank  <- apply(pmlame, 1, function(x) all(is.na(x)))
  pmlame <- pmlame[!blank, ]
  return(pmlame)
}
