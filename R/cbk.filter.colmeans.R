#' Return mean value of each element
#' @param pmlame A dataframe with rows of stone and columns of chem
#' @return A dataframe with mean value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"))
#' pmlame0 <- pmlame[,colnames(pmlame) != "sample_id"]
#' pmlame1 <- cbk.filter.colmeans(pmlame0)
cbk.filter.colmeans <- function(pmlame) {
  pmlame0 <- data.frame(t(colMeans(pmlame,na.rm=TRUE)))
  return(pmlame0)
}
