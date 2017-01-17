#' Return standard deviation value of each element
#' @param pmlame A pmlame with rows of stone and columns of chem
#' @return A pmlame with standard deviation value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"))
#' pmlame0 <- pmlame[,colnames(pmlame) != "sample_id"]
#' pmlame1 <- cbk.lame.colSds(pmlame0)
cbk.lame.colSds <- function(pmlame) {
  pmlame0 <- data.frame(t(apply(pmlame, 2, sd,na.rm=T)))
  return(pmlame0)
}
