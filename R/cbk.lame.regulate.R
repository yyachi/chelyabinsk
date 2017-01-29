#' Regulate pmlame to remove non-chem components
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @return A pmlame with only chem
#' @export
#' @examples
#' pmlame0 <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame  <- cbk.lame.regulate(pmlame0)
cbk.lame.regulate <- function(pmlame) {

  colDrop <- c("image_path","sample_id","image_id","remark")
  col0    <- colnames(pmlame)
  chem    <- setdiff(col0,colDrop)
  pmlame0 <- pmlame[,chem]

  return(pmlame0)
}
