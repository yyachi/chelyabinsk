#' Regulate pmlame with mean and error
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @return A regulate pmlame with values of both mean and error.
#' @export
#' @examples
cbk.lame.regulate <- function(pmlame) {

  ## colDrop <- c("image_path","sample_id","image_id","remark")
  ## chem0   <-  colnames(pmlame)
  ## chem    <- grep(colDrop,colnames(pmlame),value=T,invert=T)
  ## pmlame0 <- pmlame[,chem]
  pmlame0 <- pmlame

  return(pmlame0)
}
