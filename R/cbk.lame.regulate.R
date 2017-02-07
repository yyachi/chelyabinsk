#' Regulate pmlame to extract chem, error, or extra columns
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem Flag to extract chem columns (default: TRUE).
#' @param error Flag to extract error columns (default: TRUE).
#' @param extra Flag to extract extra columns (default: FALSE).
#' @return A pmlame with columns of concern
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame1 <- cbk.lame.regulate(pmlame)
cbk.lame.regulate <- function(pmlame,chem=TRUE,error=TRUE,extra=FALSE) {

  col0     <- colnames(pmlame)
  colError <- grep("_error$",col0,value=T)
  colExtra <- c("x_image","y_image","x_vs","y_vs","image_path","sample_id","image_id","remark")
  colChem  <- setdiff(setdiff(col0,colError),colExtra)

  colDrop <- NULL
  if (!chem) {
    colDrop <- c(colDrop,colChem)
  }
  if (!error) {
    colDrop <- c(colDrop,colError)
  }
  if (!extra) {
    colDrop <- c(colDrop,colExtra)
  }

  col1 <- setdiff(col0,colDrop)
  pmlame1 <- pmlame[,col1,drop=FALSE]
  return(pmlame1)
}
