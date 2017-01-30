#' Regulate pmlame to remove non-chem components
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param chem Flag to extract chem columns (default: TRUE)
#' @param error Flag to extract error columns (default: TRUE)
#' @param extra Flag to extract extra columns (default: FALSE)
#' @return A pmlame with only chem
#' @export
#' @examples
#' pmlame0 <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame  <- cbk.lame.regulate(pmlame0)
cbk.lame.regulate <- function(pmlame,chem=TRUE,error=TRUE,extra=FALSE) {

  extralist <- c("x_image","y_image","x_vs","y_vs","image_path","sample_id","image_id","remark")
  errorlist <- grep("_error",colnames(pmlame),value=T)
  col0      <- colnames(pmlame)

  colDrop <- NULL
  if (!extra) {
    colDrop <- c(colDrop,extralist)
  }

  if (!error) {
    colDrop <- c(colDrop,errorlist)
  }

  if (chem) {
    colOut <- setdiff(col0,colDrop)
  } else {
    col1   <- c(errorlist,intersect(col0,extralist))
    colOut <- setdiff(col1,colDrop)
  }
  pmlame0 <- pmlame[,colOut,drop=FALSE]
  return(pmlame0)
}
