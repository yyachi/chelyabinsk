#' Regulate pmlame to extract mean, error, or extra columns
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param mean Flag to extract mean columns (default: TRUE).
#' @param error Flag to extract error columns (default: TRUE).  Note
#'   that dataframe return by option(mean=F,error=T,extra=F) is NOT
#'   errorlame.  To get errorlame, use \code{cbk.lame.fetch.error}
#'   instead.
#' @param extra Flag to extract extra columns (default: FALSE).
#' @param chem Select only certain chem for mean and error.  With chem
#'   c('Li','Sr88'), columns of 'Li','Li_error','Sr88','Sr88_error'
#'   are selected.
#' @return A pmlame with columns of concern
#' @seealso \code{\link{cbk.lame.fetch.error}}
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame1 <- cbk.lame.regulate(pmlame)
cbk.lame.regulate <- function(pmlame,mean=TRUE,error=TRUE,extra=FALSE,chem=NULL) {

  if (!is.null(chem)) {
    pmlame <- pmlame[,c(chem,paste0(chem,"_error")),drop=FALSE]
  }

  col0     <- colnames(pmlame)
  colError <- grep("_error$",col0,value=T)
  colExtra <- c("x_image","y_image","x_vs","y_vs","image_path","sample_id","image_id","remark","latitude","longitude","elevation")
  colMean  <- setdiff(setdiff(col0,colError),colExtra)

  colDrop <- NULL
  if (!mean) {
    colDrop <- c(colDrop,colMean)
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
