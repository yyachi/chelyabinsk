#' Extract error columns from pmlame
#' @param pmlame A pmlame with values of both mean and error
#' @return A pmlame of error values
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame0       <- pmlame[,colnames(pmlame) != c("sample_id","image_id")]
#' pmlame0_mean  <- cbk.lame.colMeans(pmlame0)
#' pmlame0_error <- cbk.lame.colSds(pmlame0)
#' pmlame1       <- cbk.lame.merge.error(pmlame0_mean,pmlame0_error)
#' pmlame1_error <- cbk.lame.fetch.error(pmlame1)
cbk.lame.fetch.error <- function(pmlame) {
  errorlist            <- grep("_error",colnames(pmlame),value=T)
  errorlame0           <- pmlame[,errorlist]
  colnames(errorlame0) <- unlist(lapply(strsplit(errorlist,"_error"),'[[',1))
  return(errorlame0)
}
