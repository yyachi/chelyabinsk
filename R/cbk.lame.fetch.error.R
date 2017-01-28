#' Extract error columns from pmlame
#' @param pmlame A pmlame with values of both mean and error
#' @return A pmlame of error values
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame_error  <- cbk.lame.fetch.error(pmlame)
cbk.lame.fetch.error <- function(pmlame) {
  errorlist            <- grep("_error",colnames(pmlame),value=T)
  errorlame0           <- pmlame[,errorlist]
  colnames(errorlame0) <- unlist(lapply(strsplit(errorlist,"_error"),'[[',1))
  return(errorlame0)
}
