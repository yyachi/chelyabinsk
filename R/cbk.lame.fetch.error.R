#' Extract error columns from pmlame
#' @param pmlame A pmlame with values of both mean and error
#' @param chem Target element you want to extract
#' @return A pmlame of error values
#' @export
#' @examples
#' pmlame        <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' pmlame_error  <- cbk.lame.fetch.error(pmlame)
#'
#' pmlame_error  <- cbk.lame.fetch.error(pmlame,c("Li","Lu"))
cbk.lame.fetch.error <- function(pmlame,chem=NULL) {
  if (!is.null(chem)) {
    chemlist  <- gsub("^([A-Za-z].*)_error","\\1",chem) # Li_error -> Li
    errorlist <- paste0(chemlist, "_error")
  } else {
    errorlist <- grep("_error",colnames(pmlame),value=T)
  }
  errorlame0           <- pmlame[,errorlist]
  colnames(errorlame0) <- unlist(lapply(strsplit(errorlist,"_error"),'[[',1))
  return(errorlame0)
}
