#' Extract error columns from pmlame as errorlame
#' @param pmlame A pmlame with both mean and error.
#' @param chem Target chem you want to extract.
#' @return An errorlame that is a pmlame of only error colums with
#'   label of `chem' such as `Li7' instead of `Li7_error'.  Note that
#'   columns that exist in input will be returned.
#' @export
#' @examples
#' pmlame    <- cbk.read.casteml(cbk.path("20130528105235-594267.pml"))
#' errorlame <- cbk.lame.fetch.error(pmlame)
#'
#' errorlame <- cbk.lame.fetch.error(pmlame,c("Li","Lu"))
cbk.lame.fetch.error <- function(pmlame,chem=NULL) {
  if (!is.null(chem)) {
    ## chem_error_regexp <- "^([A-Za-z].*)_error$"
    ## chemlist          <- gsub(chem_error_regexp,"\\1",chem) # Li_error -> Li
    errorlist         <- paste0(chem,"_error")
  } else {
    ## errorlist <- grep("_error$",colnames(pmlame),value=T)
    errorlame <- cbk.lame.regulate(pmlame,mean=FALSE,error=TRUE,extra=FALSE)
    errorlist <- colnames(errorlame)
  }
  errorlame0           <- pmlame[,errorlist,drop=F]
  colnames(errorlame0) <- unlist(lapply(strsplit(errorlist,"_error"),'[[',1))
  return(errorlame0)
}
