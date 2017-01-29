#' Show pmlame in console
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param format Format specifier that is passed to \code{casteml
#'   convert}.
#' @return A pmlame that is not processed.
#' @export
#' @examples
#' pmlame0 <- structure(list(SiO2 = c(0.59, 0.52), Li = c(2.08e-05, 1.37e-06), Sr = c(0.000107, 3.61e-05)), row.names = c("ref-gl-tahiti", "ref-cpx-klb1"), .Names = c("SiO2", "Li", "Sr"), class = "data.frame")
#' cbk.lame.show(pmlame0)
#' cbk.lame.show(pmlame0,'isorg')
cbk.lame.show <- function(pmlame,format=NULL) {
  if (is.null(format)){
    page(pmlame) # via dput
  } else {
    pmlfile <- cbk.write.casteml(pmlame)
    tmpfile <- cbk.convert.casteml(pmlfile,format=format)
    file.show(tmpfile)
    ## string <- readChar(tmpfile,nchars=1e9) # read until 1 GB
  }
  return(pmlame)
}
