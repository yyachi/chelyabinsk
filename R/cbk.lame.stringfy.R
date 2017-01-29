#' Show pmlame as string
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param format Format specifier that is passed to \code{casteml
#'   convert}.
#' @param show Show string in display using \code{file.show}.
#' @return A pmlame expressed in text
#' @export
#' @examples
#' pmlame0 <- structure(list(SiO2 = c(0.59, 0.52), Li = c(2.08e-05, 1.37e-06), Sr = c(0.000107, 3.61e-05)), row.names = c("ref-gl-tahiti", "ref-cpx-klb1"), .Names = c("SiO2", "Li", "Sr"), class = "data.frame")
#' cbk.lame.stringfy(pmlame0)
#' cbk.lame.stringfy(pmlame0,'isorg')
cbk.lame.stringfy <- function(pmlame,format=NULL,show=TRUE) {
  if (is.null(format)){
    ## page(pmlame) # via dput
    tempfile <- tempfile()
    dput(pmlame,file=tempfile)
  } else {
    pmlfile <- cbk.write.casteml(pmlame)
    tempfile <- cbk.convert.casteml(pmlfile,format=format)
  }
  if (show) {
    file.show(tempfile)
  }
  ## textlame <- paste(readLines(tempfile), collapse="\n")
  textlame <- readChar(tempfile,nchars=1e9) # read until 1 GB
  ## writeClipboard(textlame, format=1)

  ## return(pmlame)
  return(textlame)
}
