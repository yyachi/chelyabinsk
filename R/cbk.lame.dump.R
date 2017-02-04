#' Dump pmlame as string
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param format Format specifier that is passed to \code{casteml
#'   convert}.
#' @param show Show string in console.
#' @param verbose Output debug info (default: FALSE).
#' @param name Name of the pmlame used with option `show'.
#' @return A pmlame expressed in text
#' @export
#' @examples
#' pmlame0 <- structure(list(SiO2 = c(0.59, 0.52), Li = c(2.08e-05, 1.37e-06), Sr = c(0.000107, 3.61e-05)), row.names = c("ref-gl-tahiti", "ref-cpx-klb1"), .Names = c("SiO2", "Li", "Sr"), class = "data.frame")
#' cbk.lame.dump(pmlame0)
#' cbk.lame.dump(pmlame0,'isorg')
cbk.lame.dump <- function(pmlame,format=NULL,show=TRUE,verbose=FALSE,name=deparse(substitute(pmlame))) {
  dumpp <- is.null(format)

  if (dumpp) {
    ## page(pmlame) # via dput
    tempfile <- tempfile()
    dput(pmlame,file=tempfile)
  } else {
    pmlfile  <- cbk.write.casteml(pmlame)
    tempfile <- cbk.convert.casteml(pmlfile,format=format)
  }
  if (verbose) {
    cat(file=stderr(),"cbk.lame.dump:25: tempfile # =>",tempfile,"\n")
  }

  ## textlame <- readChar(tempfile,nchars=1e6) # read until 1 MB
  ## textlame <- paste(readLines(tempfile), collapse="\n")
  if (dumpp) {
    textlame <- paste(trimws(readLines(tempfile),which="left"),collapse="")
  } else {
    textlame <- paste((readLines(tempfile)),collapse="\n")
  }

  if (show) {
    if (dumpp) {
      ## http://stackoverflow.com/questions/24309910/how-to-get-name-of-variable-in-r-substitute
      cat(file=stderr(),name,"<-",textlame,"\n")
    } else {
      file.show(tempfile)
    }
  }

  return(textlame)
}
