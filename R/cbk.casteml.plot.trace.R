#' @title Create trace diagram from pmlfile
#'
#' @description Create trace diagram from pmlfile.  The
#'   created file is stored in current directory.  This function
#'   returns path to the file.
#' 
#' @param pmlfile file path to CASTEML file
#' @return file path to created R file
#' @export
#' @seealso \code{casteml download}, \url{https://github.com/misasa/casteml}, \code{\link{cbk.casteml.download}}
#' @examples
#' pmlfile <- cbk.casteml.download("20081202172326.hkitagawa")
#' Rfile <- cbk.casteml.plot.trace(pmlfile)
cbk.casteml.plot.trace <- function(arguments) {
  system(paste("casteml plot -c trace",arguments))
  filename <- sprintf("%s%s",tools::file_path_sans_ext(pmlfile),"_trace")
  pdffile  <- sprintf("%s%s",filename,".pdf")
  Rfile    <- sprintf("%s%s",filename,".R")
  system(paste("open",pdffile))
  return(Rfile)
}
