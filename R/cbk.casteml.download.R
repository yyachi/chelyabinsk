#' Download analysis record in temporary directory and return the file path
#' 
#' @param arguments Arguments that will pass to `casteml download'.
#'   Typically unique indentification number of stones in Medusa.
#' @return file path to CASTEML file that was downloaded in temporary
#'   directory
#' @export
#' @seealso \code{casteml download}, \url{https://github.com/misasa/casteml}, \code{\link{cbk.casteml.convert}}
cbk.casteml.download <- function(arguments) {
  ## EXAMPLES
  ## arguments <- c("20160627191317-464538","20160627191900-040404","20160627191919-895636")
  ## cbk.casteml.download(arguments)
  ## cbk.casteml.download("20160627191317-464538")
  outfile <- tempfile(pattern = paste(arguments[1],"@",sep=""), fileext=".pml")
  ## system(paste("casteml download",arguments[ii],">",outfile))
  cmd     <- paste("casteml download",arguments)
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
