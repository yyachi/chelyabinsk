#' @title Download analysis records as a CASTEML file
#'
#' @description Download analysis records as a CASTEML file.  This
#'   function returns path to the file.  The file is stored in a
#'   temporary directory.
#' 
#' @param arguments Unique indentification number of stones in Medusa.
#'   Really, those will pass to `casteml download'.
#' @return Path to CASTEML file that was downloaded in temporary
#'   directory.
#' @export
#' @seealso \code{casteml download},
#'   \url{https://github.com/misasa/casteml},
#'   \code{\link{cbk.convert.casteml}}
#' @examples
#' stone <- c("20080616170000.hk","20080616170056.hk","20080616170054.hk")
#' pmlfile <- cbk.download.casteml(stone)
#'
#' pmlfile <- cbk.download.casteml("20081202172326.hkitagawa")
cbk.download.casteml <- function(arguments) {

  ## outfile <- tempfile(pattern = paste(arguments[1],"@",sep=""), fileext=".pml")
  outfile <- tempfile(fileext=".pml")
  ## system(paste("casteml download",arguments[ii],">",outfile))
  cmd     <- paste(c("casteml download",arguments),collapse=" ")
  cat(system(cmd, intern = TRUE),file=outfile,sep="\n")
  return(outfile)
}
