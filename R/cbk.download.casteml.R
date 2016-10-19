#' @title Download analysis records as a CASTEML file
#'
#' @description Download analysis records as a CASTEML file.  This
#'   function returns path to the file.  The file is stored in a
#'   temporary directory unless specified.  Note with the same
#'   arguments, this function downloads file only once per a R
#'   session.
#' 
#' @param arguments Unique indentification number of stones in Medusa.
#'   Really, those will pass to `casteml download'.
#' @param file File path to save downloaded CASTEML file
#' @param force Force download CASTEML file
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
cbk.download.casteml <- function(arguments,file=NULL,force=FALSE) {
  cmd    <- paste(c("casteml download",arguments),collapse=" ")

  ## file <- tempfile(pattern = paste(arguments[1],"@",sep=""), fileext=".pml")
  ## system(paste("casteml download",arguments[ii],">",file))
  if(is.null(file)){
    ## file <- tempfile(fileext=".pml")
    file <- file.path(tempdir(),paste0(digest::digest(cmd,algo='md5'),".pml"))
  }

  ## Download file only when it does not exist
  if (force || !file.exists(file)) {
    cat(system(cmd, intern = TRUE),file=file,sep="\n")
  }

  return(file)
}
